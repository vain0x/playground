// ipc.cpp : アプリケーションのエントリ ポイントを定義します。
//

// 必ず x86 をターゲットにすること (ポインタが32ビットである必要があるため)

// プログラムは wWinMain からスタートします

#include "pch.h"

#include "resource.h"

// for CommCtrl
#pragma comment(lib, "comctl32")

// -----------------------------------------------
// Util
// -----------------------------------------------

using OsString = std::basic_string<TCHAR>;
using OsStringView = std::basic_string_view<TCHAR>;

static void debug(LPCTSTR fmt, ...) {
	va_list args;
	va_start(args, fmt);
	TCHAR buffer[1024];
	_vstprintf_s(buffer, fmt, args);
	OutputDebugStringW(buffer);
	OutputDebugStringW(L"\n");
}

static auto error(LPCTSTR fmt, ...) -> std::exception {
	va_list args;
	va_start(args, fmt);
	TCHAR buffer[1024];
	_vstprintf_s(buffer, fmt, args);
	OutputDebugStringW(buffer);
	throw std::exception{"error"};
}

static auto error_api(const char* prefix) -> std::exception {
	auto e = GetLastError();

	OutputDebugStringA(prefix);
	debug(L", err: %d", e);
	throw std::exception{prefix};
}

static auto utf8_to_os_str(char8_t const* utf8_str, std::size_t utf8_str_len)
    -> OsString {
	assert(utf8_str != nullptr);

	if (utf8_str_len == 0) {
		return OsString{};
	}

	auto len = MultiByteToWideChar(
	    CP_UTF8, 0, (char const*)utf8_str, (int)utf8_str_len, LPTSTR{}, 0
	);
	if (len == 0) {
		throw std::exception{"MultiByteToWideChar"};
	}
	assert(len > 0);

	auto os_str = OsString{};
	os_str.resize(len);

	auto result = MultiByteToWideChar(
	    CP_UTF8, 0, (char const*)utf8_str, (int)utf8_str_len, os_str.data(), len
	);
	if (result == 0) {
		throw std::exception{"MultiByteToWideChar"};
	}
	assert(result == len);

	return os_str;
}

static auto os_to_utf8_str(LPCTSTR os_str, std::size_t os_str_len)
    -> std::u8string {
	assert(os_str != nullptr);

	if (os_str_len == 0) {
		return std::u8string{};
	}

	auto len = WideCharToMultiByte(
	    CP_UTF8, 0, os_str, (int)os_str_len, nullptr, 0, nullptr, nullptr
	);
	if (len == 0) {
		throw std::exception{"WideCharToMultiByte"};
	}
	assert(len > 0);

	auto utf8_str = std::u8string{};
	utf8_str.resize(len);

	auto result = WideCharToMultiByte(
	    CP_UTF8, 0, os_str, (int)os_str_len, (char*)utf8_str.data(), len,
	    nullptr, nullptr
	);
	if (result == 0) {
		throw std::exception{"WideCharToMultiByte"};
	}
	assert(result == len);

	return utf8_str;
}

static constexpr auto PIPE_BUFFER_SIZE = DWORD{0x8000};
//static constexpr auto PIPE_BUFFER_SIZE = DWORD{8};

static auto compute_pipe_name(char const* prefix) -> OsString {
	auto pid = GetCurrentProcessId();

	char buf[128] = "";
	sprintf_s(
	    buf, "\\\\.\\pipe\\LOCAL\\KNOWBUG_PIPE_%s_%u", prefix, (unsigned)pid
	);
	auto s = std::u8string{(const char8_t*)buf};
	return utf8_to_os_str(s.data(), s.size());
}

static auto create_named_pipe(OsStringView name) -> HANDLE {
	auto security_attrs = SECURITY_ATTRIBUTES{sizeof(SECURITY_ATTRIBUTES)};
	security_attrs.bInheritHandle = TRUE;

	// ConnectNamedPipe を非同期で行うために FILE_FLAG_OVERLAPPED を指定する
	auto open_mode = (DWORD)(PIPE_ACCESS_DUPLEX | FILE_FLAG_OVERLAPPED);

	auto h_pipe = CreateNamedPipeW(
	    (LPCWSTR)name.data(), open_mode, PIPE_TYPE_BYTE, 1, PIPE_BUFFER_SIZE,
	    PIPE_BUFFER_SIZE, 0, &security_attrs
	);
	if (h_pipe == INVALID_HANDLE_VALUE) {
		throw error_api("CreateNamedPipe");
	}
	assert(h_pipe != NULL);
	return h_pipe;
}

// -----------------------------------------------
// Application
// -----------------------------------------------

inline constexpr uintptr_t IDC_INPUT = 130, IDC_BUTTON = 131, IDC_LABEL = 132;

static HWND s_main_hwnd;
static HINSTANCE s_instance;
static HWND s_input, s_send_button, s_label;
static TCHAR s_work_dir[1024];
static HANDLE s_in_read_pipe, s_out_write_pipe, s_child_process;
static HANDLE s_child_wait;
static HWND s_client_hwnd;

//static HANDLE s_in_read_connected_event, s_out_write_connected_event;
//static HANDLE s_in_read_connected_wait, s_out_write_connected_wait, ;
static bool s_child_exited;
//static bool s_in_read_connected, s_out_write_connected;

// アプリの開始時、メインウィンドウの生成前に呼ばれる
static auto on_start_up() -> void {
	InitCommonControls();

	GetCurrentDirectoryW(sizeof(s_work_dir) / sizeof(TCHAR) - 1, s_work_dir);
	debug(L"work_dir = %s", s_work_dir);
}

static void init_main_window(HWND hwnd) {
	auto instance = s_instance;

	// input, button, label
	s_input = CreateWindowExW(
	    0, WC_EDITW, L"", WS_VISIBLE | WS_CHILD | WS_BORDER | WS_TABSTOP, 10,
	    10, 240, 40, hwnd, (HMENU)IDC_INPUT, s_instance, nullptr
	);
	s_send_button = CreateWindowExW(
	    0, WC_BUTTON, L"Send",
	    WS_VISIBLE | WS_CHILD | WS_TABSTOP | BS_PUSHBUTTON, 10 + 240 + 10, 10,
	    120, 40, hwnd, (HMENU)IDC_BUTTON, s_instance, nullptr
	);
	//EnableWindow(s_send_button, FALSE);
	s_label = CreateWindowExW(
	    0, WC_STATICW, L"...", WS_VISIBLE | WS_CHILD, 10, 10 + 8 + 40 + 8, 240,
	    40, hwnd, (HMENU)IDC_LABEL, instance, nullptr
	);

	SetFocus(s_input);
}

// (WAITORTIMERCALLBACK)
static void CALLBACK
on_child_process_signaled(PVOID context, BOOLEAN _is_timeout) {
	debug(L"on_child_process_signaled");

	auto id = (uintptr_t)context;
	PostMessageW(s_main_hwnd, WM_APP, 1, (LPARAM)id);
}

// on WM_CREATE
static auto on_create(HWND hwnd) -> void {
	s_main_hwnd = hwnd;
	init_main_window(hwnd);

	// create pipes, spawn subprocess
	auto work_dir = OsString{s_work_dir};
	{
		auto index = work_dir.find(L"-ipc");
		assert(index != std::u8string::npos);
		work_dir.erase(index + 4);
	}
	auto name = work_dir;
	name += L"\\client.exe";

	auto cmdline = OsString{L"\""};
	cmdline += name;
	cmdline += L"\"";

	// クライアントとの通信用のパイプを作成する。
	// (in: サーバーからクライアントへ、out: クライアントからサーバーへ)
	//auto in_read_pipe_name = compute_pipe_name("in_read");
	//auto out_write_pipe_name = compute_pipe_name("out_write");
	//auto in_read_pipe = create_named_pipe(in_read_pipe_name);
	//auto out_write_pipe = create_named_pipe(out_write_pipe_name);

	//s_in_read_pipe = in_read_pipe;
	//s_out_write_pipe = out_write_pipe;

	//cmdline += L" --input=";
	//cmdline += in_read_pipe_name;
	//cmdline += L" --output=";
	//cmdline += out_write_pipe_name;

	OsString hwnd_str;
	{
		auto s = std::to_string((unsigned long long)hwnd);
		hwnd_str = utf8_to_os_str((const char8_t*)s.data(), s.size());
	}
	cmdline += L" --server-hwnd=";
	cmdline += hwnd_str;

	auto startup_info = STARTUPINFO{sizeof(STARTUPINFO)};
	auto process_info = PROCESS_INFORMATION{};

	// クライアントプロセスを起動する。
	if (!CreateProcessW(
	        LPCTSTR{}, cmdline.data(),
	        // process security
	        nullptr,
	        // thread security
	        nullptr,
	        // inherit handles
	        TRUE, NORMAL_PRIORITY_CLASS,
	        // env
	        nullptr,
	        // work dir
	        nullptr, &startup_info, &process_info
	    )) {
		throw error_api("CreateProcessW");
	}
	debug(L"spawned");

	CloseHandle(process_info.hThread);
	s_child_process = process_info.hProcess;

	if (!RegisterWaitForSingleObject(
	        &s_child_wait, s_child_process, on_child_process_signaled,
	        (LPVOID)1, INFINITE, WT_EXECUTEONLYONCE
	    )) {
		auto err = GetLastError();
		throw error(L"RegisterWaitForSingleObject %d", err);
	}

	//debug(L"wait for connected or process exit");
	//{
	//	static auto s_ol1 = OVERLAPPED{};
	//	static auto s_ol2 = OVERLAPPED{};

	//	s_in_read_connected_event = CreateEventA(nullptr, TRUE, FALSE, nullptr);
	//	if (!s_in_read_connected_event ||
	//	    s_in_read_connected_event == INVALID_HANDLE_VALUE) {
	//		auto err = GetLastError();
	//		throw error(L"CreateEvent %d", err);
	//	}
	//	s_ol1.hEvent = s_in_read_connected_event;
	//	if (!ConnectNamedPipe(s_in_read_pipe, &s_ol1)) {
	//		auto err = GetLastError();
	//		if (err == ERROR_PIPE_CONNECTED) {
	//			// OK
	//			s_in_read_connected = true;
	//		} else if (err == ERROR_IO_PENDING) {
	//			debug(L"  begin waiting for in_read");
	//		} else {
	//			throw error_api("ConnectNamedPipe");
	//		}
	//	} else {
	//		debug(L"  ConnectedNamedPipe succeeded (in_read)");
	//	}

	//	s_out_write_connected_event =
	//	    CreateEventA(nullptr, TRUE, FALSE, nullptr);
	//	if (!s_out_write_connected_event ||
	//	    s_out_write_connected_event == INVALID_HANDLE_VALUE) {
	//		auto err = GetLastError();
	//		throw error(L"CreateEvent %d", err);
	//	}
	//	s_ol2.hEvent = s_out_write_connected_event;
	//	if (!ConnectNamedPipe(s_out_write_pipe, &s_ol2)) {
	//		auto err = GetLastError();
	//		if (err == ERROR_PIPE_CONNECTED) {
	//			// OK
	//			s_out_write_connected = true;
	//		} else if (err == ERROR_IO_PENDING) {
	//			debug(L"  begin waiting for out_write");
	//		} else {
	//			throw error_api("ConnectNamedPipe");
	//		}
	//	} else {
	//		debug(L"  ConnectNamedPipe completed (out_write)");
	//	}

	//	// ブロックしてパイプへの接続を待つ
	//	// 接続完了とクライアントの終了を同時に待つ
	//	// (そうでないとクライアントが終了した場合に接続待ちがハングしてしまう)
	//	if (!s_in_read_connected) {
	//		debug(L"block waiting for in_read pipe connection");
	//		HANDLE handles[2] = {
	//		    s_child_process,
	//		    s_in_read_connected_event,
	//		};
	//		// wait any
	//		auto status = WaitForMultipleObjects(
	//		    sizeof(handles) / sizeof(HANDLE), handles, FALSE, (DWORD)5000
	//		);
	//		if (status == WAIT_OBJECT_0 || status == WAIT_ABANDONED_0) {
	//			// child exited
	//			debug(L"  detect child exit");
	//			return;
	//		} else if (status == WAIT_OBJECT_0 + 1) {
	//			// connected
	//			debug(L"  detect in_read connected");
	//		} else if (status == WAIT_ABANDONED_0 + 1) {
	//			// pipe broken?
	//			debug(L"  in_read pipe abandoned");
	//			throw std::exception{"pipe abandoned"};
	//		} else if (status == WAIT_TIMEOUT) {
	//			debug(L"  waiting for client timeout");
	//			throw std::exception{"wait timeout"};
	//		} else {
	//			auto err = GetLastError();
	//			throw error(L"WaitForMultipleObjects %d", err);
	//		}
	//		s_in_read_connected = true;
	//	}

	//	if (!s_out_write_connected) {
	//		debug(L"block waiting for out_write pipe connection");
	//		HANDLE handles[2] = {
	//		    s_child_process,
	//		    s_out_write_connected_event,
	//		};
	//		// wait any
	//		auto status = WaitForMultipleObjects(
	//		    sizeof(handles) / sizeof(HANDLE), handles, FALSE, (DWORD)5000
	//		);
	//		if (status == WAIT_OBJECT_0 || status == WAIT_ABANDONED_0) {
	//			// child exited
	//			debug(L"  detect child exit");
	//			return;
	//		} else if (status == WAIT_OBJECT_0 + 1) {
	//			// connected
	//			debug(L"  detect out_write connected");
	//		} else if (status == WAIT_ABANDONED_0 + 1) {
	//			// pipe broken?
	//			debug(L"  out_write pipe abandoned");
	//			throw std::exception{"pipe abandoned"};
	//		} else if (status == WAIT_TIMEOUT) {
	//			debug(L"  waiting for client timeout");
	//			throw std::exception{"wait timeout"};
	//		} else {
	//			auto err = GetLastError();
	//			throw error(L"WaitForMultipleObjects %d", err);
	//		}
	//		s_out_write_connected = true;
	//	}
	//}
}

// パイプからの読み取りを非同期で開始する
static void read_output(DWORD size) {
	//static std::string s_buffer;
	//if (s_buffer.size() < size) {
	//	s_buffer.resize(size);
	//}

	//auto h_pipe = s_out_write_pipe;

	//auto read_size = DWORD{};
	//if (!ReadFile(
	//        h_pipe, s_buffer.data(), (DWORD)s_buffer.size(), &read_size,
	//        LPOVERLAPPED{}
	//    )) {
	//	auto err = GetLastError();
	//	if (err != ERROR_BROKEN_PIPE) {
	//		throw error_api("ReadFileEx");
	//	}
	//	debug(L"ReadFile: broken pipe");
	//	return;
	//}

	//auto data = utf8_to_os_str((char8_t const*)s_buffer.data(), read_size);
	//debug(L"OK receive: '%s' (%d)", data.data(), (int)data.size());

	//{
	//	static OsString s;
	//	s = L"client written: '";
	//	s += data;
	//	s += L"'";
	//	SetWindowTextW(s_label, s.data());
	//}
}

static void do_write_to_input(OsStringView data) {
	// 事前条件
	assert(s_client_hwnd != nullptr && !s_child_exited);

	//auto h_pipe = s_in_read_pipe;
	auto text = os_to_utf8_str(data.data(), data.size());

	//auto written_size = DWORD{};
	//if (!WriteFile(
	//        h_pipe, text.data(), (DWORD)text.size(), &written_size,
	//        LPOVERLAPPED{}
	//    )) {
	//	auto e = GetLastError();
	//	debug(L"WriteFile e=%d", e);
	//	if (e != ERROR_BROKEN_PIPE) {
	//		assert(false && u8"WriteFile failed");
	//	}
	//	return;
	//}
	//debug(L"Written (%d)", (int)data.size());

	// クライアントに書き込んだことを伝える
	//assert(written_size <= INT32_MAX);
	//SendMessage(s_client_hwnd, WM_APP, 201, (LPARAM)written_size);

	auto copyData = COPYDATASTRUCT{};
	copyData.cbData = (DWORD)text.size();
	copyData.lpData = text.data();
	SendMessage(s_client_hwnd, WM_COPYDATA, 0, (LPARAM)&copyData);
}

// クライアントとの接続が確立する前に送ろうとしたメッセージを溜めこむためのバッファー
static std::vector<OsString> s_send_message_pending_;

static void enqueue_write_to_input(OsString data) {
	if (s_client_hwnd == nullptr) {
		s_send_message_pending_.push_back(std::move(data));
		return;
	}

	if (s_child_exited) {
		debug(L"cannot write after child exit");
		return;
	}

	assert(s_client_hwnd != nullptr && !s_child_exited);
	do_write_to_input(data);
}

static auto on_button_click() -> void {
	static TCHAR s_buffer[1024];
	auto size =
	    GetWindowTextW(s_input, s_buffer, sizeof(s_buffer) / sizeof(TCHAR) - 2);

	// クリックされたとき、テキストが入力済みならそれを子プロセスに送信する
	// 空欄なら読み取りを試みる

	if (size != 0) {
		assert(size >= 0);
		debug(L"input: %s", s_buffer);
		enqueue_write_to_input(OsString{s_buffer, (DWORD)size});
	}

	SetWindowTextW(s_input, L"");
	SetFocus(s_input);
}

static void on_destroy() {
	debug(L"on_destroy");
	CloseHandle(s_in_read_pipe);
	CloseHandle(s_out_write_pipe);

	// 子プロセスをキルする
	if (s_child_process) {
		if (!TerminateProcess(s_child_process, EXIT_SUCCESS)) {
			auto err = GetLastError();
			if (err != ERROR_ACCESS_DENIED) {
				debug(L"TerminateProcess, err=%d", err);
				assert(false && "TerminateProcess");
			}
		}
		s_child_process = nullptr;
	}
}

// WM_APP が送られたときに呼ばれる
static void on_wm_app(WPARAM wp, LPARAM lp) {
	debug(L"WM_APP %d", wp);

	switch (wp) {
	case 1: {
		// wait signaled
		auto id = (DWORD)lp;
		switch (id) {
		case 1:
			s_child_exited = true;
			SetWindowTextW(s_label, L"client exited");
			break;
		//case 2:
		//	s_in_read_connected = true;
		//	break;
		//case 3:
		//	s_out_write_connected = true;
		//	break;
		default:
			throw error(L"unknown event id");
		}
		break;
	}

		// 以下のメッセージはクライアントから送られる

	case 101: {
		// クライアントが起動時に送ってくる
		debug(L"On client_hwnd: %d", lp);
		SetWindowTextW(s_label, L"client ready");
		s_client_hwnd = (HWND)lp;
		assert(s_client_hwnd != nullptr);

		if (!s_send_message_pending_.empty()) {
			debug(L" writing pending messages");
		}
		//assert(s_out_write_connected);
		for (auto&& message : s_send_message_pending_) {
			do_write_to_input(std::move(message));
		}
		s_send_message_pending_.clear();

		//EnableWindow(s_send_button, TRUE);
		break;
	}
	//case 102: {
	//	// クライアントがパイプにメッセージを書き込んだ後に送ってくる
	//	auto size = (DWORD)lp;
	//	debug(L"On client written: %d", size);
	//	assert(s_client_hwnd != nullptr);

	//	read_output(size);
	//	break;
	//}
	case 103: {
		// クライアントの終了時に送られてくる

		PostMessageW(s_main_hwnd, WM_CLOSE, 0, 0);
		break;
	}

		// 以下のメッセージはランタイムのイベント発生をシミュレートするために送られる
	case 201: {
		debug(L"On runtime event");

		enqueue_write_to_input(OsString{L"logmes called"});
		break;
	}
	default:
		debug(L"unknown WM_APP message %d", wp);
		assert(false && "on_wm_app");
	}
}

static void on_copy_data(COPYDATASTRUCT const* copy_data) {
	auto data = std::u8string_view{
	    (char8_t const*)copy_data->lpData, (std::size_t)copy_data->cbData};

	debug(L"OK receive: '%s' (%d)", data.data(), (int)data.size());

	{
		static OsString s;
		s = L"client written: '";
		s += utf8_to_os_str(data.data(), data.size());
		s += L"'";
		SetWindowTextW(s_label, s.data());
	}
}

// ===============================================

// 以下、ほとんど自動生成されたテンプレートコード

#define MAX_LOADSTRING 100

// グローバル変数:
HINSTANCE hInst;                     // 現在のインターフェイス
WCHAR szTitle[MAX_LOADSTRING];       // タイトル バーのテキスト
WCHAR szWindowClass[MAX_LOADSTRING]; // メイン ウィンドウ クラス名

// このコード モジュールに含まれる関数の宣言を転送します:
ATOM MyRegisterClass(HINSTANCE hInstance);
BOOL InitInstance(HINSTANCE, int);
LRESULT CALLBACK WndProc(HWND, UINT, WPARAM, LPARAM);
INT_PTR CALLBACK About(HWND, UINT, WPARAM, LPARAM);

int APIENTRY wWinMain(
    _In_ HINSTANCE hInstance, _In_opt_ HINSTANCE hPrevInstance,
    _In_ LPWSTR lpCmdLine, _In_ int nCmdShow
) {
	UNREFERENCED_PARAMETER(hPrevInstance);
	UNREFERENCED_PARAMETER(lpCmdLine);

	// TODO: ここにコードを挿入してください。

	// グローバル文字列を初期化する
	LoadStringW(hInstance, IDS_APP_TITLE, szTitle, MAX_LOADSTRING);
	LoadStringW(hInstance, IDC_IPC, szWindowClass, MAX_LOADSTRING);
	MyRegisterClass(hInstance);

	// アプリケーション初期化の実行:
	if (!InitInstance(hInstance, nCmdShow)) {
		return FALSE;
	}

	HACCEL hAccelTable = LoadAccelerators(hInstance, MAKEINTRESOURCE(IDC_IPC));

	MSG msg;

	// メイン メッセージ ループ:
	while (GetMessage(&msg, nullptr, 0, 0)) {
		//debug(L"Msg %d w=%d l=%d", msg.message, msg.wParam, msg.lParam);
		//if (!TranslateAccelerator(msg.hwnd, hAccelTable, &msg))
		if (!IsDialogMessage(s_main_hwnd, &msg)) {
			TranslateMessage(&msg);
			DispatchMessage(&msg);
		}
	}

	return (int)msg.wParam;
}

//
//  関数: MyRegisterClass()
//
//  目的: ウィンドウ クラスを登録します。
//
ATOM MyRegisterClass(HINSTANCE hInstance) {
	WNDCLASSEXW wcex;

	wcex.cbSize = sizeof(WNDCLASSEX);

	wcex.style = CS_HREDRAW | CS_VREDRAW;
	wcex.lpfnWndProc = WndProc;
	wcex.cbClsExtra = 0;
	wcex.cbWndExtra = 0;
	wcex.hInstance = hInstance;
	wcex.hIcon = LoadIcon(hInstance, MAKEINTRESOURCE(IDI_IPC));
	wcex.hCursor = LoadCursor(nullptr, IDC_ARROW);
	wcex.hbrBackground = (HBRUSH)(COLOR_WINDOW + 1);
	wcex.lpszMenuName = MAKEINTRESOURCEW(IDC_IPC);
	wcex.lpszClassName = szWindowClass;
	wcex.hIconSm = LoadIcon(wcex.hInstance, MAKEINTRESOURCE(IDI_SMALL));

	return RegisterClassExW(&wcex);
}

//
//   関数: InitInstance(HINSTANCE, int)
//
//   目的: インスタンス ハンドルを保存して、メイン ウィンドウを作成します
//
//   コメント:
//
//        この関数で、グローバル変数でインスタンス ハンドルを保存し、
//        メイン プログラム ウィンドウを作成および表示します。
//
BOOL InitInstance(HINSTANCE hInstance, int nCmdShow) {
	hInst = hInstance; // グローバル変数にインスタンス ハンドルを格納する

	on_start_up();

	HWND hWnd = CreateWindowW(
	    szWindowClass, szTitle, WS_OVERLAPPEDWINDOW, CW_USEDEFAULT,
	    CW_USEDEFAULT, 480, 480, nullptr, nullptr, hInstance, nullptr
	);

	if (!hWnd) {
		return FALSE;
	}

	s_instance = hInst;
	s_main_hwnd = hWnd;

	ShowWindow(hWnd, nCmdShow);
	UpdateWindow(hWnd);

	return TRUE;
}

//
//  関数: WndProc(HWND, UINT, WPARAM, LPARAM)
//
//  目的: メイン ウィンドウのメッセージを処理します。
//
//  WM_COMMAND  - アプリケーション メニューの処理
//  WM_PAINT    - メイン ウィンドウを描画する
//  WM_DESTROY  - 中止メッセージを表示して戻る
//
//
LRESULT CALLBACK
WndProc(HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam) {
	switch (message) {
	case WM_COMMAND: {
		int wmId = LOWORD(wParam);
		int hi = HIWORD(wParam);

		if (wmId == IDC_BUTTON) {
			if (hi == BN_CLICKED) {
				on_button_click();
				break;
			}
			goto DEFAULT;
		}

		// 選択されたメニューの解析:
		switch (wmId) {
		case IDM_ABOUT:
			DialogBox(hInst, MAKEINTRESOURCE(IDD_ABOUTBOX), hWnd, About);
			break;
		case IDM_EXIT:
			DestroyWindow(hWnd);
			break;
		default:
			return DefWindowProc(hWnd, message, wParam, lParam);
		}
	} break;
	case WM_PAINT: {
		PAINTSTRUCT ps;
		HDC hdc = BeginPaint(hWnd, &ps);
		// TODO: HDC を使用する描画コードをここに追加してください...
		EndPaint(hWnd, &ps);
	} break;
	case WM_APP: {
		on_wm_app(wParam, lParam);
		break;
	}
	case WM_COPYDATA: {
		on_copy_data((COPYDATASTRUCT const*)lParam);
		break;
	}
	case WM_CREATE: {
		on_create(hWnd);
		break;
	}
	case WM_DESTROY:
		on_destroy();
		PostQuitMessage(0);
		break;
	default:
	DEFAULT:
		return DefWindowProc(hWnd, message, wParam, lParam);
	}
	return 0;
}

// バージョン情報ボックスのメッセージ ハンドラーです。
INT_PTR CALLBACK About(HWND hDlg, UINT message, WPARAM wParam, LPARAM lParam) {
	UNREFERENCED_PARAMETER(lParam);
	switch (message) {
	case WM_INITDIALOG:
		return (INT_PTR)TRUE;

	case WM_COMMAND:
		if (LOWORD(wParam) == IDOK || LOWORD(wParam) == IDCANCEL) {
			EndDialog(hDlg, LOWORD(wParam));
			return (INT_PTR)TRUE;
		}
		break;
	}
	return (INT_PTR)FALSE;
}
