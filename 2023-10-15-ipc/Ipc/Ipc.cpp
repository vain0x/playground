// Ipc.cpp : アプリケーションのエントリ ポイントを定義します。
//

// プログラムは wWinMain からスタートします

#include "pch.h"

#include "Ipc.h"
#include "framework.h"

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

static constexpr auto PIPE_BUFFER_SIZE = DWORD{0x10000};

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

	auto h_pipe = CreateNamedPipeW(
	    (LPCWSTR)name.data(), PIPE_ACCESS_DUPLEX, PIPE_TYPE_BYTE, 1,
	    PIPE_BUFFER_SIZE, PIPE_BUFFER_SIZE, 0, &security_attrs
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

inline constexpr uintptr_t IDC_INPUT = 130, IDC_BUTTON = 131;

static HWND s_main_hwnd;
static HINSTANCE s_instance;
static HWND s_input, s_send_button;
static TCHAR s_work_dir[1024];
static HANDLE s_in_read_pipe, s_out_write_pipe, s_child_process;

static auto on_start_up() -> void {
	InitCommonControls();

	GetCurrentDirectoryW(sizeof(s_work_dir) / sizeof(TCHAR) - 1, s_work_dir);
	debug(L"work_dir = %s", s_work_dir);
}

static auto on_main_window_shown() -> void {
	// create controls on the main window
	auto hwnd = s_main_hwnd;
	auto instance = s_instance;

	// input, button
	s_input = CreateWindowExW(
	    0, WC_EDITW, L"", WS_VISIBLE | WS_CHILD | WS_BORDER | WS_TABSTOP, 10,
	    10, 240, 40, hwnd, (HMENU)IDC_INPUT, s_instance, nullptr
	);
	s_send_button = CreateWindowExW(
	    0, WC_BUTTON, L"Send",
	    WS_VISIBLE | WS_CHILD | WS_TABSTOP | BS_PUSHBUTTON, 10 + 240 + 10, 10,
	    120, 40, hwnd, (HMENU)IDC_BUTTON, s_instance, nullptr
	);

	SetFocus(s_input);
}

// on WM_CREATE
static auto on_create() -> void {
	// create pipes, spawn subprocess
	auto name = OsString{s_work_dir};
	if (name.ends_with(L"\\")) {
		name.pop_back();
	}
	if (name.ends_with(L"Ipc")) {
		name.resize(name.size() - 3);
	}
	if (name.ends_with(L"\\")) {
		name.pop_back();
	}
	name += L"\\client.exe";

	auto cmdline = OsString{L"\""};
	cmdline += name;
	cmdline += L"\"";

	// クライアントとの通信用のパイプを作成する。
	// (in: サーバーからクライアントへ、out: クライアントからサーバーへ)
	auto in_read_pipe_name = compute_pipe_name("in_read");
	auto out_write_pipe_name = compute_pipe_name("out_write");
	auto in_read_pipe = create_named_pipe(in_read_pipe_name);
	auto out_write_pipe = create_named_pipe(out_write_pipe_name);

	s_in_read_pipe = in_read_pipe;
	s_out_write_pipe = out_write_pipe;

	cmdline += L" --input=";
	cmdline += in_read_pipe_name;
	cmdline += L" --output=";
	cmdline += out_write_pipe_name;

	// サーバーが使うパイプをクライアントプロセスに継承しないように設定する。
	// (標準入力への書き込みと、標準出力からの読み取り。)
	//	if (!SetHandleInformation(client_stdin_pipe_opt->write_.get(), HANDLE_FLAG_INHERIT, DWORD{})) {
	//		return std::nullopt;
	//	}
	//
	//	if (!SetHandleInformation(client_stdout_pipe_opt->read_.get(), HANDLE_FLAG_INHERIT, DWORD{})) {
	//		return std::nullopt;
	//	}

	// クライアントプロセスの標準入出力のリダイレクトを設定する。
	auto startup_info = STARTUPINFO{sizeof(STARTUPINFO)};
	auto process_info = PROCESS_INFORMATION{};

	//startup_info.hStdInput = in_read_pipe;
	//startup_info.hStdOutput = out_write_pipe;
	//startup_info.hStdError = out_write_pipe;
	//startup_info.dwFlags |= STARTF_USESTDHANDLES;

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

	// wait for connection
	debug(L"wait connection");
	{
		if (!ConnectNamedPipe(in_read_pipe, nullptr)) {
			auto e = GetLastError();
			if (e != ERROR_PIPE_CONNECTED) {
				throw error_api("ConnectNamedPipe");
			}
		}
		debug(L"in_read_pipe connected");

		if (!ConnectNamedPipe(out_write_pipe, nullptr)) {
			auto e = GetLastError();
			if (e != ERROR_PIPE_CONNECTED) {
				throw error_api("ConnectNamedPipe");
			}
		}
		debug(L"out_write_pipe connected");
	}
}

static std::string s_read_buffer;

static void try_read() {
	auto stdout_handle = s_out_write_pipe;
	auto& s_buffer = s_read_buffer;

	if (s_buffer.size() == 0) {
		s_buffer.resize(0x1000);
	}

	//DWORD peek_size;
	DWORD total_size;
	//DWORD left_size;
	if (!PeekNamedPipe(
	        stdout_handle, nullptr, 0, nullptr, &total_size, nullptr
	    )) {
		auto e = GetLastError();
		debug(L"WARN PeekNamedPipe e=%d", e);
		return;
	}
	if (total_size == 0) {
		debug(L"No data");
		return;
	}

	auto read_size = DWORD{};
	if (!ReadFile(
	        stdout_handle, s_buffer.data(), (DWORD)s_buffer.size(), &read_size,
	        LPOVERLAPPED{}
	    )) {
		auto e = GetLastError();
		if (e != ERROR_BROKEN_PIPE) {
			assert(false && "ReadFile");
		}
		debug(L"WARN ReadFile e=%d", e);
		return;
	}

	auto data = utf8_to_os_str((char8_t const*)s_buffer.data(), read_size);
	debug(L"OK receive: '%s' (%d)", data.data(), (int)data.size());
}

static void try_write(OsStringView data) {
	auto handle = s_in_read_pipe;
	auto text = os_to_utf8_str(data.data(), data.size());

	auto written_size = DWORD{};
	if (!WriteFile(
	        handle, text.data(), (DWORD)text.size(), &written_size,
	        LPOVERLAPPED{}
	    )) {
		auto e = GetLastError();
		debug(L"WriteFile e=%d", e);
		if (e != ERROR_BROKEN_PIPE) {
			assert(false && u8"WriteFile failed");
		}
		return;
	}
	debug(L"Written (%d)", (int)data.size());
}

static auto on_button_click() -> void {
	static TCHAR s_buffer[1024];
	auto size =
	    GetWindowTextW(s_input, s_buffer, sizeof(s_buffer) / sizeof(TCHAR) - 2);

	// クリックされたとき、テキストが入力済みならそれを子プロセスに送信する
	// 空欄なら読み取りを試みる

	if (size == 0) {
		try_read();
	} else {
		assert(size >= 0);
		debug(L"input: %s", s_buffer);
		try_write(OsStringView{s_buffer, (DWORD)size});
	}

	SetWindowTextW(s_input, L"");
}

static void on_destroy() {
	CloseHandle(s_in_read_pipe);
	CloseHandle(s_out_write_pipe);

	if (s_child_process) {
		if (!TerminateProcess(s_child_process, EXIT_SUCCESS)) {
			debug(L"TerminateProecss");
			assert(false && "TerminateProcess");
		}
		s_child_process = nullptr;
	}
}

// -----------------------------------------------
// Template
// -----------------------------------------------

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

	on_main_window_shown();

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
	case WM_CREATE: {
		on_create();
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
