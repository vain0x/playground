// プリコンパイルヘッダー

#pragma once

// C言語のヘッダーファイル
#include <cassert>
#include <cstdarg>
#include <cstddef>
#include <cstdint>

// C++の標準ライブラリ (汎用的なもの)
#include <exception>
#include <functional>
#include <memory>
#include <string>

// ===============================================

// Windows のヘッダーファイル

#include "targetver.h"

// Windows ヘッダーからほとんど使用されていない部分を除外する
#define WIN32_LEAN_AND_MEAN

#include <windows.h>

// C ランタイム ヘッダー ファイル
//#include <malloc.h>
//#include <memory.h>
//#include <stdlib.h>
#include <tchar.h>

#include <CommCtrl.h>
