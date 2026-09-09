// Xi standard library: win32.

#link "user32"
#link "gdi32"

// --- basics ----------------------------------------------------------------
cextern f system_metrics(int32 index) -> int32 = "GetSystemMetrics";
cextern f message_box(uint64 hwnd, string text, string caption, uint32 flags) -> int32 = "MessageBoxA";

// --- module / class / cursor / brush ---------------------------------------
cextern f get_module_handle(cptr name) -> cptr = "GetModuleHandleW";
cextern f load_cursor(cptr hinst, cptr name) -> cptr = "LoadCursorW";
cextern f get_sys_color_brush(int32 index) -> cptr = "GetSysColorBrush";
cextern f register_class(cptr wndclass) -> uint16 = "RegisterClassW";

// --- windows ---------------------------------------------------------------
cextern f create_window_ex(uint32 ex_style, cptr class_name, cptr window_name, uint32 style,
                           int32 x, int32 y, int32 w, int32 h,
                           cptr parent, cptr menu, cptr hinst, cptr param) -> cptr = "CreateWindowExW";
cextern f show_window(cptr hwnd, int32 cmd) -> int32 = "ShowWindow";
cextern f update_window(cptr hwnd) -> int32 = "UpdateWindow";
cextern f set_window_text(cptr hwnd, cptr text) -> int32 = "SetWindowTextW";

// Per-window state: stash a LONG_PTR (e.g. a counter) with index GWLP_USERDATA (-21).
cextern f set_window_long_ptr(cptr hwnd, int32 index, int64 value) -> int64 = "SetWindowLongPtrW";
cextern f get_window_long_ptr(cptr hwnd, int32 index) -> int64 = "GetWindowLongPtrW";

// --- message loop ----------------------------------------------------------
cextern f get_message(cptr msg, cptr hwnd, uint32 min, uint32 max) -> int32 = "GetMessageW";
cextern f translate_message(cptr msg) -> int32 = "TranslateMessage";
cextern f dispatch_message(cptr msg) -> int64 = "DispatchMessageW";
cextern f def_window_proc(cptr hwnd, uint32 msg, uint64 wparam, int64 lparam) -> int64 = "DefWindowProcW";
cextern f post_quit_message(int32 code) -> void = "PostQuitMessage";

// --- messages / fonts ------------------------------------------------------
// General control message channel (WM_SETFONT, WM_GETTEXT, BM_SETCHECK, …).
cextern f send_message(cptr hwnd, uint32 msg, uint64 wparam, int64 lparam) -> int64 = "SendMessageW";

// CreateFontW — the full LOGFONT-style constructor (GDI). Returns an HFONT.
// A negative `height` requests a character height in logical units; `weight` is
// 400 = normal, 700 = bold. Pass 0 for the "don't care" fields and 1 for charset
// (DEFAULT_CHARSET). `face` is a wide (UTF-16) font-name string.
cextern f create_font(int32 height, int32 width, int32 escapement, int32 orientation,
                      int32 weight, uint32 italic, uint32 underline, uint32 strikeout,
                      uint32 charset, uint32 out_precision, uint32 clip_precision,
                      uint32 quality, uint32 pitch_family, cptr face) -> cptr = "CreateFontW";
