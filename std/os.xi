// Xi standard library: os

extern f name() -> string = "xi_os_name";
extern f version() -> string = "xi_os_version";
extern f arch() -> string = "xi_os_arch";
extern f cpu_count() -> int = "xi_os_cpu_count";
extern f page_size() -> int = "xi_os_page_size";
extern f hostname() -> string = "xi_os_hostname";
extern f username() -> string = "xi_os_username";

extern f pid() -> int = "xi_os_pid";
extern f ppid() -> int = "xi_os_ppid";
extern f exe_path() -> string = "xi_os_exe_path";
extern f cwd() -> string = "xi_os_cwd";
extern f chdir(string path) -> int = "xi_os_chdir";
extern f exit(int code) -> int = "xi_os_exit";
extern f abort() -> int = "xi_os_abort";

extern f getenv(string name) -> string = "xi_os_getenv";
extern f getenv_or(string name, string fallback) -> string = "xi_os_getenv_or";
extern f has_env(string name) -> bool = "xi_os_has_env";
extern f setenv(string name, string value) -> int = "xi_os_setenv";
extern f unsetenv(string name) -> int = "xi_os_unsetenv";
extern f env_keys() -> array<string> = "xi_os_env_keys";
extern f env_values() -> array<string> = "xi_os_env_values";

extern f home_dir() -> string = "xi_os_home_dir";
extern f temp_dir() -> string = "xi_os_temp_dir";
extern f config_dir() -> string = "xi_os_config_dir";
extern f data_dir() -> string = "xi_os_data_dir";
extern f cache_dir() -> string = "xi_os_cache_dir";

extern f exists(string path) -> bool = "xi_os_exists";
extern f is_file(string path) -> bool = "xi_os_is_file";
extern f is_dir(string path) -> bool = "xi_os_is_dir";
extern f is_symlink(string path) -> bool = "xi_os_is_symlink";

extern f file_size(string path) -> int = "xi_os_file_size";
extern f file_modified_unix(string path) -> int = "xi_os_file_modified_unix";
extern f file_created_unix(string path) -> int = "xi_os_file_created_unix";
extern f file_accessed_unix(string path) -> int = "xi_os_file_accessed_unix";

extern f mkdir(string path) -> int = "xi_os_mkdir";
extern f mkdir_all(string path) -> int = "xi_os_mkdir_all";
extern f remove_file(string path) -> int = "xi_os_remove_file";
extern f remove_dir(string path) -> int = "xi_os_remove_dir";
extern f remove_dir_all(string path) -> int = "xi_os_remove_dir_all";
extern f rename(string old_path, string new_path) -> int = "xi_os_rename";
extern f copy_file(string src, string dst) -> int = "xi_os_copy_file";

extern f read_dir(string path) -> array<string> = "xi_os_read_dir";
extern f read_dir_files(string path) -> array<string> = "xi_os_read_dir_files";
extern f read_dir_dirs(string path) -> array<string> = "xi_os_read_dir_dirs";

extern f path_join(string a, string b) -> string = "xi_os_path_join";
extern f path_join3(string a, string b, string c) -> string = "xi_os_path_join3";
extern f path_basename(string path) -> string = "xi_os_path_basename";
extern f path_dirname(string path) -> string = "xi_os_path_dirname";
extern f path_extension(string path) -> string = "xi_os_path_extension";
extern f path_stem(string path) -> string = "xi_os_path_stem";
extern f path_absolute(string path) -> string = "xi_os_path_absolute";
extern f path_normalize(string path) -> string = "xi_os_path_normalize";
extern f path_is_absolute(string path) -> bool = "xi_os_path_is_absolute";
extern f path_separator() -> string = "xi_os_path_separator";

extern f run(string command) -> int = "xi_os_run";
extern f run_status(string command) -> int = "xi_os_run_status";
extern f run_capture(string command) -> string = "xi_os_run_capture";

extern f spawn(string exe, array<string> args) -> int = "xi_os_spawn";
extern f wait(int process) -> int = "xi_os_wait";
extern f kill(int process) -> int = "xi_os_kill";
extern f process_exit_code(int process) -> int = "xi_os_process_exit_code";

extern f random_bytes(int n) -> array<uint8> = "xi_os_random_bytes";
extern f random_u64() -> uint64 = "xi_os_random_u64";
extern f random_u32() -> uint32 = "xi_os_random_u32";

extern f stdin_is_tty() -> bool = "xi_os_stdin_is_tty";
extern f stdout_is_tty() -> bool = "xi_os_stdout_is_tty";
extern f stderr_is_tty() -> bool = "xi_os_stderr_is_tty";

extern f signal_ignore(string signal_name) -> int = "xi_os_signal_ignore";
extern f signal_default(string signal_name) -> int = "xi_os_signal_default";

extern f last_error() -> string = "xi_os_last_error";
extern f clear_error() -> int = "xi_os_clear_error";
