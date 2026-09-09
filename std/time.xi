// Xi standard library: time

extern f now() -> int = "xi_time_now";
extern f now_unix() -> int = "xi_time_now_unix";
extern f now_unix_ms() -> int = "xi_time_now_unix_ms";
extern f now_unix_us() -> int = "xi_time_now_unix_us";
extern f now_unix_ns() -> int = "xi_time_now_unix_ns";
extern f mono() -> int = "xi_time_mono";
extern f mono_ms() -> int = "xi_time_mono_ms";
extern f mono_us() -> int = "xi_time_mono_us";
extern f mono_ns() -> int = "xi_time_mono_ns";

extern f sleep(int seconds) -> int = "xi_time_sleep";
extern f sleep_ms(int ms) -> int = "xi_time_sleep_ms";
extern f sleep_us(int us) -> int = "xi_time_sleep_us";
extern f sleep_ns(int ns) -> int = "xi_time_sleep_ns";
extern f sleep_until(int unix_ms) -> int = "xi_time_sleep_until";

extern f timer_start() -> int = "xi_time_timer_start";
extern f timer_elapsed(int timer) -> int = "xi_time_timer_elapsed";
extern f timer_elapsed_ms(int timer) -> int = "xi_time_timer_elapsed_ms";
extern f timer_elapsed_us(int timer) -> int = "xi_time_timer_elapsed_us";
extern f timer_elapsed_ns(int timer) -> int = "xi_time_timer_elapsed_ns";
extern f timer_reset(ref int timer) -> int = "xi_time_timer_reset";

extern f deadline_after_ms(int ms) -> int = "xi_time_deadline_after_ms";
extern f deadline_after_us(int us) -> int = "xi_time_deadline_after_us";
extern f deadline_after_ns(int ns) -> int = "xi_time_deadline_after_ns";
extern f deadline_expired(int deadline) -> bool = "xi_time_deadline_expired";
extern f deadline_remaining_ms(int deadline) -> int = "xi_time_deadline_remaining_ms";
extern f deadline_remaining_ns(int deadline) -> int = "xi_time_deadline_remaining_ns";

extern f iso_now() -> string = "xi_time_iso_now";
extern f iso_from_unix(int seconds) -> string = "xi_time_iso_from_unix";
extern f format_unix(int seconds, string fmt) -> string = "xi_time_format_unix";
extern f parse_unix(string text, string fmt) -> int = "xi_time_parse_unix";

extern f year() -> int = "xi_time_year";
extern f month() -> int = "xi_time_month";
extern f day() -> int = "xi_time_day";
extern f hour() -> int = "xi_time_hour";
extern f minute() -> int = "xi_time_minute";
extern f second() -> int = "xi_time_second";
extern f weekday() -> int = "xi_time_weekday";
extern f yearday() -> int = "xi_time_yearday";

extern f year_from_unix(int seconds) -> int = "xi_time_year_from_unix";
extern f month_from_unix(int seconds) -> int = "xi_time_month_from_unix";
extern f day_from_unix(int seconds) -> int = "xi_time_day_from_unix";
extern f hour_from_unix(int seconds) -> int = "xi_time_hour_from_unix";
extern f minute_from_unix(int seconds) -> int = "xi_time_minute_from_unix";
extern f second_from_unix(int seconds) -> int = "xi_time_second_from_unix";
extern f weekday_from_unix(int seconds) -> int = "xi_time_weekday_from_unix";
extern f yearday_from_unix(int seconds) -> int = "xi_time_yearday_from_unix";

extern f is_leap_year(int year) -> bool = "xi_time_is_leap_year";
extern f days_in_month(int year, int month) -> int = "xi_time_days_in_month";

extern f make_unix(int year, int month, int day, int hour, int minute, int second) -> int = "xi_time_make_unix";

extern f add_seconds(int unix_seconds, int seconds) -> int = "xi_time_add_seconds";
extern f add_minutes(int unix_seconds, int minutes) -> int = "xi_time_add_minutes";
extern f add_hours(int unix_seconds, int hours) -> int = "xi_time_add_hours";
extern f add_days(int unix_seconds, int days) -> int = "xi_time_add_days";
extern f diff_seconds(int unix_a, int unix_b) -> int = "xi_time_diff_seconds";
extern f diff_ms(int unix_ms_a, int unix_ms_b) -> int = "xi_time_diff_ms";

extern f timezone_name() -> string = "xi_time_timezone_name";
extern f timezone_offset_seconds() -> int = "xi_time_timezone_offset_seconds";
extern f utc_offset_seconds() -> int = "xi_time_utc_offset_seconds";
