#include <stdint.h>

#include <caml/alloc.h>
#include <caml/mlvalues.h>

#if defined(_WIN32)
#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#include <psapi.h>
#elif defined(__unix__) || defined(__APPLE__)
#include <sys/resource.h>
#include <time.h>
#endif

CAMLprim value xic_monotonic_time(value unit)
{
  double seconds = 0.0;
  (void)unit;

#if defined(_WIN32)
  LARGE_INTEGER counter;
  LARGE_INTEGER frequency;
  if (QueryPerformanceFrequency(&frequency) && QueryPerformanceCounter(&counter)) {
    seconds = (double)counter.QuadPart / (double)frequency.QuadPart;
  }
#elif defined(CLOCK_MONOTONIC)
  struct timespec now;
  if (clock_gettime(CLOCK_MONOTONIC, &now) == 0) {
    seconds = (double)now.tv_sec + (double)now.tv_nsec / 1000000000.0;
  }
#endif

  return caml_copy_double(seconds);
}

CAMLprim value xic_peak_rss_bytes(value unit)
{
  uint64_t bytes = 0;
  (void)unit;

#if defined(_WIN32)
  PROCESS_MEMORY_COUNTERS counters;
  typedef BOOL(WINAPI * get_process_memory_info_fn)(
      HANDLE, PPROCESS_MEMORY_COUNTERS, DWORD);
  HMODULE kernel32 = GetModuleHandleA("kernel32.dll");
  get_process_memory_info_fn get_memory_info =
      kernel32 == NULL
          ? NULL
          : (get_process_memory_info_fn)(void *)
                GetProcAddress(kernel32, "K32GetProcessMemoryInfo");
  HMODULE psapi = NULL;

  if (get_memory_info == NULL) {
    psapi = LoadLibraryA("psapi.dll");
    if (psapi != NULL) {
      get_memory_info = (get_process_memory_info_fn)(void *)
          GetProcAddress(psapi, "GetProcessMemoryInfo");
    }
  }
  counters.cb = sizeof(counters);
  if (get_memory_info != NULL &&
      get_memory_info(GetCurrentProcess(), &counters, sizeof(counters))) {
    bytes = (uint64_t)counters.PeakWorkingSetSize;
  }
  if (psapi != NULL) {
    FreeLibrary(psapi);
  }
#elif defined(__unix__) || defined(__APPLE__)
  struct rusage usage;
  if (getrusage(RUSAGE_SELF, &usage) == 0) {
#if defined(__APPLE__)
    bytes = (uint64_t)usage.ru_maxrss;
#else
    bytes = (uint64_t)usage.ru_maxrss * UINT64_C(1024);
#endif
  }
#endif

  return caml_copy_int64((int64_t)bytes);
}
