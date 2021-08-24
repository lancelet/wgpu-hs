/*
 * Logging callback.
 *
 * If the logging function is implemented in Haskell, then all of the API
 * functions must be marked as "safe", since they could potentially call back
 * into the GHC runtime to invoke the logging callback. To avoid this, implement
 * the logging callback in C.
 */

#include <stdio.h>
#include "wgpu.h"

void wgpuhs_logging_callback(WGPULogLevel level, const char *msg) {
  char* level_str;
  switch (level) {
    case WGPULogLevel_Error: level_str = "Error"; break;
    case WGPULogLevel_Warn: level_str = "Warn"; break;
    case WGPULogLevel_Info: level_str = "Info"; break;
    case WGPULogLevel_Debug: level_str = "Debug"; break;
    case WGPULogLevel_Trace: level_str = "Trace"; break;
    default: level_str = "Unknown";
  }
  printf("[%s] %s\n", level_str, msg);
}
