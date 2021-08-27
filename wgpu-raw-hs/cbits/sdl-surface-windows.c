/*
 * SDL surface handling (Microsoft Windows).
 *
 * The Haskell binding's used of 'getWindowWMInfo' is essentially useless
 * because 'SysWMInfo' is an opaque pointer. Here, we DIY for now.
 */

#define WGPUHS_TARGET_MACOS 1
#define WGPUHS_TARGET_LINUX 2
#define WGPUHS_TARGET_WINDOWS 3

#if WGPUHS_TARGET == WGPUHS_TARGET_WINDOWS

#include <SDL.h>
#include <SDL_syswm.h>

HWND wgpuhs_sdl_to_hwnd(SDL_Window *window) {
  SDL_SysWMinfo info;
  SDL_VERSION(&info.version);
  if (SDL_GetWindowWMInfo(window, &info)) {
    return info.info.win.window;
  } else {
    printf("WGPUHS: ERROR: Could not find MS Windows window from SDL window.");
    return NULL;
  }
}

#endif
