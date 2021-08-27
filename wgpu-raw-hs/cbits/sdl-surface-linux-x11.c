/*
 * SDL surface handling (Linux X11).
 *
 * The Haskell binding's used of 'getWindowWMInfo' is essentially useless
 * because 'SysWMInfo' is an opaque pointer. Here, we DIY for now.
 */

#define WGPUHS_TARGET_MACOS 1
#define WGPUHS_TARGET_LINUX 2
#define WGPUHS_TARGET_WINDOWS 3

#if WGPUHS_TARGET == WGPUHS_TARGET_LINUX

#include <SDL.h>
#include <SDL_syswm.h>

Window wgpuhs_sdl_to_x11_window(SDL_Window *window) {
  SDL_SysWMinfo info;
  SDL_VERSION(&info.version);
  if (SDL_GetWindowWMInfo(window, &info)) {
    return info.info.x11.window;
  } else {
    printf("WGPUHS: ERROR: Could not find X11 window from SDL window.");
    return 0;
  }
}

Display* wgpuhs_sdl_to_x11_display(SDL_Window *window) {
  SDL_SysWMinfo info;
  SDL_VERSION(&info.version);
  if (SDL_GetWindowWMInfo(window, &info)) {
    return info.info.x11.display;
  } else {
    printf("WGPUHS: ERROR: Could not find X11 display from SDL window.");
    return NULL;
  }
}

#endif
