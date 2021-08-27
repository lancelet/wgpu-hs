/*
 * SDL surface handling (macOS).
 *
 * The Haskell binding's use of 'getWindowWMInfo' is essentially useless
 * because 'SysWMInfo' is an opaque pointer. Here, we DIY for now.
 */

#define WGPUHS_TARGET_MACOS 1
#define WGPUHS_TARGET_LINUX 2
#define WGPUHS_TARGET_WINDOWS 3

#if WGPUHS_TARGET == WGPUHS_TARGET_MACOS

#include <AppKit/AppKit.h>
#include <Foundation/Foundation.h>
#include <QuartzCore/CAMetalLayer.h>
#include <SDL.h>
#include <SDL_syswm.h>

NSWindow *wgpuhs_sdl_to_ns_window(SDL_Window *window) {
  SDL_SysWMinfo info;
  SDL_VERSION(&info.version);
  if (SDL_GetWindowWMInfo(window, &info)) {
    return info.info.cocoa.window;
  } else {
    printf("WGPUHS: ERROR: Could not find macOS window from SDL window.");
    return NULL;
  }
}

#endif
