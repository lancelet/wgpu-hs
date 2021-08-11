/* Surface handling (macOS).
 *
 * Before WGPU can draw, it needs a surface. On macOS, WGPU uses the Metal API
 * from Apple, and SDL2 does not yet include any general capability to create a
 * Metal surface for a window. Here, we hack in this capability.
 *
 * Basic surface handling using GLFW is described in the examples here:
 *   https://github.com/gfx-rs/wgpu-native
 * (Also licensed under BSD-3-Clause.)
 *
 * For SDL2, the function `SDL_GetWindowWMInfo` can return a pointer to the
 * `NSWindow` underlying the SDL window:
 *   https://wiki.libsdl.org/SDL_GetWindowWMInfo
 * This function requires the `SDL_syswm.h` header.
 */

#include <SDL.h>
#include <SDL_syswm.h>
#include <AppKit/AppKit.h>
#include <Foundation/Foundation.h>
#include <QuartzCore/CAMetalLayer.h>
#include "wgpu.h"

/** Create a WGPU Surface for an SDL window on macOS.
  *
  * @param window the SDL window for which to create the surface
  * @return WGPUSurface handle if successful, or NULL if unsuccessful.
  */
WGPUSurface wgpuhs_create_surface(SDL_Window *window) {
  /* bail immediately if the window is invalid */
  if (window == NULL) {
    printf("WGPUHS: ERROR: Window handle was NULL.\n");
    return NULL;
  }

  /* fetch the NSWindow (for macOS) from SDL */
  SDL_SysWMinfo wm_info;
  SDL_VERSION(&wm_info.version);
  if (SDL_GetWindowWMInfo(window, &wm_info) != SDL_TRUE) {
    printf(
      "WGPUHS: ERROR: SDL_GetWindowWMInfo failed: %s\n",
      SDL_GetError()
    );
    return NULL;
  }
  NSWindow *ns_window = wm_info.info.cocoa.window;
  if (ns_window == NULL) {
    printf("WGPUHS: ERROR: NSWindow retrieved from SDL was NULL.\n");
  }

  /* set a metal layer on the window */
  id metal_layer = NULL;
  [ns_window.contentView setWantsLayer:YES];
  metal_layer = [CAMetalLayer layer];
  [ns_window.contentView setLayer:metal_layer];

  /* create the surface */
  WGPUChainedStruct chain_end =
    {
      .next = NULL,
      .sType = WGPUSType_SurfaceDescriptorFromMetalLayer
    };
  WGPUSurfaceDescriptorFromMetalLayer metal_surface_descriptor =
    {
      .chain = chain_end,
      .layer = metal_layer
    };
  WGPUSurfaceDescriptor surface_descriptor =
    {
      .label = NULL,
      .nextInChain = (const WGPUChainedStruct*) &metal_surface_descriptor
    };
  WGPUSurface surface = wgpuInstanceCreateSurface(NULL, &surface_descriptor);

  return surface;
}
