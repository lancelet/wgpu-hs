/*
 * Surface handling for Metal on macOS.
 */

#include <AppKit/AppKit.h>
#include <Foundation/Foundation.h>
#include <QuartzCore/CAMetalLayer.h>

void *wgpuhs_metal_layer(NSWindow *ns_window) {
  id metal_layer = NULL;
  [ns_window.contentView setWantsLayer:YES];
  metal_layer = [CAMetalLayer layer];
  [ns_window.contentView setLayer:metal_layer];
  return metal_layer;
}
