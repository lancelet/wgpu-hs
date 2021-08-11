/* Surface handling
 *
 * Knowledge of how to handle surfaces was basically copied from:
 *   https://github.com/gfx-rs/wgpu-native
 * (Also licensed under BSD-3-Clause.)
 */

#include "wgpu.h"
#include "SDL.h"

#define WGPU_TARGET_MACOS 1

