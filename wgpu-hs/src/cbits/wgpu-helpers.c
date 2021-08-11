#include "wgpu.h"

void request_adapter_callback(WGPUAdapter received, void* userdata) {
  *(WGPUAdapter*)userdata = received;
}

WGPUAdapter wgpuhs_request_compatible_adapter(WGPUSurface surface) {
  WGPURequestAdapterOptions request_adapter_options =
    {
      .nextInChain = NULL,
      .compatibleSurface = surface
    };
  WGPUAdapter adapter;
  wgpuInstanceRequestAdapter(
    NULL,
    &request_adapter_options,
    request_adapter_callback,
    (void*)&adapter
  );
  return adapter;
}
