#include "wgpu.h"

void wgpuhs_request_adapter_callback(WGPUAdapter received, void* userdata) {
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
    wgpuhs_request_adapter_callback,
    (void*)&adapter
  );
  return adapter;
}

void wgpuhs_request_device_callback(WGPUDevice received, void *userdata) {
  *(WGPUDevice*)userdata = received;
}
