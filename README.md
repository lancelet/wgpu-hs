# wgpu-hs: Haskell bindings for wgpu-native

## Building

[`wgpu-native`](https://github.com/gfx-rs/wgpu-native) is still under
development, so it is provided as a git submodule.

To compile both `wgpu-native` and the Haskell code:

``` sh
# checkout the repo, and the submodule reference to wgpu-native
git clone https://github.com/lancelet/wgpu-hs.git
cd wgpu-hs
git submodule update --init --recursive

# compile wgpu-native; this will require a Rust compiler
pushd wgpu-native; make all; popd

# provide links so cabal can find the wgpu-native libraries
echo "extra-lib-dirs: $(pwd)/wgpu-native/target/debug" > cabal.project.local
echo "extra-include-dirs: $(pwd)/wgpu-native/ffi" >> cabal.project.local

# build the project and test an example
cabal build all
cabal run triangle
```
