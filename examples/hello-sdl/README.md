# SDL3 + OpenGL RGB triangle

A native Magelang application for **Apple Silicon macOS**. SDL3 owns the window,
OpenGL context, and event loop. OpenGL 3.2 core / GLSL 150 renders a triangle with
red, green, and blue vertices and smoothly interpolated colors.

All application code and FFI bindings are Magelang; no C shim or SDL renderer is
used. The vertex shader uses `gl_VertexID` to generate three vertices, so only a
VAO is needed, not a vertex buffer. The shaders are embedded into the executable.

## Requirements

- Rust/Cargo and Apple's Xcode Command Line Tools (`clang`, macOS SDK).
- SDL3 >= 3.2 and `pkg-config`, with an **arm64** SDL3 library.
- A logged-in macOS desktop session (also required for the smoke test).

For a Homebrew installation, `sdl3` and `pkgconf` provide the dependencies.
Check your installation with `pkg-config --modversion sdl3`.

## Build and run

From the repository root:

```bash
./examples/hello-sdl/build.sh
./examples/hello-sdl/build/hello-sdl
```

The build script can be invoked from any directory. It emits unoptimized ARM64
assembly using `--backend naive --emit-asm`, then links that assembly with SDL3
and Apple's OpenGL framework using `clang`. Build artifacts stay in the ignored
`build/` directory. The compiler itself does not need any SDL-specific changes.

Resize the window freely; the viewport preserves the triangle's proportions and
uses physical framebuffer pixels on Retina displays. Close the window, press
**Escape**, or use **Cmd+Q** to quit. Resources are released with `defer`.

## Smoke test

```bash
HELLO_SDL_SMOKE_TEST=1 ./examples/hello-sdl/build/hello-sdl
```

This briefly opens the window, renders a frame, reads back pixels near each of
the three colored vertices and in the blended center, checks for GL errors,
and exits. Success prints `RGB triangle smoke test passed`; initialization,
shader compilation/linking, or pixel verification failures exit nonzero.

`main.mg` contains the application, `sdl.mg` / `gl.mg` contain the minimal native
bindings, and `triangle.vert` / `triangle.frag` contain the GLSL shaders.
OpenGL is deprecated on macOS but is still supplied by the OS.
