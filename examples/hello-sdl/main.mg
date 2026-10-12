import sdl "examples/hello-sdl/sdl";
import gl "examples/hello-sdl/gl";

@embed_file("examples/hello-sdl/triangle.vert")
let vertex_source: *[u8];

@embed_file("examples/hello-sdl/triangle.frag")
let fragment_source: *[u8];

@native_import("malloc") fn malloc(size: usize): *void;
@native_import("free") fn free(ptr: *void);
@native_import("puts") fn puts(message: [*]u8): i32;
@native_import("getenv") fn getenv(name: [*]u8): [*]u8;
@native_import("exit") fn exit(code: i32);
@intrinsic("size_of") fn size_of[T](): usize;

struct ShaderInput { source: [*]u8, length: i32, status: i32 }
struct WindowState { width: i32, height: i32, vao: u32 }
struct Pixel { red: u8, green: u8, blue: u8, alpha: u8 }

fn allocate(size: usize): *void {
  let ptr = malloc(size);
  if ptr as usize == 0 {
    puts("Out of memory");
    exit(1);
  }
  return ptr;
}

fn sdl_error(message: [*]u8): bool {
  puts(message);
  puts(sdl.get_error());
  return false;
}

fn compile_shader(kind: u32, source: *[u8]): u32 {
  let shader = gl.create_shader(kind);
  if shader == 0 {
    puts("glCreateShader failed");
    return 0;
  }
  let input = allocate(size_of[ShaderInput]()) as *ShaderInput;
  defer free(input as *void);
  input.* = ShaderInput{source: source.ptr, length: source.len as i32};
  gl.shader_source(shader, 1, input.source, input.length);
  gl.compile_shader(shader);
  gl.get_shader_iv(shader, gl.COMPILE_STATUS, input.status);
  if input.status.* == 0 {
    let log = allocate(4096) as [*]u8;
    defer free(log as *void);
    log[0].* = 0;
    gl.get_shader_info_log(shader, 4096, 0 as *i32, log);
    puts("Shader compilation failed:");
    puts(log);
    gl.delete_shader(shader);
    return 0;
  }
  return shader;
}

fn create_program(): u32 {
  let vertex = compile_shader(gl.VERTEX_SHADER, vertex_source);
  if vertex == 0 { return 0; }
  defer gl.delete_shader(vertex);
  let fragment = compile_shader(gl.FRAGMENT_SHADER, fragment_source);
  if fragment == 0 { return 0; }
  defer gl.delete_shader(fragment);

  let program = gl.create_program();
  if program == 0 {
    puts("glCreateProgram failed");
    return 0;
  }
  gl.attach_shader(program, vertex);
  gl.attach_shader(program, fragment);
  gl.link_program(program);
  let status = allocate(size_of[i32]()) as *i32;
  defer free(status as *void);
  status.* = 0;
  gl.get_program_iv(program, gl.LINK_STATUS, status);
  if status.* == 0 {
    let log = allocate(4096) as [*]u8;
    defer free(log as *void);
    log[0].* = 0;
    gl.get_program_info_log(program, 4096, 0 as *i32, log);
    puts("Shader linking failed:");
    puts(log);
    gl.delete_program(program);
    return 0;
  }
  return program;
}

fn smoke_test(x: i32, y: i32, side: i32): bool {
  let pixel = allocate(size_of[Pixel]()) as *Pixel;
  defer free(pixel as *void);
  // Read the back buffer before swapping; test color interpolation, not just context creation.
  gl.read_pixels(x + side / 2, y + side * 4 / 5, 1, 1, gl.RGBA, gl.UNSIGNED_BYTE, pixel as *void);
  if pixel.red.* < 160 || pixel.green.* > 70 || pixel.blue.* > 70 { return false; }
  gl.read_pixels(x + side / 4, y + side / 4, 1, 1, gl.RGBA, gl.UNSIGNED_BYTE, pixel as *void);
  if pixel.green.* < 160 || pixel.red.* > 70 || pixel.blue.* > 70 { return false; }
  gl.read_pixels(x + side * 3 / 4, y + side / 4, 1, 1, gl.RGBA, gl.UNSIGNED_BYTE, pixel as *void);
  if pixel.blue.* < 160 || pixel.red.* > 70 || pixel.green.* > 70 { return false; }
  gl.read_pixels(x + side / 2, y + side / 2, 1, 1, gl.RGBA, gl.UNSIGNED_BYTE, pixel as *void);
  if pixel.red.* < 40 || pixel.green.* < 40 || pixel.blue.* < 40 { return false; }
  return gl.get_error() == 0;
}

fn run(): bool {
  sdl.set_main_ready();
  if !sdl.init(sdl.INIT_VIDEO) { return sdl_error("SDL_Init failed"); }
  defer sdl.quit();

  // macOS needs a core profile for modern OpenGL; 3.2 provides GLSL 150 and gl_VertexID.
  if !sdl.gl_set_attribute(sdl.GL_CONTEXT_MAJOR_VERSION, 3)
      || !sdl.gl_set_attribute(sdl.GL_CONTEXT_MINOR_VERSION, 2)
      || !sdl.gl_set_attribute(sdl.GL_CONTEXT_PROFILE_MASK, sdl.GL_CONTEXT_PROFILE_CORE)
      || !sdl.gl_set_attribute(sdl.GL_CONTEXT_FLAGS, sdl.GL_CONTEXT_FORWARD_COMPATIBLE_FLAG)
      || !sdl.gl_set_attribute(sdl.GL_DOUBLEBUFFER, 1) {
    return sdl_error("Could not configure OpenGL");
  }
  let flags = sdl.WINDOW_OPENGL | sdl.WINDOW_RESIZABLE | sdl.WINDOW_HIGH_PIXEL_DENSITY;
  let window = sdl.create_window("Magelang - SDL3 / OpenGL RGB triangle", 800, 600, flags);
  if window as usize == 0 { return sdl_error("SDL_CreateWindow failed"); }
  defer sdl.destroy_window(window);
  let context = sdl.gl_create_context(window);
  if context as usize == 0 { return sdl_error("SDL_GL_CreateContext failed"); }
  defer sdl.gl_destroy_context(context);
  let vsync = sdl.gl_set_swap_interval(1);
  puts("OpenGL version:");
  puts(gl.get_string(gl.VERSION));

  let program = create_program();
  if program == 0 { return false; }
  defer gl.delete_program(program);
  let state = allocate(size_of[WindowState]()) as *WindowState;
  defer free(state as *void);
  state.* = WindowState{};
  // Core OpenGL requires a VAO even though the shader generates vertices without a VBO.
  gl.gen_vertex_arrays(1, state.vao);
  defer gl.delete_vertex_arrays(1, state.vao);
  gl.bind_vertex_array(state.vao.*);
  gl.use_program(program);
  gl.clear_color(0.05, 0.07, 0.10, 1.0);

  let event = allocate(sdl.EVENT_SIZE);
  defer free(event);
  let smoke = getenv("HELLO_SDL_SMOKE_TEST") as usize != 0;
  puts("Close the window or press Escape to quit.");
  let running = true;
  while running {
    while sdl.poll_event(event) {
      let kind = (event as *u32).*;
      if kind == sdl.EVENT_QUIT || kind == sdl.EVENT_WINDOW_CLOSE_REQUESTED { running = false; }
    }
    if sdl.get_keyboard_state(0 as *i32)[sdl.SCANCODE_ESCAPE].* { running = false; }
    if !running { break; }
    if !sdl.get_window_size_in_pixels(window, state.width, state.height) {
      return sdl_error("Could not get framebuffer size");
    }
    let width = state.width.*;
    let height = state.height.*;
    if width <= 0 || height <= 0 {
      sdl.delay(16);
      continue;
    }
    // Keep the triangle's proportions on resize, using physical pixels on Retina displays.
    let side = width;
    if height < side { side = height; }
    let x = (width - side) / 2;
    let y = (height - side) / 2;
    gl.viewport(x, y, side, side);
    gl.clear(gl.COLOR_BUFFER_BIT);
    gl.draw_arrays(gl.TRIANGLES, 0, 3);
    if smoke && !smoke_test(x, y, side) {
      puts("RGB triangle pixel verification failed");
      return false;
    }
    if !sdl.gl_swap_window(window) { return sdl_error("SDL_GL_SwapWindow failed"); }
    if smoke {
      puts("RGB triangle smoke test passed");
      return true;
    }
    if !vsync { sdl.delay(16); }
  }
  return true;
}

@main()
fn main() {
  if !run() { exit(1); }
}
