let INIT_VIDEO: u32 = 0x20;
let WINDOW_OPENGL: u64 = 0x2;
let WINDOW_RESIZABLE: u64 = 0x20;
let WINDOW_HIGH_PIXEL_DENSITY: u64 = 0x2000;

let GL_DOUBLEBUFFER: i32 = 5;
let GL_CONTEXT_MAJOR_VERSION: i32 = 17;
let GL_CONTEXT_MINOR_VERSION: i32 = 18;
let GL_CONTEXT_FLAGS: i32 = 19;
let GL_CONTEXT_PROFILE_MASK: i32 = 20;
let GL_CONTEXT_PROFILE_CORE: i32 = 1;
let GL_CONTEXT_FORWARD_COMPATIBLE_FLAG: i32 = 2;

let EVENT_QUIT: u32 = 0x100;
let EVENT_WINDOW_CLOSE_REQUESTED: u32 = 0x210;
// SDL3 fixes SDL_Event's union size at 128 bytes; its first member is a Uint32 type.
let EVENT_SIZE: usize = 128;
let SCANCODE_ESCAPE: usize = 41;

@native_import("SDL_SetMainReady")
fn set_main_ready();

@native_import("SDL_Init")
fn init(flags: u32): bool;

@native_import("SDL_Quit")
fn quit();

@native_import("SDL_GetError")
fn get_error(): [*]u8;

@native_import("SDL_CreateWindow")
fn create_window(title: [*]u8, width: i32, height: i32, flags: u64): *void;

@native_import("SDL_DestroyWindow")
fn destroy_window(window: *void);

@native_import("SDL_GetWindowSizeInPixels")
fn get_window_size_in_pixels(window: *void, width: *i32, height: *i32): bool;

@native_import("SDL_GL_SetAttribute")
fn gl_set_attribute(attribute: i32, value: i32): bool;

@native_import("SDL_GL_CreateContext")
fn gl_create_context(window: *void): *void;

@native_import("SDL_GL_DestroyContext")
fn gl_destroy_context(context: *void): bool;

@native_import("SDL_GL_SetSwapInterval")
fn gl_set_swap_interval(interval: i32): bool;

@native_import("SDL_GL_SwapWindow")
fn gl_swap_window(window: *void): bool;

@native_import("SDL_PollEvent")
fn poll_event(event: *void): bool;

@native_import("SDL_GetKeyboardState")
fn get_keyboard_state(count: *i32): [*]bool;

@native_import("SDL_Delay")
fn delay(milliseconds: u32);
