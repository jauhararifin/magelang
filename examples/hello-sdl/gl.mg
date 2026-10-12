let COLOR_BUFFER_BIT: u32 = 0x4000;
let TRIANGLES: u32 = 0x4;
let VERTEX_SHADER: u32 = 0x8B31;
let FRAGMENT_SHADER: u32 = 0x8B30;
let COMPILE_STATUS: u32 = 0x8B81;
let LINK_STATUS: u32 = 0x8B82;
let VERSION: u32 = 0x1F02;
let RGBA: u32 = 0x1908;
let UNSIGNED_BYTE: u32 = 0x1401;

@native_import("glCreateShader")
fn create_shader(kind: u32): u32;

@native_import("glShaderSource")
fn shader_source(shader: u32, count: i32, source: *[*]u8, lengths: *i32);

@native_import("glCompileShader")
fn compile_shader(shader: u32);

@native_import("glGetShaderiv")
fn get_shader_iv(shader: u32, name: u32, value: *i32);

@native_import("glGetShaderInfoLog")
fn get_shader_info_log(shader: u32, capacity: i32, length: *i32, log: [*]u8);

@native_import("glDeleteShader")
fn delete_shader(shader: u32);

@native_import("glCreateProgram")
fn create_program(): u32;

@native_import("glAttachShader")
fn attach_shader(program: u32, shader: u32);

@native_import("glLinkProgram")
fn link_program(program: u32);

@native_import("glGetProgramiv")
fn get_program_iv(program: u32, name: u32, value: *i32);

@native_import("glGetProgramInfoLog")
fn get_program_info_log(program: u32, capacity: i32, length: *i32, log: [*]u8);

@native_import("glDeleteProgram")
fn delete_program(program: u32);

@native_import("glUseProgram")
fn use_program(program: u32);

@native_import("glGenVertexArrays")
fn gen_vertex_arrays(count: i32, arrays: *u32);

@native_import("glDeleteVertexArrays")
fn delete_vertex_arrays(count: i32, arrays: *u32);

@native_import("glBindVertexArray")
fn bind_vertex_array(array: u32);

@native_import("glViewport")
fn viewport(x: i32, y: i32, width: i32, height: i32);

@native_import("glClearColor")
fn clear_color(red: f32, green: f32, blue: f32, alpha: f32);

@native_import("glClear")
fn clear(mask: u32);

@native_import("glDrawArrays")
fn draw_arrays(mode: u32, first: i32, count: i32);

@native_import("glGetString")
fn get_string(name: u32): [*]u8;

@native_import("glGetError")
fn get_error(): u32;

@native_import("glReadPixels")
fn read_pixels(x: i32, y: i32, width: i32, height: i32, format: u32, kind: u32, pixels: *void);
