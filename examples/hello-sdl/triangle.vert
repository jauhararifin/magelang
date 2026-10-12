#version 150

out vec3 vertex_color;

const vec2 positions[3] = vec2[3](
    vec2( 0.00,  0.75),
    vec2(-0.75, -0.65),
    vec2( 0.75, -0.65)
);

const vec3 colors[3] = vec3[3](
    vec3(1.0, 0.0, 0.0),
    vec3(0.0, 1.0, 0.0),
    vec3(0.0, 0.0, 1.0)
);

void main() {
    gl_Position = vec4(positions[gl_VertexID], 0.0, 1.0);
    vertex_color = colors[gl_VertexID];
}
