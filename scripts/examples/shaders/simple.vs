#version 330 core
layout (location = 0) in vec3 aPos;
layout (location = 1) in vec3 aNormal;
layout (location = 2) in vec2 aTexCoords;

uniform vec4 texRect;
uniform mat4 model;
uniform mat4 view;
uniform mat4 projection;

out vec2 TexCoords;
out vec4 TexRect;

void main()
{
    TexCoords = aTexCoords;
    TexRect = texRect;
    gl_Position = projection * view * model * vec4(aPos, 1.0);
}