#version 330 core

layout (location = 0) in vec3 aPos;
layout (location = 1) in vec3 aNormals;
layout (location = 2) in vec3 aTexCoords;

void main()
{
   gl_Position = vec4(aPos, 1.0f);
}
