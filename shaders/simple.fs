#version 330 core

out vec4 FragColor;
in vec2 TexCoords;

uniform sampler2D texture1;
uniform vec3 tint;

void main()
{    
	FragColor = texture(texture1, TexCoords) * vec4(tint, 1.0);
}
