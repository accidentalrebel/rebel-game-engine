#version 330 core

out vec4 FragColor;
in vec2 TexCoords;

uniform sampler2D texture1;
uniform vec3 objectColor;
uniform vec3 lightColor;

void main()
{
	float ambientStrength = 0.5;
	vec3 ambient = ambientStrength * lightColor;
	vec3 result = ambient * objectColor;
	FragColor = texture(texture1, TexCoords) * vec4(result, 1.0);
}
