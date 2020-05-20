#version 330 core

out vec4 FragColor;
in vec3 FragPos;
in vec3 Normals;
in vec2 TexCoords;

uniform sampler2D texture1;
uniform vec3 objectColor;
uniform vec3 lightColor;
uniform vec3 lightPos;

void main()
{
	float ambientStrength = 0.5;
	vec3 ambient = ambientStrength * lightColor;

	vec3 normals = normalize(Normals);
	vec3 lightDir = normalize(lightPos - FragPos);
	float diffuse = max(dot(normals, lightDir), 0.0);
	
	vec3 result = (ambient + diffuse)* objectColor;
	/* FragColor = texture(texture1, TexCoords) * vec4(result, 1.0); */
	FragColor = vec4(result, 1.0);
}
