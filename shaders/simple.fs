#version 330 core

out vec4 FragColor;
in vec3 FragPos;
in vec3 Normals;
in vec2 TexCoords;

uniform sampler2D texture1;
uniform vec3 viewPos;
uniform vec3 objectColor;
uniform vec3 lightColor;
uniform vec3 lightDirection;

void main()
{
	float ambientStrength = 0.1;
	float specularStrength = 0.5;
	
	vec3 ambient = ambientStrength * lightColor;

	vec3 normals = normalize(Normals);
	vec3 lightDir = normalize(-lightDirection - FragPos);
	float diffuse = max(dot(normals, lightDir), 0.0);

	vec3 viewDir = normalize(viewPos - FragPos);
	vec3 reflectDir = reflect(-lightDir, normals);
	float spec  = pow(max(dot(viewDir, reflectDir), 0.0), 32);
	vec3 specular = specularStrength * spec * lightColor;
	
	vec3 result = (specular + ambient + diffuse)* objectColor;
	FragColor = texture(texture1, TexCoords) * vec4(result, 1.0);
}
