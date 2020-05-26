#version 330 core
struct Material
{
	sampler2D texture_diffuse1;
	sampler2D texture_specular1;
	float shininess;
};

struct DirectionLight {
	vec3 direction;

	vec3 ambient;
	vec3 diffuse;
	vec3 specular;
};

out vec4 FragColor;
in vec3 FragPos;
in vec3 Normals;
in vec2 TexCoords;

uniform DirectionLight directionLight;
uniform Material material;
uniform vec3 viewPos;

void main()
{
	vec3 ambient = directionLight.ambient * vec3(texture(material.texture_diffuse1, TexCoords));

	vec3 normals = normalize(Normals);
	vec3 lightDir = normalize(-directionLight.direction - FragPos);
	float diff = max(dot(normals, lightDir), 0.0);
	vec3 diffuse = diff * directionLight.diffuse * vec3(texture(material.texture_diffuse1, TexCoords));

	vec3 viewDir = normalize(viewPos - FragPos);
	vec3 reflectDir = reflect(-lightDir, normals);
	float spec  = pow(max(dot(viewDir, reflectDir), 0.0), material.shininess);
	vec3 specular = spec * directionLight.specular * vec3(texture(material.texture_specular1, TexCoords));
	
	vec3 result = ambient + diffuse + specular;
	FragColor = vec4(result, 1.0);
}
