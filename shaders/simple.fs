#version 330 core
struct Material
{
	vec3 ambient;
	vec3 diffuse;
	vec3 specular;
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
uniform sampler2D texture1;
uniform vec3 viewPos;

void main()
{
	vec3 ambient = material.ambient; // * directionLight.ambient;

	vec3 normals = normalize(Normals);
	vec3 lightDir = normalize(-directionLight.direction - FragPos);
	float diff = max(dot(normals, lightDir), 0.0);
	vec3 diffuse = (diff * material.diffuse) * directionLight.diffuse;

	vec3 viewDir = normalize(viewPos - FragPos);
	vec3 reflectDir = reflect(-lightDir, normals);
	float spec  = pow(max(dot(viewDir, reflectDir), 0.0), material.shininess);
	vec3 specular = directionLight.specular * (spec * material.specular);;
	
	vec3 result = ambient + diffuse + specular;
	FragColor = texture(texture1, TexCoords) * vec4(result, 1.0);
}
