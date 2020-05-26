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

vec3 CalculateDirectionalLight(DirectionLight light, vec3 normal, vec3 viewDir);

void main()
{
	vec3 normal = normalize(Normals);
	vec3 viewDir = normalize(viewPos - FragPos);
	vec3 result = CalculateDirectionalLight(directionLight, normal, viewDir);
	FragColor = vec4(result, 1.0);
}

vec3 CalculateDirectionalLight(DirectionLight light, vec3 normal, vec3 viewDir)
{
	vec3 lightDir = normalize(-light.direction - FragPos);
	float diff = max(dot(normal, lightDir), 0.0);

	vec3 reflectDir = reflect(-lightDir, normal);
	float spec  = pow(max(dot(viewDir, reflectDir), 0.0), material.shininess);

	vec3 ambient = light.ambient * vec3(texture(material.texture_diffuse1, TexCoords));
	vec3 diffuse = diff * light.diffuse * vec3(texture(material.texture_diffuse1, TexCoords));
	vec3 specular = spec * light.specular * vec3(texture(material.texture_specular1, TexCoords));
	
	return (ambient + diffuse + specular);
}
