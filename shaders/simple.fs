#version 330 core
struct Material
{
	vec3 ambient;
	vec3 diffuse;
	vec3 specular;
	float shininess;
};

out vec4 FragColor;
in vec3 FragPos;
in vec3 Normals;
in vec2 TexCoords;

uniform Material material;
uniform sampler2D texture1;
uniform vec3 viewPos;
uniform vec3 lightColor;
uniform vec3 lightDirection;

void main()
{
	vec3 ambient = lightColor * material.ambient;

	vec3 normals = normalize(Normals);
	vec3 lightDir = normalize(-lightDirection - FragPos);
	float diff = max(dot(normals, lightDir), 0.0);
	vec3 diffuse = lightColor * (diff * material.diffuse);

	vec3 viewDir = normalize(viewPos - FragPos);
	vec3 reflectDir = reflect(-lightDir, normals);
	float spec  = pow(max(dot(viewDir, reflectDir), 0.0), material.shininess);
	vec3 specular = lightColor * (spec * material.specular);;
	
	vec3 result = (specular + ambient + diffuse);
	FragColor = texture(texture1, TexCoords) * vec4(result, 1.0);
}
