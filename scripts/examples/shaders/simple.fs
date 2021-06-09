#version 330 core
out vec4 FragColor;

in vec2 TexCoords;

struct Material
{
	sampler2D texture_diffuse1;
	sampler2D texture_specular1;
	vec4 color;
};

uniform Material material;

void main()
{    
	vec4 textureColor = texture(material.texture_diffuse1, TexCoords);
	if(textureColor.a < 0.1)
		discard;

	FragColor = textureColor * material.color;
}
