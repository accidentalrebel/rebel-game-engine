#version 330 core
out vec4 FragColor;

in vec2 TexCoords;
in vec4 TexRect;

struct Material
{
	sampler2D texture_diffuse1;
	sampler2D texture_specular1;
	vec4 color;
};

uniform Material material;

void main()
{
	vec2 tex = vec2((TexCoords.x * TexRect.z) + TexRect.x, (TexCoords.y * TexRect.w) + TexRect.y);
	vec4 textureColor = texture(material.texture_diffuse1, tex);
	if(textureColor.a < 0.1)
		discard;

	FragColor = textureColor * material.color;
}
