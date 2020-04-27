#version 330 core

in vec3 Normal;
in vec3 FragPos;
in vec2 TexCoords;

struct Material {
	sampler2D diffuse;
	sampler2D specular;
	float shininess;
};

struct Light {
	vec3 position;
	vec3 ambient;
	vec3 diffuse;
	vec3 specular;
};

uniform Material material;
uniform Light light;
uniform vec3 viewPos;

out vec4 FragColor;

void main() {
	vec3 fragDiff = texture(material.diffuse, TexCoords).xyz;
	vec3 norm = normalize(Normal);

	vec3 lightDir = normalize(light.position - FragPos);
	float viewDiff = max(0,dot(norm, lightDir));
	vec3 diffColor = fragDiff * (light.ambient +  light.diffuse * viewDiff);

	vec3 fragSpec = texture(material.specular, TexCoords).xyz;
	vec3 viewDir = normalize(viewPos - FragPos);
	vec3 reflectDir = reflect(-lightDir, norm);
	float viewSpec = pow(max(dot(viewDir, reflectDir), 0.0), material.shininess);
	vec3 specularColor = fragSpec * light.specular * viewSpec;

	vec3 result = diffColor + specularColor;
	FragColor = vec4 (result, 1);
}
