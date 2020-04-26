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

/* uniform Material material; */
uniform Light light;
uniform vec3 viewPos;

out vec4 FragColor;

void main() {
	vec3 norm = normalize(Normal);
	vec3 lightDir = normalize(light.position - FragPos);
	float diff = max(0,dot(norm, lightDir));
	vec3 diffColor = light.diffuse * diff;

	vec3 viewDir = normalize(viewPos - FragPos);
	vec3 reflectDir = reflect(-lightDir, norm);
	float spec = pow(max(dot(viewDir, reflectDir), 0.0), 32);
	vec3 specularColor = light.specular * spec;

	vec3 result = diffColor + light.ambient + specularColor;
	FragColor = vec4 (result, 1);
}
