#version 330 core

in vec2 texCoord;

out vec4 FragColor;

uniform sampler2D texFace;
uniform sampler2D texBox;

void main() {
	float r = step(1,texCoord.x+texCoord.y);
	FragColor = r*texture(texBox, texCoord)+(1-r)*texture(texFace, texCoord);
}
