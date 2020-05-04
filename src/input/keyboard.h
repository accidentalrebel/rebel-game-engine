#pragma once

enum Keys {
	KEY_PERIOD,
	KEY_COMMA
};

bool IsKeyDown(Keys key);
int ConvertToGLFWKey(Keys key);
