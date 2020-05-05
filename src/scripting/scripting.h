#pragma once

#include "chibi/eval.h"

namespace scripting
{
	sexp Init();
	void Destroy();
	void Eval(const char* str);
}
