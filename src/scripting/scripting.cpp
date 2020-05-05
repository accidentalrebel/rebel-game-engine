#include "scripting.h"
#include "../rebel.h"

sexp scripting::Init()
{
	sexp schemeContext = sexp_make_eval_context(NULL, NULL, NULL, 0, 0);
  sexp_load_standard_env(schemeContext, NULL, SEXP_SEVEN);
  sexp_load_standard_ports(schemeContext, NULL, stdin, stdout, stderr, 1);
	sexp_init_library(schemeContext, NULL, 3, sexp_context_env(schemeContext), sexp_version, SEXP_ABI_IDENTIFIER);
	// sexp_eval_string(schemeContext,"(import (scheme base) (chibi))",-1,NULL);
	sexp_eval_string(schemeContext,"(import (chibi))",-1,NULL);
	sexp_eval_string(schemeContext,"(load \"main.scm\")",-1,NULL);
	return schemeContext;
}

void scripting::Eval(const char* str)
{
	sexp_gc_var1(result);
	result = sexp_eval_string(g_rebel.scriptCtx, str, -1, NULL);
	if (sexp_exceptionp(result))
	{
    puts("FAILURE: EXCEPTION:");
		sexp_print_exception(g_rebel.scriptCtx, result, SEXP_FALSE);
	}
	sexp_gc_release1(g_rebel.scriptCtx);
}

void scripting::Destroy()
{
	sexp_destroy_context(g_rebel.scriptCtx);
}
