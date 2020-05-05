#include "scripting.h"

sexp InitializeScripting()
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
