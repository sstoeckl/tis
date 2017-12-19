#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* FIXME: 
   Check these declarations against the C/Fortran source code.
*/

/* .C calls */
extern void dgtsv(void *, void *, void *, void *, void *, void *, void *, void *);
extern void lintegrate(void *, void *, void *, void *, void *, void *, void *);

static const R_CMethodDef CEntries[] = {
    {"dgtsv",      (DL_FUNC) &dgtsv,      8},
    {"lintegrate", (DL_FUNC) &lintegrate, 7},
    {NULL, NULL, 0}
};

void R_init_tis(DllInfo *dll)
{
    R_registerRoutines(dll, CEntries, NULL, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
