/*
 * qnb.c
 */

#include "qnb.h"

/*
 * The following function is a dummy one; replace it for
 * your C function definitions.
 */

ScmObj test_qnb(void)
{
    return SCM_MAKE_STR("qnb is working");
}

/*
 * Module initialization function.
 */
extern void Scm_Init_qnblib(ScmModule*);

ScmObj Scm_Init_qnb(void)
{
    ScmModule *mod;

    /* Register this DSO to Gauche */
    SCM_INIT_EXTENSION(qnb);

    /* Create the module if it doesn't exist yet. */
    mod = SCM_MODULE(SCM_FIND_MODULE("qnb", TRUE));

    /* Register stub-generated procedures */
    Scm_Init_qnblib(mod);
}
