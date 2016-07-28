#include "genheader.h"

#include "grammar.h"

#if HEADER_ONLY

#include "utils.h"
#include "error.h"
#include "term.h"

#define MAX_PREC 1200

#endif

#include <ctype.h>
#include <string.h>

HEADER_DECLARE
bool issymbol(char c){
    return !isalnum(c) && !isspace(c) && !strchr("()[],'_\"%", c) && isprint(c);
}

HEADER_DECLARE
int op_type_arg_size(atom_t type){
    switch(type){
    case atom_xf:
    case atom_yf:
    case atom_fx:
    case atom_fy:
        return 1;
    case atom_xfx:
    case atom_yfx:
    case atom_xfy:
        return 2;
    default:
        fatal_error("invalid op type");
        UNREACHABLE;
    }
}

HEADER_DECLARE
void op_type(integer_t prec, atom_t atom, integer_t *left, integer_t *right){
    switch(atom){
    case atom_xf:
    case atom_xfx:
    case atom_xfy:
        *left = prec; break;
    case atom_yf:
    case atom_yfx:
        *left = prec -1; break;
    case atom_fx:
    case atom_fy:
        *left = 0; break;
    default:
        fatal_error("invalid op spec");
    }
    switch(atom){
    case atom_fx:
    case atom_xfx:
    case atom_yfx:
        *right = prec; break;
    case atom_fy:
    case atom_xfy:
        *right = prec -1; break;
    case atom_xf:
    case atom_yf:
        *right = 0;
    default:
        fatal_error("invalid op spec");
    }
}
