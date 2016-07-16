#include "genheader.h"

#include "frame.h"

#if HEADER_ONLY

#include "assert.h"
#include "settings.h"
#include "roots.h"
#include "debug.h"
#include "term.h"
#include "gc.h"

#define FRAME_ENTER \
    ASSERT_CODE(\
                char use_FRAME_RETURN_or_FRAME_LEAVE_instead_of_return; \
                const char* parent_frame_func = current_frame_func; \
                current_frame_func = __func__; \
                size_t current_c_frame_count = ++c_frame_count; ) \
    size_t current_c_frame = next_c_term

#define FRAME_ENTER_1(a) FRAME_ENTER; FRAME_TRACK_VAR(a)
#define FRAME_ENTER_2(a, b) FRAME_ENTER_1(a); FRAME_TRACK_VAR(b)
#define FRAME_ENTER_3(a, b, c) FRAME_ENTER_2(a, b); FRAME_TRACK_VAR(c)
#define FRAME_ENTER_4(a, b, c, d) FRAME_ENTER_3(a, b, c); FRAME_TRACK_VAR(d)
#define FRAME_ENTER_5(a, b, c, d, e) FRAME_ENTER_4(a, b, c, d); FRAME_TRACK_VAR(e)

#define ENSURE_INSIDE_FRAME (void)current_c_frame

#define FRAME_TRACK_VAR(name) ENSURE_INSIDE_FRAME; \
    guarantee(next_c_term < MAX_C_TERMS, "C stack too big"); \
    root.c_terms[next_c_term++] = &(name)

#define FRAME_LOCAL(name) ENSURE_INSIDE_FRAME; \
    Term* name = NULL; \
    FRAME_TRACK_VAR(name); \
    name

#define FRAME_LEAVE ENSURE_INSIDE_FRAME; \
    ASSERT_CODE((void)use_FRAME_RETURN_or_FRAME_LEAVE_instead_of_return;) \
    D_SANITY{ \
        guarantee( \
                  current_c_frame <= next_c_term \
                  ASSERT_CODE(&& c_frame_count == current_c_frame_count), \
                  "internal error: c frame mismatch: leaving %s after entering %s", __func__, current_frame_func); \
    } \
    ASSERT_CODE(current_frame_func = parent_frame_func;) \
    ASSERT_CODE(--c_frame_count;) \
    next_c_term = current_c_frame \

#define FRAME_RETURN(type, ret) do{ type _frame_ret = ret; FRAME_LEAVE; return _frame_ret; }while(0)

#endif

HEADER_DECLARE
const char* current_frame_func = NULL;

HEADER_DECLARE
size_t c_frame_count = 0;
