/*
 *  TccGen - The Tiny C Compiler backend
 *
 *  Copyright (c) 2001-2004 Fabrice Bellard
 *            (c) 2024 Rochus Keller
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 */

#include "cparser.h"
#include "machine.h"
#include "token.h"
#include "helper.h"
#include "tccelf.h"
#include "generator.h"
#include <memory.h>
#include <assert.h>
#include <math.h>
#include <errno.h>
#include <fcntl.h> // TODO: Windows
#include <unistd.h> // TODO: Windows

// the c parser only exists during development for testing purpose!

static const char tcc_keywords[] =
#define DEF(id, str) str "\0"
#include "tcctok.h"
#undef DEF
;

/* isidnum_table flags: */
#define IS_SPC 1
#define IS_ID  2
#define IS_NUM 4
static unsigned char isidnum_table[256 - CH_EOF];

static int in_sizeof;
static CString tokcstr; /* current parsed string, if any */
static int gnu_ext = 0; /* use GNU C extensions */
static int global_expr = 0;  /* true if compound literals must be allocated globally (used during initializers parsing */
static Sym *local_label_stack;

static CValue tokc;
static const int *macro_ptr;
static TokenString *macro_stack;

static char token_buf[STRING_MAX_SIZE + 1];

#define TOK_MAX_SIZE        4 /* token max size in int unit when stored in string */
#define BN_SIZE 2

static const int char_is_unsigned = 0;

/*-------forward decls----------------*/
static int parse_btype(CType *type, AttributeDef *ad);
static CType *type_decl(CType *type, AttributeDef *ad, int *v, int td);
static void parse_expr_type(CType *type);
static void assignment_expression(void);
static inline int64_t const_int64_expr(void);
static void statement(int *bsym, int *csym, int is_expr);
static void skip_or_save_block(TokenString **str);
static void decl_initializer(CType *type, Section *sec, unsigned long c, int first, int size_only);
static void decl_initializer_alloc(CType *type, AttributeDef *ad, int r, int has_init, int v, int scope);
static int decl0(int l, int is_for_loop_init, Sym *);
static void expression(void);
static void decl(int l);
static int const_int32_expr(void);
static void gen_inline_functions(TCCState *s);
/*-----------------------------------*/

static void tok_str_add2(TokenString *s, int t, CValue *cv)
{
    int len, *str;

    len = s->lastlen = s->len;
    str = s->str;

    /* allocate space for worst case */
    if (len + TOK_MAX_SIZE >= s->allocated_len)
        str = tok_str_realloc(s, len + TOK_MAX_SIZE + 1);
    str[len++] = t;
    switch(t) {
    case TOK_CINT:
    case TOK_CUINT:
    case TOK_CCHAR:
    case TOK_LCHAR:
    case TOK_CFLOAT:
    case TOK_LINENUM:
#if LONG_SIZE == 4
    case TOK_CLONG:
    case TOK_CULONG:
#endif
        str[len++] = cv->tab[0];
        break;
    case TOK_PPNUM:
    case TOK_PPSTR:
    case TOK_STR:
    case TOK_LSTR:
        {
            /* Insert the string into the int array. */
            size_t nb_words =
                1 + (cv->str.size + sizeof(int) - 1) / sizeof(int);
            if (len + nb_words >= s->allocated_len)
                str = tok_str_realloc(s, len + nb_words + 1);
            str[len] = cv->str.size;
            memcpy(&str[len + 1], cv->str.data, cv->str.size);
            len += nb_words;
        }
        break;
    case TOK_CDOUBLE:
    case TOK_CLLONG:
    case TOK_CULLONG:
#if LONG_SIZE == 8
    case TOK_CLONG:
    case TOK_CULONG:
#endif
#if LDOUBLE_SIZE == 8
    case TOK_CLDOUBLE:
#endif
        str[len++] = cv->tab[0];
        str[len++] = cv->tab[1];
        break;
#if LDOUBLE_SIZE == 12
    case TOK_CLDOUBLE:
        str[len++] = cv->tab[0];
        str[len++] = cv->tab[1];
        str[len++] = cv->tab[2];
#elif LDOUBLE_SIZE == 16
    case TOK_CLDOUBLE:
        str[len++] = cv->tab[0];
        str[len++] = cv->tab[1];
        str[len++] = cv->tab[2];
        str[len++] = cv->tab[3];
#elif LDOUBLE_SIZE != 8
#error add long double size support
#endif
        break;
    default:
        break;
    }
    s->len = len;
}

/* add the current parse token in token string 's' */
static void tok_str_add_tok(TokenString *s)
{
    CValue cval;

    /* save line number info */
    if (file->line_num != s->last_line_num) {
        s->last_line_num = file->line_num;
        cval.i = s->last_line_num;
        tok_str_add2(s, TOK_LINENUM, &cval);
    }
    tok_str_add2(s, tok, &tokc);
}

void begin_macro(TokenString *str, int alloc)
{
    str->alloc = alloc;
    str->prev = macro_stack;
    str->prev_ptr = macro_ptr;
    str->save_line_num = file->line_num;
    macro_ptr = str->str;
    macro_stack = str;
}

void end_macro(void)
{
    TokenString *str = macro_stack;
    macro_stack = str->prev;
    macro_ptr = str->prev_ptr;
    file->line_num = str->save_line_num;
    if (str->alloc == 2) {
        str->alloc = 3; /* just mark as finished */
    } else {
        tok_str_free(str);
    }
}

/* push back current token and set current token to 'last_tok'. Only
   identifier case handled for labels. */
static void unget_tok(int last_tok) {
    TokenString *str = tok_str_alloc();
    tok_str_add2(str, tok, &tokc);
    tok_str_add(str, 0);
    begin_macro(str, 1);
    tok = last_tok;
}

/* handle '\[\r]\n' */
static int handle_stray_noerror(void)
{
    while (ch == '\\') {
        inp();
        if (ch == '\n') {
            file->line_num++;
            inp();
        } else if (ch == '\r') {
            inp();
            if (ch != '\n')
                goto fail;
            file->line_num++;
            inp();
        } else {
        fail:
            return 1;
        }
    }
    return 0;
}

/* skip the stray and handle the \\n case. Output an error if
   incorrect char after the stray */
static int handle_stray1(uint8_t *p)
{
    int c;

    file->buf_ptr = p;
    if (p >= file->buf_end) {
        c = handle_eob();
        if (c != '\\')
            return c;
        p = file->buf_ptr;
    }
    ch = *p;
    if (handle_stray_noerror()) {
        tcc_error("stray '\\' in program");
        *--file->buf_ptr = '\\';
    }
    p = file->buf_ptr;
    c = *p;
    return c;
}

/* single line C++ comments */
static uint8_t *parse_line_comment(uint8_t *p)
{
    int c;

    p++;
    for(;;) {
        c = *p;
    redo:
        if (c == '\n' || c == CH_EOF) {
            break;
        } else if (c == '\\') {
            file->buf_ptr = p;
            c = handle_eob();
            p = file->buf_ptr;
            if (c == '\\') {
                PEEKC_EOB(c, p);
                if (c == '\n') {
                    file->line_num++;
                    PEEKC_EOB(c, p);
                } else if (c == '\r') {
                    PEEKC_EOB(c, p);
                    if (c == '\n') {
                        file->line_num++;
                        PEEKC_EOB(c, p);
                    }
                }
            } else {
                goto redo;
            }
        } else {
            p++;
        }
    }
    return p;
}

/* handle the complicated stray case */
#define PEEKC(c, p)\
{\
    p++;\
    c = *p;\
    if (c == '\\') {\
        c = handle_stray1(p);\
        p = file->buf_ptr;\
    }\
}

#define PARSE2(c1, tok1, c2, tok2)              \
    case c1:                                    \
        PEEKC(c, p);                            \
        if (c == c2) {                          \
            p++;                                \
            tok = tok2;                         \
        } else {                                \
            tok = tok1;                         \
        }                                       \
        break;

/* parse a string without interpreting escapes */
static uint8_t *parse_pp_string(uint8_t *p,
                                int sep, CString *str)
{
    int c;
    p++;
    for(;;) {
        c = *p;
        if (c == sep) {
            break;
        } else if (c == '\\') {
            file->buf_ptr = p;
            c = handle_eob();
            p = file->buf_ptr;
            if (c == CH_EOF) {
            unterminated_string:
                /* XXX: indicate line number of start of string */
                tcc_error("missing terminating %c character", sep);
            } else if (c == '\\') {
                /* escape : just skip \[\r]\n */
                PEEKC_EOB(c, p);
                if (c == '\n') {
                    file->line_num++;
                    p++;
                } else if (c == '\r') {
                    PEEKC_EOB(c, p);
                    if (c != '\n')
                        expect("'\n' after '\r'");
                    file->line_num++;
                    p++;
                } else if (c == CH_EOF) {
                    goto unterminated_string;
                } else {
                    if (str) {
                        cstr_ccat(str, '\\');
                        cstr_ccat(str, c);
                    }
                    p++;
                }
            }
        } else if (c == '\n') {
            file->line_num++;
            goto add_char;
        } else if (c == '\r') {
            PEEKC_EOB(c, p);
            if (c != '\n') {
                if (str)
                    cstr_ccat(str, '\r');
            } else {
                file->line_num++;
                goto add_char;
            }
        } else {
        add_char:
            if (str)
                cstr_ccat(str, c);
            p++;
        }
    }
    p++;
    return p;
}

/* return next token without macro substitution */
static inline void next_nomacro1(void)
{
    int t, c, is_long, len;
    TokenSym *ts;
    uint8_t *p, *p1;
    unsigned int h;

    p = file->buf_ptr;
 redo_no_start:
    c = *p;
    switch(c) {
    case ' ':
    case '\t':
        tok = c;
        p++;
        while (isidnum_table[*p - CH_EOF] & IS_SPC)
            ++p;
        goto redo_no_start;
    case '\f':
    case '\v':
    case '\r':
        p++;
        goto redo_no_start;
    case '\\':
        /* first look if it is in fact an end of buffer */
        c = handle_stray1(p);
        p = file->buf_ptr;
        if (c == '\\')
            goto parse_simple;
        if (c != CH_EOF)
            goto redo_no_start;
        tok = TOK_EOF;
        break;

    case '\n':
        file->line_num++;
        p++;
        goto redo_no_start;

    /* dollar is allowed to start identifiers when not parsing asm */
    case '$':
        if (!(isidnum_table[c - CH_EOF] & IS_ID) )
            goto parse_simple;
    case 'a': case 'b': case 'c': case 'd':
    case 'e': case 'f': case 'g': case 'h':
    case 'i': case 'j': case 'k': case 'l':
    case 'm': case 'n': case 'o': case 'p':
    case 'q': case 'r': case 's': case 't':
    case 'u': case 'v': case 'w': case 'x':
    case 'y': case 'z':
    case 'A': case 'B': case 'C': case 'D':
    case 'E': case 'F': case 'G': case 'H':
    case 'I': case 'J': case 'K':
    case 'M': case 'N': case 'O': case 'P':
    case 'Q': case 'R': case 'S': case 'T':
    case 'U': case 'V': case 'W': case 'X':
    case 'Y': case 'Z':
    case '_':
    parse_ident_fast:
        p1 = p;
        h = TOK_HASH_INIT;
        h = TOK_HASH_FUNC(h, c);
        while (c = *++p, isidnum_table[c - CH_EOF] & (IS_ID|IS_NUM))
            h = TOK_HASH_FUNC(h, c);
        len = p - p1;
        if (c != '\\') {
            /* fast case : no stray found, so we have the full token
               and we have already hashed it */
            ts = tok_alloc2(h, (char*)p1, len);
        } else {
            /* slower case */
            cstr_reset(&tokcstr);
            cstr_cat(&tokcstr, (char *) p1, len);
            p--;
            PEEKC(c, p);
        parse_ident_slow:
            while (isidnum_table[c - CH_EOF] & (IS_ID|IS_NUM))
            {
                cstr_ccat(&tokcstr, c);
                PEEKC(c, p);
            }
            ts = tok_alloc((char*)tokcstr.data, tokcstr.size);
        }
        tok = ts->tok;
        break;
    case 'L':
        t = p[1];
        if (t != '\\' && t != '\'' && t != '\"') {
            /* fast case */
            goto parse_ident_fast;
        } else {
            PEEKC(c, p);
            if (c == '\'' || c == '\"') {
                is_long = 1;
                goto str_const;
            } else {
                cstr_reset(&tokcstr);
                cstr_ccat(&tokcstr, 'L');
                goto parse_ident_slow;
            }
        }
        break;

    case '0': case '1': case '2': case '3':
    case '4': case '5': case '6': case '7':
    case '8': case '9':
        t = c;
        PEEKC(c, p);
        /* after the first digit, accept digits, alpha, '.' or sign if
           prefixed by 'eEpP' */
    parse_num:
        cstr_reset(&tokcstr);
        for(;;) {
            cstr_ccat(&tokcstr, t);
            if (!((isidnum_table[c - CH_EOF] & (IS_ID|IS_NUM))
                  || c == '.'
                  || ((c == '+' || c == '-')
                      && (t == 'e' || t == 'E'
                          || t == 'p' || t == 'P'))))
                break;
            t = c;
            PEEKC(c, p);
        }
        /* We add a trailing '\0' to ease parsing */
        cstr_ccat(&tokcstr, '\0');
        tokc.str.size = tokcstr.size;
        tokc.str.data = tokcstr.data;
        tok = TOK_PPNUM;
        break;

    case '.':
        /* special dot handling because it can also start a number */
        PEEKC(c, p);
        if (isnum(c)) {
            t = '.';
            goto parse_num;
        } else if ((isidnum_table['.' - CH_EOF] & IS_ID)
                    && (isidnum_table[c - CH_EOF] & (IS_ID|IS_NUM))) {
             *--p = c = '.';
             goto parse_ident_fast;
         } else if (c == '.') {
            PEEKC(c, p);
            if (c == '.') {
                p++;
                tok = TOK_DOTS;
            } else {
                *--p = '.'; /* may underflow into file->unget[] */
                tok = '.';
            }
        } else {
            tok = '.';
        }
        break;
    case '\'':
    case '\"':
        is_long = 0;
str_const:
        cstr_reset(&tokcstr);
        if (is_long)
            cstr_ccat(&tokcstr, 'L');
        cstr_ccat(&tokcstr, c);
        p = parse_pp_string(p, c, &tokcstr);
        cstr_ccat(&tokcstr, c);
        cstr_ccat(&tokcstr, '\0');
        tokc.str.size = tokcstr.size;
        tokc.str.data = tokcstr.data;
        tok = TOK_PPSTR;
        break;

    case '<':
        PEEKC(c, p);
        if (c == '=') {
            p++;
            tok = TOK_LE;
        } else if (c == '<') {
            PEEKC(c, p);
            if (c == '=') {
                p++;
                tok = TOK_A_SHL;
            } else {
                tok = TOK_SHL;
            }
        } else {
            tok = TOK_LT;
        }
        break;
    case '>':
        PEEKC(c, p);
        if (c == '=') {
            p++;
            tok = TOK_GE;
        } else if (c == '>') {
            PEEKC(c, p);
            if (c == '=') {
                p++;
                tok = TOK_A_SAR;
            } else {
                tok = TOK_SAR;
            }
        } else {
            tok = TOK_GT;
        }
        break;

    case '&':
        PEEKC(c, p);
        if (c == '&') {
            p++;
            tok = TOK_LAND;
        } else if (c == '=') {
            p++;
            tok = TOK_A_AND;
        } else {
            tok = '&';
        }
        break;

    case '|':
        PEEKC(c, p);
        if (c == '|') {
            p++;
            tok = TOK_LOR;
        } else if (c == '=') {
            p++;
            tok = TOK_A_OR;
        } else {
            tok = '|';
        }
        break;

    case '+':
        PEEKC(c, p);
        if (c == '+') {
            p++;
            tok = TOK_INC;
        } else if (c == '=') {
            p++;
            tok = TOK_A_ADD;
        } else {
            tok = '+';
        }
        break;

    case '-':
        PEEKC(c, p);
        if (c == '-') {
            p++;
            tok = TOK_DEC;
        } else if (c == '=') {
            p++;
            tok = TOK_A_SUB;
        } else if (c == '>') {
            p++;
            tok = TOK_ARROW;
        } else {
            tok = '-';
        }
        break;

    PARSE2('!', '!', '=', TOK_NE)
    PARSE2('=', '=', '=', TOK_EQ)
    PARSE2('*', '*', '=', TOK_A_MUL)
    PARSE2('%', '%', '=', TOK_A_MOD)
    PARSE2('^', '^', '=', TOK_A_XOR)

        /* comments or operator */
    case '/':
        PEEKC(c, p);
        if (c == '*') {
            p = parse_comment(p);
            /* comments replaced by a blank */
            tok = ' ';
            goto keep_tok_flags;
        } else if (c == '/') {
            p = parse_line_comment(p);
            tok = ' ';
            goto keep_tok_flags;
        } else if (c == '=') {
            p++;
            tok = TOK_A_DIV;
        } else {
            tok = '/';
        }
        break;

        /* simple tokens */
    case '(':
    case ')':
    case '[':
    case ']':
    case '{':
    case '}':
    case ',':
    case ';':
    case ':':
    case '?':
    case '~':
    case '@': /* only used in assembler */
    parse_simple:
        tok = c;
        p++;
        break;
    default:
        if (c >= 0x80 && c <= 0xFF) /* utf8 identifiers */
            goto parse_ident_fast;
        tcc_error("unrecognized character \\x%02x", c);
        break;
    }
    // tok_flags = 0;
keep_tok_flags:
    file->buf_ptr = p;
#if defined(PARSE_DEBUG)
    printf("token = %d %s\n", tok, get_tok_str(tok, &tokc));
#endif
}

/* get a token from an integer array and increment pointer
   accordingly. we code it as a macro to avoid pointer aliasing. */
static inline void TOK_GET(int *t, const int **pp, CValue *cv)
{
    const int *p = *pp;
    int n, *tab;

    tab = cv->tab;
    switch(*t = *p++) {
#if LONG_SIZE == 4
    case TOK_CLONG:
#endif
    case TOK_CINT:
    case TOK_CCHAR:
    case TOK_LCHAR:
    case TOK_LINENUM:
        cv->i = *p++;
        break;
#if LONG_SIZE == 4
    case TOK_CULONG:
#endif
    case TOK_CUINT:
        cv->i = (unsigned)*p++;
        break;
    case TOK_CFLOAT:
    tab[0] = *p++;
    break;
    case TOK_STR:
    case TOK_LSTR:
    case TOK_PPNUM:
    case TOK_PPSTR:
        cv->str.size = *p++;
        cv->str.data = p;
        p += (cv->str.size + sizeof(int) - 1) / sizeof(int);
        break;
    case TOK_CDOUBLE:
    case TOK_CLLONG:
    case TOK_CULLONG:
#if LONG_SIZE == 8
    case TOK_CLONG:
    case TOK_CULONG:
#endif
        n = 2;
        goto copy;
    case TOK_CLDOUBLE:
#if LDOUBLE_SIZE == 16
        n = 4;
#elif LDOUBLE_SIZE == 12
        n = 3;
#elif LDOUBLE_SIZE == 8
        n = 2;
#else
# error add long double size support
#endif
    copy:
        do
            *tab++ = *p++;
        while (--n);
        break;
    default:
        break;
    }
    *pp = p;
}

/* return next token without macro substitution. Can read input from
   macro_ptr buffer */
static void next_nomacro_spc(void)
{
    if (macro_ptr) {
    redo:
        tok = *macro_ptr;
        if (tok) {
            TOK_GET(&tok, &macro_ptr, &tokc);
            if (tok == TOK_LINENUM) {
                file->line_num = tokc.i;
                goto redo;
            }
        }
    } else {
        next_nomacro1();
    }
    //printf("token = %s\n", get_tok_str(tok, &tokc));
}

static void next_nomacro(void)
{
    do {
        next_nomacro_spc();
    } while (tok < 256 && (isidnum_table[tok - CH_EOF] & IS_SPC));
}

/* evaluate escape codes in a string. */
static void parse_escape_string(CString *outstr, const uint8_t *buf, int is_long)
{
    int c, n;
    const uint8_t *p;

    p = buf;
    for(;;) {
        c = *p;
        if (c == '\0')
            break;
        if (c == '\\') {
            p++;
            /* escape */
            c = *p;
            switch(c) {
            case '0': case '1': case '2': case '3':
            case '4': case '5': case '6': case '7':
                /* at most three octal digits */
                n = c - '0';
                p++;
                c = *p;
                if (isoct(c)) {
                    n = n * 8 + c - '0';
                    p++;
                    c = *p;
                    if (isoct(c)) {
                        n = n * 8 + c - '0';
                        p++;
                    }
                }
                c = n;
                goto add_char_nonext;
            case 'x':
            case 'u':
            case 'U':
                p++;
                n = 0;
                for(;;) {
                    c = *p;
                    if (c >= 'a' && c <= 'f')
                        c = c - 'a' + 10;
                    else if (c >= 'A' && c <= 'F')
                        c = c - 'A' + 10;
                    else if (isnum(c))
                        c = c - '0';
                    else
                        break;
                    n = n * 16 + c;
                    p++;
                }
                c = n;
                goto add_char_nonext;
            case 'a':
                c = '\a';
                break;
            case 'b':
                c = '\b';
                break;
            case 'f':
                c = '\f';
                break;
            case 'n':
                c = '\n';
                break;
            case 'r':
                c = '\r';
                break;
            case 't':
                c = '\t';
                break;
            case 'v':
                c = '\v';
                break;
            case 'e':
                if (!gnu_ext)
                    goto invalid_escape;
                c = 27;
                break;
            case '\'':
            case '\"':
            case '\\':
            case '?':
                break;
            default:
            invalid_escape:
                if (c >= '!' && c <= '~')
                    tcc_warning("unknown escape sequence: \'\\%c\'", c);
                else
                    tcc_warning("unknown escape sequence: \'\\x%x\'", c);
                break;
            }
        } else if (is_long && c >= 0x80) {
            /* assume we are processing UTF-8 sequence */
            /* reference: The Unicode Standard, Version 10.0, ch3.9 */

            int cont; /* count of continuation bytes */
            int skip; /* how many bytes should skip when error occurred */
            int i;

            /* decode leading byte */
            if (c < 0xC2) {
                skip = 1; goto invalid_utf8_sequence;
            } else if (c <= 0xDF) {
                cont = 1; n = c & 0x1f;
            } else if (c <= 0xEF) {
                cont = 2; n = c & 0xf;
            } else if (c <= 0xF4) {
                cont = 3; n = c & 0x7;
            } else {
                skip = 1; goto invalid_utf8_sequence;
            }

            /* decode continuation bytes */
            for (i = 1; i <= cont; i++) {
                int l = 0x80, h = 0xBF;

                /* adjust limit for second byte */
                if (i == 1) {
                    switch (c) {
                    case 0xE0: l = 0xA0; break;
                    case 0xED: h = 0x9F; break;
                    case 0xF0: l = 0x90; break;
                    case 0xF4: h = 0x8F; break;
                    }
                }

                if (p[i] < l || p[i] > h) {
                    skip = i; goto invalid_utf8_sequence;
                }

                n = (n << 6) | (p[i] & 0x3f);
            }

            /* advance pointer */
            p += 1 + cont;
            c = n;
            goto add_char_nonext;

            /* error handling */
        invalid_utf8_sequence:
            tcc_warning("ill-formed UTF-8 subsequence starting with: \'\\x%x\'", c);
            c = 0xFFFD;
            p += skip;
            goto add_char_nonext;

        }
        p++;
    add_char_nonext:
        if (!is_long)
            cstr_ccat(outstr, c);
        else {
#ifdef TCC_TARGET_PE
            /* store as UTF-16 */
            if (c < 0x10000) {
                cstr_wccat(outstr, c);
            } else {
                c -= 0x10000;
                cstr_wccat(outstr, (c >> 10) + 0xD800);
                cstr_wccat(outstr, (c & 0x3FF) + 0xDC00);
            }
#else
            cstr_wccat(outstr, c);
#endif
        }
    }
    /* add a trailing '\0' */
    if (!is_long)
        cstr_ccat(outstr, '\0');
    else
        cstr_wccat(outstr, '\0');
}

static void parse_string(const char *s, int len)
{
    uint8_t buf[1000], *p = buf;
    int is_long, sep;

    if ((is_long = *s == 'L'))
        ++s, --len;
    sep = *s++;
    len -= 2;
    if (len >= sizeof buf)
        p = (uint8_t*)tcc_malloc(len + 1);
    memcpy(p, s, len);
    p[len] = 0;

    cstr_reset(&tokcstr);
    parse_escape_string(&tokcstr, p, is_long);
    if (p != buf)
        tcc_free(p);

    if (sep == '\'') {
        int char_size, i, n, c;
        /* XXX: make it portable */
        if (!is_long)
            tok = TOK_CCHAR, char_size = 1;
        else
            tok = TOK_LCHAR, char_size = sizeof(nwchar_t);
        n = tokcstr.size / char_size - 1;
        if (n < 1)
            tcc_error("empty character constant");
        if (n > 1)
            tcc_warning("multi-character character constant");
        for (c = i = 0; i < n; ++i) {
            if (is_long)
                c = ((nwchar_t *)tokcstr.data)[i];
            else
                c = (c << 8) | ((char *)tokcstr.data)[i];
        }
        tokc.i = c;
    } else {
        tokc.str.size = tokcstr.size;
        tokc.str.data = tokcstr.data;
        if (!is_long)
            tok = TOK_STR;
        else
            tok = TOK_LSTR;
    }
}

/* bn = (bn << shift) | or_val */
static void bn_lshift(unsigned int *bn, int shift, int or_val)
{
    int i;
    unsigned int v;
    for(i=0;i<BN_SIZE;i++) {
        v = bn[i];
        bn[i] = (v << shift) | or_val;
        or_val = v >> (32 - shift);
    }
}

static void bn_zero(unsigned int *bn)
{
    int i;
    for(i=0;i<BN_SIZE;i++) {
        bn[i] = 0;
    }
}

/* parse number in null terminated string 'p' and return it in the
   current token */
static void parse_number(const char *p)
{
    int b, t, shift, frac_bits, s, exp_val, ch;
    char *q;
    unsigned int bn[BN_SIZE];
    double d;

    /* number */
    q = token_buf;
    ch = *p++;
    t = ch;
    ch = *p++;
    *q++ = t;
    b = 10;
    if (t == '.') {
        goto float_frac_parse;
    } else if (t == '0') {
        if (ch == 'x' || ch == 'X') {
            q--;
            ch = *p++;
            b = 16;
        }
    }
    /* parse all digits. cannot check octal numbers at this stage
       because of floating point constants */
    while (1) {
        if (ch >= 'a' && ch <= 'f')
            t = ch - 'a' + 10;
        else if (ch >= 'A' && ch <= 'F')
            t = ch - 'A' + 10;
        else if (isnum(ch))
            t = ch - '0';
        else
            break;
        if (t >= b)
            break;
        if (q >= token_buf + STRING_MAX_SIZE) {
        num_too_long:
            tcc_error("number too long");
        }
        *q++ = ch;
        ch = *p++;
    }
    if (ch == '.' ||
        ((ch == 'e' || ch == 'E') && b == 10) ||
        ((ch == 'p' || ch == 'P') && (b == 16 || b == 2))) {
        if (b != 10) {
            /* NOTE: strtox should support that for hexa numbers, but
               non ISOC99 libcs do not support it, so we prefer to do
               it by hand */
            /* hexadecimal or binary floats */
            /* XXX: handle overflows */
            *q = '\0';
            if (b == 16)
                shift = 4;
            else
                shift = 1;
            bn_zero(bn);
            q = token_buf;
            while (1) {
                t = *q++;
                if (t == '\0') {
                    break;
                } else if (t >= 'a') {
                    t = t - 'a' + 10;
                } else if (t >= 'A') {
                    t = t - 'A' + 10;
                } else {
                    t = t - '0';
                }
                bn_lshift(bn, shift, t);
            }
            frac_bits = 0;
            if (ch == '.') {
                ch = *p++;
                while (1) {
                    t = ch;
                    if (t >= 'a' && t <= 'f') {
                        t = t - 'a' + 10;
                    } else if (t >= 'A' && t <= 'F') {
                        t = t - 'A' + 10;
                    } else if (t >= '0' && t <= '9') {
                        t = t - '0';
                    } else {
                        break;
                    }
                    if (t >= b)
                        tcc_error("invalid digit");
                    bn_lshift(bn, shift, t);
                    frac_bits += shift;
                    ch = *p++;
                }
            }
            if (ch != 'p' && ch != 'P')
                expect("exponent");
            ch = *p++;
            s = 1;
            exp_val = 0;
            if (ch == '+') {
                ch = *p++;
            } else if (ch == '-') {
                s = -1;
                ch = *p++;
            }
            if (ch < '0' || ch > '9')
                expect("exponent digits");
            while (ch >= '0' && ch <= '9') {
                exp_val = exp_val * 10 + ch - '0';
                ch = *p++;
            }
            exp_val = exp_val * s;

            /* now we can generate the number */
            /* XXX: should patch directly float number */
            d = (double)bn[1] * 4294967296.0 + (double)bn[0];
            d = ldexp(d, exp_val - frac_bits);
            t = toup(ch);
            if (t == 'F') {
                ch = *p++;
                tok = TOK_CFLOAT;
                /* float : should handle overflow */
                tokc.f = (float)d;
            } else if (t == 'L') {
                ch = *p++;
#ifdef TCC_TARGET_PE
                tok = TOK_CDOUBLE;
                tokc.d = d;
#else
                tok = TOK_CLDOUBLE;
                /* XXX: not large enough */
                tokc.ld = (long double)d;
#endif
            } else {
                tok = TOK_CDOUBLE;
                tokc.d = d;
            }
        } else {
            /* decimal floats */
            if (ch == '.') {
                if (q >= token_buf + STRING_MAX_SIZE)
                    goto num_too_long;
                *q++ = ch;
                ch = *p++;
            float_frac_parse:
                while (ch >= '0' && ch <= '9') {
                    if (q >= token_buf + STRING_MAX_SIZE)
                        goto num_too_long;
                    *q++ = ch;
                    ch = *p++;
                }
            }
            if (ch == 'e' || ch == 'E') {
                if (q >= token_buf + STRING_MAX_SIZE)
                    goto num_too_long;
                *q++ = ch;
                ch = *p++;
                if (ch == '-' || ch == '+') {
                    if (q >= token_buf + STRING_MAX_SIZE)
                        goto num_too_long;
                    *q++ = ch;
                    ch = *p++;
                }
                if (ch < '0' || ch > '9')
                    expect("exponent digits");
                while (ch >= '0' && ch <= '9') {
                    if (q >= token_buf + STRING_MAX_SIZE)
                        goto num_too_long;
                    *q++ = ch;
                    ch = *p++;
                }
            }
            *q = '\0';
            t = toup(ch);
            errno = 0;
            if (t == 'F') {
                ch = *p++;
                tok = TOK_CFLOAT;
                tokc.f = strtof(token_buf, NULL);
            } else if (t == 'L') {
                ch = *p++;
#ifdef TCC_TARGET_PE
                tok = TOK_CDOUBLE;
                tokc.d = strtod(token_buf, NULL);
#else
                tok = TOK_CLDOUBLE;
                tokc.ld = strtold(token_buf, NULL);
#endif
            } else {
                tok = TOK_CDOUBLE;
                tokc.d = strtod(token_buf, NULL);
            }
        }
    } else {
        unsigned long long n, n1;
        int lcount, ucount, ov = 0;
        const char *p1;

        /* integer number */
        *q = '\0';
        q = token_buf;
        if (b == 10 && *q == '0') {
            b = 8;
            q++;
        }
        n = 0;
        while(1) {
            t = *q++;
            /* no need for checks except for base 10 / 8 errors */
            if (t == '\0')
                break;
            else if (t >= 'a')
                t = t - 'a' + 10;
            else if (t >= 'A')
                t = t - 'A' + 10;
            else
                t = t - '0';
            if (t >= b)
                tcc_error("invalid digit");
            n1 = n;
            n = n * b + t;
            /* detect overflow */
            if (n1 >= 0x1000000000000000ULL && n / b != n1)
                ov = 1;
        }

        /* Determine the characteristics (unsigned and/or 64bit) the type of
           the constant must have according to the constant suffix(es) */
        lcount = ucount = 0;
        p1 = p;
        for(;;) {
            t = toup(ch);
            if (t == 'L') {
                if (lcount >= 2)
                    tcc_error("three 'l's in integer constant");
                if (lcount && *(p - 1) != ch)
                    tcc_error("incorrect integer suffix: %s", p1);
                lcount++;
                ch = *p++;
            } else if (t == 'U') {
                if (ucount >= 1)
                    tcc_error("two 'u's in integer constant");
                ucount++;
                ch = *p++;
            } else {
                break;
            }
        }

        /* Determine if it needs 64 bits and/or unsigned in order to fit */
        if (ucount == 0 && b == 10) {
            if (lcount <= (LONG_SIZE == 4)) {
                if (n >= 0x80000000U)
                    lcount = (LONG_SIZE == 4) + 1;
            }
            if (n >= 0x8000000000000000ULL)
                ov = 1, ucount = 1;
        } else {
            if (lcount <= (LONG_SIZE == 4)) {
                if (n >= 0x100000000ULL)
                    lcount = (LONG_SIZE == 4) + 1;
                else if (n >= 0x80000000U)
                    ucount = 1;
            }
            if (n >= 0x8000000000000000ULL)
                ucount = 1;
        }

        if (ov)
            tcc_warning("integer constant overflow");

        tok = TOK_CINT;
    if (lcount) {
            tok = TOK_CLONG;
            if (lcount == 2)
                tok = TOK_CLLONG;
    }
    if (ucount)
        ++tok; /* TOK_CU... */
        tokc.i = n;
    }
    if (ch)
        tcc_error("invalid number\n");
}

void next()
{
redo:
    next_nomacro();

    if (macro_ptr) {
        if (tok == TOK_NOSUBST || tok == TOK_PLCHLDR) {
        /* discard preprocessor markers */
            goto redo;
        } else if (tok == 0) {
            /* end of macro or unget token string */
            end_macro();
            goto redo;
        }
    }
    /* convert preprocessor tokens into C tokens */
    if (tok == TOK_PPNUM) {
        /* if  (parse_flags  & PARSE_FLAG_TOK_NUM ) */
            parse_number((char *)tokc.str.data);
    } else if (tok == TOK_PPSTR) {
        /* if (parse_flags & PARSE_FLAG_TOK_STR  ) */
            parse_string((char *)tokc.str.data, tokc.str.size - 1);
    }
}

/* cleanup from error/setjmp */
static void preprocess_end(TCCState *s1)
{
    while (macro_stack)
        end_macro();
    macro_ptr = NULL;
}

void skip(int c)
{
    if (tok != c)
        tcc_error("'%c' expected (got \"%s\")", c, get_tok_str(tok, &tokc));
    next();
}

static int tccgen_compile(TCCState *s1)
{
    tccgen_init_compiler();

    tcc_debug_start(s1);

#ifdef TCC_TARGET_ARM
    arm_init(s1);
#endif

#ifdef INC_DEBUG
    printf("%s: **** new file\n", file->filename);
#endif

    //parse_flags = PARSE_FLAG_PREPROCESS | PARSE_FLAG_TOK_NUM | PARSE_FLAG_TOK_STR;
    next();
    decl(VT_CONST);
    gen_inline_functions(s1);
    check_vstack();
    /* end of translation unit info */
    tcc_debug_end(s1);
    return 0;
}

/* compile the file opened in 'file'. Return non zero if errors. */
static int tcc_compile(TCCState *s1, const char* filename, int flags)
{
    int ret;

    /* open the file */
    ret = tcc_open(s1, filename);
    if (ret < 0) {
        if (flags & AFF_PRINT_ERROR)
            tcc_error_noabort("file '%s' not found", filename);
        return ret;
    }

    tccelf_begin_file(s1);

    if (setjmp(s1->error_jmp_buf) == 0) {
        s1->nb_errors = 0;
        s1->error_set_jmp_enabled = 1;

        pvtop = vtop = vstack - 1; // instead preprocess_start
        if (s1->output_type == TCC_OUTPUT_PREPROCESS) {
            tcc_error_noabort("preprocess not supported");
        } else {
            tccgen_compile(s1);
        }
    }
    s1->error_set_jmp_enabled = 0;

    preprocess_end(s1);
    free_inline_functions(s1);
    sym_pop(&global_stack, NULL, 0);
    sym_pop(&local_stack, NULL, 0);
    tccelf_end_file(s1);
    tcc_close();
    return s1->nb_errors != 0 ? -1 : 0;
}

int add_file(TCCState* s, const char* filename)
{
    int filetype = s->filetype;
    int flags = AFF_PRINT_ERROR;
    if (filetype == 0) {
        /* use a file extension to detect a filetype */
        const char *ext = tcc_fileextension(filename);
        if (ext[0]) {
            ext++;
            if (!strcmp(ext, "S") || !strcmp(ext, "s") ) {
                tcc_error_noabort("file type '%s' not supported", filename);
                return -1;
            } else if (!PATHCMP(ext, "c") || !PATHCMP(ext, "i"))
                filetype = AFF_TYPE_C;
            else
                flags |= AFF_TYPE_BIN;
        } else {
            filetype = AFF_TYPE_C;
        }
        s->filetype = filetype;
    }
    if (flags & AFF_TYPE_BIN)
        return tcc_add_binary_file(s, filename, flags);
    else
        return tcc_compile(s, filename, flags);
}

void parse_mult_str (CString *astr, const char *msg)
{
    /* read the string */
    if (tok != TOK_STR)
        expect(msg);
    cstr_new(astr);
    while (tok == TOK_STR) {
        /* XXX: add \0 handling too ? */
        cstr_cat(astr, (const char*)tokc.str.data, -1);
        next();
    }
    cstr_ccat(astr, '\0');
}

/* Parse __attribute__((...)) GNUC extension. */
static void parse_attribute(AttributeDef *ad)
{
    int t, n;
    CString astr;

redo:
    if (tok != TOK_ATTRIBUTE1 && tok != TOK_ATTRIBUTE2)
        return;
    next();
    skip('(');
    skip('(');
    while (tok != ')') {
        if (tok < TOK_IDENT)
            expect("attribute name");
        t = tok;
        next();
        switch(t) {
        case TOK_SECTION1:
        case TOK_SECTION2:
            skip('(');
        parse_mult_str(&astr, "section name");
            ad->section = find_section(tcc_state, (char *)astr.data);
            skip(')');
        cstr_free(&astr);
            break;
        case TOK_ALIAS1:
        case TOK_ALIAS2:
            skip('(');
        parse_mult_str(&astr, "alias(\"target\")");
            ad->alias_target = /* save string as token, for later */
              tok_alloc((char*)astr.data, astr.size-1)->tok;
            skip(')');
        cstr_free(&astr);
            break;
    case TOK_VISIBILITY1:
    case TOK_VISIBILITY2:
            skip('(');
        parse_mult_str(&astr,
               "visibility(\"default|hidden|internal|protected\")");
        if (!strcmp (astr.data, "default"))
            ad->a.visibility = STV_DEFAULT;
        else if (!strcmp (astr.data, "hidden"))
            ad->a.visibility = STV_HIDDEN;
        else if (!strcmp (astr.data, "internal"))
            ad->a.visibility = STV_INTERNAL;
        else if (!strcmp (astr.data, "protected"))
            ad->a.visibility = STV_PROTECTED;
        else
                expect("visibility(\"default|hidden|internal|protected\")");
            skip(')');
        cstr_free(&astr);
            break;
        case TOK_ALIGNED1:
        case TOK_ALIGNED2:
            if (tok == '(') {
                next();
                n = const_int32_expr();
                if (n <= 0 || (n & (n - 1)) != 0)
                    tcc_error("alignment must be a positive power of two");
                skip(')');
            } else {
                n = MAX_ALIGN;
            }
            ad->a.aligned = exact_log2p1(n);
        if (n != 1 << (ad->a.aligned - 1))
          tcc_error("alignment of %d is larger than implemented", n);
            break;
        case TOK_PACKED1:
        case TOK_PACKED2:
            ad->a.packed = 1;
            break;
        case TOK_WEAK1:
        case TOK_WEAK2:
            ad->a.weak = 1;
            break;
        case TOK_UNUSED1:
        case TOK_UNUSED2:
            /* currently, no need to handle it because tcc does not
               track unused objects */
            break;
        case TOK_NORETURN1:
        case TOK_NORETURN2:
            /* currently, no need to handle it because tcc does not
               track unused objects */
            break;
        case TOK_CDECL1:
        case TOK_CDECL2:
        case TOK_CDECL3:
            ad->f.func_call = FUNC_CDECL;
            break;
        case TOK_STDCALL1:
        case TOK_STDCALL2:
        case TOK_STDCALL3:
            ad->f.func_call = FUNC_STDCALL;
            break;
#ifdef TCC_TARGET_I386
        case TOK_REGPARM1:
        case TOK_REGPARM2:
            skip('(');
            n = const_int32_expr();
            if (n > 3)
                n = 3;
            else if (n < 0)
                n = 0;
            if (n > 0)
                ad->f.func_call = FUNC_FASTCALL1 + n - 1;
            skip(')');
            break;
        case TOK_FASTCALL1:
        case TOK_FASTCALL2:
        case TOK_FASTCALL3:
            ad->f.func_call = FUNC_FASTCALLW;
            break;
#endif
        case TOK_MODE:
            skip('(');
            switch(tok) {
                case TOK_MODE_DI:
                    ad->attr_mode = VT_LLONG + 1;
                    break;
                case TOK_MODE_QI:
                    ad->attr_mode = VT_BYTE + 1;
                    break;
                case TOK_MODE_HI:
                    ad->attr_mode = VT_SHORT + 1;
                    break;
                case TOK_MODE_SI:
                case TOK_MODE_word:
                    ad->attr_mode = VT_INT + 1;
                    break;
                default:
                    tcc_warning("__mode__(%s) not supported\n", get_tok_str(tok, NULL));
                    break;
            }
            next();
            skip(')');
            break;
        case TOK_DLLEXPORT:
            ad->a.dllexport = 1;
            break;
        case TOK_DLLIMPORT:
            ad->a.dllimport = 1;
            break;
        default:
            tcc_warning("'%s' attribute ignored", get_tok_str(t, NULL));
            /* skip parameters */
            if (tok == '(') {
                int parenthesis = 0;
                do {
                    if (tok == '(')
                        parenthesis++;
                    else if (tok == ')')
                        parenthesis--;
                    next();
                } while (parenthesis && tok != -1);
            }
            break;
        }
        if (tok != ',')
            break;
        next();
    }
    skip(')');
    skip(')');
    goto redo;
}

/* parse a function defined by symbol 'sym' and generate its code in
   'cur_text_section' */
static void gen_function(Sym *sym)
{
    gen_function_begin(sym);
    statement(NULL, NULL, 0);
    gen_function_end(sym);
}

static void gen_inline_functions(TCCState *s)
{
    Sym *sym;
    int inline_generated, i, ln;
    struct InlineFunc *fn;

    ln = file->line_num;
    /* iterate while inline function are referenced */
    do {
        inline_generated = 0;
        for (i = 0; i < s->nb_inline_fns; ++i) {
            fn = s->inline_fns[i];
            sym = fn->sym;
            if (sym && sym->c) {
                /* the function was used: generate its code and
                   convert it to a normal function */
                fn->sym = NULL;
                if (file)
                    pstrcpy(file->filename, sizeof file->filename, fn->filename);
                sym->type.t &= ~VT_INLINE;

                begin_macro(fn->func_str, 1);
                next();
                cur_text_section = text_section;
                gen_function(sym);
                end_macro();

                inline_generated = 1;
            }
        }
    } while (inline_generated);
    file->line_num = ln;
}

static int set_idnum(int c, int val)
{
    int prev = isidnum_table[c - CH_EOF];
    isidnum_table[c - CH_EOF] = val;
    return prev;
}

void parser_init()
{
    /* init isid table */
    int i;
    for(i = CH_EOF; i<128; i++)
        set_idnum(i,
            is_space(i) ? IS_SPC
            : isid(i) ? IS_ID
            : isnum(i) ? IS_NUM
            : 0);

    for(i = 128; i<256; i++)
        set_idnum(i, IS_ID);

    set_idnum('$',  IS_ID );

    symbols_init();

    int c;
    const char *p, *r;
    p = tcc_keywords;
    while (*p) {
        r = p;
        for(;;) {
            c = *r++;
            if (c == '\0')
                break;
        }
        tok_alloc(p, r - p - 1);
        p = r;
    }
}

void parser_free()
{
    cstr_free(&tokcstr);
    while (file)
        tcc_close();
    symbols_free();
}

/* enum/struct/union declaration. u is VT_ENUM/VT_STRUCT/VT_UNION */
static void struct_decl(CType *type, int u)
{
    int v, c, size, align, flexible;
    int bit_size, bsize, bt;
    Sym *s, *ss, **ps;
    AttributeDef ad, ad1;
    CType type1, btype;

    memset(&ad, 0, sizeof ad);
    next();
    parse_attribute(&ad);
    if (tok != '{') {
        v = tok;
        next();
        /* struct already defined ? return it */
        if (v < TOK_IDENT)
            expect("struct/union/enum name");
        s = struct_find(v);
        if (s && (s->sym_scope == local_scope || tok != '{')) {
            if (u == s->type.t)
                goto do_decl;
            if (u == VT_ENUM && IS_ENUM(s->type.t))
                goto do_decl;
            tcc_error("redefinition of '%s'", get_tok_str(v, NULL));
        }
    } else {
        v = anon_sym++;
    }
    /* Record the original enum/struct/union token.  */
    type1.t = u == VT_ENUM ? u | VT_INT | VT_UNSIGNED : u;
    type1.ref = NULL;
    /* we put an undefined size for struct/union */
    s = sym_push(v | SYM_STRUCT, &type1, 0, -1);
    s->r = 0; /* default alignment is zero as gcc */
do_decl:
    type->t = s->type.t;
    type->ref = s;

    if (tok == '{') {
        next();
        if (s->c != -1)
            tcc_error("struct/union/enum already defined");
        /* cannot be empty */
        /* non empty enums are not allowed */
        ps = &s->next;
        if (u == VT_ENUM) {
            long long ll = 0, pl = 0, nl = 0;
            CType t;
            t.ref = s;
            /* enum symbols have static storage */
            t.t = VT_INT|VT_STATIC|VT_ENUM_VAL;
            for(;;) {
                v = tok;
                if (v < TOK_UIDENT)
                    expect("identifier");
                ss = sym_find(v);
                if (ss && !local_stack)
                    tcc_error("redefinition of enumerator '%s'",
                              get_tok_str(v, NULL));
                next();
                if (tok == '=') {
                    next();
                    ll = const_int64_expr();
                }
                ss = sym_push(v, &t, VT_CONST, 0);
                ss->enum_val = ll;
                *ps = ss, ps = &ss->next;
                if (ll < nl)
                    nl = ll;
                if (ll > pl)
                    pl = ll;
                if (tok != ',')
                    break;
                next();
                ll++;
                /* NOTE: we accept a trailing comma */
                if (tok == '}')
                    break;
            }
            skip('}');
            /* set integral type of the enum */
            t.t = VT_INT;
            if (nl >= 0) {
                if (pl != (unsigned)pl)
                    t.t = (LONG_SIZE==8 ? VT_LLONG|VT_LONG : VT_LLONG);
                t.t |= VT_UNSIGNED;
            } else if (pl != (int)pl || nl != (int)nl)
                t.t = (LONG_SIZE==8 ? VT_LLONG|VT_LONG : VT_LLONG);
            s->type.t = type->t = t.t | VT_ENUM;
            s->c = 0;
            /* set type for enum members */
            for (ss = s->next; ss; ss = ss->next) {
                ll = ss->enum_val;
                if (ll == (int)ll) /* default is int if it fits */
                    continue;
                if (t.t & VT_UNSIGNED) {
                    ss->type.t |= VT_UNSIGNED;
                    if (ll == (unsigned)ll)
                        continue;
                }
                ss->type.t = (ss->type.t & ~VT_BTYPE)
                        | (LONG_SIZE==8 ? VT_LLONG|VT_LONG : VT_LLONG);
            }
        } else {
            c = 0;
            flexible = 0;
            while (tok != '}') {
                if (!parse_btype(&btype, &ad1)) {
                    skip(';');
                    continue;
                }
                while (1) {
                    if (flexible)
                        tcc_error("flexible array member '%s' not at the end of struct",
                                  get_tok_str(v, NULL));
                    bit_size = -1;
                    v = 0;
                    type1 = btype;
                    if (tok != ':') {
                        if (tok != ';')
                            type_decl(&type1, &ad1, &v, TYPE_DIRECT);
                        if (v == 0) {
                            if ((type1.t & VT_BTYPE) != VT_STRUCT)
                                expect("identifier");
                            else {
                                int v = btype.ref->v;
                                if (!(v & SYM_FIELD) && (v & ~SYM_STRUCT) < SYM_FIRST_ANOM) {
                                    if (tcc_state->ms_extensions == 0)
                                        expect("identifier");
                                }
                            }
                        }
                        if (type_size(&type1, &align) < 0) {
                            if ((u == VT_STRUCT) && (type1.t & VT_ARRAY) && c)
                                flexible = 1;
                            else
                                tcc_error("field '%s' has incomplete type",
                                          get_tok_str(v, NULL));
                        }
                        if ((type1.t & VT_BTYPE) == VT_FUNC ||
                                (type1.t & VT_STORAGE))
                            tcc_error("invalid type for '%s'",
                                      get_tok_str(v, NULL));
                    }
                    if (tok == ':') {
                        next();
                        bit_size = const_int32_expr();
                        /* XXX: handle v = 0 case for messages */
                        if (bit_size < 0)
                            tcc_error("negative width in bit-field '%s'",
                                      get_tok_str(v, NULL));
                        if (v && bit_size == 0)
                            tcc_error("zero width for bit-field '%s'",
                                      get_tok_str(v, NULL));
                        parse_attribute(&ad1);
                    }
                    size = type_size(&type1, &align);
                    if (bit_size >= 0) {
                        bt = type1.t & VT_BTYPE;
                        if (bt != VT_INT &&
                                bt != VT_BYTE &&
                                bt != VT_SHORT &&
                                bt != VT_BOOL &&
                                bt != VT_LLONG)
                            tcc_error("bitfields must have scalar type");
                        bsize = size * 8;
                        if (bit_size > bsize) {
                            tcc_error("width of '%s' exceeds its type",
                                      get_tok_str(v, NULL));
                        } else if (bit_size == bsize
                                   && !ad.a.packed && !ad1.a.packed) {
                            /* no need for bit fields */
                            ;
                        } else if (bit_size == 64) {
                            tcc_error("field width 64 not implemented");
                        } else {
                            type1.t = (type1.t & ~VT_STRUCT_MASK)
                                    | VT_BITFIELD
                                    | (bit_size << (VT_STRUCT_SHIFT + 6));
                        }
                    }
                    if (v != 0 || (type1.t & VT_BTYPE) == VT_STRUCT) {
                        /* Remember we've seen a real field to check
               for placement of flexible array member. */
                        c = 1;
                    }
                    /* If member is a struct or bit-field, enforce
               placing into the struct (as anonymous).  */
                    if (v == 0 &&
                            ((type1.t & VT_BTYPE) == VT_STRUCT ||
                             bit_size >= 0)) {
                        v = anon_sym++;
                    }
                    if (v) {
                        ss = sym_push(v | SYM_FIELD, &type1, 0, 0);
                        ss->a = ad1.a;
                        *ps = ss;
                        ps = &ss->next;
                    }
                    if (tok == ';' || tok == TOK_EOF)
                        break;
                    skip(',');
                }
                skip(';');
            }
            skip('}');
            parse_attribute(&ad);
            struct_layout(type, &ad);
        }
    }
}


/* Add type qualifiers to a type. If the type is an array then the qualifiers
   are added to the element type, copied because it could be a typedef. */
static void parse_btype_qualify(CType *type, int qualifiers)
{
    while (type->t & VT_ARRAY) {
        type->ref = sym_push(SYM_FIELD, &type->ref->type, 0, type->ref->c);
        type = &type->ref->type;
    }
    type->t |= qualifiers;
}

/* return 0 if no type declaration. otherwise, return the basic type
   and skip it.
 */
static int parse_btype(CType *type, AttributeDef *ad)
{
    int t, u, bt, st, type_found, typespec_found, g;
    Sym *s;
    CType type1;

    memset(ad, 0, sizeof(AttributeDef));
    type_found = 0;
    typespec_found = 0;
    t = VT_INT;
    bt = st = -1;
    type->ref = NULL;

    while(1) {
        switch(tok) {
        case TOK_EXTENSION:
            /* currently, we really ignore extension */
            next();
            continue;

            /* basic types */
        case TOK_CHAR:
            u = VT_BYTE;
basic_type:
            next();
basic_type1:
            if (u == VT_SHORT || u == VT_LONG) {
                if (st != -1 || (bt != -1 && bt != VT_INT))
tmbt: tcc_error("too many basic types");
                st = u;
            } else {
                if (bt != -1 || (st != -1 && u != VT_INT))
                    goto tmbt;
                bt = u;
            }
            if (u != VT_INT)
                t = (t & ~(VT_BTYPE|VT_LONG)) | u;
            typespec_found = 1;
            break;
        case TOK_VOID:
            u = VT_VOID;
            goto basic_type;
        case TOK_SHORT:
            u = VT_SHORT;
            goto basic_type;
        case TOK_INT:
            u = VT_INT;
            goto basic_type;
        case TOK_LONG:
            if ((t & VT_BTYPE) == VT_DOUBLE) {
                t = (t & ~(VT_BTYPE|VT_LONG)) | VT_LDOUBLE;
            } else if ((t & (VT_BTYPE|VT_LONG)) == VT_LONG) {
                t = (t & ~(VT_BTYPE|VT_LONG)) | VT_LLONG;
            } else {
                u = VT_LONG;
                goto basic_type;
            }
            next();
            break;
#ifdef TCC_TARGET_ARM64
        case TOK_UINT128:
            /* GCC's __uint128_t appears in some Linux header files. Make it a
               synonym for long double to get the size and alignment right. */
            u = VT_LDOUBLE;
            goto basic_type;
#endif
        case TOK_BOOL:
            u = VT_BOOL;
            goto basic_type;
        case TOK_FLOAT:
            u = VT_FLOAT;
            goto basic_type;
        case TOK_DOUBLE:
            if ((t & (VT_BTYPE|VT_LONG)) == VT_LONG) {
                t = (t & ~(VT_BTYPE|VT_LONG)) | VT_LDOUBLE;
            } else {
                u = VT_DOUBLE;
                goto basic_type;
            }
            next();
            break;
        case TOK_ENUM:
            struct_decl(&type1, VT_ENUM);
basic_type2:
            u = type1.t;
            type->ref = type1.ref;
            goto basic_type1;
        case TOK_STRUCT:
            struct_decl(&type1, VT_STRUCT);
            goto basic_type2;
        case TOK_UNION:
            struct_decl(&type1, VT_UNION);
            goto basic_type2;

            /* type modifiers */
        case TOK_CONST1:
        case TOK_CONST2:
        case TOK_CONST3:
            type->t = t;
            parse_btype_qualify(type, VT_CONSTANT);
            t = type->t;
            next();
            break;
        case TOK_VOLATILE1:
        case TOK_VOLATILE2:
        case TOK_VOLATILE3:
            type->t = t;
            parse_btype_qualify(type, VT_VOLATILE);
            t = type->t;
            next();
            break;
        case TOK_SIGNED1:
        case TOK_SIGNED2:
        case TOK_SIGNED3:
            if ((t & (VT_DEFSIGN|VT_UNSIGNED)) == (VT_DEFSIGN|VT_UNSIGNED))
                tcc_error("signed and unsigned modifier");
            t |= VT_DEFSIGN;
            next();
            typespec_found = 1;
            break;
        case TOK_REGISTER:
        case TOK_AUTO:
        case TOK_RESTRICT1:
        case TOK_RESTRICT2:
        case TOK_RESTRICT3:
            next();
            break;
        case TOK_UNSIGNED:
            if ((t & (VT_DEFSIGN|VT_UNSIGNED)) == VT_DEFSIGN)
                tcc_error("signed and unsigned modifier");
            t |= VT_DEFSIGN | VT_UNSIGNED;
            next();
            typespec_found = 1;
            break;

            /* storage */
        case TOK_EXTERN:
            g = VT_EXTERN;
            goto storage;
        case TOK_STATIC:
            g = VT_STATIC;
            goto storage;
        case TOK_TYPEDEF:
            g = VT_TYPEDEF;
            goto storage;
storage:
            if (t & (VT_EXTERN|VT_STATIC|VT_TYPEDEF) & ~g)
                tcc_error("multiple storage classes");
            t |= g;
            next();
            break;
        case TOK_INLINE1:
        case TOK_INLINE2:
        case TOK_INLINE3:
            t |= VT_INLINE;
            next();
            break;

            /* GNUC attribute */
        case TOK_ATTRIBUTE1:
        case TOK_ATTRIBUTE2:
            parse_attribute(ad);
            if (ad->attr_mode) {
                u = ad->attr_mode -1;
                t = (t & ~(VT_BTYPE|VT_LONG)) | u;
            }
            break;
            /* GNUC typeof */
        case TOK_TYPEOF1:
        case TOK_TYPEOF2:
        case TOK_TYPEOF3:
            next();
            parse_expr_type(&type1);
            /* remove all storage modifiers except typedef */
            type1.t &= ~(VT_STORAGE&~VT_TYPEDEF);
            if (type1.ref)
                sym_to_attr(ad, type1.ref);
            goto basic_type2;
        default:
            if (typespec_found)
                goto the_end;
            s = sym_find(tok);
            if (!s || !(s->type.t & VT_TYPEDEF))
                goto the_end;
            t &= ~(VT_BTYPE|VT_LONG);
            u = t & ~(VT_CONSTANT | VT_VOLATILE), t ^= u;
            type->t = (s->type.t & ~VT_TYPEDEF) | u;
            type->ref = s->type.ref;
            if (t)
                parse_btype_qualify(type, t);
            t = type->t;
            /* get attributes from typedef */
            sym_to_attr(ad, s);
            next();
            typespec_found = 1;
            st = bt = -2;
            break;
        }
        type_found = 1;
    }
the_end:
    if (char_is_unsigned) {
        if ((t & (VT_DEFSIGN|VT_BTYPE)) == VT_BYTE)
            t |= VT_UNSIGNED;
    }
    /* VT_LONG is used just as a modifier for VT_INT / VT_LLONG */
    bt = t & (VT_BTYPE|VT_LONG);
    if (bt == VT_LONG)
        t |= LONG_SIZE == 8 ? VT_LLONG : VT_INT;
#ifdef TCC_TARGET_PE
    if (bt == VT_LDOUBLE)
        t = (t & ~(VT_BTYPE|VT_LONG)) | VT_DOUBLE;
#endif
    type->t = t;
    return type_found;
}

/* convert a function parameter type (array to pointer and function to
   function pointer) */
static inline void convert_parameter_type(CType *pt)
{
    /* remove const and volatile qualifiers (XXX: const could be used
       to indicate a const function parameter */
    pt->t &= ~(VT_CONSTANT | VT_VOLATILE);
    /* array must be transformed to pointer according to ANSI C */
    pt->t &= ~VT_ARRAY;
    if ((pt->t & VT_BTYPE) == VT_FUNC) {
        make_pointer(pt);
    }
}

void parse_asm_str(CString *astr)
{
    skip('(');
    parse_mult_str(astr, "string constant");
}

/* Parse an asm label and return the token */
static int asm_label_instr(void)
{
    int v;
    CString astr;

    next();
    parse_asm_str(&astr);
    skip(')');
#ifdef ASM_DEBUG
    printf("asm_alias: \"%s\"\n", (char *)astr.data);
#endif
    v = tok_alloc(astr.data, astr.size - 1)->tok;
    cstr_free(&astr);
    return v;
}

static int post_type(CType *type, AttributeDef *ad, int storage, int td)
{
    int n, l, t1, arg_size, align;
    Sym **plast, *s, *first;
    AttributeDef ad1;
    CType pt;

    if (tok == '(') {
        /* function type, or recursive declarator (return if so) */
        next();
        if (td && !(td & TYPE_ABSTRACT))
            return 0;
        if (tok == ')')
            l = 0;
        else if (parse_btype(&pt, &ad1))
            l = FUNC_NEW;
        else if (td)
            return 0;
        else
            l = FUNC_OLD;
        first = NULL;
        plast = &first;
        arg_size = 0;
        if (l) {
            for(;;) {
                /* read param name and compute offset */
                if (l != FUNC_OLD) {
                    if ((pt.t & VT_BTYPE) == VT_VOID && tok == ')')
                        break;
                    type_decl(&pt, &ad1, &n, TYPE_DIRECT | TYPE_ABSTRACT);
                    if ((pt.t & VT_BTYPE) == VT_VOID)
                        tcc_error("parameter declared as void");
                    arg_size += (type_size(&pt, &align) + PTR_SIZE - 1) / PTR_SIZE;
                } else {
                    n = tok;
                    if (n < TOK_UIDENT)
                        expect("identifier");
                    pt.t = VT_VOID; /* invalid type */
                    next();
                }
                convert_parameter_type(&pt);
                s = sym_push(n | SYM_FIELD, &pt, 0, 0);
                *plast = s;
                plast = &s->next;
                if (tok == ')')
                    break;
                skip(',');
                if (l == FUNC_NEW && tok == TOK_DOTS) {
                    l = FUNC_ELLIPSIS;
                    next();
                    break;
                }
                if (l == FUNC_NEW && !parse_btype(&pt, &ad1))
                    tcc_error("invalid type");
            }
        } else
            /* if no parameters, then old type prototype */
            l = FUNC_OLD;
        skip(')');
        /* NOTE: const is ignored in returned type as it has a special
           meaning in gcc / C++ */
        type->t &= ~VT_CONSTANT;
        /* some ancient pre-K&R C allows a function to return an array
           and the array brackets to be put after the arguments, such
           that "int c()[]" means something like "int[] c()" */
        if (tok == '[') {
            next();
            skip(']'); /* only handle simple "[]" */
            make_pointer(type);
        }
        /* we push a anonymous symbol which will contain the function prototype */
        ad->f.func_args = arg_size;
        ad->f.func_type = l;
        s = sym_push(SYM_FIELD, type, 0, 0);
        s->a = ad->a;
        s->f = ad->f;
        s->next = first;
        type->t = VT_FUNC;
        type->ref = s;
    } else if (tok == '[') {
        int saved_nocode_wanted = nocode_wanted;
        /* array definition */
        next();
        if (tok == TOK_RESTRICT1)
            next();
        n = -1;
        t1 = 0;
        if (tok != ']') {
            if (!local_stack || (storage & VT_STATIC))
                vpushi(const_int32_expr());
            else {
                /* VLAs (which can only happen with local_stack && !VT_STATIC)
           length must always be evaluated, even under nocode_wanted,
           so that its size slot is initialized (e.g. under sizeof
           or typeof).  */
                nocode_wanted = 0;
                expression();
            }
            if ((vtop->r & (VT_VALMASK | VT_LVAL | VT_SYM)) == VT_CONST) {
                n = vtop->c.i;
                if (n < 0)
                    tcc_error("invalid array size");
            } else {
                if (!is_integer_btype(vtop->type.t & VT_BTYPE))
                    tcc_error("size of variable length array should be an integer");
                t1 = VT_VLA;
            }
        }
        skip(']');
        /* parse next post type */
        post_type(type, ad, storage, 0);
        if (type->t == VT_FUNC)
            tcc_error("declaration of an array of functions");
        t1 |= type->t & VT_VLA;

        if (t1 & VT_VLA) {
            loc -= type_size(&int_type, &align);
            loc &= -align;
            n = loc;

            vla_runtime_type_size(type, &align);
            gen_op('*');
            vset(&int_type, VT_LOCAL|VT_LVAL, n);
            vswap();
            vstore();
        }
        if (n != -1)
            vpop();
        nocode_wanted = saved_nocode_wanted;

        /* we push an anonymous symbol which will contain the array
           element type */
        s = sym_push(SYM_FIELD, type, 0, n);
        type->t = (t1 ? VT_VLA : VT_ARRAY) | VT_PTR;
        type->ref = s;
    }
    return 1;
}

/* Parse a type declarator (except basic type), and return the type
   in 'type'. 'td' is a bitmask indicating which kind of type decl is
   expected. 'type' should contain the basic type. 'ad' is the
   attribute definition of the basic type. It can be modified by
   type_decl().  If this (possibly abstract) declarator is a pointer chain
   it returns the innermost pointed to type (equals *type, but is a different
   pointer), otherwise returns type itself, that's used for recursive calls.  */
static CType *type_decl(CType *type, AttributeDef *ad, int *v, int td)
{
    CType *post, *ret;
    int qualifiers, storage;

    /* recursive type, remove storage bits first, apply them later again */
    storage = type->t & VT_STORAGE;
    type->t &= ~VT_STORAGE;
    post = ret = type;

    while (tok == '*') {
        qualifiers = 0;
redo:
        next();
        switch(tok) {
        case TOK_CONST1:
        case TOK_CONST2:
        case TOK_CONST3:
            qualifiers |= VT_CONSTANT;
            goto redo;
        case TOK_VOLATILE1:
        case TOK_VOLATILE2:
        case TOK_VOLATILE3:
            qualifiers |= VT_VOLATILE;
            goto redo;
        case TOK_RESTRICT1:
        case TOK_RESTRICT2:
        case TOK_RESTRICT3:
            goto redo;
            /* XXX: clarify attribute handling */
        case TOK_ATTRIBUTE1:
        case TOK_ATTRIBUTE2:
            parse_attribute(ad);
            break;
        }
        make_pointer(type);
        type->t |= qualifiers;
        if (ret == type)
            /* innermost pointed to type is the one for the first derivation */
            ret = pointed_type(type);
    }

    if (tok == '(') {
        /* This is possibly a parameter type list for abstract declarators
       ('int ()'), use post_type for testing this.  */
        if (!post_type(type, ad, 0, td)) {
            /* It's not, so it's a nested declarator, and the post operations
           apply to the innermost pointed to type (if any).  */
            /* XXX: this is not correct to modify 'ad' at this point, but
           the syntax is not clear */
            parse_attribute(ad);
            post = type_decl(type, ad, v, td);
            skip(')');
        }
    } else if (tok >= TOK_IDENT && (td & TYPE_DIRECT)) {
        /* type identifier */
        *v = tok;
        next();
    } else {
        if (!(td & TYPE_ABSTRACT))
            expect("identifier");
        *v = 0;
    }
    post_type(post, ad, storage, 0);
    parse_attribute(ad);
    type->t |= storage;
    return ret;
}

/* pass a parameter to a function and do type checking and casting */
static void gfunc_param_typed(Sym *func, Sym *arg)
{
    int func_type;
    CType type;

    func_type = func->f.func_type;
    if (func_type == FUNC_OLD ||
            (func_type == FUNC_ELLIPSIS && arg == NULL)) {
        /* default casting : only need to convert float to double */
        if ((vtop->type.t & VT_BTYPE) == VT_FLOAT) {
            gen_cast_s(VT_DOUBLE);
        } else if (vtop->type.t & VT_BITFIELD) {
            type.t = vtop->type.t & (VT_BTYPE | VT_UNSIGNED);
            type.ref = vtop->type.ref;
            gen_cast(&type);
        }
    } else if (arg == NULL) {
        tcc_error("too many arguments to function");
    } else {
        type = arg->type;
        type.t &= ~VT_CONSTANT; /* need to do that to avoid false warning */
        gen_assign_cast(&type);
    }
}

/* parse an expression and return its type without any side effect. */
static void expr_type(CType *type, void (*expr_fn)(void))
{
    nocode_wanted++;
    expr_fn();
    *type = vtop->type;
    vpop();
    nocode_wanted--;
}

/* parse an expression of the form '(type)' or '(expr)' and return its
   type */
static void parse_expr_type(CType *type)
{
    int n;
    AttributeDef ad;

    skip('(');
    if (parse_btype(type, &ad)) {
        type_decl(type, &ad, &n, TYPE_ABSTRACT);
    } else {
        expr_type(type, expression);
    }
    skip(')');
}

static void parse_type(CType *type)
{
    AttributeDef ad;
    int n;

    if (!parse_btype(type, &ad)) {
        expect("type");
    }
    type_decl(type, &ad, &n, TYPE_ABSTRACT);
}

static void parse_builtin_params(int nc, const char *args)
{
    char c, sep = '(';
    CType t;
    if (nc)
        nocode_wanted++;
    next();
    while ((c = *args++)) {
        skip(sep);
        sep = ',';
        switch (c) {
        case 'e': assignment_expression(); continue;
        case 't': parse_type(&t); vpush(&t); continue;
        default: tcc_error("internal error"); break;
        }
    }
    skip(')');
    if (nc)
        nocode_wanted--;
}

void unary_expression(void)
{
    int n, t, align, size, r, sizeof_caller;
    CType type;
    Sym *s;
    AttributeDef ad;

    sizeof_caller = in_sizeof;
    in_sizeof = 0;
    type.ref = NULL;
    /* XXX: GCC 2.95.3 does not generate a table although it should be
       better here */
tok_next:
    switch(tok) {
    case TOK_EXTENSION:
        next();
        goto tok_next;
    case TOK_LCHAR:
#ifdef TCC_TARGET_PE
        t = VT_SHORT|VT_UNSIGNED;
        goto push_tokc;
#endif
    case TOK_CINT:
    case TOK_CCHAR:
        t = VT_INT;
push_tokc:
        type.t = t;
        vsetc(&type, VT_CONST, &tokc);
        next();
        break;
    case TOK_CUINT:
        t = VT_INT | VT_UNSIGNED;
        goto push_tokc;
    case TOK_CLLONG:
        t = VT_LLONG;
        goto push_tokc;
    case TOK_CULLONG:
        t = VT_LLONG | VT_UNSIGNED;
        goto push_tokc;
    case TOK_CFLOAT:
        t = VT_FLOAT;
        goto push_tokc;
    case TOK_CDOUBLE:
        t = VT_DOUBLE;
        goto push_tokc;
    case TOK_CLDOUBLE:
        t = VT_LDOUBLE;
        goto push_tokc;
    case TOK_CLONG:
        t = (LONG_SIZE == 8 ? VT_LLONG : VT_INT) | VT_LONG;
        goto push_tokc;
    case TOK_CULONG:
        t = (LONG_SIZE == 8 ? VT_LLONG : VT_INT) | VT_LONG | VT_UNSIGNED;
        goto push_tokc;
    case TOK___FUNCTION__:
        if (!gnu_ext)
            goto tok_identifier;
        /* fall thru */
    case TOK___FUNC__:
        {
            void *ptr;
            int len;
            /* special function name identifier */
            len = strlen(tcc_state->funcname) + 1;
            /* generate char[len] type */
            type.t = VT_BYTE;
            make_pointer(&type);
            type.t |= VT_ARRAY;
            type.ref->c = len;
            vpush_ref(&type, data_section, data_section->data_offset, len);
            if (!NODATA_WANTED) {
                ptr = section_ptr_add(data_section, len);
                memcpy(ptr, tcc_state->funcname, len);
            }
            next();
        }
        break;
    case TOK_LSTR:
#ifdef TCC_TARGET_PE
        t = VT_SHORT | VT_UNSIGNED;
#else
        t = VT_INT;
#endif
        goto str_init;
    case TOK_STR:
        /* string parsing */
        t = VT_BYTE;
        if (char_is_unsigned)
            t = VT_BYTE | VT_UNSIGNED;
str_init:
        if (tcc_state->warn_write_strings)
            t |= VT_CONSTANT;
        type.t = t;
        make_pointer(&type);
        type.t |= VT_ARRAY;
        memset(&ad, 0, sizeof(AttributeDef));
        decl_initializer_alloc(&type, &ad, VT_CONST, 2, 0, 0);
        break;
    case '(':
        next();
        /* cast ? */
        if (parse_btype(&type, &ad)) {
            type_decl(&type, &ad, &n, TYPE_ABSTRACT);
            skip(')');
            /* check ISOC99 compound literal */
            if (tok == '{') {
                /* data is allocated locally by default */
                if (global_expr)
                    r = VT_CONST;
                else
                    r = VT_LOCAL;
                /* all except arrays are lvalues */
                if (!(type.t & VT_ARRAY))
                    r |= lvalue_type(type.t);
                memset(&ad, 0, sizeof(AttributeDef));
                decl_initializer_alloc(&type, &ad, r, 1, 0, 0);
            } else {
                if (sizeof_caller) {
                    vpush(&type);
                    return;
                }
                unary_expression();
                gen_cast(&type);
            }
        } else if (tok == '{') {
            int saved_nocode_wanted = nocode_wanted;
            if (const_wanted)
                tcc_error("expected constant");
            /* save all registers */
            save_regs(0);
            /* statement expression : we do not accept break/continue
               inside as GCC does.  We do retain the nocode_wanted state,
           as statement expressions can't ever be entered from the
           outside, so any reactivation of code emission (from labels
           or loop heads) can be disabled again after the end of it. */
            statement(NULL, NULL, 1);
            nocode_wanted = saved_nocode_wanted;
            skip(')');
        } else {
            expression();
            skip(')');
        }
        break;
    case '*':
        next();
        unary_expression();
        indir();
        break;
    case '&':
        next();
        unary_expression();
        /* functions names must be treated as function pointers,
           except for unary '&' and sizeof. Since we consider that
           functions are not lvalues, we only have to handle it
           there and in function calls. */
        /* arrays can also be used although they are not lvalues */
        if ((vtop->type.t & VT_BTYPE) != VT_FUNC &&
                !(vtop->type.t & VT_ARRAY))
            assure_lvalue();
        make_pointer(&vtop->type);
        address_of();
        break;
    case '!':
        next();
        unary_expression();
        if ((vtop->r & (VT_VALMASK | VT_LVAL | VT_SYM)) == VT_CONST) {
            gen_cast_s(VT_BOOL);
            vtop->c.i = !vtop->c.i;
        } else if ((vtop->r & VT_VALMASK) == VT_CMP)
            vtop->c.i ^= 1;
        else {
            save_regs(1);
            vseti(VT_JMP, gvtst(1, 0));
        }
        break;
    case '~':
        next();
        unary_expression();
        vpushi(-1);
        gen_op('^');
        break;
    case '+':
        next();
        unary_expression();
        if ((vtop->type.t & VT_BTYPE) == VT_PTR)
            tcc_error("pointer not accepted for unary plus");
        /* In order to force cast, we add zero, except for floating point
       where we really need an noop (otherwise -0.0 will be transformed
       into +0.0).  */
        if (!is_float(vtop->type.t)) {
            vpushi(0);
            gen_op('+');
        }
        break;
    case TOK_SIZEOF:
    case TOK_ALIGNOF1:
    case TOK_ALIGNOF2:
        t = tok;
        next();
        in_sizeof++;
        expr_type(&type, unary_expression); /* Perform a in_sizeof = 0; */
        s = vtop[1].sym; /* hack: accessing previous vtop */
        size = type_size(&type, &align);
        if (s && s->a.aligned)
            align = 1 << (s->a.aligned - 1);
        if (t == TOK_SIZEOF) {
            if (!(type.t & VT_VLA)) {
                if (size < 0)
                    tcc_error("sizeof applied to an incomplete type");
                vpushs(size);
            } else {
                vla_runtime_type_size(&type, &align);
            }
        } else {
            vpushs(align);
        }
        vtop->type.t |= VT_UNSIGNED;
        break;

    case TOK_builtin_expect:
        /* __builtin_expect is a no-op for now */
        parse_builtin_params(0, "ee");
        vpop();
        break;
    case TOK_builtin_types_compatible_p:
        parse_builtin_params(0, "tt");
        vtop[-1].type.t &= ~(VT_CONSTANT | VT_VOLATILE);
        vtop[0].type.t &= ~(VT_CONSTANT | VT_VOLATILE);
        n = is_compatible_types(&vtop[-1].type, &vtop[0].type);
        vtop -= 2;
        vpushi(n);
        break;
    case TOK_builtin_choose_expr:
        {
            int64_t c;
            next();
            skip('(');
            c = const_int64_expr();
            skip(',');
            if (!c) {
                nocode_wanted++;
            }
            assignment_expression();
            if (!c) {
                vpop();
                nocode_wanted--;
            }
            skip(',');
            if (c) {
                nocode_wanted++;
            }
            assignment_expression();
            if (c) {
                vpop();
                nocode_wanted--;
            }
            skip(')');
        }
        break;
    case TOK_builtin_constant_p:
        parse_builtin_params(1, "e");
        n = (vtop->r & (VT_VALMASK | VT_LVAL | VT_SYM)) == VT_CONST;
        vtop--;
        vpushi(n);
        break;
    case TOK_builtin_frame_address:
    case TOK_builtin_return_address:
        {
            int tok1 = tok;
            int level;
            next();
            skip('(');
            if (tok != TOK_CINT) {
                tcc_error("%s only takes positive integers",
                          tok1 == TOK_builtin_return_address ?
                              "__builtin_return_address" :
                              "__builtin_frame_address");
            }
            level = (uint32_t)tokc.i;
            next();
            skip(')');
            type.t = VT_VOID;
            make_pointer(&type);
            vset(&type, VT_LOCAL, 0);       /* local frame */
            while (level--) {
                make_pointer(&vtop->type);
                indir();                    /* -> parent frame */
            }
            if (tok1 == TOK_builtin_return_address) {
                // assume return address is just above frame pointer on stack
                vpushi(PTR_SIZE);
                gen_op('+');
                make_pointer(&vtop->type);
                indir();
            }
        }
        break;
#ifdef TCC_TARGET_X86_64
#ifdef TCC_TARGET_PE
    case TOK_builtin_va_start:
        parse_builtin_params(0, "ee");
        r = vtop->r & VT_VALMASK;
        if (r == VT_LLOCAL)
            r = VT_LOCAL;
        if (r != VT_LOCAL)
            tcc_error("__builtin_va_start expects a local variable");
        vtop->r = r;
        vtop->type = char_pointer_type;
        vtop->c.i += 8;
        vstore();
        break;
#else
    case TOK_builtin_va_arg_types:
        parse_builtin_params(0, "t");
        vpushi(classify_x86_64_va_arg(&vtop->type));
        vswap();
        vpop();
        break;
#endif
#endif

#ifdef TCC_TARGET_ARM64
    case TOK___va_start: {
            parse_builtin_params(0, "ee");
            //xx check types
            gen_va_start();
            vpushi(0);
            vtop->type.t = VT_VOID;
            break;
        }
    case TOK___va_arg: {
            parse_builtin_params(0, "et");
            type = vtop->type;
            vpop();
            //xx check types
            gen_va_arg(&type);
            vtop->type = type;
            break;
        }
    case TOK___arm64_clear_cache: {
            parse_builtin_params(0, "ee");
            gen_clear_cache();
            vpushi(0);
            vtop->type.t = VT_VOID;
            break;
        }
#endif
        /* pre operations */
    case TOK_INC:
    case TOK_DEC:
        t = tok;
        next();
        unary_expression();
        inc(0, t);
        break;
    case '-':
        next();
        unary_expression();
        t = vtop->type.t & VT_BTYPE;
        if (is_float(t)) {
            /* In IEEE negate(x) isn't subtract(0,x), but rather
           subtract(-0, x).  */
            vpush(&vtop->type);
            if (t == VT_FLOAT)
                vtop->c.f = -1.0 * 0.0;
            else if (t == VT_DOUBLE)
                vtop->c.d = -1.0 * 0.0;
            else
                vtop->c.ld = -1.0 * 0.0;
        } else
            vpushi(0);
        vswap();
        gen_op('-');
        break;
    case TOK_LAND:
        if (!gnu_ext)
            goto tok_identifier;
        next();
        /* allow to take the address of a label */
        if (tok < TOK_UIDENT)
            expect("label identifier");
        s = label_find(tok);
        if (!s) {
            s = label_push(&global_label_stack, tok, LABEL_FORWARD);
        } else {
            if (s->r == LABEL_DECLARED)
                s->r = LABEL_FORWARD;
        }
        if (!s->type.t) {
            s->type.t = VT_VOID;
            make_pointer(&s->type);
            s->type.t |= VT_STATIC;
        }
        vpushsym(&s->type, s);
        next();
        break;

    case TOK_GENERIC:
        {
            CType controlling_type;
            int has_default = 0;
            int has_match = 0;
            int learn = 0;
            TokenString *str = NULL;

            next();
            skip('(');
            expr_type(&controlling_type, assignment_expression);
            controlling_type.t &= ~(VT_CONSTANT | VT_VOLATILE | VT_ARRAY);
            for (;;) {
                learn = 0;
                skip(',');
                if (tok == TOK_DEFAULT) {
                    if (has_default)
                        tcc_error("too many 'default'");
                    has_default = 1;
                    if (!has_match)
                        learn = 1;
                    next();
                } else {
                    AttributeDef ad_tmp;
                    int itmp;
                    CType cur_type;
                    parse_btype(&cur_type, &ad_tmp);
                    type_decl(&cur_type, &ad_tmp, &itmp, TYPE_ABSTRACT);
                    if (compare_types(&controlling_type, &cur_type, 0)) {
                        if (has_match) {
                            tcc_error("type match twice");
                        }
                        has_match = 1;
                        learn = 1;
                    }
                }
                skip(':');
                if (learn) {
                    if (str)
                        tok_str_free(str);
                    skip_or_save_block(&str);
                } else {
                    skip_or_save_block(NULL);
                }
                if (tok == ')')
                    break;
            }
            if (!str) {
                char buf[60];
                type_to_str(buf, sizeof buf, &controlling_type, NULL);
                tcc_error("type '%s' does not match any association", buf);
            }
            begin_macro(str, 1);
            next();
            assignment_expression();
            if (tok != TOK_EOF)
                expect(",");
            end_macro();
            next();
            break;
        }
        // special qnan , snan and infinity values
    case TOK___NAN__:
        vpush64(VT_DOUBLE, 0x7ff8000000000000ULL);
        next();
        break;
    case TOK___SNAN__:
        vpush64(VT_DOUBLE, 0x7ff0000000000001ULL);
        next();
        break;
    case TOK___INF__:
        vpush64(VT_DOUBLE, 0x7ff0000000000000ULL);
        next();
        break;

    default:
tok_identifier:
        t = tok;
        next();
        if (t < TOK_UIDENT)
            expect("identifier");
        s = sym_find(t);
        if (!s || IS_ASM_SYM(s)) {
            const char *name = get_tok_str(t, NULL);
            if (tok != '(')
                tcc_error("'%s' undeclared", name);
            /* for simple function calls, we tolerate undeclared
               external reference to int() function */
            if (tcc_state->warn_implicit_function_declaration
        #ifdef TCC_TARGET_PE
                    /* people must be warned about using undeclared WINAPI functions
                           (which usually start with uppercase letter) */
                    || (name[0] >= 'A' && name[0] <= 'Z')
        #endif
                    )
                tcc_warning("implicit declaration of function '%s'", name);
            s = external_global_sym(t, &func_old_type, 0);
        }

        r = s->r;
        /* A symbol that has a register is a local register variable,
           which starts out as VT_LOCAL value.  */
        if ((r & VT_VALMASK) < VT_CONST)
            r = (r & ~VT_VALMASK) | VT_LOCAL;

        vset(&s->type, r, s->c);
        /* Point to s as backpointer (even without r&VT_SYM).
       Will be used by at least the x86 inline asm parser for
       regvars.  */
        vtop->sym = s;

        if (r & VT_SYM) {
            vtop->c.i = 0;
        } else if (r == VT_CONST && IS_ENUM_VAL(s->type.t)) {
            vtop->c.i = s->enum_val;
        }
        break;
    }

    /* post operations */
    while (1) {
        if (tok == TOK_INC || tok == TOK_DEC) {
            inc(1, tok);
            next();
        } else if (tok == '.' || tok == TOK_ARROW || tok == TOK_CDOUBLE) {
            int qualifiers;
            /* field */
            if (tok == TOK_ARROW)
                indir();
            qualifiers = vtop->type.t & (VT_CONSTANT | VT_VOLATILE);
            assure_lvalue();
            address_of();
            /* expect pointer on structure */
            if ((vtop->type.t & VT_BTYPE) != VT_STRUCT)
                expect("struct or union");
            if (tok == TOK_CDOUBLE)
                expect("field name");
            next();
            if (tok == TOK_CINT || tok == TOK_CUINT)
                expect("field name");
            s = find_field(&vtop->type, tok);
            if (!s)
                tcc_error("field not found: %s",  get_tok_str(tok & ~SYM_FIELD, &tokc));
            /* add field offset to pointer */
            vtop->type = char_pointer_type; /* change type to 'char *' */
            vpushi(s->c);
            gen_op('+');
            /* change type to field type, and set to lvalue */
            vtop->type = s->type;
            vtop->type.t |= qualifiers;
            /* an array is never an lvalue */
            if (!(vtop->type.t & VT_ARRAY)) {
                vtop->r |= lvalue_type(vtop->type.t);
#ifdef CONFIG_TCC_BCHECK
                /* if bound checking, the referenced pointer must be checked */
                if (tcc_state->do_bounds_check && (vtop->r & VT_VALMASK) != VT_LOCAL)
                    vtop->r |= VT_MUSTBOUND;
#endif
            }
            next();
        } else if (tok == '[') {
            next();
            expression();
            gen_op('+');
            indir();
            skip(']');
        } else if (tok == '(') {
            SValue ret;
            Sym *sa;
            int nb_args, ret_nregs, ret_align, regsize, variadic;

            /* function call  */
            if ((vtop->type.t & VT_BTYPE) != VT_FUNC) {
                /* pointer test (no array accepted) */
                if ((vtop->type.t & (VT_BTYPE | VT_ARRAY)) == VT_PTR) {
                    vtop->type = *pointed_type(&vtop->type);
                    if ((vtop->type.t & VT_BTYPE) != VT_FUNC)
                        goto error_func;
                } else {
error_func:
                    expect("function pointer");
                }
            } else {
                vtop->r &= ~VT_LVAL; /* no lvalue */
            }
            /* get return type */
            s = vtop->type.ref;
            next();
            sa = s->next; /* first parameter */
            nb_args = regsize = 0;
            ret.r2 = VT_CONST;
            /* compute first implicit argument if a structure is returned */
            if ((s->type.t & VT_BTYPE) == VT_STRUCT) {
                variadic = (s->f.func_type == FUNC_ELLIPSIS);
                ret_nregs = gfunc_sret(&s->type, variadic, &ret.type,
                                       &ret_align, &regsize);
                if (!ret_nregs) {
                    /* get some space for the returned structure */
                    size = type_size(&s->type, &align);
#ifdef TCC_TARGET_ARM64
                    /* On arm64, a small struct is return in registers.
                   It is much easier to write it to memory if we know
                   that we are allowed to write some extra bytes, so
                   round the allocated space up to a power of 2: */
                    if (size < 16)
                        while (size & (size - 1))
                            size = (size | (size - 1)) + 1;
#endif
                    loc = (loc - size) & -align;
                    ret.type = s->type;
                    ret.r = VT_LOCAL | VT_LVAL;
                    /* pass it as 'int' to avoid structure arg passing
                       problems */
                    vseti(VT_LOCAL, loc);
                    ret.c = vtop->c;
                    nb_args++;
                }
            } else {
                ret_nregs = 1;
                ret.type = s->type;
            }

            if (ret_nregs) {
                /* return in register */
                if (is_float(ret.type.t)) {
                    ret.r = reg_fret(ret.type.t);
#ifdef TCC_TARGET_X86_64
                    if ((ret.type.t & VT_BTYPE) == VT_QFLOAT)
                        ret.r2 = REG_QRET;
#endif
                } else {
#ifndef TCC_TARGET_ARM64
#ifdef TCC_TARGET_X86_64
                    if ((ret.type.t & VT_BTYPE) == VT_QLONG)
#else
                    if ((ret.type.t & VT_BTYPE) == VT_LLONG)
#endif
                        ret.r2 = REG_LRET;
#endif
                    ret.r = REG_IRET;
                }
                ret.c.i = 0;
            }
            if (tok != ')') {
                for(;;) {
                    assignment_expression();
                    gfunc_param_typed(s, sa);
                    nb_args++;
                    if (sa)
                        sa = sa->next;
                    if (tok == ')')
                        break;
                    skip(',');
                }
            }
            if (sa)
                tcc_error("too few arguments to function");
            skip(')');
            gfunc_call(nb_args);

            /* return value */
            for (r = ret.r + ret_nregs + !ret_nregs; r-- > ret.r;) {
                vsetc(&ret.type, r, &ret.c);
                vtop->r2 = ret.r2; /* Loop only happens when r2 is VT_CONST */
            }

            /* handle packed struct return */
            if (((s->type.t & VT_BTYPE) == VT_STRUCT) && ret_nregs) {
                int addr, offset;

                size = type_size(&s->type, &align);
                /* We're writing whole regs often, make sure there's enough
                   space.  Assume register size is power of 2.  */
                if (regsize > align)
                    align = regsize;
                loc = (loc - size) & -align;
                addr = loc;
                offset = 0;
                for (;;) {
                    vset(&ret.type, VT_LOCAL | VT_LVAL, addr + offset);
                    vswap();
                    vstore();
                    vtop--;
                    if (--ret_nregs == 0)
                        break;
                    offset += regsize;
                }
                vset(&s->type, VT_LOCAL | VT_LVAL, addr);
            }
        } else {
            break;
        }
    }
}

void multiplicative_expression(void)
{
    int t;

    t = tok;
    unary_expression(); /* includes cast_expression */
    while (tok == '*' || tok == '/' || tok == '%') {
        t = tok;
        next();
        unary_expression();
        gen_op(t);
    }
}

void additive_expression(void)
{
    int t;

    multiplicative_expression();
    while (tok == '+' || tok == '-') {
        t = tok;
        next();
        multiplicative_expression();
        gen_op(t);
    }
}

static void shift_expression(void)
{
    int t;

    additive_expression();
    while (tok == TOK_SHL || tok == TOK_SAR) {
        t = tok;
        next();
        additive_expression();
        gen_op(t);
    }
}

static void relational_expression(void)
{
    int t;

    shift_expression();
    while ((tok >= TOK_ULE && tok <= TOK_GT) ||
           tok == TOK_ULT || tok == TOK_UGE) {
        t = tok;
        next();
        shift_expression();
        gen_op(t);
    }
}

static void equality_expression(void)
{
    int t;

    relational_expression();
    while (tok == TOK_EQ || tok == TOK_NE) {
        t = tok;
        next();
        relational_expression();
        gen_op(t);
    }
}

static void and_expression(void)
{
    equality_expression();
    while (tok == '&') {
        next();
        equality_expression();
        gen_op('&');
    }
}

static void exclusive_or_expression(void)
{
    and_expression();
    while (tok == '^') {
        next();
        and_expression();
        gen_op('^');
    }
}

static void inclusive_or_expression(void)
{
    exclusive_or_expression();
    while (tok == '|') {
        next();
        exclusive_or_expression();
        gen_op('|');
    }
}

static void logical_and_expression(void)
{
    inclusive_or_expression();
    if (tok == TOK_LAND) { /* && */
        int t = 0;
        for(;;) {
            if ((vtop->r & (VT_VALMASK | VT_LVAL | VT_SYM)) == VT_CONST) {
                gen_cast_s(VT_BOOL);
                if (vtop->c.i) {
                    vpop();
                } else {
                    nocode_wanted++;
                    while (tok == TOK_LAND) {
                        next();
                        inclusive_or_expression();
                        vpop();
                    }
                    nocode_wanted--;
                    if (t)
                        gsym(t);
                    gen_cast_s(VT_INT);
                    break;
                }
            } else {
                if (!t)
                    save_regs(1);
                t = gvtst(1, t);
            }
            if (tok != TOK_LAND) {
                if (t)
                    vseti(VT_JMPI, t);
                else
                    vpushi(1);
                break;
            }
            next();
            inclusive_or_expression();
        }
    }
}

static void logical_or_expression(void)
{
    logical_and_expression();
    if (tok == TOK_LOR) { /* || */
        int t = 0;
        for(;;) {
            if ((vtop->r & (VT_VALMASK | VT_LVAL | VT_SYM)) == VT_CONST) {
                gen_cast_s(VT_BOOL);
                if (!vtop->c.i) {
                    vpop();
                } else {
                    nocode_wanted++;
                    while (tok == TOK_LOR) {
                        next();
                        logical_and_expression();
                        vpop();
                    }
                    nocode_wanted--;
                    if (t)
                        gsym(t);
                    gen_cast_s(VT_INT);
                    break;
                }
            } else {
                if (!t)
                    save_regs(1);
                t = gvtst(0, t);
            }
            if (tok != TOK_LOR) {
                if (t)
                    vseti(VT_JMP, t);
                else
                    vpushi(0);
                break;
            }
            next();
            logical_and_expression();
        }
    }
}

/* Assuming vtop is a value used in a conditional context
   (i.e. compared with zero) return 0 if it's false, 1 if
   true and -1 if it can't be statically determined.  */
static int condition_3way(void)
{
    int c = -1;
    if ((vtop->r & (VT_VALMASK | VT_LVAL)) == VT_CONST &&
            (!(vtop->r & VT_SYM) || !vtop->sym->a.weak)) {
        vdup();
        gen_cast_s(VT_BOOL);
        c = vtop->c.i;
        vpop();
    }
    return c;
}

static void conditional_expression(void)
{
    int tt, u, r1, r2, rc, t1, t2, bt1, bt2, islv, c, g;
    SValue sv;
    CType type, type1, type2;

    logical_or_expression();
    if (tok == '?') {
        next();
        c = condition_3way();
        g = (tok == ':' && gnu_ext);
        if (c < 0) {
            /* needed to avoid having different registers saved in
               each branch */
            if (is_float(vtop->type.t)) {
                rc = RC_FLOAT;
#ifdef TCC_TARGET_X86_64
                if ((vtop->type.t & VT_BTYPE) == VT_LDOUBLE) {
                    rc = RC_ST0;
                }
#endif
            } else
                rc = RC_INT;
            gv(rc);
            save_regs(1);
            if (g)
                gv_dup();
            tt = gvtst(1, 0);

        } else {
            if (!g)
                vpop();
            tt = 0;
        }

        if (1) {
            if (c == 0)
                nocode_wanted++;
            if (!g)
                expression();

            type1 = vtop->type;
            sv = *vtop; /* save value to handle it later */
            vtop--; /* no vpop so that FP stack is not flushed */
            skip(':');

            u = 0;
            if (c < 0)
                u = gjmp(0);
            gsym(tt);

            if (c == 0)
                nocode_wanted--;
            if (c == 1)
                nocode_wanted++;
            conditional_expression();
            if (c == 1)
                nocode_wanted--;

            type2 = vtop->type;
            t1 = type1.t;
            bt1 = t1 & VT_BTYPE;
            t2 = type2.t;
            bt2 = t2 & VT_BTYPE;
            type.ref = NULL;

            /* cast operands to correct type according to ISOC rules */
            if (is_float(bt1) || is_float(bt2)) {
                if (bt1 == VT_LDOUBLE || bt2 == VT_LDOUBLE) {
                    type.t = VT_LDOUBLE;

                } else if (bt1 == VT_DOUBLE || bt2 == VT_DOUBLE) {
                    type.t = VT_DOUBLE;
                } else {
                    type.t = VT_FLOAT;
                }
            } else if (bt1 == VT_LLONG || bt2 == VT_LLONG) {
                /* cast to biggest op */
                type.t = VT_LLONG | VT_LONG;
                if (bt1 == VT_LLONG)
                    type.t &= t1;
                if (bt2 == VT_LLONG)
                    type.t &= t2;
                /* convert to unsigned if it does not fit in a long long */
                if ((t1 & (VT_BTYPE | VT_UNSIGNED | VT_BITFIELD)) == (VT_LLONG | VT_UNSIGNED) ||
                        (t2 & (VT_BTYPE | VT_UNSIGNED | VT_BITFIELD)) == (VT_LLONG | VT_UNSIGNED))
                    type.t |= VT_UNSIGNED;
            } else if (bt1 == VT_PTR || bt2 == VT_PTR) {
                /* If one is a null ptr constant the result type
           is the other.  */
                if (is_null_pointer (vtop))
                    type = type1;
                else if (is_null_pointer (&sv))
                    type = type2;
                /* XXX: test pointer compatibility, C99 has more elaborate
           rules here.  */
                else
                    type = type1;
            } else if (bt1 == VT_FUNC || bt2 == VT_FUNC) {
                /* XXX: test function pointer compatibility */
                type = bt1 == VT_FUNC ? type1 : type2;
            } else if (bt1 == VT_STRUCT || bt2 == VT_STRUCT) {
                /* XXX: test structure compatibility */
                type = bt1 == VT_STRUCT ? type1 : type2;
            } else if (bt1 == VT_VOID || bt2 == VT_VOID) {
                /* NOTE: as an extension, we accept void on only one side */
                type.t = VT_VOID;
            } else {
                /* integer operations */
                type.t = VT_INT | (VT_LONG & (t1 | t2));
                /* convert to unsigned if it does not fit in an integer */
                if ((t1 & (VT_BTYPE | VT_UNSIGNED | VT_BITFIELD)) == (VT_INT | VT_UNSIGNED) ||
                        (t2 & (VT_BTYPE | VT_UNSIGNED | VT_BITFIELD)) == (VT_INT | VT_UNSIGNED))
                    type.t |= VT_UNSIGNED;
            }
            /* keep structs lvalue by transforming `(expr ? a : b)` to `*(expr ? &a : &b)` so
               that `(expr ? a : b).mem` does not error  with "lvalue expected" */
            islv = (vtop->r & VT_LVAL) && (sv.r & VT_LVAL) && VT_STRUCT == (type.t & VT_BTYPE);
            islv &= c < 0;

            /* now we convert second operand */
            if (c != 1) {
                gen_cast(&type);
                if (islv) {
                    make_pointer(&vtop->type);
                    address_of();
                } else if (VT_STRUCT == (vtop->type.t & VT_BTYPE))
                    address_of();
            }

            rc = RC_INT;
            if (is_float(type.t)) {
                rc = RC_FLOAT;
#ifdef TCC_TARGET_X86_64
                if ((type.t & VT_BTYPE) == VT_LDOUBLE) {
                    rc = RC_ST0;
                }
#endif
            } else if ((type.t & VT_BTYPE) == VT_LLONG) {
                /* for long longs, we use fixed registers to avoid having
                   to handle a complicated move */
                rc = RC_IRET;
            }

            tt = r2 = 0;
            if (c < 0) {
                r2 = gv(rc);
                tt = gjmp(0);
            }
            gsym(u);

            /* this is horrible, but we must also convert first
               operand */
            if (c != 0) {
                *vtop = sv;
                gen_cast(&type);
                if (islv) {
                    make_pointer(&vtop->type);
                    address_of();
                } else if (VT_STRUCT == (vtop->type.t & VT_BTYPE))
                    address_of();
            }

            if (c < 0) {
                r1 = gv(rc);
                move_reg(r2, r1, type.t);
                vtop->r = r2;
                gsym(tt);
                if (islv)
                    indir();
            }
        }
    }
}

static void assignment_expression(void)
{
    int t;

    conditional_expression();
    if (tok == '=' ||
            (tok >= TOK_A_MOD && tok <= TOK_A_DIV) ||
            tok == TOK_A_XOR || tok == TOK_A_OR ||
            tok == TOK_A_SHL || tok == TOK_A_SAR) {
        assure_lvalue();
        t = tok;
        next();
        if (t == '=') {
            assignment_expression();
        } else {
            vdup();
            assignment_expression();
            gen_op(t & 0x7f);
        }
        vstore();
    }
}

void expression(void)
{
    while (1) {
        assignment_expression();
        if (tok != ',')
            break;
        vpop();
        next();
    }
}

/* parse a constant expression and return value in vtop.  */
static void constant_expression(void)
{
    const_wanted++;
    nocode_wanted++;
    conditional_expression();
    nocode_wanted--;
    const_wanted--;
}

/* parse an integer constant and return its value. */
static inline int64_t const_int64_expr(void)
{
    int64_t c;
    constant_expression();
    if ((vtop->r & (VT_VALMASK | VT_LVAL | VT_SYM)) != VT_CONST)
        expect("constant expression");
    c = vtop->c.i;
    vpop();
    return c;
}

/* parse an integer constant and return its value.
   Complain if it doesn't fit 32bit (signed or unsigned).  */
static int const_int32_expr(void)
{
    int c;
    int64_t wc = const_int64_expr();
    c = wc;
    if (c != wc && (unsigned)c != wc)
        tcc_error("constant exceeds 32 bit");
    return c;
}

/* return the label token if current token is a label, otherwise
   return zero */
static int is_label(void)
{
    int last_tok;

    /* fast test first */
    if (tok < TOK_UIDENT)
        return 0;
    /* no need to save tokc because tok is an identifier */
    last_tok = tok;
    next();
    if (tok == ':') {
        return last_tok;
    } else {
        unget_tok(last_tok);
        return 0;
    }
}

static int case_cmp(const void *pa, const void *pb)
{
    int64_t a = (*(struct case_t**) pa)->v1;
    int64_t b = (*(struct case_t**) pb)->v1;
    return a < b ? -1 : a > b;
}

static void if_statement(int *bsym, int *csym)
{
    int a, c, d, cond;

    /* if test */
    int saved_nocode_wanted = nocode_wanted;
    next();
    skip('(');
    expression();
    skip(')');
    cond = condition_3way();
    if (cond == 1)
        a = 0, vpop();
    else
        a = gvtst(1, 0);
    if (cond == 0)
        nocode_wanted |= 0x20000000;
    statement(bsym, csym, 0);
    if (cond != 1)
        nocode_wanted = saved_nocode_wanted;
    c = tok;
    if (c == TOK_ELSE) {
        next();
        d = gjmp(0);
        gsym(a);
        if (cond == 1)
            nocode_wanted |= 0x20000000;
        statement(bsym, csym, 0);
        gsym(d); /* patch else jmp */
        if (cond != 0)
            nocode_wanted = saved_nocode_wanted;
    } else
        gsym(a);
}

static void while_statement()
{
    int a, b, d;
    int saved_nocode_wanted;
    nocode_wanted &= ~0x20000000;
    next();
    d = ind;
    vla_sp_restore();
    skip('(');
    expression();
    skip(')');
    a = gvtst(1, 0);
    b = 0;
    ++local_scope;
    saved_nocode_wanted = nocode_wanted;
    statement(&a, &b, 0);
    nocode_wanted = saved_nocode_wanted;
    --local_scope;
    gjmp_addr(d);
    gsym(a);
    gsym_addr(b, d);
}

static void block_statement(int *bsym, int *csym, int is_expr)
{
    int block_vla_sp_loc = vla_sp_loc, saved_vlas_in_scope = vlas_in_scope;

    next();
    /* record local declaration stack position */
    Sym * s = local_stack;
    Sym * llabel = local_label_stack;
    ++local_scope;

    /* handle local labels declarations */
    if (tok == TOK_LABEL) {
        next();
        for(;;) {
            if (tok < TOK_UIDENT)
                expect("label identifier");
            label_push(&local_label_stack, tok, LABEL_DECLARED);
            next();
            if (tok == ',') {
                next();
            } else {
                skip(';');
                break;
            }
        }
    }
    while (tok != '}') {
        int a = is_label();
        if (a)
            unget_tok(a);
        else
            decl(VT_LOCAL);
        if (tok != '}') {
            if (is_expr)
                vpop();
            statement(bsym, csym, is_expr);
        }
    }
    /* pop locally defined labels */
    label_pop(&local_label_stack, llabel, is_expr);
    /* pop locally defined symbols */
    --local_scope;

    /* In the is_expr case (a statement expression is finished here),
   vtop might refer to symbols on the local_stack.  Either via the
   type or via vtop->sym.  We can't pop those nor any that in turn
   might be referred to.  To make it easier we don't roll back
   any symbols in that case; some upper level call to block() will
   do that.  We do have to remove such symbols from the lookup
   tables, though.  sym_pop will do that.  */
    sym_pop(&local_stack, s, is_expr);

    /* Pop VLA frames and restore stack pointer if required */
    if (vlas_in_scope > saved_vlas_in_scope) {
        vla_sp_loc = saved_vlas_in_scope ? block_vla_sp_loc : vla_sp_root_loc;
        vla_sp_restore();
    }
    vlas_in_scope = saved_vlas_in_scope;

    next();
}

static void return_statement()
{
    next();
    if (tok != ';') {
        expression();
        gen_assign_cast(&func_vt);
        if ((func_vt.t & VT_BTYPE) == VT_VOID)
            vtop--;
        else
            gfunc_return(&func_vt);
    }
    skip(';');
    /* jump unless last stmt in top-level block */
    if (tok != '}' || local_scope != 1)
        rsym = gjmp(rsym);
    nocode_wanted |= 0x20000000;
}

static void break_statement(int *bsym)
{
    /* compute jump */
    if (!bsym)
        tcc_error("cannot break");
    *bsym = gjmp(*bsym);
    next();
    skip(';');
    nocode_wanted |= 0x20000000;
}

static void continue_statement(int *csym)
{
    /* compute jump */
    if (!csym)
        tcc_error("cannot continue");
    vla_sp_restore_root();
    *csym = gjmp(*csym);
    next();
    skip(';');
}

static void for_statement()
{
    int a, b, c, d;
    Sym *s;
    int e;
    int saved_nocode_wanted;
    nocode_wanted &= ~0x20000000;
    next();
    skip('(');
    s = local_stack;
    ++local_scope;
    if (tok != ';') {
        /* c99 for-loop init decl? */
        if (!decl0(VT_LOCAL, 1, NULL)) {
            /* no, regular for-loop init expr */
            expression();
            vpop();
        }
    }
    skip(';');
    d = ind;
    c = ind;
    vla_sp_restore();
    a = 0;
    b = 0;
    if (tok != ';') {
        expression();
        a = gvtst(1, 0);
    }
    skip(';');
    if (tok != ')') {
        e = gjmp(0);
        c = ind;
        vla_sp_restore();
        expression();
        vpop();
        gjmp_addr(d);
        gsym(e);
    }
    skip(')');
    saved_nocode_wanted = nocode_wanted;
    statement(&a, &b, 0);
    nocode_wanted = saved_nocode_wanted;
    gjmp_addr(c);
    gsym(a);
    gsym_addr(b, c);
    --local_scope;
    sym_pop(&local_stack, s, 0);
}

static void do_while_statement()
{
    int a, b, c, d;
    int saved_nocode_wanted;
    nocode_wanted &= ~0x20000000;
    next();
    a = 0;
    b = 0;
    d = ind;
    vla_sp_restore();
    saved_nocode_wanted = nocode_wanted;
    statement(&a, &b, 0);
    skip(TOK_WHILE);
    skip('(');
    gsym(b);
    expression();
    c = gvtst(0, 0);
    gsym_addr(c, d);
    nocode_wanted = saved_nocode_wanted;
    skip(')');
    gsym(a);
    skip(';');
}

static void switch_statement(int *csym)
{
    int a, b;
    struct switch_t *saved, sw;
    int saved_nocode_wanted = nocode_wanted;
    SValue switchval;
    next();
    skip('(');
    expression();
    skip(')');
    switchval = *vtop--;
    a = 0;
    b = gjmp(0); /* jump to first case */
    sw.p = NULL; sw.n = 0; sw.def_sym = 0;
    saved = cur_switch;
    cur_switch = &sw;
    statement(&a, csym, 0);
    nocode_wanted = saved_nocode_wanted;
    a = gjmp(a); /* add implicit break */
    /* case lookup */
    gsym(b);
    qsort(sw.p, sw.n, sizeof(void*), case_cmp);
    for (b = 1; b < sw.n; b++)
        if (sw.p[b - 1]->v2 >= sw.p[b]->v1)
            tcc_error("duplicate case value");
    /* Our switch table sorting is signed, so the compared
        value needs to be as well when it's 64bit.  */
    if ((switchval.type.t & VT_BTYPE) == VT_LLONG)
        switchval.type.t &= ~VT_UNSIGNED;
    vpushv(&switchval);
    gcase(sw.p, sw.n, &a);
    vpop();
    if (sw.def_sym)
        gjmp_addr(sw.def_sym);
    dynarray_reset(&sw.p, &sw.n);
    cur_switch = saved;
    /* break label */
    gsym(a);
}

static void case_part()
{
    struct case_t *cr = (struct case_t *)tcc_malloc(sizeof(struct case_t));
    if (!cur_switch)
        expect("switch");
    nocode_wanted &= ~0x20000000;
    next();
    cr->v1 = cr->v2 = const_int64_expr();
    if (gnu_ext && tok == TOK_DOTS) {
        next();
        cr->v2 = const_int64_expr();
        if (cr->v2 < cr->v1)
            tcc_warning("empty case range");
    }
    cr->sym = ind;
    dynarray_add(&cur_switch->p, &cur_switch->n, cr);
    skip(':');
}

static void default_part()
{
    next();
    skip(':');
    if (!cur_switch)
        expect("switch");
    if (cur_switch->def_sym)
        tcc_error("too many 'default'");
    cur_switch->def_sym = ind;
}

static void goto_statement()
{
    Sym *s;
    next();
    if (tok == '*' && gnu_ext) {
        /* computed goto */
        next();
        expression();
        if ((vtop->type.t & VT_BTYPE) != VT_PTR)
            expect("pointer");
        ggoto();
    } else if (tok >= TOK_UIDENT) {
        s = label_find(tok);
        /* put forward definition if needed */
        if (!s) {
            s = label_push(&global_label_stack, tok, LABEL_FORWARD);
        } else {
            if (s->r == LABEL_DECLARED)
                s->r = LABEL_FORWARD;
        }
        vla_sp_restore_root();
        if (s->r & LABEL_FORWARD)
            s->jnext = gjmp(s->jnext);
        else
            gjmp_addr(s->jnext);
        next();
    } else {
        expect("label identifier");
    }
    skip(';');
}

static void statement_label(int b)
{
    /* label case */
    next();
    Sym* s = label_find(b);
    if (s) {
        if (s->r == LABEL_DEFINED)
            tcc_error("duplicate label '%s'", get_tok_str(s->v, NULL));
        gsym(s->jnext);
        s->r = LABEL_DEFINED;
    } else {
        s = label_push(&global_label_stack, b, LABEL_DEFINED);
    }
    s->jnext = ind;
    vla_sp_restore();
    /* we accept this, but it is a mistake */
}

static void statement(int *bsym, int *csym, int is_expr) /* was called "block" in original code */
{
    /* generate line number info */
    if (tcc_state->do_debug)
        tcc_debug_line(tcc_state);

    if (is_expr) {
        /* default return value is (void) */
        vpushi(0);
        vtop->type.t = VT_VOID;
    }

    if (tok == TOK_IF) {
        if_statement(bsym,csym);
    } else if (tok == TOK_WHILE) {
        while_statement();
    } else if (tok == '{') {
        block_statement(bsym,csym,is_expr);
    } else if (tok == TOK_RETURN) {
        return_statement();
    } else if (tok == TOK_BREAK) {
        break_statement(bsym);
    } else if (tok == TOK_CONTINUE) {
        continue_statement(csym);
    } else if (tok == TOK_FOR) {
        for_statement();
    } else {
        if (tok == TOK_DO) {
            do_while_statement();
        } else {
            if (tok == TOK_SWITCH) {
                switch_statement(csym);
            } else {
                if (tok == TOK_CASE) {
                    case_part();
                    is_expr = 0;
                    goto block_after_label;
                } else {
                    if (tok == TOK_DEFAULT) {
                        default_part();
                        is_expr = 0;
                        goto block_after_label;
                    } else {
                        if (tok == TOK_GOTO) {
                            goto_statement();
                        } else {
                            const int b = is_label();
                            if (b) {
                                statement_label(b);
block_after_label:
                                nocode_wanted &= ~0x20000000;
                                if (tok == '}') {
                                    tcc_warning("deprecated use of label at end of compound statement");
                                } else {
                                    if (is_expr)
                                        vpop();
                                    statement(bsym, csym, is_expr);
                                }
                            } else {
                                /* expression case */
                                if (tok != ';') {
                                    if (is_expr) {
                                        vpop();
                                        expression();
                                    } else {
                                        expression();
                                        vpop();
                                    }
                                }
                                skip(';');
                            }
                        }
                    }
                }
            }
        }
    }
}

/* This skips over a stream of tokens containing balanced {} and ()
   pairs, stopping at outer ',' ';' and '}' (or matching '}' if we started
   with a '{').  If STR then allocates and stores the skipped tokens
   in *STR.  This doesn't check if () and {} are nested correctly,
   i.e. "({)}" is accepted.  */
static void skip_or_save_block(TokenString **str)
{
    int braces = tok == '{';
    int level = 0;
    if (str)
        *str = tok_str_alloc();

    while ((level > 0 || (tok != '}' && tok != ',' && tok != ';' && tok != ')'))) {
        int t;
        if (tok == TOK_EOF) {
            if (str || level > 0)
                tcc_error("unexpected end of file");
            else
                break;
        }
        if (str)
            tok_str_add_tok(*str);
        t = tok;
        next();
        if (t == '{' || t == '(') {
            level++;
        } else if (t == '}' || t == ')') {
            level--;
            if (level == 0 && braces && t == '}')
                break;
        }
    }
    if (str) {
        tok_str_add(*str, -1);
        tok_str_add(*str, 0);
    }
}

#define EXPR_CONST 1
#define EXPR_ANY   2

static void parse_init_elem(int expr_type)
{
    int saved_global_expr;
    switch(expr_type) {
    case EXPR_CONST:
        /* compound literals must be allocated globally in this case */
        saved_global_expr = global_expr;
        global_expr = 1;
        constant_expression();
        global_expr = saved_global_expr;
        /* NOTE: symbols are accepted, as well as lvalue for anon symbols
       (compound literals).  */
        if (((vtop->r & (VT_VALMASK | VT_LVAL)) != VT_CONST
             && ((vtop->r & (VT_SYM|VT_LVAL)) != (VT_SYM|VT_LVAL)
                 || vtop->sym->v < SYM_FIRST_ANOM))
        #ifdef TCC_TARGET_PE
                || ((vtop->r & VT_SYM) && vtop->sym->a.dllimport)
        #endif
                )
            tcc_error("initializer element is not constant");
        break;
    case EXPR_ANY:
        assignment_expression();
        break;
    }
}

/* t is the array or struct type. c is the array or struct
   address. cur_field is the pointer to the current
   field, for arrays the 'c' member contains the current start
   index.  'size_only' is true if only size info is needed (only used
   in arrays).  al contains the already initialized length of the
   current container (starting at c).  This returns the new length of that.  */
static int decl_designator(CType *type, Section *sec, unsigned long c,
                           Sym **cur_field, int size_only, int al)
{
    Sym *s, *f;
    int index, index_last, align, l, nb_elems, elem_size;
    unsigned long corig = c;

    elem_size = 0;
    nb_elems = 1;
    if (gnu_ext && (l = is_label()) != 0)
        goto struct_field;
    /* NOTE: we only support ranges for last designator */
    while (nb_elems == 1 && (tok == '[' || tok == '.')) {
        if (tok == '[') {
            if (!(type->t & VT_ARRAY))
                expect("array type");
            next();
            index = index_last = const_int32_expr();
            if (tok == TOK_DOTS && gnu_ext) {
                next();
                index_last = const_int32_expr();
            }
            skip(']');
            s = type->ref;
            if (index < 0 || (s->c >= 0 && index_last >= s->c) ||
                    index_last < index)
                tcc_error("invalid index");
            if (cur_field)
                (*cur_field)->c = index_last;
            type = pointed_type(type);
            elem_size = type_size(type, &align);
            c += index * elem_size;
            nb_elems = index_last - index + 1;
        } else {
            next();
            l = tok;
struct_field:
            next();
            if ((type->t & VT_BTYPE) != VT_STRUCT)
                expect("struct/union type");
            f = find_field(type, l);
            if (!f)
                expect("field");
            if (cur_field)
                *cur_field = f;
            type = &f->type;
            c += f->c;
        }
        cur_field = NULL;
    }
    if (!cur_field) {
        if (tok == '=') {
            next();
        } else if (!gnu_ext) {
            expect("=");
        }
    } else {
        if (type->t & VT_ARRAY) {
            index = (*cur_field)->c;
            if (type->ref->c >= 0 && index >= type->ref->c)
                tcc_error("index too large");
            type = pointed_type(type);
            c += index * type_size(type, &align);
        } else {
            f = *cur_field;
            while (f && (f->v & SYM_FIRST_ANOM) && (f->type.t & VT_BITFIELD))
                *cur_field = f = f->next;
            if (!f)
                tcc_error("too many field init");
            type = &f->type;
            c += f->c;
        }
    }
    /* must put zero in holes (note that doing it that way
       ensures that it even works with designators) */
    if (!size_only && c - corig > al)
        init_putz(sec, corig + al, c - corig - al);
    decl_initializer(type, sec, c, 0, size_only);

    /* XXX: make it more general */
    if (!size_only && nb_elems > 1) {
        unsigned long c_end;
        uint8_t *src, *dst;
        int i;

        if (!sec) {
            vset(type, VT_LOCAL|VT_LVAL, c);
            for (i = 1; i < nb_elems; i++) {
                vset(type, VT_LOCAL|VT_LVAL, c + elem_size * i);
                vswap();
                vstore();
            }
            vpop();
        } else if (!NODATA_WANTED) {
            c_end = c + nb_elems * elem_size;
            if (c_end > sec->data_allocated)
                section_realloc(sec, c_end);
            src = sec->data + c;
            dst = src;
            for(i = 1; i < nb_elems; i++) {
                dst += elem_size;
                memcpy(dst, src, elem_size);
            }
        }
    }
    c += nb_elems * type_size(type, &align);
    if (c - corig > al)
        al = c - corig;
    return al;
}

/* 't' contains the type and storage info. 'c' is the offset of the
   object in section 'sec'. If 'sec' is NULL, it means stack based
   allocation. 'first' is true if array '{' must be read (multi
   dimension implicit array init handling). 'size_only' is true if
   size only evaluation is wanted (only for arrays). */
static void decl_initializer(CType *type, Section *sec, unsigned long c,
                             int first, int size_only)
{
    int len, n, no_oblock, nb, i;
    int size1, align1;
    int have_elem;
    Sym *s, *f;
    Sym indexsym;
    CType *t1;

    /* If we currently are at an '}' or ',' we have read an initializer
       element in one of our callers, and not yet consumed it.  */
    have_elem = tok == '}' || tok == ',';
    if (!have_elem && tok != '{' &&
            /* In case of strings we have special handling for arrays, so
               don't consume them as initializer value (which would commit them
               to some anonymous symbol).  */
            tok != TOK_LSTR && tok != TOK_STR &&
            !size_only) {
        parse_init_elem(!sec ? EXPR_ANY : EXPR_CONST);
        have_elem = 1;
    }

    if (have_elem &&
            !(type->t & VT_ARRAY) &&
            /* Use i_c_parameter_t, to strip toplevel qualifiers.
               The source type might have VT_CONSTANT set, which is
               of course assignable to non-const elements.  */
            is_compatible_unqualified_types(type, &vtop->type)) {
        init_putv(type, sec, c);
    } else if (type->t & VT_ARRAY) {
        s = type->ref;
        n = s->c;
        t1 = pointed_type(type);
        size1 = type_size(t1, &align1);

        no_oblock = 1;
        if ((first && tok != TOK_LSTR && tok != TOK_STR) ||
                tok == '{') {
            if (tok != '{')
                tcc_error("character array initializer must be a literal,"
                          " optionally enclosed in braces");
            skip('{');
            no_oblock = 0;
        }

        /* only parse strings here if correct type (otherwise: handle
           them as ((w)char *) expressions */
        if ((tok == TOK_LSTR &&
     #ifdef TCC_TARGET_PE
             (t1->t & VT_BTYPE) == VT_SHORT && (t1->t & VT_UNSIGNED)
     #else
             (t1->t & VT_BTYPE) == VT_INT
     #endif
             ) || (tok == TOK_STR && (t1->t & VT_BTYPE) == VT_BYTE)) {
            len = 0;
            while (tok == TOK_STR || tok == TOK_LSTR) {
                int cstr_len, ch;

                /* compute maximum number of chars wanted */
                if (tok == TOK_STR)
                    cstr_len = tokc.str.size;
                else
                    cstr_len = tokc.str.size / sizeof(nwchar_t);
                cstr_len--;
                nb = cstr_len;
                if (n >= 0 && nb > (n - len))
                    nb = n - len;
                if (!size_only) {
                    if (cstr_len > nb)
                        tcc_warning("initializer-string for array is too long");
                    /* in order to go faster for common case (char
                       string in global variable, we handle it
                       specifically */
                    if (sec && tok == TOK_STR && size1 == 1) {
                        if (!NODATA_WANTED)
                            memcpy(sec->data + c + len, tokc.str.data, nb);
                    } else {
                        for(i=0;i<nb;i++) {
                            if (tok == TOK_STR)
                                ch = ((unsigned char *)tokc.str.data)[i];
                            else
                                ch = ((nwchar_t *)tokc.str.data)[i];
                            vpushi(ch);
                            init_putv(t1, sec, c + (len + i) * size1);
                        }
                    }
                }
                len += nb;
                next();
            }
            /* only add trailing zero if enough storage (no
               warning in this case since it is standard) */
            if (n < 0 || len < n) {
                if (!size_only) {
                    vpushi(0);
                    init_putv(t1, sec, c + (len * size1));
                }
                len++;
            }
            len *= size1;
        } else {
            indexsym.c = 0;
            f = &indexsym;

do_init_list:
            len = 0;
            while (tok != '}' || have_elem) {
                len = decl_designator(type, sec, c, &f, size_only, len);
                have_elem = 0;
                if (type->t & VT_ARRAY) {
                    ++indexsym.c;
                    /* special test for multi dimensional arrays (may not
               be strictly correct if designators are used at the
               same time) */
                    if (no_oblock && len >= n*size1)
                        break;
                } else {
                    if (s->type.t == VT_UNION)
                        f = NULL;
                    else
                        f = f->next;
                    if (no_oblock && f == NULL)
                        break;
                }

                if (tok == '}')
                    break;
                skip(',');
            }
        }
        /* put zeros at the end */
        if (!size_only && len < n*size1)
            init_putz(sec, c + len, n*size1 - len);
        if (!no_oblock)
            skip('}');
        /* patch type size if needed, which happens only for array types */
        if (n < 0)
            s->c = size1 == 1 ? len : ((len + size1 - 1)/size1);
    } else if ((type->t & VT_BTYPE) == VT_STRUCT) {
        size1 = 1;
        no_oblock = 1;
        if (first || tok == '{') {
            skip('{');
            no_oblock = 0;
        }
        s = type->ref;
        f = s->next;
        n = s->c;
        goto do_init_list;
    } else if (tok == '{') {
        next();
        decl_initializer(type, sec, c, first, size_only);
        skip('}');
    } else if (size_only) {
        /* If we supported only ISO C we wouldn't have to accept calling
       this on anything than an array size_only==1 (and even then
       only on the outermost level, so no recursion would be needed),
       because initializing a flex array member isn't supported.
       But GNU C supports it, so we need to recurse even into
       subfields of structs and arrays when size_only is set.  */
        /* just skip expression */
        skip_or_save_block(NULL);
    } else {
        if (!have_elem) {
            /* This should happen only when we haven't parsed
           the init element above for fear of committing a
           string constant to memory too early.  */
            if (tok != TOK_STR && tok != TOK_LSTR)
                expect("string constant");
            parse_init_elem(!sec ? EXPR_ANY : EXPR_CONST);
        }
        init_putv(type, sec, c);
    }
}

/* parse an initializer for type 't' if 'has_init' is non zero, and
   allocate space in local or global data space ('r' is either
   VT_LOCAL or VT_CONST). If 'v' is non zero, then an associated
   variable 'v' of scope 'scope' is declared before initializers
   are parsed. If 'v' is zero, then a reference to the new object
   is put in the value stack. If 'has_init' is 2, a special parsing
   is done to handle string constants. */
static void decl_initializer_alloc(CType *type, AttributeDef *ad, int r,
                                   int has_init, int v, int scope)
{
    int size, align, addr;
    TokenString *init_str = NULL;

    Section *sec;
    Sym *flexible_array;
    Sym *sym = NULL;
    int saved_nocode_wanted = nocode_wanted;
#ifdef CONFIG_TCC_BCHECK
    int bcheck = tcc_state->do_bounds_check && !NODATA_WANTED;
#endif

    if (type->t & VT_STATIC)
        nocode_wanted |= NODATA_WANTED ? 0x40000000 : 0x80000000;

    flexible_array = NULL;
    if ((type->t & VT_BTYPE) == VT_STRUCT) {
        Sym *field = type->ref->next;
        if (field) {
            while (field->next)
                field = field->next;
            if (field->type.t & VT_ARRAY && field->type.ref->c < 0)
                flexible_array = field;
        }
    }

    size = type_size(type, &align);
    /* If unknown size, we must evaluate it before
       evaluating initializers because
       initializers can generate global data too
       (e.g. string pointers or ISOC99 compound
       literals). It also simplifies local
       initializers handling */
    if (size < 0 || (flexible_array && has_init)) {
        if (!has_init)
            tcc_error("unknown type size");
        /* get all init string */
        if (has_init == 2) {
            init_str = tok_str_alloc();
            /* only get strings */
            while (tok == TOK_STR || tok == TOK_LSTR) {
                tok_str_add_tok(init_str);
                next();
            }
            tok_str_add(init_str, -1);
            tok_str_add(init_str, 0);
        } else {
            skip_or_save_block(&init_str);
        }
        unget_tok(0);

        /* compute size */
        begin_macro(init_str, 1);
        next();
        decl_initializer(type, NULL, 0, 1, 1);
        /* prepare second initializer parsing */
        macro_ptr = init_str->str;
        next();

        /* if still unknown size, error */
        size = type_size(type, &align);
        if (size < 0)
            tcc_error("unknown type size");
    }
    /* If there's a flex member and it was used in the initializer
       adjust size.  */
    if (flexible_array &&
            flexible_array->type.ref->c > 0)
        size += flexible_array->type.ref->c
                * pointed_size(&flexible_array->type);
    /* take into account specified alignment if bigger */
    if (ad->a.aligned) {
        int speca = 1 << (ad->a.aligned - 1);
        if (speca > align)
            align = speca;
    } else if (ad->a.packed) {
        align = 1;
    }

    if (NODATA_WANTED)
        size = 0, align = 1;

    if ((r & VT_VALMASK) == VT_LOCAL) {
        sec = NULL;
#ifdef CONFIG_TCC_BCHECK
        if (bcheck && (type->t & VT_ARRAY)) {
            loc--;
        }
#endif
        loc = (loc - size) & -align;
        addr = loc;
#ifdef CONFIG_TCC_BCHECK
        /* handles bounds */
        /* XXX: currently, since we do only one pass, we cannot track
           '&' operators, so we add only arrays */
        if (bcheck && (type->t & VT_ARRAY)) {
            addr_t *bounds_ptr;
            /* add padding between regions */
            loc--;
            /* then add local bound info */
            bounds_ptr = section_ptr_add(lbounds_section, 2 * sizeof(addr_t));
            bounds_ptr[0] = addr;
            bounds_ptr[1] = size;
        }
#endif
        if (v) {
            /* local variable */
#ifdef CONFIG_TCC_ASM
            if (ad->asm_label) {
                int reg = asm_parse_regvar(ad->asm_label);
                if (reg >= 0)
                    r = (r & ~VT_VALMASK) | reg;
            }
#endif
            sym = sym_push(v, type, r, addr);
            sym->a = ad->a;
        } else {
            /* push local reference */
            vset(type, r, addr);
        }
    } else {
        if (v && scope == VT_CONST) {
            /* see if the symbol was already defined */
            sym = sym_find(v);
            if (sym) {
                patch_storage(sym, ad, type);
                /* we accept several definitions of the same global variable. */
                if (!has_init && sym->c && elfsym(sym)->st_shndx != SHN_UNDEF)
                    goto no_alloc;
            }
        }

        /* allocate symbol in corresponding section */
        sec = ad->section;
        if (!sec) {
            if (has_init)
                sec = data_section;
            else if (tcc_state->nocommon)
                sec = bss_section;
        }

        if (sec) {
            addr = section_add(sec, size, align);
#ifdef CONFIG_TCC_BCHECK
            /* add padding if bound check */
            if (bcheck)
                section_add(sec, 1, 1);
#endif
        } else {
            addr = align; /* SHN_COMMON is special, symbol value is align */
            sec = common_section;
        }

        if (v) {
            if (!sym) {
                sym = sym_push(v, type, r | VT_SYM, 0);
                patch_storage(sym, ad, NULL);
            }
            /* Local statics have a scope until now (for
               warnings), remove it here.  */
            sym->sym_scope = 0;
            /* update symbol definition */
            put_extern_sym(sym, sec, addr, size);
        } else {
            /* push global reference */
            sym = get_sym_ref(type, sec, addr, size);
            vpushsym(type, sym);
            vtop->r |= r;
        }

#ifdef CONFIG_TCC_BCHECK
        /* handles bounds now because the symbol must be defined
           before for the relocation */
        if (bcheck) {
            addr_t *bounds_ptr;

            greloca(bounds_section, sym, bounds_section->data_offset, R_DATA_PTR, 0);
            /* then add global bound info */
            bounds_ptr = section_ptr_add(bounds_section, 2 * sizeof(addr_t));
            bounds_ptr[0] = 0; /* relocated */
            bounds_ptr[1] = size;
        }
#endif
    }

    if (type->t & VT_VLA) {
        int a;

        if (NODATA_WANTED)
            goto no_alloc;

        /* save current stack pointer */
        if (vlas_in_scope == 0) {
            if (vla_sp_root_loc == -1)
                vla_sp_root_loc = (loc -= PTR_SIZE);
            gen_vla_sp_save(vla_sp_root_loc);
        }

        vla_runtime_type_size(type, &a);
        gen_vla_alloc(type, a);
#if defined TCC_TARGET_PE && defined TCC_TARGET_X86_64
        /* on _WIN64, because of the function args scratch area, the
           result of alloca differs from RSP and is returned in RAX.  */
        gen_vla_result(addr), addr = (loc -= PTR_SIZE);
#endif
        gen_vla_sp_save(addr);
        vla_sp_loc = addr;
        vlas_in_scope++;

    } else if (has_init) {
        size_t oldreloc_offset = 0;
        if (sec && sec->reloc)
            oldreloc_offset = sec->reloc->data_offset;
        decl_initializer(type, sec, addr, 1, 0);
        if (sec && sec->reloc)
            squeeze_multi_relocs(sec, oldreloc_offset);
        /* patch flexible array member size back to -1, */
        /* for possible subsequent similar declarations */
        if (flexible_array)
            flexible_array->type.ref->c = -1;
    }

no_alloc:
    /* restore parse state if needed */
    if (init_str) {
        end_macro();
        next();
    }

    nocode_wanted = saved_nocode_wanted;
}

/* 'l' is VT_LOCAL or VT_CONST to define default storage type, or VT_CMP
   if parsing old style parameter decl list (and FUNC_SYM is set then) */
static int decl0(int l, int is_for_loop_init, Sym *func_sym)
{
    int v, has_init, r;
    CType type, btype;
    Sym *sym;
    AttributeDef ad;

    while (1) {
        if (!parse_btype(&btype, &ad)) {
            if (is_for_loop_init)
                return 0;
            /* skip redundant ';' if not in old parameter decl scope */
            if (tok == ';' && l != VT_CMP) {
                next();
                continue;
            }
            if (l != VT_CONST)
                break;

            if (tok >= TOK_UIDENT) {
                /* special test for old K&R protos without explicit int
                  type. Only accepted when defining global data */
                btype.t = VT_INT;
            } else {
                if (tok != TOK_EOF)
                    expect("declaration");
                break;
            }
        }
        if (tok == ';') {
            if ((btype.t & VT_BTYPE) == VT_STRUCT) {
                int v = btype.ref->v;
                if (!(v & SYM_FIELD) && (v & ~SYM_STRUCT) >= SYM_FIRST_ANOM)
                    tcc_warning("unnamed struct/union that defines no instances");
                next();
                continue;
            }
            if (IS_ENUM(btype.t)) {
                next();
                continue;
            }
        }
        while (1) { /* iterate thru each declaration */
            type = btype;
            /* If the base type itself was an array type of unspecified
           size (like in 'typedef int arr[]; arr x = {1};') then
           we will overwrite the unknown size by the real one for
           this decl.  We need to unshare the ref symbol holding
           that size.  */
            if ((type.t & VT_ARRAY) && type.ref->c < 0) {
                type.ref = sym_push(SYM_FIELD, &type.ref->type, 0, type.ref->c);
            }
            type_decl(&type, &ad, &v, TYPE_DIRECT);
            if ((type.t & VT_BTYPE) == VT_FUNC) {
                if ((type.t & VT_STATIC) && (l == VT_LOCAL)) {
                    tcc_error("function without file scope cannot be static");
                }
                /* if old style function prototype, we accept a
                   declaration list */
                sym = type.ref;
                if (sym->f.func_type == FUNC_OLD && l == VT_CONST)
                    decl0(VT_CMP, 0, sym);
            }

            if (gnu_ext && (tok == TOK_ASM1 || tok == TOK_ASM2 || tok == TOK_ASM3)) {
                ad.asm_label = asm_label_instr();
                /* parse one last attribute list, after asm label */
                parse_attribute(&ad);
                if (tok == '{')
                    expect(";");
            }

#ifdef TCC_TARGET_PE
            if (ad.a.dllimport || ad.a.dllexport) {
                if (type.t & (VT_STATIC|VT_TYPEDEF))
                    tcc_error("cannot have dll linkage with static or typedef");
                if (ad.a.dllimport) {
                    if ((type.t & VT_BTYPE) == VT_FUNC)
                        ad.a.dllimport = 0;
                    else
                        type.t |= VT_EXTERN;
                }
            }
#endif
            if (tok == '{') {
                if (l != VT_CONST)
                    tcc_error("cannot use local functions");
                if ((type.t & VT_BTYPE) != VT_FUNC)
                    expect("function definition");

                /* reject abstract declarators in function definition
                   make old style params without decl have int type */
                sym = type.ref;
                while ((sym = sym->next) != NULL) {
                    if (!(sym->v & ~SYM_FIELD))
                        expect("identifier");
                    if (sym->type.t == VT_VOID)
                        sym->type = int_type;
                }

                /* XXX: cannot do better now: convert extern line to static inline */
                if ((type.t & (VT_EXTERN | VT_INLINE)) == (VT_EXTERN | VT_INLINE))
                    type.t = (type.t & ~VT_EXTERN) | VT_STATIC;

                /* put function symbol */
                sym = external_global_sym(v, &type, 0);
                type.t &= ~VT_EXTERN;
                patch_storage(sym, &ad, &type);

                /* static inline functions are just recorded as a kind
                   of macro. Their code will be emitted at the end of
                   the compilation unit only if they are used */
                if ((type.t & (VT_INLINE | VT_STATIC)) ==
                        (VT_INLINE | VT_STATIC)) {
                    struct InlineFunc *fn;
                    const char *filename;

                    filename = file ? file->filename : "";
                    fn = (struct InlineFunc *)tcc_malloc(sizeof *fn + strlen(filename));
                    strcpy(fn->filename, filename);
                    fn->sym = sym;
                    skip_or_save_block(&fn->func_str);
                    dynarray_add(&tcc_state->inline_fns,
                                 &tcc_state->nb_inline_fns, fn);
                } else {
                    /* compute text section */
                    cur_text_section = ad.section;
                    if (!cur_text_section)
                        cur_text_section = text_section;
                    gen_function(sym);
                }
                break;
            } else {
                if (l == VT_CMP) { /* find parameter in function parameter list */
                    for (sym = func_sym->next; sym; sym = sym->next)
                        if ((sym->v & ~SYM_FIELD) == v)
                            goto found;
                    tcc_error("declaration for parameter '%s' but no such parameter",
                              get_tok_str(v, NULL));
found:
                    if (type.t & VT_STORAGE) /* 'register' is okay */
                        tcc_error("storage class specified for '%s'",
                                  get_tok_str(v, NULL));
                    if (sym->type.t != VT_VOID)
                        tcc_error("redefinition of parameter '%s'",
                                  get_tok_str(v, NULL));
                    convert_parameter_type(&type);
                    sym->type = type;
                } else if (type.t & VT_TYPEDEF) { /* save typedefed type  */
                    /* XXX: test storage specifiers ? */
                    sym = sym_find(v);
                    if (sym && sym->sym_scope == local_scope) {
                        if (!is_compatible_types(&sym->type, &type)
                                || !(sym->type.t & VT_TYPEDEF))
                            tcc_error("incompatible redefinition of '%s'",
                                      get_tok_str(v, NULL));
                        sym->type = type;
                    } else {
                        sym = sym_push(v, &type, 0, 0);
                    }
                    sym->a = ad.a;
                    sym->f = ad.f;
                } else {
                    r = 0;
                    if ((type.t & VT_BTYPE) == VT_FUNC) {
                        /* external function definition */
                        /* specific case for func_call attribute */
                        type.ref->f = ad.f;
                    } else if (!(type.t & VT_ARRAY)) {
                        /* not lvalue if array */
                        r |= lvalue_type(type.t);
                    }
                    has_init = (tok == '=');
                    if (has_init && (type.t & VT_VLA))
                        tcc_error("variable length array cannot be initialized");
                    if (((type.t & VT_EXTERN) && (!has_init || l != VT_CONST)) ||
                            ((type.t & VT_BTYPE) == VT_FUNC) ||
                            ((type.t & VT_ARRAY) && (type.t & VT_STATIC) &&
                             !has_init && l == VT_CONST && type.ref->c < 0)) {
                        /* external variable or function */
                        /* NOTE: as GCC, uninitialized global static
                           arrays of null size are considered as
                           extern */
                        type.t |= VT_EXTERN;
                        sym = external_sym(v, &type, r, &ad);
                        if (ad.alias_target) {
                            ElfSym *esym;
                            Sym *alias_target;
                            alias_target = sym_find(ad.alias_target);
                            esym = elfsym(alias_target);
                            if (!esym)
                                tcc_error("unsupported forward __alias__ attribute");
                            /* Local statics have a scope until now (for
                               warnings), remove it here.  */
                            sym->sym_scope = 0;
                            put_extern_sym2(sym, esym->st_shndx, esym->st_value, esym->st_size, 0);
                        }
                    } else {
                        if (type.t & VT_STATIC)
                            r |= VT_CONST;
                        else
                            r |= l;
                        if (has_init)
                            next();
                        else if (l == VT_CONST)
                            /* uninitialized global variables may be overridden */
                            type.t |= VT_EXTERN;
                        decl_initializer_alloc(&type, &ad, r, has_init, v, l);
                    }
                }
                if (tok != ',') {
                    if (is_for_loop_init)
                        return 1;
                    skip(';');
                    break;
                }
                next();
            }
            ad.a.aligned = 0;
        }
    }
    return 0;
}

static void decl(int l)
{
    decl0(l, 0, NULL);
}
