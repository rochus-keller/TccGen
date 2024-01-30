#ifndef SYMBOLS
#define SYMBOLS

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

#include "tcc.h"
#include "types.h"
#include "values.h"
#include "tccelf.h"

/* -------------------------------------------------- */

#define SYM_STRUCT     0x40000000 /* struct/union/enum symbol space */
#define SYM_FIELD      0x20000000 /* struct/union field symbol space */
#define SYM_FIRST_ANOM 0x10000000 /* first anonymous sym */

/* stored in 'Sym->f.func_type' field */
#define FUNC_NEW       1 /* ansi function prototype */
#define FUNC_OLD       2 /* old function prototype */
#define FUNC_ELLIPSIS  3 /* ansi function prototype with ... */

/* stored in 'Sym->f.func_call' field */
#define FUNC_CDECL     0 /* standard c call */
#define FUNC_STDCALL   1 /* pascal c call */
#define FUNC_FASTCALL1 2 /* first param in %eax */
#define FUNC_FASTCALL2 3 /* first parameters in %eax, %edx */
#define FUNC_FASTCALL3 4 /* first parameter in %eax, %edx, %ecx */
#define FUNC_FASTCALLW 5 /* first parameter in %ecx, %edx */

/* field 'Sym.r' for C labels */
#define LABEL_DEFINED  0 /* label is defined */
#define LABEL_FORWARD  1 /* label is forward defined */
#define LABEL_DECLARED 2 /* label is declared but never used */

/* symbol attributes */
struct SymAttr {
    unsigned short
    aligned     : 5, /* alignment as log2+1 (0 == unspecified) */
    packed      : 1,
    weak        : 1,
    visibility  : 2,
    dllexport   : 1,
    dllimport   : 1,
    unused      : 5;
};

/* function attributes or temporary attributes for parsing */
struct FuncAttr {
    unsigned
    func_call   : 3, /* calling convention (0..5), see above */
    func_type   : 2, /* FUNC_OLD/NEW/ELLIPSIS */
    func_args   : 8; /* PE __stdcall args */
};

/* GNUC attribute definition */
typedef struct AttributeDef {
    struct SymAttr a;
    struct FuncAttr f;
    struct Section *section;
    int alias_target; /* token */
    int asm_label; /* associated asm label */
    char attr_mode; /* __attribute__((__mode__(...))) */
} AttributeDef;

/* symbol management */
typedef struct Sym {
    int v; /* symbol token */
    unsigned short r; /* associated register or VT_CONST/VT_LOCAL and LVAL type */
    struct SymAttr a; /* symbol attributes */
    union {
        struct {
            int c; /* associated number or Elf symbol index */
            union {
                int sym_scope; /* scope level for locals */
                int jnext; /* next jump label */
                struct FuncAttr f; /* function attributes */
                int auxtype; /* bitfield access type */
            };
        };
        long long enum_val; /* enum constant if IS_ENUM_VAL */
    };
    CType type; /* associated type */
    union {
        struct Sym *next; /* next related symbol (for fields and anoms) */
        int asm_label; /* associated asm label */
    };
    struct Sym *prev; /* prev symbol in stack */
    struct Sym *prev_tok; /* previous symbol for this token */
} Sym;

/* token symbol management */
typedef struct TokenSym {
    struct TokenSym *hash_next;
    struct Sym *sym_label; /* direct pointer to label */
    struct Sym *sym_struct; /* direct pointer to structure */
    struct Sym *sym_identifier; /* direct pointer to identifier */
    int tok; /* token number */
    int len;
    char str[1];
} TokenSym;

#define SYM_POOL_NB (8192 / sizeof(Sym))
extern void **sym_pools;
extern int nb_sym_pools;

extern Sym *global_stack;
extern Sym *local_stack;
extern struct TokenSym **table_ident;
extern int tok_ident;
extern int local_scope;
extern Sym *global_label_stack;

#define TOK_HASH_INIT 1
#define TOK_HASH_FUNC(h, c) ((h) + ((h) << 5) + ((h) >> 27) + (c))
#define TOK_HASH_SIZE       16384 /* must be a power of two */

void sym_free(Sym *sym);
Sym *sym_push2(Sym **ps, int v, int t, int c);
Sym *sym_find2(Sym *s, int v);
Sym *sym_push(int v, CType *type, int r, int c);
void sym_pop(Sym **ptop, Sym *b, int keep);
Sym *struct_find(int v);
Sym *sym_find(int v);
Sym *global_identifier_push(int v, int t, int c);

TokenSym *tok_alloc(const char *str, int len);
TokenSym *tok_alloc2(unsigned int h, const char* str, int len);
void symbols_init();
void symbols_free();
const char*get_tok_str(int v, CValue* cv);

Sym *label_push(Sym **ptop, int v, int flags);
void label_pop(Sym **ptop, Sym *slast, int keep);
Sym *label_find(int v);

void put_extern_sym(Sym *sym, Section *section,
                            addr_t value, unsigned long size);
ElfSym *elfsym(Sym *s);
void update_storage(Sym *sym);
void patch_storage(Sym *sym, AttributeDef *ad, CType *type);
void put_extern_sym2(Sym *sym, int sh_num,
                             addr_t value, unsigned long size,
                             int can_add_underscore);
Sym *external_global_sym(int v, CType *type, int r);
Sym * find_field (CType *type, int v);
void struct_layout(CType *type, AttributeDef *ad);
Sym *external_sym(int v, CType *type, int r, AttributeDef *ad);
Sym *get_sym_ref(CType *type, Section *sec, unsigned long offset, unsigned long size);

void sym_to_attr(AttributeDef *ad, Sym *s);

#endif // SYMBOLS

