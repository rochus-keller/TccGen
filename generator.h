#ifndef GENERATOR_H
#define GENERATOR_H

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

#include "symbols.h"
#include "tccelf.h"
#include "vstack.h"

#if PTR_SIZE == 8 && !defined TCC_TARGET_PE
# define LONG_SIZE 8
#else
# define LONG_SIZE 4
#endif

struct case_t {
    int64_t v1, v2;
    int sym;
};

struct switch_t {
    struct case_t **p; int n; /* list of case ranges */
    int def_sym; /* default symbol */
};

/* all globals here are used by the frontend (and possibly by the backend) */

extern CType char_pointer_type, int_type;
extern struct switch_t *cur_switch; /* current switch */
extern int const_wanted; /* true if constant wanted */

extern int vlas_in_scope;
extern int vla_sp_root_loc;
extern int vla_sp_loc;

extern int rsym; /* return symbol */
extern int anon_sym; /* anonymous symbol index */
extern int ind; /* output code index */
extern int loc; /* local variable index */
extern CType func_vt; /* current function return type */
extern int nocode_wanted; /* suppress code and/or data generation */
extern  CType func_old_type;


/* all functions here are used by the frontend (and possibly by the backend) */

/* ----------- vstack ----------- */
void vdup(void);
void vpop(void);
void vpush_ref(CType *type, Section *sec, unsigned long offset, unsigned long size);
void vpush(CType *type);
void vpush64(int ty, unsigned long long v);
void vpushi(int v);
void vpushs(addr_t v);
void vpushsym(CType *type, Sym *sym);
void vpushv(SValue *v);
void vset(CType *type, int r, int v);
void vsetc(CType *type, int r, CValue *vc);
void vseti(int r, int v);
void vstore(void);
void vswap(void);
void check_vstack(void);

/* ------------ vla -------------*/
void vla_runtime_type_size(CType *type, int *a);
void vla_sp_restore_root(void);
void vla_sp_restore(void);

/* ----------- debugging ----------*/
void tcc_debug_end(TCCState *s1);
void tcc_debug_line(TCCState *s1);
void tcc_debug_start(TCCState *s1);

/* ---------- global state ---------*/
TCCState* tcc_new();
void tcc_delete(TCCState* s1);
void tccgen_init_compiler();
int tcc_set_output_type(TCCState *s, int output_type);

/* ----------- register ----------*/
void move_reg(int r, int s, int t);
void save_regs(int n);
int get_reg(int rc);
int reg_fret(int t);

/* ---------- generator functions --------- */
void gcase(struct case_t **base, int len, int *bsym);
void gen_assign_cast(CType *dt);
void gen_cast_s(int t);
void gen_cast(CType *type);
void gen_function_begin(Sym *sym);
void gen_function_end(Sym *sym);
void gen_op(int op);
void gfunc_return(CType *func_type);
void gv_dup(void);
int  gv(int rc); /* store value in register */
void gv2(int rc1, int rc2);
int  gvtst(int inv, int t); /* test value */
int  gjmp(int t);
void gjmp_addr(int a);
void gfunc_call(int nb_args);
int  gfunc_sret(CType *vt, int variadic, CType *ret, int *align, int *regsize);
void gsym(int t);

#ifdef TCC_TARGET_ARM64
void gfunc_return(CType *func_type);
void gen_clear_cache(void);
void gen_va_start(void);
void gen_va_arg(CType *t);
#endif
#ifdef TCC_TARGET_X86_64
#ifdef TCC_TARGET_PE
void gen_vla_result(int addr);
#else
int classify_x86_64_va_arg(CType *ty);
#endif
#endif

/* --------- aggregated functions -------- */
void address_of(void);
void inc(int post, int c);
void indir(void); /* indirection */
void init_putv(CType *type, Section *sec, unsigned long c);
void init_putz(Section *sec, unsigned long c, int size);

/* -------- other stuff -------- */
char *default_outputfile(TCCState *s, const char *first_file);
int is_null_pointer(SValue *p);
int lvalue_type(int t);
void assure_lvalue(void);

#if defined(TCC_TARGET_ARM) && defined(TCC_ARM_EABI)
const char *default_elfinterp(struct TCCState *s);
#endif


#endif // GENERATOR_H
