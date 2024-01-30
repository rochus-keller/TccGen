#ifndef MACHINE
#define MACHINE

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
#include "vstack.h"
#include "symbols.h"
#include "architecture.h"

/* all functions here are used by the backend only */

void load(int r, SValue *sv);
void store(int r, SValue *v);

void gen_vla_sp_restore(int addr);
void gen_vla_alloc(CType *type, int align);
void gen_vla_sp_save(int addr);

void gen_cvt_ftof(int t);
void gen_cvt_ftoi(int t);
void gen_cvt_itof(int t);

void gen_opf(int op);
void gen_opi(int op);

void gfunc_prolog(CType *func_type);
void gfunc_epilog(void);

void ggoto(void);

void gsym_addr(int t, int a);

int gtst(int inv, int t);

void o(unsigned int c);
void save_reg(int r);
void vpush_global_sym(CType *type, int v);
void vrotb(int n);
void vrott(int n);

/* ------------ x86_64-gen.c ------------ */
#ifdef TCC_TARGET_X86_64
void gen_addr64(int r, Sym *sym, int64_t c);
void gen_opl(int op);
void gtst_addr(int inv, int a);
#endif

/* ------------ arm-gen.c ------------ */
#ifdef TCC_TARGET_ARM
void gen_cvt_itof1(int t);
void arm_init(struct TCCState *s);
void lexpand_nr(void);
int ieee_finite(double d);
int get_reg_ex(int rc, int rc2);
#define gtst_addr(inv, a) gsym_addr(gtst(inv, 0), a)
#endif
/* ------------ arm64-gen.c ------------ */
#ifdef TCC_TARGET_ARM64
void gen_opl(int op);
void gen_cvt_sxtw(void);
void gaddrof(void);
#define gtst_addr(inv, a) gsym_addr(gtst(inv, 0), a)
#endif

/* ------------ i386-gen.c ------------ */
#if defined TCC_TARGET_I386 || defined TCC_TARGET_X86_64
void g(int c);
void gen_le16(int c);
void gen_le32(int c);
void gen_addr32(int r, Sym *sym, int c);
void gen_addrpc32(int r, Sym *sym, int c);
void gtst_addr(int inv, int a);
void save_reg_upstack(int r, int n);
#endif


/* all globals here are used by the backend only */

extern int func_var; /* true if current function is variadic */
extern int func_vc;
extern const int reg_classes[NB_REGS];

#define NODATA_WANTED (nocode_wanted > 0) /* no static data output wanted either */
#define STATIC_DATA_WANTED (nocode_wanted & 0xC0000000) /* only static data output */

#endif // MACHINE

