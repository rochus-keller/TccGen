#ifndef TCCPE
#define TCCPE

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

#include "tccstate.h"
#include "tccelf.h"
#include "vstack.h"

/* ------------ tccpe.c -------------- */
#ifdef TCC_TARGET_PE
int pe_load_file(struct TCCState *s1, const char *filename, int fd);
int pe_output_file(TCCState * s1, const char *filename);
int pe_putimport(TCCState *s1, int dllindex, const char *name, addr_t value);
#if defined TCC_TARGET_I386 || defined TCC_TARGET_X86_64
SValue *pe_getimport(SValue *sv, SValue *v2);
#endif
#ifdef TCC_TARGET_X86_64
void pe_add_unwind_data(unsigned start, unsigned end, unsigned stack);
#endif
int tcc_get_dllexports(const char *filename, char **pp);
/* symbol properties stored in Elf32_Sym->st_other */
# define ST_PE_EXPORT 0x10
# define ST_PE_IMPORT 0x20
# define ST_PE_STDCALL 0x40
#endif

#endif // TCCPE

