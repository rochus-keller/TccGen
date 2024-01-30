#ifndef TCCELF_H
#define TCCELF_H

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
#include "elf.h"
#include "architecture.h"

/* -------------------------------------------- */

#if PTR_SIZE == 8
# define ELFCLASSW ELFCLASS64
# define ElfW(type) Elf##64##_##type
# define ELFW(type) ELF##64##_##type
# define ElfW_Rel ElfW(Rela)
# define SHT_RELX SHT_RELA
# define REL_SECTION_FMT ".rela%s"
#else
# define ELFCLASSW ELFCLASS32
# define ElfW(type) Elf##32##_##type
# define ELFW(type) ELF##32##_##type
# define ElfW_Rel ElfW(Rel)
# define SHT_RELX SHT_REL
# define REL_SECTION_FMT ".rel%s"
#endif

/* target address type */
#define addr_t ElfW(Addr)
#define ElfSym ElfW(Sym)

/* section definition */
typedef struct Section {
    unsigned long data_offset; /* current data offset */
    unsigned char *data;       /* section data */
    unsigned long data_allocated; /* used for realloc() handling */
    int sh_name;             /* elf section name (only used during output) */
    int sh_num;              /* elf section number */
    int sh_type;             /* elf section type */
    int sh_flags;            /* elf section flags */
    int sh_info;             /* elf section info */
    int sh_addralign;        /* elf section alignment */
    int sh_entsize;          /* elf entry size */
    unsigned long sh_size;   /* section size (only used during output) */
    addr_t sh_addr;          /* address at which the section is relocated */
    unsigned long sh_offset; /* file offset */
    int nb_hashed_syms;      /* used to resize the hash table */
    struct Section *link;    /* link to another section */
    struct Section *reloc;   /* corresponding section for relocation, if any */
    struct Section *hash;    /* hash table for symbols */
    struct Section *prev;    /* previous section on section stack */
    char name[1];           /* section name */
} Section;

#define TCC_OUTPUT_FORMAT_ELF    0 /* default output format: ELF */
#define TCC_OUTPUT_FORMAT_BINARY 1 /* binary image output */
#define TCC_OUTPUT_FORMAT_COFF   2 /* COFF */

#define TCC_OUTPUT_MEMORY   1 /* output will be run in memory (default) */
#define TCC_OUTPUT_EXE      2 /* executable file */
#define TCC_OUTPUT_DLL      3 /* dynamic library */
#define TCC_OUTPUT_OBJ      4 /* object file */
#define TCC_OUTPUT_PREPROCESS 5 /* only preprocess (used internally) */

# define TCC_LIBTCC1 "libtcc1.a"

#define AFF_BINTYPE_REL 1
#define AFF_BINTYPE_DYN 2
#define AFF_BINTYPE_AR  3
#define AFF_BINTYPE_C67 4

#define AFF_REFERENCED_DLL  0x20 /* load a referenced dll from another dll */
#define AFF_TYPE_BIN        0x40 /* file to add is binary */
#define AFF_PRINT_ERROR     0x10 /* print error if file not found */

/* ------------ tccelf.c ------------ */

#define ARMAG  "!<arch>\012"    /* For COFF and a.out archives */

/* symbols N_FUN, N_SLINE, N_BINCL, N_EINCL, N_SO declared in stab.h */

typedef struct {
    unsigned int n_strx;         /* index into string table of name */
    unsigned char n_type;         /* type of symbol */
    unsigned char n_other;        /* misc info (usually empty) */
    unsigned short n_desc;        /* description field */
    unsigned int n_value;        /* value of symbol */
} Stab_Sym;

extern Section *text_section, *data_section, *bss_section; /* predefined sections */
extern Section *common_section;
extern Section *cur_text_section; /* current section where function code is generated */
extern Section *symtab_section; /* symbol sections */
extern Section *stab_section, *stabstr_section; /* debug sections */

struct TCCState;
typedef struct TCCState TCCState;

void tccelf_new(TCCState *s);
void tccelf_delete(TCCState *s);
void tccelf_stab_new(TCCState *s);
void tccelf_begin_file(TCCState *s1);
void tccelf_end_file(TCCState *s1);

Section *new_section(TCCState *s1, const char *name, int sh_type, int sh_flags);
void section_realloc(Section *sec, unsigned long new_size);
size_t section_add(Section *sec, addr_t size, int align);
void *section_ptr_add(Section *sec, addr_t size);
void section_reserve(Section *sec, unsigned long size);
Section *find_section(TCCState *s1, const char *name);
Section *new_symtab(TCCState *s1, const char *symtab_name, int sh_type, int sh_flags, const char *strtab_name, const char *hash_name, int hash_sh_flags);

struct Sym;
typedef struct Sym Sym;

void put_extern_sym2(Sym *sym, int sh_num, addr_t value, unsigned long size, int can_add_underscore);
void put_extern_sym(Sym *sym, Section *section, addr_t value, unsigned long size);
#if PTR_SIZE == 4
void greloc(Section *s, Sym *sym, unsigned long offset, int type);
#endif
void greloca(Section *s, Sym *sym, unsigned long offset, int type, addr_t addend);

int put_elf_str(Section *s, const char *sym);
int put_elf_sym(Section *s, addr_t value, unsigned long size, int info, int other, int shndx, const char *name);
int set_elf_sym(Section *s, addr_t value, unsigned long size, int info, int other, int shndx, const char *name);
int find_elf_sym(Section *s, const char *name);
void put_elf_reloc(Section *symtab, Section *s, unsigned long offset, int type, int symbol);
void put_elf_reloca(Section *symtab, Section *s, unsigned long offset, int type, int symbol, addr_t addend);

void put_stabs(const char *str, int type, int other, int desc, unsigned long value);
void put_stabs_r(const char *str, int type, int other, int desc, unsigned long value, Section *sec, int sym_index);
void put_stabn(int type, int other, int desc, int value);
void put_stabd(int type, int other, int desc);

void resolve_common_syms(TCCState *s1);
void relocate_syms(TCCState *s1, Section *symtab, int do_resolve);
void relocate_section(TCCState *s1, Section *s);

int tcc_object_type(int fd, ElfW(Ehdr) *h);
int tcc_load_object_file(TCCState *s1, int fd, unsigned long file_offset);
int tcc_load_archive(TCCState *s1, int fd);
void tcc_add_runtime(TCCState *s1);

void build_got_entries(TCCState *s1);
struct sym_attr *get_sym_attr(TCCState *s1, int index, int alloc);
void squeeze_multi_relocs(Section *sec, size_t oldrelocoffset);

addr_t get_elf_sym_addr(TCCState *s, const char *name, int err);
#if defined TCC_TARGET_PE
void *tcc_get_symbol_err(TCCState *s, const char *name);
#endif

#ifndef TCC_TARGET_PE
int tcc_load_dll(TCCState *s1, int fd, const char *filename, int level);
int tcc_load_ldscript(TCCState *s1);
void minp(void);
#endif
void inp(void);
uint8_t *parse_comment(uint8_t *p);

int tcc_add_dll(TCCState *s, const char *filename, int flags);
int tcc_add_crt(TCCState *s, const char *filename);
/* add a symbol to the compiled program */
int tcc_add_symbol(TCCState *s, const char *name, const void *val);
/* add a file (C file, dll, object, library, ld script). Return -1 if error. */
int tcc_add_binary_file(TCCState *s1, const char *filename, int flags);
int tcc_add_library_err(TCCState *s, const char *f);

#define DEFAULT_ELFINTERP(s) "/lib/ld-linux.so.2" // TODO

/* ------------ xxx-link.c ------------ */

/* Whether to generate a GOT/PLT entry and when. NO_GOTPLT_ENTRY is first so
   that unknown relocation don't create a GOT or PLT entry */
enum gotplt_entry {
    NO_GOTPLT_ENTRY,	/* never generate (eg. GLOB_DAT & JMP_SLOT relocs) */
    BUILD_GOT_ONLY,	/* only build GOT (eg. TPOFF relocs) */
    AUTO_GOTPLT_ENTRY,	/* generate if sym is UNDEF */
    ALWAYS_GOTPLT_ENTRY	/* always generate (eg. PLTOFF relocs) */
};

int code_reloc (int reloc_type);
int gotplt_entry_type (int reloc_type);
unsigned create_plt_entry(TCCState *s1, unsigned got_offset, struct sym_attr *attr);
void relocate_init(Section *sr);
void relocate(TCCState *s1, ElfW_Rel *rel, int type, unsigned char *ptr, addr_t addr, addr_t val);
void relocate_plt(TCCState *s1);
int tcc_output_file(TCCState *s, const char *filename);

#endif // TCCELF_H

