#ifndef TCCSTATE_H
#define TCCSTATE_H

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
#include <setjmp.h>
#include "architecture.h"

struct Section;
typedef struct Section Section;

/* extra symbol attributes (not in symbol table) */
struct sym_attr {
    unsigned got_offset;
    unsigned plt_offset;
    int plt_sym;
    int dyn_index;
#ifdef TCC_TARGET_ARM
    unsigned char plt_thumb_stub:1;
#endif
};

typedef struct DLLReference {
    int level;
    void *handle;
    char name[1];
} DLLReference;

/* s->filetype: */
#define AFF_TYPE_NONE   0
#define AFF_TYPE_C      1
#define AFF_TYPE_LIB    4

struct filespec {
    char type;
    char alacarte;
    char name[1];
};

struct TokenString;

/* inline functions */
typedef struct InlineFunc {
    struct TokenString *func_str;
    struct Sym *sym;
    char filename[1];
} InlineFunc;

typedef struct TCCState {

    /* output type, see TCC_OUTPUT_XXX */
    int output_type;
    /* output format, see TCC_OUTPUT_FORMAT_xxx */
    int output_format;

    char *tcc_lib_path; /* CONFIG_TCCDIR or -B option */
    int nostdlib; /* if true, no standard libraries are added */
    int rdynamic; /* if true, all symbols are exported */
    char *soname; /* as specified on the command line (-soname) */
    int verbose; /* if true, display some information during compilation */
    int static_link; /* if true, static linking is performed */
    char *rpath; /* as specified on the command line (-Wl,-rpath=) */
    int enable_new_dtags; /* ditto, (-Wl,--enable-new-dtags) */
    int symbolic; /* if true, resolve symbols in the current module first */
    int alacarte_link; /* if true, only link in referenced objects from archive */
    int nocommon; /* if true, do not use common symbols for .bss data */

    /* compile with debug symbol (and use them if error during execution) */
    int do_debug;

    unsigned section_align; /* section alignment */

    uint64_t text_addr; /* address of text section, originally addr_t */
    int has_text_addr;

    int nb_errors;

    /* #pragma pack stack */
    int *pack_stack_ptr;

    /* inline functions are stored as token lists and compiled last
       only if referenced */
    struct InlineFunc **inline_fns;
    int nb_inline_fns;

    /* sections */
    Section **sections;
    int nb_sections; /* number of sections, including first dummy section */
    Section **priv_sections;
    int nb_priv_sections; /* number of private sections */

    /* got & plt handling */
    Section *got;
    Section *plt;

    /* temporary dynamic symbol sections (for dll loading) */
    Section *dynsymtab_section;
    /* exported dynamic symbol section */
    Section *dynsym;
    /* copy of the global symtab_section variable */
    Section *symtab;
    /* extra attributes (eg. GOT/PLT value) for symtab symbols */
    struct sym_attr *sym_attrs;
    int nb_sym_attrs;

#ifdef TCC_TARGET_X86_64
    int nosse; /* For -mno-sse support. */
#endif

#ifdef TCC_TARGET_PE
    /* PE info */
    int pe_subsystem;
    unsigned pe_characteristics;
    unsigned pe_file_align;
    unsigned pe_stack_size;
    uint64_t pe_imagebase; /* original addr_t */
# ifdef TCC_TARGET_X86_64
    Section *uw_pdata;
    int uw_sym;
    unsigned uw_offs;
# endif
#endif
#if defined(TCC_TARGET_ARM)
    int float_abi;
#endif

    /* array of all loaded dlls (including those referenced by loaded dlls) */
    DLLReference **loaded_dlls;
    int nb_loaded_dlls;

    /* C language options */
    int leading_underscore;
    int ms_extensions;	/* allow nested named struct w/o identifier behave like unnamed */
    int ms_bitfields; /* if true, emulate MS algorithm for aligning bitfields */

    /* warning switches */
    int warn_write_strings;
    int warn_error;
    int warn_none;
    int warn_implicit_function_declaration;

    /* used by main and tcc_parse_args only */
    struct filespec **files; /* files seen on command line */
    int nb_files; /* number thereof */
    int filetype;

    int error_set_jmp_enabled;
    jmp_buf error_jmp_buf;

    char* filename;    /* address of file->filename */
    int* line_num; /* address of file->line_num */

    int func_ind, last_ind, last_line_num; /* debug last line number and pc */
    const char *funcname;

    /* library paths */
    char **library_paths;
    int nb_library_paths;

    /* crt?.o object path */
    char **crt_paths;
    int nb_crt_paths;

    char *outfile; /* output filename */
    int option_r; /* option -r */


} TCCState;

extern struct TCCState *tcc_state;

void tccstate_add_file(TCCState *s, const char* filename, int filetype);
int tcc_add_library_path(TCCState *s, const char *pathname);
int tcc_set_crt_path(TCCState *s, const char *pathname);
void tcc_split_path(TCCState *s, void *p_ary, int *p_nb_ary, const char *in);
void free_inline_functions(TCCState *s);

#endif // TCCSTATE_H
