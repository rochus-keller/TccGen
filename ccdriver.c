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

// the cc driver only exists during development for testing purpose!

#include "tccstate.h"
#include "tccelf.h"
#include "cparser.h"
#include "helper.h"
#include "generator.h"
#include <string.h>
#include <stdio.h>

typedef struct TCCOption {
    const char *name;
    uint16_t index;
    uint16_t flags;
} TCCOption;

enum {
    TCC_OPTION_L,
    TCC_OPTION_l,
    TCC_OPTION_g,
    TCC_OPTION_c,
    TCC_OPTION_static,
    TCC_OPTION_shared,
    TCC_OPTION_soname,
    TCC_OPTION_o,
    TCC_OPTION_r,
    TCC_OPTION_mfloat_abi,
    TCC_OPTION_nostdlib,
    TCC_OPTION_rdynamic,
    TCC_OPTION_w,
    TCC_OPTION_ar
};

#define TCC_OPTION_HAS_ARG 0x0001
#define TCC_OPTION_NOSEP   0x0002 /* cannot have space before option and arg */

static const TCCOption tcc_options[] = {
    { "L", TCC_OPTION_L, TCC_OPTION_HAS_ARG },
    { "l", TCC_OPTION_l, TCC_OPTION_HAS_ARG | TCC_OPTION_NOSEP },
    { "g", TCC_OPTION_g, TCC_OPTION_HAS_ARG | TCC_OPTION_NOSEP },
    { "c", TCC_OPTION_c, 0 },
    { "static", TCC_OPTION_static, 0 },
    { "shared", TCC_OPTION_shared, 0 },
    { "soname", TCC_OPTION_soname, TCC_OPTION_HAS_ARG },
    { "o", TCC_OPTION_o, TCC_OPTION_HAS_ARG },
    { "rdynamic", TCC_OPTION_rdynamic, 0 },
    { "r", TCC_OPTION_r, 0 },
#ifdef TCC_TARGET_ARM
    { "mfloat-abi", TCC_OPTION_mfloat_abi, TCC_OPTION_HAS_ARG },
#endif
    { "nostdlib", TCC_OPTION_nostdlib, 0 },
    { "w", TCC_OPTION_w, 0 },
    { "ar", TCC_OPTION_ar, 0},
    { NULL, 0, 0 },
};

static int strstart(const char *val, const char **str)
{
    const char *p, *q;
    p = *str;
    q = val;
    while (*q) {
        if (*p != *q)
            return 0;
        p++;
        q++;
    }
    *str = p;
    return 1;
}

static int tcc_parse_args(TCCState *s, int argc, char **argv, int optind)
{
    const TCCOption *popt;
    const char *optarg, *r;
    int x;
    CString linker_arg; /* collect -Wl options */
    int tool = 0, noaction = optind;
    const char *run = NULL;

    cstr_new(&linker_arg);

    while (optind < argc) {
        r = argv[optind];
        optind++;
        if (tool) {
            if (r[0] == '-' && r[1] == 'v' && r[2] == 0)
                ++s->verbose;
            continue;
        }
        if (r[0] != '-' || r[1] == '\0') {
            if (r[0] != '@') /* allow "tcc file(s) -run @ args ..." */
                tccstate_add_file(s, r, s->filetype);
            continue;
        }

        /* find option in table */
        for(popt = tcc_options; ; ++popt) {
            const char *p1 = popt->name;
            const char *r1 = r + 1;
            if (p1 == NULL)
                tcc_error("invalid option -- '%s'", r);
            if (!strstart(p1, &r1))
                continue;
            optarg = r1;
            if (popt->flags & TCC_OPTION_HAS_ARG) {
                if (*r1 == '\0' && !(popt->flags & TCC_OPTION_NOSEP)) {
                    if (optind >= argc)
                arg_err:
                        tcc_error("argument to '%s' is missing", r);
                    optarg = argv[optind++];
                }
            } else if (*r1 != '\0')
                continue;
            break;
        }

        switch(popt->index) {
        case TCC_OPTION_L: /* add library path 'dir' */
            tcc_add_library_path(s, optarg);
            break;
        case TCC_OPTION_l: /* link with dynamic or static library 'lib' */
            tccstate_add_file(s, optarg, AFF_TYPE_LIB);
            break;
        case TCC_OPTION_g: /* generate runtime debug info */
            s->do_debug = 1;
            break;
        case TCC_OPTION_c: /* compile only - generate an object file */
            x = TCC_OUTPUT_OBJ;
        set_output_type:
            if (s->output_type)
                tcc_warning("-%s: overriding compiler action already specified", popt->name);
            s->output_type = x;
            break;
        case TCC_OPTION_static:
            s->static_link = 1;
            break;
        case TCC_OPTION_shared: /* generate a shared library/dll */
            x = TCC_OUTPUT_DLL;
            goto set_output_type;
        case TCC_OPTION_soname: /* set name for shared library to be used at runtime */
            s->soname = tcc_strdup(optarg);
            break;
        case TCC_OPTION_o: /* set output filename */
            if (s->outfile) {
                tcc_warning("multiple -o option");
                tcc_free(s->outfile);
            }
            s->outfile = tcc_strdup(optarg);
            break;
        case TCC_OPTION_r: /* generate (relocatable) object file */
            /* generate a .o merging several output files */
            s->option_r = 1;
            x = TCC_OUTPUT_OBJ;
            goto set_output_type;
        case TCC_OPTION_nostdlib:
            s->nostdlib = 1;
            break;
#ifdef TCC_TARGET_ARM
        case TCC_OPTION_mfloat_abi:
            /* tcc doesn't support soft float yet */
            if (!strcmp(optarg, "softfp")) {
                s->float_abi = ARM_SOFTFP_FLOAT;
            } else if (!strcmp(optarg, "hard"))
                s->float_abi = ARM_HARD_FLOAT;
            else
                tcc_error("unsupported float abi '%s'", optarg);
            break;
#endif
        case TCC_OPTION_w:
            s->warn_none = 1;
            break;
        case TCC_OPTION_rdynamic: /* export all global symbols to dynamic linker */
            s->rdynamic = 1;
            break;
        default:
            tcc_warning("unsupported option '%s'", r);
            break;
        }
    }
    if (linker_arg.size) {
        r = (const char*)linker_arg.data;
        goto arg_err;
    }
    if (tool)
        return tool;
    if (optind != noaction)
        return 0;
    return 1;
}


int main(int argc, char** argv)
{
    if( argc < 2 )
        return -1;

    TCCState *s;

    parser_init();
    s = tcc_new();

    tcc_parse_args(s, argc, argv, 1);

    // set_environment:
    const char* path = getenv("LIBRARY_PATH");
    if(path != NULL) {
        tcc_add_library_path(s, path);
    }

    if (s->output_type == 0)
        s->output_type = TCC_OUTPUT_EXE;
    tcc_set_output_type(s, s->output_type );

    /* compile or add each files or library */
    const char *first_file;
    int ret, n = 0;
    n = s->nb_files;
    if (n == 0)
        tcc_error("no input files\n");
    for (first_file = NULL, ret = 0;;) {
        struct filespec *f = s->files[s->nb_files - n];
        s->filetype = f->type;
        s->alacarte_link = f->alacarte;
        if (f->type == AFF_TYPE_LIB) {
            if (tcc_add_library_err(s, f->name) < 0)
                ret = 1;
        } else {
            if (1 == s->verbose)
                printf("-> %s\n", f->name);
            if (!first_file)
                first_file = f->name;
            if (add_file(s, f->name) < 0)
                ret = 1;
        }
        s->filetype = 0;
        s->alacarte_link = 1;
        if (--n == 0 || ret )
            break;
    }

    if (0 == ret) {
        if (s->output_type == TCC_OUTPUT_MEMORY) {
            // we dont support run: ret = tcc_run(s, argc, argv);
        } else {
            if (!s->outfile)
                s->outfile = default_outputfile(s, first_file);
            if (tcc_output_file(s, s->outfile))
                ret = 1;
        }
    }

    tcc_delete(s);

	return 0;
}
