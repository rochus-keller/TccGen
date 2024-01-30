#ifndef HELPER_H
#define HELPER_H

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

typedef struct BufferedFile {
    uint8_t *buf_ptr;
    uint8_t *buf_end;
    int fd;
    struct BufferedFile *prev;
    int line_num;    /* current line number - here to simplify code */
    int line_ref;    /* tcc -E: last printed line */
    int ifndef_macro;  /* #ifndef macro / #endif search */
    int ifndef_macro_saved; /* saved ifndef_macro */
    int *ifdef_stack_ptr; /* ifdef_stack value at the start of the file */
    int include_next_index; /* next search path */
    char filename[1024];    /* filename, was in BufferedFile */
    char *true_filename; /* filename not modified by # line directive */
    unsigned char unget[4];
    unsigned char buffer[1]; /* extra size for CH_EOB char */
} BufferedFile;

extern struct BufferedFile *file;
extern int ch, tok;

#define CH_EOF   (-1)   /* end of file */
#define CH_EOB   '\\'       /* end of buffer or '\0' char in file */
#define STRING_MAX_SIZE     1024

#ifdef _WIN32
# define IS_DIRSEP(c) (c == '/' || c == '\\')
# define IS_ABSPATH(p) (IS_DIRSEP(p[0]) || (p[0] && p[1] == ':' && IS_DIRSEP(p[2])))
# define PATHCMP stricmp
# define PATHSEP ";"
#else
# define IS_DIRSEP(c) (c == '/')
# define IS_ABSPATH(p) IS_DIRSEP(p[0])
# define PATHCMP strcmp
# define PATHSEP ":"
#endif

/* public functions currently used by the tcc main function */
char *pstrcpy(char *buf, int buf_size, const char *s);
char *pstrcat(char *buf, int buf_size, const char *s);
char *pstrncpy(char *out, const char *in, size_t num);
char *tcc_basename(const char *name);
char *tcc_fileextension (const char *name);

void tcc_free(void *ptr);
void *tcc_malloc(unsigned long size);
void *tcc_mallocz(unsigned long size);
void *tcc_realloc(void *ptr, unsigned long size);
char *tcc_strdup(const char *str);

#define free(p) use_tcc_free(p)
#define malloc(s) use_tcc_malloc(s)
#define realloc(p, s) use_tcc_realloc(p, s)
#undef strdup
#define strdup(s) use_tcc_strdup(s)
void tcc_memcheck(void);
void tcc_error_noabort(const char *fmt, ...);
NORETURN void tcc_error(const char *fmt, ...);
void tcc_warning(const char *fmt, ...);
NORETURN void expect(const char *msg);

void dynarray_add(void *ptab, int *nb_ptr, void *data);
void dynarray_reset(void *pp, int *n);

void tcc_close(void);
int tcc_open(TCCState *s1, const char *filename);
int handle_eob(void);

static inline int isid(int c) {
    return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c == '_';
}
static inline int isnum(int c) {
    return c >= '0' && c <= '9';
}
static inline int is_space(int ch) {
    return ch == ' ' || ch == '\t' || ch == '\v' || ch == '\f' || ch == '\r';
}
static inline int toup(int c) {
    return (c >= 'a' && c <= 'z') ? c - 'a' + 'A' : c;
}
static inline uint16_t read16le(unsigned char *p) {
    return p[0] | (uint16_t)p[1] << 8;
}
static inline void write16le(unsigned char *p, uint16_t x) {
    p[0] = x & 255;  p[1] = x >> 8 & 255;
}
static inline uint32_t read32le(unsigned char *p) {
  return read16le(p) | (uint32_t)read16le(p + 2) << 16;
}
static inline void write32le(unsigned char *p, uint32_t x) {
    write16le(p, x);  write16le(p + 2, x >> 16);
}
static inline void add32le(unsigned char *p, int32_t x) {
    write32le(p, read32le(p) + x);
}
static inline uint64_t read64le(unsigned char *p) {
  return read32le(p) | (uint64_t)read32le(p + 4) << 32;
}
static inline void write64le(unsigned char *p, uint64_t x) {
    write32le(p, x);  write32le(p + 4, x >> 32);
}
static inline void add64le(unsigned char *p, int64_t x) {
    write64le(p, read64le(p) + x);
}
static inline int isoct(int c) {
    return c >= '0' && c <= '7';
}

/* If I is >= 1 and a power of two, returns log2(i)+1.
   If I is 0 returns 0.  */
static inline int exact_log2p1(int i)
{
    int ret;
    if (!i)
        return 0;
    for (ret = 1; i >= 1 << 8; ret += 8)
        i >>= 8;
    if (i >= 1 << 4)
        ret += 4, i >>= 4;
    if (i >= 1 << 2)
        ret += 2, i >>= 2;
    if (i >= 1 << 1)
        ret++;
    return ret;
}
/* handle just the EOB case, but not stray */
#define PEEKC_EOB(c, p)\
{\
    p++;\
    c = *p;\
    if (c == '\\') {\
        file->buf_ptr = p;\
        c = handle_eob();\
        p = file->buf_ptr;\
    }\
}

#endif // HELPER_H
