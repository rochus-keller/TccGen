#ifndef TCC_H
#define TCC_H

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

#include <inttypes.h>
#include <stdlib.h>

#ifdef _MSC_VER
# define NORETURN __declspec(noreturn)
# define ALIGNED(x) __declspec(align(x))
#else
# define NORETURN __attribute__((noreturn))
# define ALIGNED(x) __attribute__((aligned(x)))
#endif

#ifndef O_BINARY
# define O_BINARY 0
#endif

#define TCC_VERSION "0.9.27"

#define CONFIG_TCCDIR "./lib"
#define CONFIG_SYSROOT "" // TODO
// this works to use musl with crt1.o, crti.o, crtn.o and libc.a
// even if libc.a is 2MB, the a.out is only 22.7 kB (1.7kB with libc.so)
// does not yet work with musl libc.so
#define CONFIG_TCC_CRTPREFIX "./lib"
#define CONFIG_TCC_LIBPATHS CONFIG_TCC_CRTPREFIX

#endif // TCC_H

