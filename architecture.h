#ifndef ARCHITECTURE
#define ARCHITECTURE
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

/* -------------------------------------------- */
/* include the target specific definitions */

#ifdef TCC_TARGET_I386
# include "i386.h"
#endif
#ifdef TCC_TARGET_X86_64
# include "x86_64.h"
#endif
#ifdef TCC_TARGET_ARM
# include "arm.h"
#endif
#ifdef TCC_TARGET_ARM64
# include "arm64.h"
#endif


#endif // ARCHITECTURE

