QT       += core

QT       -= gui

TARGET = oc
CONFIG   += console
CONFIG   -= app_bundle

TEMPLATE = app

SOURCES += \
    i386_gen.c \
    i386_link.c \
    arm_gen.c \
    arm_link.c \
    x86_64_gen.c \
    x86_64_link.c \
    arm64_gen.c \
    arm64_link.c \
    tccelf.c \
    tcccoff.c \
    tccpe.c \
    helper.c \
    values.c \
    generator.c \
    tccstate.c \
    symbols.c \
    types.c \
    oparser.c \
    ccdriver.c \
    token.c \
    machine.c

# TCC_ARM_EABI TCC_ARM_VFP TCC_TARGET_PE
# TCC_TARGET_I386 TCC_TARGET_X86_64 TCC_TARGET_ARM TCC_TARGET_ARM64
DEFINES += TCC_TARGET_I386

HEADERS += \
    i386.h \
    x86_64.h \
    arm.h \
    arm64.h \
    symbols.h \
    tcc.h \
    values.h \
    types.h \
    architecture.h \
    vstack.h \
    elf.h \
    tccelf.h \
    token.h \
    coff.h \
    i386_tok.h \
    i386_asm.h \
    x86_64_asm.h \
    tccpe.h \
    helper.h \
    generator.h \
    tccstate.h \
    tcctok.h \
    cparser.h \
    stab.h \
    machine.h

QMAKE_CFLAGS += -Wno-unused-parameter -Wno-unused-function -Wno-unused-variable -Wno-sign-compare -Werror

LIBS += -ldl

