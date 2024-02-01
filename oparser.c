/*
 *  TccGen - Oberon-0 parser example
 *
 *  Copyright (c) 2024 Rochus Keller
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

// The token types and decoder as well as the parser stub were generated using EbnfStudio from Oberon0.ebnf

#include "cparser.h"
#include "helper.h"
#include "generator.h"
#include <string.h>
#include <ctype.h>
#include <stdio.h>

typedef enum TokenType {
    Tok_Invalid = 0,

    TT_Literals,
    Tok_Hash,
    Tok_Amp,
    Tok_Lpar,
    Tok_Rpar,
    Tok_Star,
    Tok_Plus,
    Tok_Comma,
    Tok_Minus,
    Tok_Dot,
    Tok_Colon,
    Tok_ColonEq,
    Tok_Semi,
    Tok_Lt,
    Tok_Leq,
    Tok_Eq,
    Tok_Gt,
    Tok_Geq,
    Tok_Lbrack,
    Tok_Rbrack,
    Tok_Tilde,

    TT_Keywords,
    Tok_ARRAY,
    Tok_BEGIN,
    Tok_CONST,
    Tok_DIV,
    Tok_DO,
    Tok_ELSE,
    Tok_ELSIF,
    Tok_END,
    Tok_IF,
    Tok_MOD,
    Tok_MODULE,
    Tok_OF,
    Tok_OR,
    Tok_PROCEDURE,
    Tok_RECORD,
    Tok_THEN,
    Tok_TYPE,
    Tok_VAR,
    Tok_WHILE,

    TT_Specials,
    Tok_ident,
    Tok_integer,
    Tok_Eof,

    TT_MaxToken,

    TT_Max
} TokenType;

static const char* tokenTypeString( int r ) {
    switch(r) {
        case Tok_Invalid: return "<invalid>";
        case Tok_Hash: return "#";
        case Tok_Amp: return "&";
        case Tok_Lpar: return "(";
        case Tok_Rpar: return ")";
        case Tok_Star: return "*";
        case Tok_Plus: return "+";
        case Tok_Comma: return ",";
        case Tok_Minus: return "-";
        case Tok_Dot: return ".";
        case Tok_Colon: return ":";
        case Tok_ColonEq: return ":=";
        case Tok_Semi: return ";";
        case Tok_Lt: return "<";
        case Tok_Leq: return "<=";
        case Tok_Eq: return "=";
        case Tok_Gt: return ">";
        case Tok_Geq: return ">=";
        case Tok_Lbrack: return "[";
        case Tok_Rbrack: return "]";
        case Tok_Tilde: return "~";
        case Tok_ARRAY: return "ARRAY";
        case Tok_BEGIN: return "BEGIN";
        case Tok_CONST: return "CONST";
        case Tok_DIV: return "DIV";
        case Tok_DO: return "DO";
        case Tok_ELSE: return "ELSE";
        case Tok_ELSIF: return "ELSIF";
        case Tok_END: return "END";
        case Tok_IF: return "IF";
        case Tok_MOD: return "MOD";
        case Tok_MODULE: return "MODULE";
        case Tok_OF: return "OF";
        case Tok_OR: return "OR";
        case Tok_PROCEDURE: return "PROCEDURE";
        case Tok_RECORD: return "RECORD";
        case Tok_THEN: return "THEN";
        case Tok_TYPE: return "TYPE";
        case Tok_VAR: return "VAR";
        case Tok_WHILE: return "WHILE";
        case Tok_ident: return "ident";
        case Tok_integer: return "integer";
        case Tok_Eof: return "<eof>";
        default: return "";
    }
}

static inline char at( const char* str, const char* eof, int i ){
    return ( str+i < eof ? str[i] : 0 );
}

static TokenType tokenTypeFromString( const char* str, const char* eof, int* pos ) {
    int i = ( pos != 0 ? *pos: 0 );
    TokenType res = Tok_Invalid;
    switch( at(str,eof,i) ){
    case '#':
        res = Tok_Hash; i += 1;
        break;
    case '&':
        res = Tok_Amp; i += 1;
        break;
    case '(':
        res = Tok_Lpar; i += 1;
        break;
    case ')':
        res = Tok_Rpar; i += 1;
        break;
    case '*':
        res = Tok_Star; i += 1;
        break;
    case '+':
        res = Tok_Plus; i += 1;
        break;
    case ',':
        res = Tok_Comma; i += 1;
        break;
    case '-':
        res = Tok_Minus; i += 1;
        break;
    case '.':
        res = Tok_Dot; i += 1;
        break;
    case ':':
        if( at(str,eof,i+1) == '=' ){
            res = Tok_ColonEq; i += 2;
        } else {
            res = Tok_Colon; i += 1;
        }
        break;
    case ';':
        res = Tok_Semi; i += 1;
        break;
    case '<':
        if( at(str,eof,i+1) == '=' ){
            res = Tok_Leq; i += 2;
        } else {
            res = Tok_Lt; i += 1;
        }
        break;
    case '=':
        res = Tok_Eq; i += 1;
        break;
    case '>':
        if( at(str,eof,i+1) == '=' ){
            res = Tok_Geq; i += 2;
        } else {
            res = Tok_Gt; i += 1;
        }
        break;
    case 'A':
        if( at(str,eof,i+1) == 'R' ){
            if( at(str,eof,i+2) == 'R' ){
                if( at(str,eof,i+3) == 'A' ){
                    if( at(str,eof,i+4) == 'Y' ){
                        res = Tok_ARRAY; i += 5;
                    }
                }
            }
        }
        break;
    case 'B':
        if( at(str,eof,i+1) == 'E' ){
            if( at(str,eof,i+2) == 'G' ){
                if( at(str,eof,i+3) == 'I' ){
                    if( at(str,eof,i+4) == 'N' ){
                        res = Tok_BEGIN; i += 5;
                    }
                }
            }
        }
        break;
    case 'C':
        if( at(str,eof,i+1) == 'O' ){
            if( at(str,eof,i+2) == 'N' ){
                if( at(str,eof,i+3) == 'S' ){
                    if( at(str,eof,i+4) == 'T' ){
                        res = Tok_CONST; i += 5;
                    }
                }
            }
        }
        break;
    case 'D':
        switch( at(str,eof,i+1) ){
        case 'I':
            if( at(str,eof,i+2) == 'V' ){
                res = Tok_DIV; i += 3;
            }
            break;
        case 'O':
            res = Tok_DO; i += 2;
            break;
        }
        break;
    case 'E':
        switch( at(str,eof,i+1) ){
        case 'L':
            if( at(str,eof,i+2) == 'S' ){
                switch( at(str,eof,i+3) ){
                case 'E':
                    res = Tok_ELSE; i += 4;
                    break;
                case 'I':
                    if( at(str,eof,i+4) == 'F' ){
                        res = Tok_ELSIF; i += 5;
                    }
                    break;
                }
            }
            break;
        case 'N':
            if( at(str,eof,i+2) == 'D' ){
                res = Tok_END; i += 3;
            }
            break;
        }
        break;
    case 'I':
        if( at(str,eof,i+1) == 'F' ){
            res = Tok_IF; i += 2;
        }
        break;
    case 'M':
        if( at(str,eof,i+1) == 'O' ){
            if( at(str,eof,i+2) == 'D' ){
                if( at(str,eof,i+3) == 'U' ){
                    if( at(str,eof,i+4) == 'L' ){
                        if( at(str,eof,i+5) == 'E' ){
                            res = Tok_MODULE; i += 6;
                        }
                    }
                } else {
                    res = Tok_MOD; i += 3;
                }
            }
        }
        break;
    case 'O':
        switch( at(str,eof,i+1) ){
        case 'F':
            res = Tok_OF; i += 2;
            break;
        case 'R':
            res = Tok_OR; i += 2;
            break;
        }
        break;
    case 'P':
        if( at(str,eof,i+1) == 'R' ){
            if( at(str,eof,i+2) == 'O' ){
                if( at(str,eof,i+3) == 'C' ){
                    if( at(str,eof,i+4) == 'E' ){
                        if( at(str,eof,i+5) == 'D' ){
                            if( at(str,eof,i+6) == 'U' ){
                                if( at(str,eof,i+7) == 'R' ){
                                    if( at(str,eof,i+8) == 'E' ){
                                        res = Tok_PROCEDURE; i += 9;
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
        break;
    case 'R':
        if( at(str,eof,i+1) == 'E' ){
            if( at(str,eof,i+2) == 'C' ){
                if( at(str,eof,i+3) == 'O' ){
                    if( at(str,eof,i+4) == 'R' ){
                        if( at(str,eof,i+5) == 'D' ){
                            res = Tok_RECORD; i += 6;
                        }
                    }
                }
            }
        }
        break;
    case 'T':
        switch( at(str,eof,i+1) ){
        case 'H':
            if( at(str,eof,i+2) == 'E' ){
                if( at(str,eof,i+3) == 'N' ){
                    res = Tok_THEN; i += 4;
                }
            }
            break;
        case 'Y':
            if( at(str,eof,i+2) == 'P' ){
                if( at(str,eof,i+3) == 'E' ){
                    res = Tok_TYPE; i += 4;
                }
            }
            break;
        }
        break;
    case 'V':
        if( at(str,eof,i+1) == 'A' ){
            if( at(str,eof,i+2) == 'R' ){
                res = Tok_VAR; i += 3;
            }
        }
        break;
    case 'W':
        if( at(str,eof,i+1) == 'H' ){
            if( at(str,eof,i+2) == 'I' ){
                if( at(str,eof,i+3) == 'L' ){
                    if( at(str,eof,i+4) == 'E' ){
                        res = Tok_WHILE; i += 5;
                    }
                }
            }
        }
        break;
    case '[':
        res = Tok_Lbrack; i += 1;
        break;
    case ']':
        res = Tok_Rbrack; i += 1;
        break;
    case '~':
        res = Tok_Tilde; i += 1;
        break;
    }
    if(pos) *pos = i;
    return res;
}

typedef struct Token {
    int type;
    int line;
    const char* val;
    int len;
} Token;

static char str[64];

static inline Token token(int t, const char* v, int l )
{
    Token tok;
    tok.type = t;
    tok.line = file->line_num;
    tok.val = v;
    tok.len = l;
    return tok;
}

static int skipWhiteSpace()
{
    if( file == 0 )
        return 0;
    while( file->buf_ptr < file->buf_end && isspace( *file->buf_ptr ) )
    {
        if( *file->buf_ptr == '\n' )
            file->line_num++;
        file->buf_ptr++;
    }
    return file->buf_ptr >= file->buf_end;
}

static Token ident()
{
    int off = 1;
    while( file->buf_ptr+off < file->buf_end )
    {
        const char c = file->buf_ptr[off];
        if( !isalnum(c) )
            break;
        else
            off++;
    }
    int pos = 0;
    TokenType t = tokenTypeFromString( (const char*)file->buf_ptr, (const char*)file->buf_end, &pos );
    const char* v = (const char*)file->buf_ptr;
    file->buf_ptr += off;
    if( t != Tok_Invalid && pos != off )
        t = Tok_Invalid;
    if( t != Tok_Invalid )
        return token( t, 0, 0 ); // keyword found
    else
        return token( Tok_ident, v, off );
}

static Token number()
{
    int off = 1;
    while( file->buf_ptr+off < file->buf_end )
    {
        const char c = file->buf_ptr[off];
        if( !isnum(c) )
            break;
        else
            off++;
    }
    const char* v = (const char*)file->buf_ptr;
    file->buf_ptr += off;
    return token( Tok_integer, v, off );
}

static Token lexNextToken()
{
    Token t;

    if( file != 0 && file->buf_ptr >= file->buf_end )
        handle_eob();

    if( skipWhiteSpace() )
        return token(Tok_Eof, 0, 0);

    const char ch = *file->buf_ptr;


    if( isalpha(ch) )
        return ident();
    else if( isdigit(ch) )
        return number();
    // else
    int pos = 0;
    TokenType tt = tokenTypeFromString((const char*)file->buf_ptr, (const char*)file->buf_end,&pos);

    if( tt == Tok_Invalid || pos == 0 )
    {
        sprintf(str, "unexpected character '%c' %d", ch, ch );
        return token( Tok_Invalid, str, 0 );
    }else {
        const char* v = (const char*)file->buf_ptr;
        file->buf_ptr += pos;
        return token( tt, v, pos );
    }

    return t;
}

static inline int FIRST_Oberon0(int tt) {
    return tt == Tok_MODULE;
}

static inline int FIRST_selector(int tt) {
    return tt == Tok_Dot || tt == Tok_Lbrack;
}

static inline int FIRST_factor(int tt) {
    return tt == Tok_Tilde || tt == Tok_integer || tt == Tok_Lpar || tt == Tok_ident;
}

static inline int FIRST_term(int tt) {
    return tt == Tok_Tilde || tt == Tok_integer || tt == Tok_Lpar || tt == Tok_ident;
}

static inline int FIRST_SimpleExpression(int tt) {
    switch(tt){
    case Tok_Minus:
    case Tok_Tilde:
    case Tok_integer:
    case Tok_Lpar:
    case Tok_ident:
    case Tok_Plus:
        return 1;
    default: return 0;
    }
}

static inline int FIRST_expression(int tt) {
    switch(tt){
    case Tok_Minus:
    case Tok_Tilde:
    case Tok_integer:
    case Tok_Lpar:
    case Tok_ident:
    case Tok_Plus:
        return 1;
    default: return 0;
    }
}

static inline int FIRST_assignment(int tt) {
    return tt == Tok_ident;
}

static inline int FIRST_ActualParameters(int tt) {
    return tt == Tok_Lpar;
}

static inline int FIRST_ProcedureCall(int tt) {
    return tt == Tok_ident;
}

static inline int FIRST_IfStatement(int tt) {
    return tt == Tok_IF;
}

static inline int FIRST_WhileStatement(int tt) {
    return tt == Tok_WHILE;
}

static inline int FIRST_statement(int tt) {
    return tt == Tok_IF || tt == Tok_WHILE || tt == Tok_ident;
}

static inline int FIRST_StatementSequence(int tt) {
    return tt == Tok_Semi || tt == Tok_IF || tt == Tok_WHILE || tt == Tok_ident;
}

static inline int FIRST_IdentList(int tt) {
    return tt == Tok_ident;
}

static inline int FIRST_ArrayType(int tt) {
    return tt == Tok_ARRAY;
}

static inline int FIRST_FieldList(int tt) {
    return tt == Tok_ident;
}

static inline int FIRST_RecordType(int tt) {
    return tt == Tok_RECORD;
}

static inline int FIRST_type(int tt) {
    return tt == Tok_RECORD || tt == Tok_ident || tt == Tok_ARRAY;
}

static inline int FIRST_FPSection(int tt) {
    return tt == Tok_VAR || tt == Tok_ident;
}

static inline int FIRST_FormalParameters(int tt) {
    return tt == Tok_Lpar;
}

static inline int FIRST_ProcedureHeading(int tt) {
    return tt == Tok_PROCEDURE;
}

static inline int FIRST_ProcedureBody(int tt) {
    switch(tt){
    case Tok_PROCEDURE:
    case Tok_TYPE:
    case Tok_VAR:
    case Tok_END:
    case Tok_BEGIN:
    case Tok_CONST:
        return 1;
    default: return 0;
    }
}

static inline int FIRST_ProcedureDeclaration(int tt) {
    return tt == Tok_PROCEDURE;
}

static inline int FIRST_declarations(int tt) {
    return tt == Tok_PROCEDURE || tt == Tok_TYPE || tt == Tok_VAR || tt == Tok_CONST;
}

static inline int FIRST_module(int tt) {
    return tt == Tok_MODULE;
}

static Token cur;
static Token la1, la2;

void next() {
    cur = la1;
    la1 = la2;
    if( la1.type < 0 )
        la1 = lexNextToken();
    la2 = lexNextToken();
    if( la1.type == Tok_Invalid )
        tcc_error(la1.val);
}

static void invalid(const char* what) {
    // errors << Error(QString("invalid %1").arg(what),la.d_lineNr, la.d_colNr, la.d_sourcePath);
    tcc_error("invalid %s", what);
}

static int expect2(int tt, int pkw, const char* where) {
    if( la1.type == tt) {
        next();
        return 1;
    }
    else {
        tcc_error("'%s' expected in %s", tokenTypeString(tt), where);
        return 0;
    }
       //errors << Error(QString("'%1' expected in %2").arg().arg(where),la.d_lineNr, la.d_colNr, la.d_sourcePath); return 0; }
}

static inline void dummy() {}

static void declarations();
static void expression();
static void StatementSequence();
static void type();

static void selector() {
    while( la1.type == Tok_Dot || la1.type == Tok_Lbrack ) {
        if( la1.type == Tok_Dot ) {
            expect2(Tok_Dot, 0, "selector");
            expect2(Tok_ident, 0, "selector");
        } else if( la1.type == Tok_Lbrack ) {
            expect2(Tok_Lbrack, 0, "selector");
            expression();
            expect2(Tok_Rbrack, 0, "selector");
        } else
            invalid("selector");
    }
}

static void factor() {
    if( la1.type == Tok_ident ) {
        expect2(Tok_ident, 0, "factor");
        selector();
    } else if( la1.type == Tok_integer ) {
        expect2(Tok_integer, 0, "factor");
    } else if( la1.type == Tok_Lpar ) {
        expect2(Tok_Lpar, 0, "factor");
        expression();
        expect2(Tok_Rpar, 0, "factor");
    } else if( la1.type == Tok_Tilde ) {
        expect2(Tok_Tilde, 0, "factor");
        factor();
    } else
        invalid("factor");
}

static void term() {
    factor();
    while( la1.type == Tok_Star || la1.type == Tok_DIV || la1.type == Tok_MOD || la1.type == Tok_Amp ) {
        if( la1.type == Tok_Star ) {
            expect2(Tok_Star, 0, "term");
        } else if( la1.type == Tok_DIV ) {
            expect2(Tok_DIV, 1, "term");
        } else if( la1.type == Tok_MOD ) {
            expect2(Tok_MOD, 1, "term");
        } else if( la1.type == Tok_Amp ) {
            expect2(Tok_Amp, 0, "term");
        } else
            invalid("term");
        factor();
    }
}

static void SimpleExpression() {
    if( la1.type == Tok_Plus || la1.type == Tok_Minus ) {
        if( la1.type == Tok_Plus ) {
            expect2(Tok_Plus, 0, "SimpleExpression");
        } else if( la1.type == Tok_Minus ) {
            expect2(Tok_Minus, 0, "SimpleExpression");
        } else
            invalid("SimpleExpression");
    }
    term();
    while( la1.type == Tok_Plus || la1.type == Tok_Minus || la1.type == Tok_OR ) {
        if( la1.type == Tok_Plus ) {
            expect2(Tok_Plus, 0, "SimpleExpression");
        } else if( la1.type == Tok_Minus ) {
            expect2(Tok_Minus, 0, "SimpleExpression");
        } else if( la1.type == Tok_OR ) {
            expect2(Tok_OR, 1, "SimpleExpression");
        } else
            invalid("SimpleExpression");
        term();
    }
}

static void expression() {
    SimpleExpression();
    if( la1.type == Tok_Eq || la1.type == Tok_Hash || la1.type == Tok_Lt || la1.type == Tok_Leq || la1.type == Tok_Gt || la1.type == Tok_Geq ) {
        if( la1.type == Tok_Eq ) {
            expect2(Tok_Eq, 0, "expression");
        } else if( la1.type == Tok_Hash ) {
            expect2(Tok_Hash, 0, "expression");
        } else if( la1.type == Tok_Lt ) {
            expect2(Tok_Lt, 0, "expression");
        } else if( la1.type == Tok_Leq ) {
            expect2(Tok_Leq, 0, "expression");
        } else if( la1.type == Tok_Gt ) {
            expect2(Tok_Gt, 0, "expression");
        } else if( la1.type == Tok_Geq ) {
            expect2(Tok_Geq, 0, "expression");
        } else
            invalid("expression");
        SimpleExpression();
    }
}

static void assignment() {
    expect2(Tok_ident, 0, "assignment");
    selector();
    expect2(Tok_ColonEq, 0, "assignment");
    expression();
}

static void ActualParameters() {
    expect2(Tok_Lpar, 0, "ActualParameters");
    if( FIRST_expression(la1.type) ) {
        expression();
        while( la1.type == Tok_Comma ) {
            expect2(Tok_Comma, 0, "ActualParameters");
            expression();
        }
    }
    expect2(Tok_Rpar, 0, "ActualParameters");
}

static void ProcedureCall() {
    expect2(Tok_ident, 0, "ProcedureCall");
    if( FIRST_ActualParameters(la1.type) ) {
        ActualParameters();
    }
}

static void IfStatement() {
    expect2(Tok_IF, 1, "IfStatement");
    expression();
    expect2(Tok_THEN, 1, "IfStatement");
    StatementSequence();
    while( la1.type == Tok_ELSIF ) {
        expect2(Tok_ELSIF, 1, "IfStatement");
        expression();
        expect2(Tok_THEN, 1, "IfStatement");
        StatementSequence();
    }
    if( la1.type == Tok_ELSE ) {
        expect2(Tok_ELSE, 1, "IfStatement");
        StatementSequence();
    }
    expect2(Tok_END, 1, "IfStatement");
}

static void WhileStatement() {
    expect2(Tok_WHILE, 1, "WhileStatement");
    expression();
    expect2(Tok_DO, 1, "WhileStatement");
    StatementSequence();
    expect2(Tok_END, 1, "WhileStatement");
}

static void statement() {
    if( ( la1.type == Tok_ident &&
          ( la2.type == Tok_ColonEq || la2.type == Tok_Dot || la2.type == Tok_Lbrack ) )  ||
            FIRST_ProcedureCall(la1.type) || FIRST_IfStatement(la1.type) || FIRST_WhileStatement(la1.type) ) {
        if( ( la1.type == Tok_ident &&
              ( la2.type == Tok_ColonEq || la2.type == Tok_Dot || la2.type == Tok_Lbrack ) )  ) {
            assignment();
        } else if( FIRST_ProcedureCall(la1.type) ) {
            ProcedureCall();
        } else if( FIRST_IfStatement(la1.type) ) {
            IfStatement();
        } else if( FIRST_WhileStatement(la1.type) ) {
            WhileStatement();
        } else
            invalid("statement");
    }
}

static void StatementSequence() {
    statement();
    while( la1.type == Tok_Semi ) {
        expect2(Tok_Semi, 0, "StatementSequence");
        statement();
    }
}

static void IdentList() {
    expect2(Tok_ident, 0, "IdentList");
    while( la1.type == Tok_Comma ) {
        expect2(Tok_Comma, 0, "IdentList");
        expect2(Tok_ident, 0, "IdentList");
    }
}

static void ArrayType() {
    expect2(Tok_ARRAY, 1, "ArrayType");
    expression();
    expect2(Tok_OF, 1, "ArrayType");
    type();
}

static void FieldList() {
    if( FIRST_IdentList(la1.type) ) {
        IdentList();
        expect2(Tok_Colon, 0, "FieldList");
        type();
    }
}

static void RecordType() {
    expect2(Tok_RECORD, 1, "RecordType");
    FieldList();
    while( la1.type == Tok_Semi ) {
        expect2(Tok_Semi, 0, "RecordType");
        FieldList();
    }
    expect2(Tok_END, 1, "RecordType");
}

static void type() {
    if( la1.type == Tok_ident ) {
        expect2(Tok_ident, 0, "type");
    } else if( FIRST_ArrayType(la1.type) ) {
        ArrayType();
    } else if( FIRST_RecordType(la1.type) ) {
        RecordType();
    } else
        invalid("type");
}

static void FPSection() {
    if( la1.type == Tok_VAR ) {
        expect2(Tok_VAR, 1, "FPSection");
    }
    IdentList();
    expect2(Tok_Colon, 0, "FPSection");
    type();
}

static void FormalParameters() {
    expect2(Tok_Lpar, 0, "FormalParameters");
    if( FIRST_FPSection(la1.type) ) {
        FPSection();
        while( la1.type == Tok_Semi ) {
            expect2(Tok_Semi, 0, "FormalParameters");
            FPSection();
        }
    }
    expect2(Tok_Rpar, 0, "FormalParameters");
}

static void ProcedureHeading() {
    expect2(Tok_PROCEDURE, 1, "ProcedureHeading");
    expect2(Tok_ident, 0, "ProcedureHeading");
    if( FIRST_FormalParameters(la1.type) ) {
        FormalParameters();
    }
}

static void ProcedureBody() {
    declarations();
    if( la1.type == Tok_BEGIN ) {
        expect2(Tok_BEGIN, 1, "ProcedureBody");
        StatementSequence();
    }
    expect2(Tok_END, 1, "ProcedureBody");
}

static void ProcedureDeclaration() {
    ProcedureHeading();
    expect2(Tok_Semi, 0, "ProcedureDeclaration");
    ProcedureBody();
    expect2(Tok_ident, 0, "ProcedureDeclaration");
}

static void declarations() {
    if( la1.type == Tok_CONST ) {
        expect2(Tok_CONST, 1, "declarations");
        while( la1.type == Tok_ident ) {
            expect2(Tok_ident, 0, "declarations");
            expect2(Tok_Eq, 0, "declarations");
            expression();
            expect2(Tok_Semi, 0, "declarations");
        }
    }
    if( la1.type == Tok_TYPE ) {
        expect2(Tok_TYPE, 1, "declarations");
        while( la1.type == Tok_ident ) {
            expect2(Tok_ident, 0, "declarations");
            expect2(Tok_Eq, 0, "declarations");
            type();
            expect2(Tok_Semi, 0, "declarations");
        }
    }
    if( la1.type == Tok_VAR ) {
        expect2(Tok_VAR, 1, "declarations");
        while( FIRST_IdentList(la1.type) ) {
            IdentList();
            expect2(Tok_Colon, 0, "declarations");
            type();
            expect2(Tok_Semi, 0, "declarations");
        }
    }
    while( FIRST_ProcedureDeclaration(la1.type) ) {
        ProcedureDeclaration();
        expect2(Tok_Semi, 0, "declarations");
    }
}

static void module() {
    expect2(Tok_MODULE, 1, "module");
    expect2(Tok_ident, 0, "module");
    expect2(Tok_Semi, 0, "module");
    declarations();
    if( la1.type == Tok_BEGIN ) {
        expect2(Tok_BEGIN, 1, "module");
        StatementSequence();
    }
    expect2(Tok_END, 1, "module");
    expect2(Tok_ident, 0, "module");
    expect2(Tok_Dot, 0, "module");
}

static void Oberon0() {
    module();
}

void parser_init()
{
    la1.type = -1;
    la2.type = -1;
    symbols_init();
}

void parser_free()
{
    symbols_free();
}

static int tccgen_compile(TCCState *s1)
{
    tccgen_init_compiler();

    tcc_debug_start(s1);

#ifdef TCC_TARGET_ARM
    arm_init(s1);
#endif

    next();
#if 0
    while(la1.type != Tok_Eof )
    {
        const char* str = tokenTypeString(la1.type);
        if( la1.val )
            printf("%s %d '%.*s'\n", str, la1.line, la1.len, la1.val );
        else
            printf("%s %d\n", str, la1.line );
        fflush(stdout);
        next();
    }
#else
    Oberon0();
#endif
    check_vstack();
    /* end of translation unit info */
    tcc_debug_end(s1);
    return 0;
}

/* compile the file opened in 'file'. Return non zero if errors. */
static int tcc_compile(TCCState *s1, const char* filename, int flags)
{
    int ret;

    /* open the file */
    ret = tcc_open(s1, filename);
    if (ret < 0) {
        if (flags & AFF_PRINT_ERROR)
            tcc_error_noabort("file '%s' not found", filename);
        return ret;
    }

    tccelf_begin_file(s1);

    if (setjmp(s1->error_jmp_buf) == 0) {
        s1->nb_errors = 0;
        s1->error_set_jmp_enabled = 1;

        pvtop = vtop = vstack - 1; // instead preprocess_start
        if (s1->output_type == TCC_OUTPUT_PREPROCESS) {
            tcc_error_noabort("preprocess not supported");
        } else {
            tccgen_compile(s1);
        }
    }
    s1->error_set_jmp_enabled = 0;

    free_inline_functions(s1);
    sym_pop(&global_stack, NULL, 0);
    sym_pop(&local_stack, NULL, 0);
    tccelf_end_file(s1);
    tcc_close();
    return s1->nb_errors != 0 ? -1 : 0;
}

int add_file(TCCState* s, const char* filename)
{
    int filetype = s->filetype;
    int flags = AFF_PRINT_ERROR;
    if (filetype == 0) {
        /* use a file extension to detect a filetype */
        const char *ext = tcc_fileextension(filename);
        if (ext[0]) {
            ext++;
            if (!strcmp(ext, "S") || !strcmp(ext, "s") ) {
                tcc_error_noabort("file type '%s' not supported", filename);
                return -1;
            } else if (!PATHCMP(ext, "Mod") || !PATHCMP(ext, "mod") || !PATHCMP(ext, "ob0") )
                filetype = AFF_TYPE_C;
            else
                flags |= AFF_TYPE_BIN;
        } else {
            filetype = AFF_TYPE_C;
        }
        s->filetype = filetype;
    }
    if (flags & AFF_TYPE_BIN)
        return tcc_add_binary_file(s, filename, flags);
    else
        return tcc_compile(s, filename, flags);
}
