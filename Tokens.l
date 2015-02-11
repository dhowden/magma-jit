/* Copyright 2015, David Howden
   Use of this source code is governed by a BSD-style
   license that can be found in the LICENSE file.
*/

/* magma-jit small lex */
/* tokens.l */
%option yylineno
%{
#include <string>
#include "AST.hpp"
#include "Parser.hpp"

#define SAVE_TOKEN yylval.string = new std::string(yytext, yyleng)
#define TOKEN(t) (yylval.token = t)

int yycolumn = 1;
std::string *filename;

#define YY_USER_ACTION yylloc.filename = filename; \
   yylloc.first_line = yylloc.last_line = yylineno; \
   yylloc.first_column = yycolumn; yylloc.last_column = yycolumn + yyleng-1; \
   yycolumn += yyleng;

extern "C" int yywrap() { return 1; }
char linebuf[500];
%}

%x M_COMMENT
%x S_COMMENT
%%

"/*"                       { BEGIN(M_COMMENT); }
<M_COMMENT>"*/"            { BEGIN(INITIAL); }
<M_COMMENT>.               { }
"//"                       { BEGIN(S_COMMENT); }
<S_COMMENT>"\n"            { BEGIN(INITIAL); yycolumn = 1; }
<S_COMMENT>.               { }
\n.* { strncpy(linebuf, yytext+1, sizeof(linebuf)); /* save the next line */
yyless(1); /* give back all but the \n to rescan */ yycolumn = 1;}
[ \t]                      /* whitespace */
[\n]                       { yycolumn = 1; }
"print"                    return TOKEN(TPRINT);
"printf"                   return TOKEN(TPRINTF);
"time"                     return TOKEN(TTIME);
"assert"                   return TOKEN(TASSERT);
"function"                 return TOKEN(TFUNCTION);
"procedure"                return TOKEN(TPROCEDURE);
"return"                   return TOKEN(TRETURN);
"break"                    return TOKEN(TBREAK);
"while"                    return TOKEN(TWHILE);
"for"                      return TOKEN(TFOR);
"repeat"                   return TOKEN(TREPEAT);
"until"                    return TOKEN(TUNTIL);
"in"                       return TOKEN(TIN);
"to"                       return TOKEN(TTO);
"by"                       return TOKEN(TBY);
"do"                       return TOKEN(TDO);
"if"                       return TOKEN(TIF);
"then"                     return TOKEN(TTHEN);
"else"                     return TOKEN(TELSE);
"end"                      return TOKEN(TEND);
"eq"                       return TOKEN(TCEQ);
"ne"                       return TOKEN(TCNE);
"lt"                       return TOKEN(TCLT);
"le"                       return TOKEN(TCLE);
"gt"                       return TOKEN(TCGT);
"ge"                       return TOKEN(TCGE);
"and"                      return TOKEN(TAND);
"or"                       return TOKEN(TOR);
"xor"                      return TOKEN(TXOR);
"not"                      return TOKEN(TNOT);
"mod"                      return TOKEN(TMOD);
"div"                      return TOKEN(TDIV);
"true"                     return TOKEN(TTRUE);
"false"                    return TOKEN(TFALSE);
[a-zA-Z_][a-zA-Z0-9_]*     SAVE_TOKEN; return TIDENTIFIER;
[0-9]+                     SAVE_TOKEN; return TINTEGER;
":="                       return TOKEN(TAEQ);
"("                        return TOKEN(TLPAREN);
")"                        return TOKEN(TRPAREN);
"["                        return TOKEN(TLBRACK);
"]"                        return TOKEN(TRBRACK);
","                        return TOKEN(TCOMMA);
"+"                        return TOKEN(TPLUS);
"-"                        return TOKEN(TMINUS);
"*"                        return TOKEN(TMUL);
"^"                        return TOKEN(THAT);
"+:="                      return TOKEN(TAPLUS_EQ);
"-:="                      return TOKEN(TAMINUS_EQ);
"*:="                      return TOKEN(TAMUL_EQ);
"^:="                      return TOKEN(TAHAT_EQ);
"mod:="                    return TOKEN(TAMOD_EQ);
"div:="                    return TOKEN(TADIV_EQ);
"and:="                    return TOKEN(TAAND_EQ);
"xor:="                    return TOKEN(TAXOR_EQ);
"or:="                     return TOKEN(TAOR_EQ);
"::"                       return TOKEN(TDOUBLECOLON);
":"                        return TOKEN(TCOLON);
";"                        return TOKEN(TSEMICOLON);
".."                       return TOKEN(TDOTS);
"~"                        return TOKEN(TTILDE);
"#"                        return TOKEN(THASH);
"<"                        return TOKEN(TLARROW);
">"                        return TOKEN(TRARROW);
\"(\\.|[^"])*\"            SAVE_TOKEN; return TSTRING;
.                          { std::cerr << "Lex: Unmatched character " << yytext; /*yyerror("");*/ exit(1); }

%%
