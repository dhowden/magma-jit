/* Copyright 2015, David Howden
   Use of this source code is governed by a BSD-style
   license that can be found in the LICENSE file.
*/

%{
   #include "AST.hpp"
   #include <cstdio>
   #include <cstdlib>
   BlockAST *programBlock; /* the top level root node of our final AST */

   extern int yylex();
   extern char *yytext;
   extern char linebuf[500];
   extern int yylineno;
   void yyerror(char *s, ...);

%}

%code requires {
   extern std::string *filename; /* current filename for lexer */

   # define YYLLOC_DEFAULT(Current, Rhs, N)                                \
       do                                                                  \
         if (N)                                                            \
           {                                                               \
             (Current).first_line   = YYRHSLOC (Rhs, 1).first_line;        \
             (Current).first_column = YYRHSLOC (Rhs, 1).first_column;      \
             (Current).last_line    = YYRHSLOC (Rhs, N).last_line;         \
             (Current).last_column  = YYRHSLOC (Rhs, N).last_column;       \
             (Current).filename     = YYRHSLOC (Rhs, N).filename;          \
           }                                                               \
         else                                                              \
           {                                                               \
             (Current).first_line   = (Current).last_line   =              \
               YYRHSLOC (Rhs, 0).last_line;                                \
             (Current).first_column = (Current).last_column =              \
               YYRHSLOC (Rhs, 0).last_column;                              \
             (Current).filename = YYRHSLOC (Rhs, 0).filename;              \
           }                                                               \
       while (0)
}

%union {
   BlockAST *block;
   TypeAST *type;
   ExpressionAST *expr;
   StringExpressionAST *str_expr;
   StatementAST *stmt;
   SequenceAST *seq;
   TupleAST *tup;
   IdentifierAST *ident;
   IfStatementAST *if_stmt;
   WhileStatementAST *while_stmt;
   RepeatStatementAST *repeat_stmt;
   FunctionDeclarationStatementAST *func_decl_stmt;
   ReturnStatementAST *ret_stmt;
   IterativeStatementsAST *iterative_stmts;
   IterativeStatementAST *iterative_stmt;
   EvaluatedSequenceAST *eval_seq;
   BreakStatementAST *break_stmt;
   IdentifierList *idents;
   ExpressionList *exprs;
   TypeList *types;
   std::string *string;
   int token;
}

%locations

/* terminal symbols */
%token <string> TIDENTIFIER TTILDE TINTEGER TSTRING
%token <token> TTRUE TFALSE

/* statement keywords */
%token <token> TTIME TPRINT TPRINTF TASSERT TBREAK
%token <token> TFUNCTION TPROCEDURE TRETURN TWHILE TDO TREPEAT TUNTIL TIF TTHEN
%token <token> TELSE TEND TFOR TBY TTO TIN TAEQ

/* punctuation */
%token <token> TCOMMA TSEMICOLON TCOLON TDOTS TDOUBLECOLON

/* comparison operators */
%token <token> TCEQ TCNE TCLT TCLE TCGT TCGE

/* brackets */
%token <token> TLPAREN TRPAREN TLBRACK TRBRACK THASH TLARROW TRARROW

/* boolean operators */
%token <token> TAND TOR TNOT TXOR

/* operators */
%token <token> TPLUS TMINUS TMUL TDIV TMOD THAT

/* assignment logical operators */
%token <token> TAAND_EQ TAXOR_EQ TAOR_EQ

/* assignment operators */
%token <token> TAPLUS_EQ TAMINUS_EQ TAMUL_EQ TADIV_EQ TAMOD_EQ TAHAT_EQ

/* type definitions */
%type <ident> ident ident_type
%type <expr> numeric boolean expr rec_prev_expr
%type <str_expr> str_expr
%type <block> program stmts
%type <stmt> stmt
%type <if_stmt> if_stmt
%type <while_stmt> while_stmt
%type <repeat_stmt> repeat_stmt
%type <iterative_stmt> iterative_stmt
%type <iterative_stmts> iterative_stmts for_stmt
%type <func_decl_stmt> func_decl_stmt proc_decl_stmt
%type <idents> idents_types func_decl_stmt_args_types
%type <type> type
%type <types> extended_type_list
%type <ret_stmt> ret_stmt
%type <exprs> exprs
%type <break_stmt> break_stmt
%type <eval_seq> eval_seq
%type <seq> seq
%type <tup> tup

/* set operator precedence */
%nonassoc UIDENT
%nonassoc TAAND_EQ TAXOR_EQ TAOR_EQ
%nonassoc TAPLUS_EQ TAMINUS_EQ TAMUL_EQ TADIV_EQ TAMOD_EQ TAHAT_EQ
%nonassoc TCLT TCLE TCGT TCGE TCEQ TCNE
%left TXOR
%left TOR
%left TAND
%nonassoc TNOT THASH
%left TPLUS TMINUS
%nonassoc TMOD
%left TMUL TDIV
%right THAT
%nonassoc UMINUS
%left TLPAREN TRPAREN TLBRACK TRBRACK


%start program

%%

program : stmts { programBlock = $1; }
   ;

stmts : stmt TSEMICOLON { $$ = new BlockAST(); $$->addStatement($<stmt>1); }
   | stmts stmt TSEMICOLON { $1->addStatement($<stmt>2); }
   ;

stmt : expr { $$ = $1; }
   | ident TAEQ expr { $$ = new AssignmentStatementAST($1, $3); ASTLOC($$); }
   | ident TAPLUS_EQ expr { $$ = new AssignmentStatementAST($1, new BinaryExpressionAST(TPLUS, $1, $3)); }
   | ident TAMINUS_EQ expr { $$ = new AssignmentStatementAST($1, new BinaryExpressionAST(TMINUS, $1, $3)); }
   | ident TAMUL_EQ expr { $$ = new AssignmentStatementAST($1, new BinaryExpressionAST(TMUL, $1, $3)); }
   | ident TADIV_EQ expr { $$ = new AssignmentStatementAST($1, new BinaryExpressionAST(TDIV, $1, $3)); }
   | ident TAMOD_EQ expr { $$ = new AssignmentStatementAST($1, new BinaryExpressionAST(TMOD, $1, $3)); }
   | ident TAHAT_EQ expr { $$ = new AssignmentStatementAST($1, new BinaryExpressionAST(THAT, $1, $3)); }
   | ident TAAND_EQ expr { $$ = new AssignmentStatementAST($1, new BinaryExpressionAST(TAND, $1, $3)); }
   | ident TAXOR_EQ expr { $$ = new AssignmentStatementAST($1, new BinaryExpressionAST(TXOR, $1, $3)); }
   | ident TAOR_EQ expr { $$ = new AssignmentStatementAST($1, new BinaryExpressionAST(TOR, $1, $3)); }
   | ident TLBRACK expr TRBRACK TAEQ expr { $$ = new AssignmentStatementAST($1, $6, $3); ASTLOC($$); }
   | TPRINT exprs { $$ = new PrintStatementAST($2); ASTLOC($$); }
   | TPRINTF str_expr { $$ = new PrintfStatementAST($2, new ExpressionList()); }
   | TPRINTF str_expr TCOMMA exprs { $$ = new PrintfStatementAST($2, $4); }
   | TTIME stmt { $$ = new TimeStatementAST($2); ASTLOC($$); }
   | TASSERT expr { $$ = new AssertStatementAST($2, yylineno); }
   | if_stmt { $$ = $1; }
   | while_stmt { $$ = $1; }
   | repeat_stmt { $$ = $1; }
   | for_stmt { $$ = $1; }
   | func_decl_stmt { $$ = $1; }
   | proc_decl_stmt { $$ = $1; }
   | ret_stmt { $$ = $1; }
   | break_stmt { $$ = $1; }
   ;

if_stmt : TIF expr rec_prev_expr TTHEN stmts TEND TIF { $$ = new IfStatementAST($2, $5, NULL); }
   | TIF expr rec_prev_expr TTHEN stmts TELSE stmts TEND TIF { $$ = new IfStatementAST($2, $5, $7); }
   ;

rec_prev_expr : /* empty */ { ASTLOC($<expr>0); }
   ;

while_stmt : TWHILE expr TDO stmts TEND TWHILE { $$ = new WhileStatementAST($2, $4); }
   ;

repeat_stmt : TREPEAT stmts TUNTIL expr { $$ = new RepeatStatementAST($2, $4); }
   ;

for_stmt : TFOR iterative_stmts TDO stmts TEND TFOR { $$ = $2; $$->setBody($4); }
   ;

iterative_stmts : iterative_stmt { $$ = new IterativeStatementsAST(); $$->addIterativeStatement($<iterative_stmt>1); }
   | iterative_stmts TCOMMA iterative_stmt { $1->addIterativeStatement($<iterative_stmt>3); }
   ;

iterative_stmt : ident TAEQ expr TTO expr { $$ = new IterativeStatementAST($1, $3, $5, new IntegerAST(1)); }
   | ident TIN ident { $$ = new IterativeStatementAST($1, $3); }
   | ident TIN TLBRACK expr TDOTS expr TRBRACK  { $$ = new IterativeStatementAST($1, $4, $6, new IntegerAST(1)); }
   | ident TAEQ expr TTO expr TBY expr { $$ = new IterativeStatementAST($1, $3, $5, $7); }
   | ident TIN TLBRACK expr TDOTS expr TBY expr TRBRACK { $$ = new IterativeStatementAST($1, $4, $6, $8); }
   ;

break_stmt : TBREAK { $$ = new BreakStatementAST(); ASTLOC($$); }
   | TBREAK ident { $$ = new BreakStatementAST($2); ASTLOC($$); }
   ;

func_decl_stmt : TFUNCTION ident_type TLPAREN func_decl_stmt_args_types TRPAREN stmts TEND TFUNCTION
      { $$ = new FunctionDeclarationStatementAST($2, $4, $6, false); ASTLOC($$); }
   ;

func_decl_stmt_args_types : /* empty */ { $$ = new IdentifierList(); }
   | idents_types
   ;

proc_decl_stmt : TPROCEDURE ident TLPAREN func_decl_stmt_args_types TRPAREN stmts TEND TPROCEDURE
      {$$ = new FunctionDeclarationStatementAST($2, $4, $6, true); ASTLOC($$); }
   ;

ident : TIDENTIFIER { $$ = new IdentifierAST(*$1); ASTLOC($$); }
   | TTILDE TIDENTIFIER { $$ = new IdentifierAST(*$2, true); ASTLOC($$); }
   ;

/* identifiers with type declarations */
idents_types : ident_type { $$ = new IdentifierList(); $$->push_back($1); }
   | idents_types TCOMMA ident_type { $1->push_back($3); }

ident_type : ident TDOUBLECOLON type { $$ = $1; $$->Type = $3; }
   | ident { $$ = $1; $$->Type = new TypeAST("RngIntElt"); }
   ;

type : TIDENTIFIER { $$ = new TypeAST(*$1); ASTLOC($$); } %prec UIDENT
   | TIDENTIFIER TLBRACK extended_type_list TRBRACK { $$ = new TypeAST(*$1, $3); ASTLOC($$); }
   ;

extended_type_list : type { $$ = new TypeList(); $$->push_back($1); }
   | extended_type_list TCOMMA type { $1->push_back($3); }

ret_stmt : TRETURN { $$ = new ReturnStatementAST(); ASTLOC($$); }
   | TRETURN expr { $$ = new ReturnStatementAST($2); ASTLOC($$); }
   ;

exprs : /* empty */ { $$ = new ExpressionList(); }
   | expr { $$ = new ExpressionList(); $$->push_back($1); }
   | exprs TCOMMA expr { $1->push_back($3); }
   ;

expr : TLPAREN expr TRPAREN { $$ = $2; }
   | numeric { $$ = $1; }
   | TMINUS numeric { $$ = new BinaryExpressionAST($1, new IntegerAST(0), $2); ASTLOC($$); } %prec UMINUS
   | boolean { $$ = $1; }
   | str_expr { $$ = $1; }
   | ident { $$ = $1; } %prec UIDENT
   | ident TLPAREN exprs TRPAREN { $$ = new CallExpressionAST($1, $3); }
   | expr TLBRACK expr TRBRACK { $$ = new IndexLookupAST($1, $3); }
   | expr TOR expr { $$ = new BinaryExpressionAST($2, $1, $3); ASTLOC($$); }
   | expr TAND expr { $$ = new BinaryExpressionAST($2, $1, $3); ASTLOC($$); }
   | expr TXOR expr { $$ = new BinaryExpressionAST($2, $1, $3); ASTLOC($$); }
   | expr TCEQ expr { $$ = new BinaryExpressionAST($2, $1, $3); ASTLOC($$); }
   | expr TCNE expr { $$ = new BinaryExpressionAST($2, $1, $3); ASTLOC($$); }
   | expr TCLT expr { $$ = new BinaryExpressionAST($2, $1, $3); ASTLOC($$); }
   | expr TCLE expr { $$ = new BinaryExpressionAST($2, $1, $3); ASTLOC($$); }
   | expr TCGT expr { $$ = new BinaryExpressionAST($2, $1, $3); ASTLOC($$); }
   | expr TCGE expr { $$ = new BinaryExpressionAST($2, $1, $3); ASTLOC($$); }
   | expr TPLUS expr { $$ = new BinaryExpressionAST($2, $1, $3); ASTLOC($$); }
   | expr TMINUS expr { $$ = new BinaryExpressionAST($2, $1, $3); ASTLOC($$); }
   | expr TMUL expr { $$ = new BinaryExpressionAST($2, $1, $3); ASTLOC($$); }
   | expr TDIV expr { $$ = new BinaryExpressionAST($2, $1, $3); ASTLOC($$); }
   | expr TMOD expr { $$ = new BinaryExpressionAST($2, $1, $3); ASTLOC($$); }
   | expr THAT expr { $$ = new BinaryExpressionAST($2, $1, $3); ASTLOC($$); }
   | TNOT expr { $$ = new BinaryExpressionAST($1, NULL, $2); ASTLOC($$); }
   | THASH expr { $$ = new BinaryExpressionAST($1, NULL, $2); ASTLOC($$); }
   | eval_seq { $$ = $1; }
   | seq { $$ = $1; }
   | tup { $$ = $1; }
   ;

str_expr : TSTRING { $$ = new StringExpressionAST(*$1); ASTLOC($$); }
   ;

eval_seq : TLBRACK expr TCOLON iterative_stmts TRBRACK { $$ = new EvaluatedSequenceAST($2, $4, NULL); }
   ;

seq : TLBRACK exprs TRBRACK { $$ = new SequenceAST($2); }
   ;

tup : TLARROW exprs TRARROW { $$ = new TupleAST($2); }
   ;

numeric : TINTEGER { $$ = new IntegerAST(atol($1->c_str())); delete $1; ASTLOC($$); }
   ;

boolean : TTRUE { $$ = new BooleanAST(true); ASTLOC($$); }
   | TFALSE { $$ = new BooleanAST(false); ASTLOC($$); }
   ;

%%

void yyerror(char *s, ...)
{
   fprintf(stderr, "%c[1m", ESC_CHAR);
   fprintf(stderr, "%s:%d:%d: ", yylloc.filename->c_str(), yylloc.first_line, yylloc.first_column);
   fprintf(stderr, "%c[1;31m", ESC_CHAR);
   fprintf(stderr, "error:");
   fprintf(stderr, "%c[0m%c[1m", ESC_CHAR, ESC_CHAR);
   fprintf(stderr, " %s at '%s'", s, yytext);
   fprintf(stderr, "%c[0m", ESC_CHAR);
   fprintf(stderr, "\n%s\n", linebuf);
   unsigned int i = 1;
   while (i < yylloc.first_column) {
      fprintf(stderr, " "); ++i;
   }
   fprintf(stderr, "^\n");
}
