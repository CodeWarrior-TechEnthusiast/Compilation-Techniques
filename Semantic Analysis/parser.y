%{
#include <stdio.h>
#include "parser.tab.h"
int yylex(void);
int yyerror(char *);
%}
%token ID INTEGER_CONSTANT FLOAT_CONSTANT INT FLOAT SEMICOLON COMMA ASSIGN IF ELSE AND OR NOT EQ GE LE LT GT NE WHILE RETURN CHAR PLUS MINUS MULTIPLY DIVISION
%%
prog : funDef {printf("Accepted\n");};
funDef : type ID'(' argList ')''{' declList stmtList '}';
argList : Newarg|;
Newarg : arg COMMA Newarg|arg;
arg : type ID;
declList : |decl SEMICOLON declList;
decl : type varList;
varList : ID COMMA varList|ID;
type : INT|FLOAT|CHAR;
stmtList : stmtList stmt | stmt;
stmt : assignStmt | ifStmt | elseStmt | whileStmt;
assignStmt : ID ASSIGN EXP SEMICOLON;
EXP : EXP PLUS TERM|EXP MINUS TERM|TERM;
TERM : TERM MULTIPLY ID|TERM DIVISION ID|ID;
ifStmt : IF'('bExp')''{'stmtList'}';
elseStmt : ELSE '{'stmtList'}';
whileStmt : WHILE'('bExp')''{'stmtList'}';
bExp : EXP RELOP EXP;
RELOP : EQ|GE|LE|LT|GT|NE;
%%
int main(int argc, char **argv){
    yyparse();
}
int yyerror(char *s)
{
    fprintf(stderr, "%s\n",s);
}
