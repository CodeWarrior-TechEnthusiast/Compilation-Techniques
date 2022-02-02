%{

#include "advanced_parser.tab.h"
#include "head.h"
%}

%%

[ \t]+ ;
"beg"    {  return BEG; }
"end"    {  return END; }
"("    {  return yytext[0];}
")"    {  return yytext[0];}
"="    {  return yytext[0]; }
","    {  return COMMA; }
";"    {  return SEMICOLON; }
"=="|">="|"<="|"!="|"<"|">" {printf("Detected %s\n", yytext); return RELOP;}
"||"   {  return OR; }
"&&"   {  return AND; }
"!"    {  return NOT; }
"if"   {  return IF; }
"while"   {  return WHILE; }
"+"|"-"|"/"|"*"    {  return yytext[0];}
"int"  {  strcpy(yylval.sval, "int"); return INT; }
"double"  {  strcpy(yylval.sval, "double"); return DOUBLE; }
[0-9]+     {  yylval.ival = atoi(yytext); return INT_CONST; }
[0-9]+"."[0-9]+     {  char* p; yylval.dval = strtod(yytext,&p); return DOUBLE_CONST; }
[A-Za-z0-9]* { 
               strcpy(yylval.sval, yytext); return ID; }
\n         {  }
.          { return yytext[0];}
%%
