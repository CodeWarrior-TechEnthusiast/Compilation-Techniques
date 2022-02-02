%{
    #include <string.h>
    #define MAX 100
    char symbol_table[MAX][MAX];
    int count = 0;
    void Symbol_Table(char value[]);
    void print_symbol_table();
    char Tokens[MAX][MAX];
    char Token_type[MAX][MAX];
    int token_count = 0;
    void All_Tokens(char token[],char tokentype[]);
    void print_tokens();
    int final_symbol_count = 0;
    char Symbol[MAX][MAX];
    char Final_Symbol[MAX][MAX];
    char files[MAX][MAX];
    FILE *file;
%}
numeric [0-9]
alphabet [a-zA-Z]
alphanumeric {numeric}|{alphabet}
underscore [_]
underscore_alpha {underscore}|{alphabet}
underscore_alpha_numeric {underscore}|{alphabet}|{numeric}
numerics [0-9]+
hash [#]
%s blockcomment
block1 [^*/]
star [*]
slash [/]
%%
"/*"  { printf("Comment encountered\n");
        BEGIN(blockcomment);}
"*/"  { printf("\n");
        BEGIN(INITIAL);}
[ \t]+ {}
<blockcomment>{block1}* {printf("%s",yytext);}
<blockcomment>{star}* {printf("%s",yytext);}
<blockcomment>{slash}* {printf("%s",yytext);}
"short"|"float"|"double"|"bool"|"char"|"signed"|"unsigned"|"for"|"while"|"do"|"return"|"void"|"switch"|"break"|"case"|"continue"|"goto"|"long"|"static"|"union"|"default"|"int"|"string"  {All_Tokens(yytext,"keywords");}
{numeric}+ {All_Tokens(yytext,"Integer Constant");}
{numeric}+"."{numeric}+ {All_Tokens(yytext,"Floating point Constant");}
"*"|"+"|"-"|"="|"+ ="|"− ="|"∗ ="|"/ =" {All_Tokens(yytext,"Arithmetic Operators");}
"="|"+="|"−="|"∗="|"/=" {All_Tokens(yytext,"Arithmetic Operators");}
"<"|">"|"<="|">="|"==" {All_Tokens(yytext,"Relational Operators");}
";"|"("|")"|"comma"|"["|"]"|"{"|"}" {All_Tokens(yytext,"Special symbols");}
{underscore_alpha}{underscore_alpha_numeric}* {All_Tokens(yytext,"Identifiers");
                                                Symbol_Table(yytext);}
{numerics}{underscore_alpha}+                     {All_Tokens(yytext,"Invalid Variable Definition");}
{hash}{alphabet}+[<]{alphabet}+[.][h][>]    {All_Tokens(yytext,"Header File");}
[\r\n] ;
[,] ;
%%
void All_Tokens(char token[],char tokentype[]){
    strcpy(Tokens[token_count],token);
    strcpy(Token_type[token_count],tokentype);
    token_count++;
}
void print_tokens(){
    int i,j;
    printf("\n");
    printf("printing Tokens\n");
    printf("\n");
    for(i = 0;i<token_count;i++){
      int number = 0;
      for(j = 0;j<i;j++){
        if(strcmp(Tokens[j],Tokens[i]) != 0){
          number++;
        }
      }
      if(number == i){
          printf("%s - %s\n",Tokens[i],Token_type[i]);
      }
    }
}
void print_symbol_table(){
    int i,j,k,p;
    printf("\n");
    printf("printing symbol table\n");
    printf("\n");
    for(i = 0;i<count;i++){
      for(j = 0;j<token_count;j++){
        if(strcmp(Tokens[j],symbol_table[i]) == 0){
          if(strcmp(Tokens[j+1],"(") != 0){
            strcpy(Symbol[final_symbol_count],symbol_table[i]);
            strcpy(Final_Symbol[final_symbol_count],Tokens[j-1]);
            final_symbol_count++;
          }
        }
      }
    }
    i = 0;
    j = 0;
    for(i = 0;i<final_symbol_count;i++){
      int number = 0;
      for(j = 0;j<i;j++){
        if(strcmp(Symbol[i],Symbol[j]) != 0){
          number++;
        }
      }
      if(number == i){
          printf("%s - %s\n",Symbol[i],Final_Symbol[i]);
      }
    }
    printf("\n");
}
void Symbol_Table(char value[]){
    strcpy(symbol_table[count],value);
    count++;
}
int yywrap() {
  return 1;
}
int main(int argc,char **argv){
  strcpy(files[0],"");
  if(argc == 2){
    strcpy(files[0],argv[1]);
    file = fopen(argv[1],"r");
    yyin = file;
  }
  else {
    printf("File not given\n");
    exit(1);
  }
  yylex();
  print_tokens();
  print_symbol_table();
}
