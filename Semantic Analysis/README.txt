To use bison and Lex files use the following commands :
bison -d parser.y
flex lex.l
gcc parser.tab.c lex.yy.c -lfl -L/usr/bin/ld-ly (To locate the package)
./a.out < input.txt