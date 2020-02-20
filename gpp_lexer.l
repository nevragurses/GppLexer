%{ 
int flag = 0; 
%} 
/*These are regular expressions.*/
COMMENT ";;".*
VALUE [1-9][0-9]*|"0"
IDENTIFIER  [a-zA-Z_][a-zA-Z0-9_]*

%%
"and" { printf("KW_AND\n");}
"or" { printf("KW_OR\n");}
"not" { printf("KW_NOT\n");}
"equal" { printf("KW_EQUAL\n");} 
"less" { printf("KW_LESS\n");}
"nil" { printf("KW_NIL\n");}
"list" { printf("KW_LIST\n");}
"append" {printf("KW_APPEND\n");}
"concat" { printf("KW_CONCAT\n");}
"set" { printf("KW_SET\n");}
"deffun" { printf("KW_DEFFUN\n");}
"for" { printf("KW_FOR\n");}
"if" { printf("KW_IF\n");}
"exit" { printf("KW_EXIT\n");}
"load" { printf("KW_LOAD\n");}
"disp" { printf("KW_DISP\n");}
"true" { printf("KW_TRUE\n");}
"false" { printf("KW_FALSE\n");}
"+" { printf("OP_PLUS\n");}
"-" { printf("OP_MINUS\n");}
"/" { printf("OP_DIV\n");}
"*" { printf("OP_MULT\n");}
"(" { printf("OP_OP\n");}
")" { printf("OP_CP\n");} 
"**" { printf("OP_DBLMULT\n");}
" \" " {    
            if(flag==0) //if flag is 0 , operator is OC
            {
                printf("OP_OC\n");
                flag=1;
            }             
            else{  //if flag is 1 operator is CC
                printf("OP_CC\n");
                flag=0;

            }  
        }

"," { printf("OP_COMMA\n");}
{COMMENT} {printf ("COMMENT\n");}
{VALUE} { printf("VALUE\n");}
{IDENTIFIER} {printf("IDENTIFIER\n");}
"\n"|"\t"|" " { }
. { printf ("SYNTAX ERROR %s cannot be tokonized \n",yytext);
        return 0;

  } 
"(exit)" {printf( " PROGRAM IS TERMINATING ...\n"); return 0; } //for terminating program.   
%%

int yywrap(){} 

int main(){ 
    printf ("If you want to load input file,write input file name (with extention g++ or txt or etc=>For example:  inputFile.g++),\n ");
    printf ("Or if you want to write expressions on interpreter write g++ ! \n");    
    FILE *fp; 
    char filename[50];
    char selection[15]; 
    printf("$ ");
    fgets(selection,sizeof selection,stdin); //result of whether you want to load file or not.
    selection[strcspn(selection,"\n")]='\0';
    if (strlen(selection) == 3){      //for interpreter ,to terminate program write (exit) 
        printf("Write expressions on interpreter line by line..After, lexical analysis will done\n\n");   
        printf("$ g++\n"); 
        yyin = stdin; //interpreter is doing lexical analysis.
        yylex();
    }      
    else{   //for reading file.
         printf("You want to use load input file,file is reading and lexical analysis is doing...\n");
      
        printf ("\n$ g++ %s\n",selection); //Interpreter write g++ filename.g++ on screen.
        fp = fopen(selection,"r");  
        yyin = fp; //file is readed and lexical analysis is doing.
        yylex(); 
    }    
    


  
    return 0; 
} 