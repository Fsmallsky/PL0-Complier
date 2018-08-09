#include <stdio.h>

#define NRW        13     // number of reserved words
#define TXMAX      500    // length of identifier table
#define MAXNUMLEN  14     // maximum number of digits in numbers
#define MAXARRDIM  10     // maximum size of array dimension
#define NSYM       16     // maximum number of symbols in array ssym and csym
#define MAXIDLEN   10     // length of identifiers

#define MAXPARA    10     // maximum number of procedure parameters
#define PROMAX     50
#define MAXDIM     8      // max dims of an array
#define MAXCL  5              //用于条件的跳转

#define MAXADDRESS 32767  // maximum address
#define MAXLEVEL   32     // maximum depth of nesting block
#define MAXSYM     42     // maximum number of symbols
#define CXMAX      500    // size of code array
#define STACKSIZE  1000   // maximum storage

enum symtype
{
	SYM_NULL,
	SYM_IDENTIFIER,
	SYM_NUMBER,
	SYM_PLUS,
	SYM_MINUS,
	SYM_TIMES,   //5
	SYM_SLASH,
	SYM_AND,
	SYM_OR,
	SYM_NOT,
	SYM_BITAND,   //10
	SYM_BITOR,
	SYM_BITNOT,
	SYM_ODD,
	SYM_EQU,
	SYM_NEQ,   //15
	SYM_LES,
	SYM_LEQ,
	SYM_GTR,
	SYM_GEQ,
	SYM_LPAREN,  //20
	SYM_RPAREN,
	SYM_COMMA,
	SYM_SEMICOLON,
	SYM_PERIOD,
	SYM_BECOMES,     //  :=   25
    SYM_BEGIN,
	SYM_END,
	SYM_IF,
	//SYM_THEN,
	SYM_WHILE,     //30
	SYM_DO,
	SYM_CALL,
	SYM_CONST,
	SYM_VAR,
	SYM_PROCEDURE,   //35
	SYM_ELSE,
	SYM_ELIF,
	SYM_RETURN,
	SYM_FOR,
	SYM_LBRACKET,
	SYM_RBRACKET,
	SYM_MOD,
	SYM_XOR,
	SYM_ARRAY
};

enum idtype
{
	ID_CONSTANT, ID_VARIABLE, ID_PROCEDURE,ID_ARRAY
};

enum opcode
{
	LIT, OPR, LOD, STO, CAL, RET, INT, JMP, JZ, JNZ, JG, JGE, JL, JLE, JE, JNE, LODA, STOA
};

enum oprcode
{
    OPR_NEG, OPR_ADD, OPR_MIN, OPR_MUL, OPR_DIV, OPR_ODD, OPR_EQU,
	OPR_NEQ, OPR_LES, OPR_LEQ, OPR_GTR, OPR_GEQ, OPR_AND, OPR_OR,  OPR_NOT,
	OPR_BITAND, OPR_BITOR, OPR_BITNOT, OPR_XOR, OPR_MOD
};


typedef struct
{
	int f; // function code
	int l; // level
	int a; // displacement address
} instruction;

//////////////////////////////////////////////////////////////////////
char* err_msg[] =
{
/*  0 */    "",
/*  1 */    "Found ':=' when expecting '='.",
/*  2 */    "There must be a number to follow '='.",
/*  3 */    "There must be an '=' to follow the identifier.",
/*  4 */    "There must be an identifier to follow 'const', 'var', or 'procedure'.",
/*  5 */    "Missing ',' or ';'.",
/*  6 */    "Incorrect procedure name.",
/*  7 */    "Statement expected.",
/*  8 */    "Follow the statement is an incorrect symbol.",
/*  9 */    "'.' expected.",
/* 10 */    "';' expected.",
/* 11 */    "Undeclared identifier or array.",
/* 12 */    "Illegal assignment.",
/* 13 */    "':=' expected.",
/* 14 */    "There must be an identifier to follow the 'call'.",
/* 15 */    "A constant or variable can not be called.",
/* 16 */    "'then' expected.",
/* 17 */    "';' or 'end' expected.",
/* 18 */    "'do' expected.",
/* 19 */    "Incorrect symbol.",
/* 20 */    "Relative operators expected.",
/* 21 */    "Procedure identifier can not be in an expression.",
/* 22 */    "Missing ')'.",
/* 23 */    "The symbol can not be followed by a factor.",
/* 24 */    "The symbol can not be as the beginning of an expression.",
/* 25 */    "The number is too great.",
/* 26 */    "Missing ']'.",
/* 27 */    "'(' expected.",
/* 28 */    "There must be an identifier",
/* 29 */    "There are too many parameters.",
/* 30 */    "Missing ','or')'.",
/* 31 */    "The number of parameter is wrong.",
/* 32 */    "There are too many procedure levels.",
/* 33 */    "There are too many condition levels.",
/* 34 */    "Missing 'return'.",
/* 35 */    "Procedure error.",
/* 36 */    ""
};

//////////////////////////////////////////////////////////////////////
char ch;         // last character read
int  sym;        // last symbol read
char id[MAXIDLEN + 1]; // last identifier read
int  num;        // last number read
int  cc;         // character count
int  ll;         // line length
int  kk;
int  err;
int  cx;         // index of current instruction to be generated.
int  level = 0;
int  tx = 0;
int  ax = 0;

char line[80];

instruction code[CXMAX];

char* word[NRW + 1] =
{
	"", /* place holder */
	"begin", "const", "do", "end","if",
	"odd", "procedure", "var", "while",
	"else","return","for", "elif"
};

int wsym[NRW + 1] =
{
	SYM_NULL, SYM_BEGIN, SYM_CONST, SYM_DO, SYM_END,
	SYM_IF, SYM_ODD, SYM_PROCEDURE, SYM_VAR, SYM_WHILE,
	SYM_ELSE,SYM_RETURN,SYM_FOR, SYM_ELIF
};

int ssym[NSYM + 1] =
{
	SYM_NULL, SYM_PLUS, SYM_MINUS, SYM_TIMES, SYM_SLASH, SYM_NOT, SYM_BITNOT,
	SYM_LPAREN, SYM_RPAREN, SYM_EQU, SYM_COMMA, SYM_PERIOD, SYM_SEMICOLON,
	SYM_LBRACKET, SYM_RBRACKET,SYM_MOD,SYM_XOR
};

char csym[NSYM + 1] =
{
	' ', '+', '-', '*', '/', '!', '~', '(', ')', '=', ',', '.', ';', '[', ']', '%', '^'
};

#define MAXINS  18
char* mnemonic[MAXINS] =
{
	"LIT", "OPR", "LOD", "STO", "CAL", "RET", "INT", "JMP", "JZ", "JNZ", "JG", "JGE", "JL", "JLE", "JE", "JNE", "LODA","STOA"
};

typedef struct
{
	char name[MAXIDLEN + 1];
	int  kind;
	int  number;
	int  value;
} comtab;    //标识符

comtab table[TXMAX];   //标识符表

typedef struct
{
	char  name[MAXIDLEN + 1];
	int   kind;
	int   number;     // 函数参数个数
	short level;
	short address;
} mask;

#define MAXDIM     8      // max dims of an array

typedef struct
{
	char  name[MAXIDLEN + 1]; // 数组名
	int   dim;                // 数组维数
	int   edim[MAXDIM];       // 数组每一维的值
	int   sumsize;            // 数组的总大小
	int   dimsize[MAXDIM];    // 数组每一维的偏移大小
	short address;		      // 数组在符号表中的地址
} array;

//int procedure[20];     //函数参数个数

array arrtable[TXMAX], temparray; // 定义一个数组名构成的符号表，以及一个记录当前数组信息的数据结构

int true_out[MAXCL+1][10]={0};
int false_out[MAXCL+1][10]={0};
int true_count[MAXCL+1]={0};
int false_count[MAXCL+1]={0};
int condition_level =0;

enum expr_l
{
    FL, TL, EL, CL, BL, AL, OL       //用作下面数组的下标
};
int expr_level[7];
/*  数组各项依次表示在每个表达式中所含的运算类型
    运算类型依次为factor、term、express、condition、bit_condition、and、or
    含有该类型时相应数组元素非零
    添加该数组为实现短路计算
 */

FILE* infile;

// EOF PL0.h
