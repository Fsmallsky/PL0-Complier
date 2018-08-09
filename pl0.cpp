// pl0 compiler source code

#pragma warning(disable:4996)


#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

#include "PL0.h"
#include "set.c"

//////////////////////////////////////////////////////////////////////
// print error message.
void error(int n)
{
	int i;

	printf("      ");
	for (i = 1; i <= cc - 1; i++)
		printf(" ");
	printf("^\n");
	printf("Error %3d: %s\n", n, err_msg[n]);
	err++;
} // error

//////////////////////////////////////////////////////////////////////
void getch(void)
{
	if (cc == ll)   // 每次读取一行字符保存在line数组中
	{
		if (feof(infile))    // 在读字符时发现文件流结束
		{
			printf("\nPROGRAM INCOMPLETE\n");
			exit(1);
		}
		ll = cc = 0;   // 开始下一行
		printf("%5d  ", cx);
		while ((!feof(infile)) // added & modified by alex 01-02-09
			&& ((ch = getc(infile)) != '\n'))
		{
			printf("%c", ch);   // 输出该行源程序
			line[++ll] = ch;
		} // while
		printf("\n");
		line[++ll] = ' ';  // 换行符用' '表示
	}
	ch = line[++cc]; // 获取当前cc对应的字符
} // getch

//////////////////////////////////////////////////////////////////////
// gets a symbol from input stream.
void getsym(void)
{
	int i, k;
	char a[MAXIDLEN + 1];

	while (ch == ' '|| ch == '\t')
		getch();

	if (isalpha(ch))
	{ // symbol is a reserved word or an identifier.
		k = 0;
		do
		{
			if (k < MAXIDLEN)
				a[k++] = ch;
			getch();
		}
		while (isalpha(ch) || isdigit(ch));
		a[k] = 0;
		strcpy(id, a);
		word[0] = id;
		i = NRW;
		while (strcmp(id, word[i--]));
		if (++i)
			sym = wsym[i]; // symbol is a reserved word
		else
		{
			if (ch == '[')               // 判断是否是数组名
				sym = SYM_ARRAY;
			else sym = SYM_IDENTIFIER;   // symbol is an identifier
		}
	}
	else if (isdigit(ch))
	{ // symbol is a number.
		k = num = 0;
		sym = SYM_NUMBER;
		do
		{
			num = num * 10 + ch - '0';
			k++;
			getch();
		}
		while (isdigit(ch));
		if (k > MAXNUMLEN)
			error(25);     // The number is too great.
	}
	else if (ch == ':')
	{
		getch();
		if (ch == '=')
		{
			sym = SYM_BECOMES; // :=
			getch();
		}
		else
		{
			sym = SYM_NULL;       // illegal?
		}
	}
	else if (ch == '>')
	{
		getch();
		if (ch == '=')
		{
			sym = SYM_GEQ;     // >=
			getch();
		}
		else
		{
			sym = SYM_GTR;     // >
		}
	}
	else if (ch == '<')
	{
		getch();
		if (ch == '=')
		{
			sym = SYM_LEQ;     // <=
			getch();
		}
		else if (ch == '>')
		{
			sym = SYM_NEQ;     // <>
			getch();
		}
		else
		{
			sym = SYM_LES;     // <
		}
	}
	else if (ch == '&')
    {
        getch();
        if(ch == '&')
        {
            sym = SYM_AND;      //&&
            getch();
        }
        else
        {
            sym = SYM_BITAND;   //&
        }
    }
    else if (ch == '|')
    {
        getch();
        if(ch == '|')
        {
            sym = SYM_OR;      //||
            getch();
        }
        else
        {
            sym = SYM_BITOR;    //|
        }
    }
	else if (ch == '/')
	{
		getch();
		if (ch == '/')
		{
			cc = ll;   // 行注释
			getch();
			getsym();
		}
		else if (ch == '*')
		{
			getch();
			while (1)
			{
				while (ch != '*')  // 假设源程序添加的注释是正确的
				{
					getch();
				}
				getch();
				if (ch == '/')     // 判断为块注释，跳出最外层循环
					break;
			}
			getch();
			getsym();
		}
		else sym = SYM_SLASH;  // 非注释，则判断为除法符号
	}
	else
	{ // other tokens	else

		i = NSYM;
		csym[0] = ch;
		while (csym[i--] != ch);
		if (++i)
		{
			sym = ssym[i];
			getch();
		}
		else
		{
			printf("Fatal Error: Unknown character.\n");
			exit(1);
		}
	}
} // getsym

//////////////////////////////////////////////////////////////////////
// generates (assembles) an instruction.
void gen(int x, int y, int z)
{
	if (cx > CXMAX)
	{
		printf("Fatal Error: Program too long.\n");
		exit(1);
	}
	code[cx].f = x;
	code[cx].l = y;
	code[cx++].a = z;
} // gen

//////////////////////////////////////////////////////////////////////
// tests if error occurs and skips all symbols that do not belongs to s1 or s2.
void test(symset s1, symset s2, int n)
{
	symset s;

	if (! inset(sym, s1))
	{
		error(n);
		s = uniteset(s1, s2);
		while(! inset(sym, s))
			getsym();
		destroyset(s);
	}
} // test
/////////////////////////////////////////////////////////////////////
int dx;  // data allocation index

// enter object(constant, variable or procedre) into table.
void enter(int kind)
{
	mask* mk;

	tx++;
	strcpy(table[tx].name, id);
	table[tx].kind = kind;
	switch (kind)
	{
	case ID_CONSTANT:
		if (num > MAXADDRESS)
		{
			error(25); // The number is too great.
			num = 0;
		}
		table[tx].value = num;
		break;
	case ID_VARIABLE:
		mk = (mask*) &table[tx];
		mk->level = level;
		mk->address = dx++;
		break;
	case ID_PROCEDURE:
		mk = (mask*) &table[tx];
		mk->level = level;
		break;
	case ID_ARRAY:                   // 声明数组时在符号表中添加数组名称，同时在数组符号表中添加一项，补充相关信息
		mk = (mask*)&table[tx];
		mk->level = level;
		mk->address = dx;
		ax++;
		arrtable[ax] = temparray;
		arrtable[ax].address = tx;
		strcpy(arrtable[ax].name, id);
		dx += arrtable[ax].sumsize;
	} // switch
} // enter

//////////////////////////////////////////////////////////////////////
// locates identifier in symbol table.
int position(char* id)
{
	int i;
	strcpy(table[0].name, id);
	i = tx + 1;
	while (strcmp(table[--i].name, id) != 0);
	return i;
} // position

int array_position(char* id)
{
	int i;
	strcpy(arrtable[0].name, id);
	i = ax + 1;
	while (strcmp(arrtable[--i].name, id) != 0);
	return i;
}

//////////////////////////////////////////////////////////////////////
void constdeclaration()
{
	if (sym == SYM_IDENTIFIER)
	{
		getsym();
		if (sym == SYM_EQU || sym == SYM_BECOMES)
		{
			if (sym == SYM_BECOMES)
				error(1); // Found ':=' when expecting '='.
			getsym();
			if (sym == SYM_NUMBER)
			{
				enter(ID_CONSTANT);
				getsym();
			}
			else
			{
				error(2); // There must be a number to follow '='.
			}
		}
		else
		{
			error(3); // There must be an '=' to follow the identifier.
		}
	} else	error(4);
	 // There must be an identifier to follow 'const', 'var', or 'procedure'.
} // constdeclaration

//////////////////////////////////////////////////////////////////////
void vardeclaration(void)
{
	int dimt = 0, i;
	if (sym == SYM_IDENTIFIER)
	{
		enter(ID_VARIABLE);
		getsym();
	}
	else if (sym == SYM_ARRAY)  // 该变量为数组类型
	{
		while (ch == '[')
		{
			dimt++;
			getch();
			getsym();
			if (sym != SYM_NUMBER)  error(19);     // 声明数组时未给定维数大小
			else temparray.edim[dimt - 1] = num;   // 暂存数组定义时每一维的大小
			getsym();
			if (sym != SYM_RBRACKET)  error(26);   // 右括号缺失
		}
		temparray.dim = dimt;
		temparray.dimsize[dimt - 1] = 1;           // 最低维每一个元素偏移1
		for (i = dimt - 1; i > 0; i--)             // 计算每一维的偏移
		{
			temparray.dimsize[i - 1] = temparray.dimsize[i] * temparray.edim[i];
		}
		temparray.sumsize = temparray.dimsize[0] * temparray.edim[0];
		enter(ID_ARRAY);
		getsym();
	}
	else
	{
		error(4); // There must be an identifier to follow 'const', 'var', or 'procedure'.
	}
} // vardeclaration

//////////////////////////////////////////////////////////////////////
void listcode(int from, int to)
{
	int i;

	printf("\n");
	for (i = from; i < to; i++)
	{
		printf("%5d %s\t%d\t%d\n", i, mnemonic[code[i].f], code[i].l, code[i].a);
	}
	printf("\n");
} // listcode

//////////////////////////////////////////////////////////////////////

//////////////////////////////////////////////////////////////////////
void factor(symset fsys)    //因子
{
	void or_expr(symset fsys);
	int tn;    //用于参数传递
	symset set1,set;

	int i, arradd;
	int dimt = 0;
	mask *mk;

	test(facbegsys, fsys, 24); // The symbol can not be as the beginning of an expression.

	if (inset(sym, facbegsys))
	{
		if (sym == SYM_IDENTIFIER)
		{
			if ((i = position(id)) == 0)
			{
				error(11); // Undeclared identifier.
			}
			else
			{
				switch (table[i].kind)
				{
				case ID_CONSTANT:
					gen(LIT, 0, table[i].value);
					break;
				case ID_VARIABLE:
					mk = (mask*) &table[i];
					gen(LOD, level - mk->level, mk->address);
					break;
				case ID_PROCEDURE:
                    mask *mk;
                    mk = (mask*) &table[i];
                    tn = mk->number;
                    getsym();
                    if(sym != SYM_LPAREN)
                        error(27);    //'(' expected.
                    else
                    {
                        getsym();
                        gen(INT, 0, 1);   //为返回值分配空间
                        while (inset(sym, facbegsys))
                        {
                            set1 = createset(SYM_RPAREN,SYM_NULL);
                            set = uniteset(set1, fsys);
                            or_expr(set);
                            tn--;
                            destroyset(set1);
                            destroyset(set);

                            if(sym == SYM_COMMA)
                                getsym();
                            else if(sym != SYM_RPAREN)
                                error(30);   //Missing ','or ')'.
                        }
                        if(tn) error(31);    //The number of parameter is wrong.
                    }
                    gen(CAL, level - mk->level, mk->address);
					break;
				} // switch
			}
			getsym();
		}
		else if (sym == SYM_ARRAY)
		{
			if ((i = array_position(id)) == 0)
			{
				error(11);
			}
			else
			{
				arradd = arrtable[i].address;
				mk = (mask*)&table[arradd];
				gen(LIT, 0, 0);
				while (ch == '[')
				{
					dimt++;
					getch();
					getsym();
					set = uniteset(createset(SYM_RBRACKET, SYM_NULL), fsys);
					or_expr(set);
					destroyset(set);
					gen(LIT, 0, arrtable[i].dimsize[dimt - 1]);
					gen(OPR, 0, OPR_MUL);
					gen(OPR, 0, OPR_ADD);
				}
				gen(LODA, level - mk->level, mk->address);
			}
			getsym();
		}
		else if (sym == SYM_NUMBER)
		{
			if (num > MAXADDRESS)
			{
				error(25); // The number is too great.
				num = 0;
			}
			gen(LIT, 0, num);
			getsym();
		}
		else if (sym == SYM_LPAREN)
		{
			getsym();
			set = uniteset(createset(SYM_RPAREN, SYM_NULL), fsys);
			or_expr(set);
			destroyset(set);
			if (sym == SYM_RPAREN)
			{
				getsym();
			}
			else
			{
				error(22); // Missing ')'.
			}
		}
		else if(sym == SYM_MINUS) // UMINUS,  Expr -> '-' Expr
		{
			 getsym();
			 factor(fsys);
			 gen(OPR, 0, OPR_NEG);
		}
		else  if(sym == SYM_NOT)
		{
		    getsym();
		    factor(fsys);
		    gen(OPR, 0, OPR_NOT);

		}
		else if(sym == SYM_ODD)
        {
            getsym();
            factor(fsys);
            gen(OPR, 0, OPR_ODD);
        }
        else if(sym == SYM_BITNOT)
        {
            getsym();
            factor(fsys);
            gen(OPR, 0, OPR_BITNOT);
        }
		test(fsys, createset(SYM_LPAREN, SYM_NULL), 23);
	} // if
} // factor

//////////////////////////////////////////////////////////////////////
void term(symset fsys)    //项
{
	int mulop;
	symset set;
	set = uniteset(fsys, createset(SYM_TIMES, SYM_SLASH, SYM_MOD, SYM_LBRACKET, SYM_RBRACKET, SYM_NULL));
	factor(set);
	while (sym == SYM_TIMES || sym == SYM_SLASH || sym==SYM_MOD)
	{
		mulop = sym;
		getsym();
		factor(set);
		if (mulop == SYM_TIMES)
		{
			gen(OPR, 0, OPR_MUL);
		}
		else if(mulop == SYM_SLASH)
		{
			gen(OPR, 0, OPR_DIV);
		}
		else{
            gen(OPR, 0, OPR_MOD);
		}
	} // while
	destroyset(set);
} // term

//////////////////////////////////////////////////////////////////////
void expression(symset fsys)  //表达式
{
	int addop;
	symset set;

	set = uniteset(fsys, createset(SYM_PLUS, SYM_MINUS, SYM_LBRACKET, SYM_RBRACKET, SYM_NULL));
    expr_level[EL] = 0;
	term(set);
    if (sym == SYM_PLUS || sym == SYM_MINUS)
        expr_level[EL] = 1;

 	while (sym == SYM_PLUS || sym == SYM_MINUS)
	{
		addop = sym;
		getsym();
		term(set);
		if (addop == SYM_PLUS)
		{
			gen(OPR, 0, OPR_ADD);
		}
		else
		{
			gen(OPR, 0, OPR_MIN);
		}
	} // while

	destroyset(set);
} // expression

//////////////////////////////////////////////////////////////////////
void condition(symset fsys)  //条件
{
        int relop;
        symset set;
		set = uniteset(relset, fsys);
		expr_level[CL] = 0;
		expression(set);

/*		if (sym ==SYM_THEN || sym == SYM_DO)
        {
            expr_level[CL] = 0;
            return ;
        }
*/
        if (inset(sym, relset))
        expr_level[CL] = 1;

		while (inset(sym, relset))
		{
			relop = sym;
			getsym();
			expression(set);
			switch (relop)
			{
			case SYM_EQU:
			    false_out[condition_level][false_count[condition_level]++] = cx;
				gen(JNE, 0, 0);
				break;
			case SYM_NEQ:
			    false_out[condition_level][false_count[condition_level]++] = cx;
				gen(JE, 0, 0);
				break;
			case SYM_LES:
			    false_out[condition_level][false_count[condition_level]++] = cx;
				gen(JGE, 0, 0);
				break;
			case SYM_GEQ:
			    false_out[condition_level][false_count[condition_level]++] = cx;
				gen(JL, 0, 0);
				break;
			case SYM_GTR:
			    false_out[condition_level][false_count[condition_level]++] = cx;
				gen(JLE, 0, 0);
				break;
			case SYM_LEQ:
			    false_out[condition_level][false_count[condition_level]++] = cx;
				gen(JG, 0, 0);
				break;
			} // switch
    } // while
    destroyset(set);
} // condition

//////////////////////////////////////////////////////////////////////
void con_expr(symset fsys)  //条件表达式
{
        int relop;
        symset set;
        set = uniteset(relset, fsys);
		expression(set);

/*		if (sym ==SYM_THEN || sym == SYM_DO)
            return ;
*/
		while (inset(sym, relset))
		{
			relop = sym;
			getsym();
			expression(set);
			switch (relop)
			{
			case SYM_EQU:
				gen(OPR, 0, OPR_EQU);
				break;
			case SYM_NEQ:
				gen(OPR, 0, OPR_NEQ);
				break;
			case SYM_LES:
				gen(OPR, 0, OPR_LES);
				break;
			case SYM_GEQ:
				gen(OPR, 0, OPR_GEQ);
				break;
			case SYM_GTR:
				gen(OPR, 0, OPR_GTR);
				break;
			case SYM_LEQ:
				gen(OPR, 0, OPR_LEQ);
				break;
			} // switch
    } // while
    destroyset(set);
} // condition

void bit_expr(symset fsys)
{
    symset set,set1;
    int temp_sym;

    expr_level[BL] = 0;
    set1 = createset(SYM_BITAND, SYM_BITOR, SYM_XOR, SYM_NULL);
    set = uniteset(fsys, set1);

    con_expr(set);
    if(inset(sym, set1))
        expr_level[BL] = 1;
/*    if (sym == SYM_THEN || sym == SYM_DO)
        return;
*/
    while(inset(sym,set1))
    {
        temp_sym = sym;
        getsym();
        con_expr(set);
        switch(temp_sym)
        {
        case SYM_BITAND:
            gen(OPR, 0, OPR_BITAND);
            break;
        case SYM_BITOR:
            gen(OPR, 0, OPR_BITOR);
            break;
        case SYM_XOR:
            gen(OPR, 0, OPR_XOR);
            break;
        }//switch
    }//while
    destroyset(set);
    destroyset(set1);
}

//////////////////////////////////////////////////////////////////////
void and_condition(symset fsys)
{

    symset set;
    set = uniteset(fsys,createset(SYM_AND,SYM_NULL));

    expr_level[AL] = 0;
    condition(set);
    if (expr_level[CL] == 0)
    {
        false_out[condition_level][false_count[condition_level]++] = cx;
        gen(JZ, 0, 0);
    }
    if (sym == SYM_AND)
	    expr_level[AL] = 1;

	while (sym == SYM_AND)
	{

		getsym();
        condition(set);
        if(expr_level[CL] == 0)
        {
            false_out[condition_level][false_count[condition_level]++] = cx;
            gen(JZ, 0, 0);
        }
	} // while
	true_out[condition_level][true_count[condition_level]++] = cx;
    gen(JMP, 0, 0);
	destroyset(set);
}

//////////////////////////////////////////////////////////////////////
void or_condition(symset fsys)
{

    int k;       //用于地址回填
	symset set;

    expr_level[OL] = 0;
	set = uniteset(fsys, createset(SYM_OR, SYM_NULL));

	condition_level++;
	if(condition_level > MAXCL)
        error(33);    //There are too many condition levels.
    false_count[condition_level] = 0;
    true_count[condition_level]  = 0;

	and_condition(set);

	if (sym == SYM_OR)
        expr_level[OL] = 1;

    while (sym == SYM_OR)
	{
	    for(k = 0; k < false_count[condition_level]; k++)
        {
            code[false_out[condition_level][k]].a = cx;
        }
        false_count[condition_level] = 0;
		getsym();
		and_condition(set);
	} // while
    true_count[condition_level]--;
    cx--;
	destroyset(set);
} // or_condition

//////////////////////////////////////////////////////////////////////
void and_expr(symset fsys)
{
    symset set;
    set = uniteset(fsys,createset(SYM_AND,SYM_NULL));

    bit_expr(set);
	while (sym == SYM_AND)
	{
		getsym();
		bit_expr(set);
		gen(OPR, 0, OPR_AND);
	} // while
	destroyset(set);
}

//////////////////////////////////////////////////////////////////////
void or_expr(symset fsys)
{  //  返回时已经读到了表达式的下一个字符
	symset set;

	set = uniteset(fsys, createset(SYM_OR, SYM_NULL));

	and_expr(set);
    while (sym == SYM_OR)
	{
		getsym();
		and_expr(set);
		gen(OPR, 0, OPR_OR);
	} // while

	destroyset(set);
}
//////////////////////////////////////////////////////////////////////
void statement(symset fsys)   //
{ //  返回时读的是该语句的最后一个字符
	int i, arradd, cx1, cx2;
	int k;
	int t;     //查找符号表中最近的的一个函数名
	int dimt = 0; // 数组维数
	int tn;    // 用于参数传递

	symset set1, set;

	if (sym == SYM_IDENTIFIER)
	{ // variable assignment  or procedure call
		mask* mk;
		if (! (i = position(id)))
		{
			error(11); // Undeclared identifier.
		}
		else if (table[i].kind == ID_VARIABLE)
        {
            getsym();
            if (sym == SYM_BECOMES)
            {
                getsym();
            }
            else
            {
                error(13); // ':=' expected.
            }
            or_expr(fsys);
            mk = (mask*) &table[i];
            if (i)
            {
                gen(STO, level - mk->level, mk->address);
            }
        }
        else if (table[i].kind == ID_PROCEDURE)
        {
				mask *mk;
				mk = (mask*) &table[i];
				tn = mk->number;
				getsym();
				if(sym != SYM_LPAREN)
                    error(27);    //'(' expected.
                else
                {
                    getsym();
                    gen(INT, 0, 1);   //为返回值分配空间
                    while (inset(sym, facbegsys))
                    {
                        set1 = createset(SYM_RPAREN,SYM_NULL);
                        set = uniteset(set1, fsys);
                        or_expr(set);
                        tn--;
                        destroyset(set1);
                        destroyset(set);

                        if(sym == SYM_COMMA)
                            getsym();
                    }
                    if( tn) error(31);    //The number of parameter is wrong.
                    if(sym == SYM_RPAREN)
                    {
                        getsym();
                        if(sym != SYM_SEMICOLON)
                            error(10);   //';' expected.
                        //else getsym();
                    }
                    else error(28);     //There must be an identifier
                }
				gen(CAL, level - mk->level, mk->address);
        }

	}
	else if (sym == SYM_ARRAY)
	{
		mask* mk;
		if (!(i = array_position(id)))
		{
			error(11); // 未定义的数组
		}
		else
		{
			arradd = arrtable[i].address; // 数组名在符号表中的首地址
			mk = (mask*)&table[arradd];
			gen(LIT, 0, 0);            // 计算相对数组首地址的偏移，初值为0
			while (ch == '[')
			{
				dimt++;                // 每读一个[]维数加1
				getch();
				getsym();
				set = uniteset(createset(SYM_RBRACKET, SYM_NULL), fsys);
				or_expr(set);       // []内的值（表达式、变量、常量）压栈
				destroyset(set);
				gen(LIT, 0, arrtable[i].dimsize[dimt - 1]); // 当前维数的偏移压栈
				gen(OPR, 0, OPR_MUL);                       // 当前维数偏移乘以该维上的值
				gen(OPR, 0, OPR_ADD);                       // 累加得到总偏移
			}
		}
		getsym();
		if (sym == SYM_BECOMES)
			getsym();
		else
			error(13);		// ':=' expected.
		set = uniteset(createset(SYM_RBRACKET, SYM_NULL), fsys);
		or_expr(set);
		destroyset(set);
		arradd = arrtable[i].address;
		mk = (mask*)&table[arradd];
		gen(STOA, level - mk->level, mk->address);  // 将:=右值填到栈中相对首地址的对应偏移处
	}
    else if (sym == SYM_IF)
	{ // if statement
		getsym();
		if (sym == SYM_LPAREN)
        {
            getsym();
        }
        else
        {
            error(27);     //'(' expected.
        }
		set1 = createset(SYM_RPAREN, SYM_AND,SYM_OR,SYM_NOT,SYM_DO,SYM_NULL);
		set = uniteset(set1, fsys);
		or_condition(set);
		destroyset(set1);
		destroyset(set);
		if (sym == SYM_RPAREN)
		{
			getsym();
		}
		else
		{
			error(16); // Missing ')'.
		}
        for ( k = 0; k < true_count[condition_level];k++) /*所有真出口都跳到statement的前面*/
			code[true_out[condition_level][k]].a = cx;

		//条件成立的执行语句
        set1 = createset(SYM_ELSE, SYM_ELIF, SYM_NULL);
        set = uniteset(set1, fsys);
		statement(fsys);
		destroyset(set1);
		destroyset(set);
		if (sym != SYM_SEMICOLON)
            error(10);     //';' expected.
        else getsym();

		//cx1 = cx;
		//gen(JMP, 0, 0);  //成立语句执行完之后 要么跳到else-statment之后，要么继续执行
        if (sym == SYM_ELSE)  //如果有else字句，则false出口到else字句
		{
            cx1 = cx;
		    gen(JMP, 0, 0);  //成立语句执行完之后 要么跳到else-statment之后，要么继续执行
			getsym();
			for (i = 0;i < false_count[condition_level];i++) //所有假出口都跳到else字后面
                code[false_out[condition_level][i]].a = cx;
            condition_level--;

            statement(fsys);
            code[cx1].a = cx;
		}
		else if (sym == SYM_ELIF)
        {

            for (k = 0;k < false_count[condition_level];k++) /*所有假出口都跳到if-statement 后面*/
            code[false_out[condition_level][k]].a = cx;
            condition_level--;

            sym = SYM_IF;
            set1 = createset(SYM_ELSE, SYM_ELIF, SYM_NULL);
            set = uniteset(set1, fsys);
            statement(fsys);
            destroyset(set1);
            destroyset(set);
        }
		else
		{
            for (k = 0;k < false_count[condition_level];k++) /*所有假出口都跳到if-statement 后面*/
            code[false_out[condition_level][k]].a = cx;
            condition_level--;
		}
		//code[cx1].a = cx;
		//condition_level--;
	}
	else if (sym == SYM_BEGIN)
	{ // block
		getsym();
		set1 = createset(SYM_SEMICOLON, SYM_END, SYM_NULL);
		set = uniteset(set1, fsys);
		statement(set);
		while (sym == SYM_SEMICOLON || inset(sym, statbegsys))
		{
			if (sym == SYM_SEMICOLON)
			{
				getsym();
			}
			else
			{
				error(10);
			}
			statement(set);
		} // while
		destroyset(set1);
		destroyset(set);
		if (sym == SYM_END)
		{
			getsym();
		}
		else
		{
			error(17); // ';' or 'end' expected.
		}
	}
	else if (sym == SYM_WHILE)
	{ // while statement
		cx1 = cx;
		getsym();
		set1 = createset(SYM_DO, SYM_NULL);
		set = uniteset(set1, fsys);
		or_condition(set);
		destroyset(set1);
		destroyset(set);

		if (sym == SYM_DO)
		{
			getsym();
		}
		else
		{
			error(18); // 'do' expected.
		}
		for(k = 0; k < true_count[condition_level]; k++)
            code[true_out[condition_level][k]].a = cx;

		statement(fsys);
		gen(JMP, 0, cx1);

        for(k = 0; k < false_count[condition_level]; k++)
            code[false_out[condition_level][k]].a = cx;
        condition_level--;
	}
	else if (sym == SYM_FOR)
	{
		getsym();
		if (sym == SYM_LPAREN)
		{
			getsym();
		}
		else
		{
			error(26); //expected '('
		}
		statement(fsys);  //初始化
		if (sym == SYM_SEMICOLON)
		{
			getsym();
		}
		else
		{
			error(10);   //';' expected.
		}
		cx1 = cx; //条件判断的地址，执行完循环变量的改变之后的跳转地址
		set1 = createset(SYM_RPAREN, SYM_DO, SYM_AND, SYM_NOT, SYM_OR, SYM_NULL);
		set = uniteset(set1, fsys);
		or_condition(set); //条件
		destroyset(set1);
		destroyset(set);

		if (sym == SYM_SEMICOLON)
		{
			getsym();
		}
		else
		{
			error(10);   //';' expected.
		}
		cx2 = cx; //  循环变量，循环体执行完之后的出口
		set1 = createset(SYM_RPAREN,SYM_NULL);
		set = uniteset(set1, fsys);
		statement(set);

		gen(JMP, 0, cx1);// 跳到条件判断

		if (sym == SYM_RPAREN)
		{
			getsym();
		}
		else
		{
			error(22);   //Missing ')'.
		}
		/*条件为真的出口——循环体之前*/
		for ( k = 0; k < true_count[condition_level];k++)
			code[true_out[condition_level][k]].a = cx;

		statement(fsys); //循环体
		gen(JMP, 0, cx2); //执行完循环体跳转到循环变量的改变

		/*条件为假的出口——循环体之后*/
		for ( k = 0;k < false_count[condition_level];k++)
			code[false_out[condition_level][k]].a = cx;
		condition_level--;
	}
	else if (sym == SYM_RETURN)
	{
		getsym();
		or_expr(fsys);
		for (t=tx; table[t].kind != ID_PROCEDURE; t--);
        mask *mk;
        mk = (mask*) &table[t];
        if(level == 0)
        {
            mk = (mask*)&table[0];
        }
		if(t < 0)
            error(35);    //Procedure error.
        if(mk->level != level - 1)
            error(34);    //Missing 'return'.
		gen(STO, 0, -mk->number - 1);    //保存返回值
		gen(RET, 0,mk->number);
		level--;     // 层次数减一
	}

	test(fsys, phi, 19);
} // statement

//////////////////////////////////////////////////////////////////////
void block(symset fsys)     //
{
	int cx0; // initial code inde.
	mask* mk;
	int block_dx;
	int savedTx;
	int block_lv;
	symset set1, set;
	int n;     //参数个数
	char paraName[MAXPARA][MAXIDLEN];   //参数名
    int tempTx;

	dx = 3;
	block_dx = dx;
	for(tempTx = tx; table[tempTx].kind != ID_PROCEDURE; tempTx--);
	mk = (mask*) &table[tempTx];
	mk->address = cx;
	gen(JMP, 0, 0);
	if (level > MAXLEVEL)
	{
		error(32); // There are too many levels.
	}
	do
	{
		if (sym == SYM_CONST)
		{ // constant declarations
			getsym();
			do
			{
				constdeclaration();
				while (sym == SYM_COMMA)
				{
					getsym();
					constdeclaration();
				}
				if (sym == SYM_SEMICOLON)
				{
					getsym();
				}
				else
				{
					error(5); // Missing ',' or ';'.
				}
			}
			while (sym == SYM_IDENTIFIER);
		} // if
		if (sym == SYM_VAR)
		{ // variable declarations
			getsym();
			do
			{
				vardeclaration();
				while (sym == SYM_COMMA)
				{
					getsym();
					vardeclaration();
				}
				if (sym == SYM_SEMICOLON)
				{
					getsym();
				}
				else
				{
					error(5); // Missing ',' or ';'.
				}
			}
			while (sym == SYM_IDENTIFIER || sym==SYM_ARRAY);
		} // if
		block_dx = dx; //save dx before handling procedure call!
		block_lv = level;
		while (sym == SYM_PROCEDURE)
		{ // procedure declarations
			getsym();
			if (sym == SYM_IDENTIFIER)
			{
				enter(ID_PROCEDURE);
				level++;
				getsym();
			}
            else
            {
                error(4); // There must be an identifier to follow 'const', 'var', or 'procedure'.
            }
            if (sym == SYM_LPAREN)
            {
                getsym();
                n = 0;
                while(sym != SYM_RPAREN )
                {
                    if (sym == SYM_IDENTIFIER)
                    {
                        strcpy(paraName[++n],id);
                        getsym();
                        if(sym != SYM_RPAREN)
                        {
                            if(sym == SYM_COMMA)
                                getsym();
                            else error(30);   //Missing ','or')'.
                        }
                    }
                    else
                    {
                        error(28);   //There must be an identifier
                        break;
                    }
                }
            }
            else
            {
                error(27);    //'(' expected.
            }

            for (dx = -n; dx < 0; )
            { //  将形参插入符号表，他们的偏移都是负的
                strcpy(id,paraName[n+dx+1]);
                enter(ID_VARIABLE);
            }
            table[tx - n].number = n;  //这里是存储函数参数个数信息

            if (sym == SYM_RPAREN)
                getsym();
            else
                error(22);   //Missing ')'.

			if (sym == SYM_SEMICOLON)
			{
				getsym();
			}
			else
			{
				error(5); // Missing ',' or ';'.
			}

			savedTx = tx;
			set1 = createset(SYM_SEMICOLON, SYM_NULL);
			set = uniteset(set1, fsys);
			block(set);
			destroyset(set1);
			destroyset(set);
			tx = savedTx;
			//level--;
			if(block_lv != level)
			{
				error(34);     // Missing "return".
				level = block_lv;
			}

			if (sym == SYM_SEMICOLON)
			{
				getsym();
				set1 = createset(SYM_IDENTIFIER, SYM_PROCEDURE, SYM_NULL);
				set = uniteset(statbegsys, set1);
				test(set, fsys, 6);
				destroyset(set1);
				destroyset(set);
			}
			else
			{
				error(5); // Missing ',' or ';'.
			}
		} // while
		dx = block_dx; //restore dx after handling procedure call!
		set1 = createset(SYM_IDENTIFIER, SYM_NULL);
		set = uniteset(statbegsys, set1);
		set = uniteset(set, declbegsys);
		test(set, declbegsys, 7);
		destroyset(set1);
		destroyset(set);
	}
	while (inset(sym, declbegsys));

	code[mk->address].a = cx;
	mk->address = cx;
	cx0 = cx;
	gen(INT, 0, block_dx);
	set1 = createset(SYM_SEMICOLON, SYM_END, SYM_NULL);
	set = uniteset(set1, fsys);
	statement(set);
	if(sym == SYM_PERIOD && level != -1)
        error(34);    //Missing 'return'.
	destroyset(set1);
	destroyset(set);
	//gen(OPR, 0, OPR_RET); // return
	test(fsys, phi, 8); // test for error: Follow the statement is an incorrect symbol.
	listcode(cx0, cx);
} // block

//////////////////////////////////////////////////////////////////////
int base(int stack[], int currentLevel, int levelDiff)
{
	int b = currentLevel;

	while (levelDiff--)
		b = stack[b];
	return b;
} // base

//////////////////////////////////////////////////////////////////////
// interprets and executes codes.
void interpret()
{
	int pc;        // program counter
	int stack[STACKSIZE];
	int top;       // top of stack
	int b;         // program, base, and top-stack register
	instruction i; // instruction register

	printf("Begin executing PL/0 program.\n");

	pc = 0;
	b = 1;
	top = 3;
	stack[1] = stack[2] = stack[3] = 0;
	do
	{
		i = code[pc++];
		switch (i.f)
		{
		case LIT:
			stack[++top] = i.a;
			break;
		case OPR:
			switch (i.a) // operator
			{
			case OPR_NEG:
				stack[top] = -stack[top];
				break;
			case OPR_ADD:
				top--;
				stack[top] += stack[top + 1];
				break;
			case OPR_MIN:
				top--;
				stack[top] -= stack[top + 1];
				break;
			case OPR_MUL:
				top--;
				stack[top] *= stack[top + 1];
				break;
			case OPR_DIV:
				top--;
				if (stack[top + 1] == 0)
				{
					fprintf(stderr, "Runtime Error: Divided by zero.\n");
					fprintf(stderr, "Program terminated.\n");
					continue;
				}
				stack[top] /= stack[top + 1];
				break;
			case OPR_ODD:
				stack[top] %= 2;
				break;
			case OPR_EQU:
				top--;
				stack[top] = stack[top] == stack[top + 1];
				break;
			case OPR_NEQ:
				top--;
				stack[top] = stack[top] != stack[top + 1];
				break;
			case OPR_LES:
				top--;
				stack[top] = stack[top] < stack[top + 1];
				break;
			case OPR_GEQ:
				top--;
				stack[top] = stack[top] >= stack[top + 1];
				break;
			case OPR_GTR:
				top--;
				stack[top] = stack[top] > stack[top + 1];
				break;
			case OPR_LEQ:
				top--;
				stack[top] = stack[top] <= stack[top + 1];
				break;
            case OPR_AND:
                top--;
                stack[top] = stack[top] && stack[top + 1];
                break;
            case OPR_OR:
                top--;
                stack[top] = stack[top] || stack[top + 1];
                break;
            case OPR_NOT:
                stack[top] = !stack[top];
                break;
            case OPR_BITAND:
                top--;
                stack[top] = stack[top] & stack[top + 1];
                break;
            case OPR_BITOR:
                top--;
                stack[top] = stack[top] | stack[top + 1];
                break;
            case OPR_BITNOT:
                stack[top] = ~stack[top];
                break;
            case OPR_XOR:
                top--;
                stack[top] = stack[top] ^ stack[top + 1];
                break;
            case OPR_MOD:
                top--;
                stack[top] = stack[top] % stack[top + 1];
				break;
			} // switch
			break;
		case LOD:
			stack[++top] = stack[base(stack, b, i.l) + i.a];
			break;
		case LODA:
			stack[top] = stack[base(stack, b, i.l) + i.a + stack[top]];
			break;
		case STO:
			stack[base(stack, b, i.l) + i.a] = stack[top];
			printf("%d\n", stack[top]);
			top--;
			break;
		case STOA:
			stack[base(stack, b, i.l) + i.a + stack[top - 1]] = stack[top];
			printf("%d\n", stack[top]);
			top--;
			break;
		case CAL:
			stack[top + 1] = base(stack, b, i.l);
			// generate new block mark
			stack[top + 2] = b;
			stack[top + 3] = pc;
			b = top + 1;
			pc = i.a;
			break;
        case RET:
            top = b - 1;
            pc = stack[top + 3];
            b = stack[top + 2];
            top = top - i.a;
            break;
		case INT:
			top += i.a;
			break;
		case JMP:
			pc = i.a;
			break;
		case JZ:
			if (stack[top] == 0)
            {
                pc = i.a;
            }
            if (!i.l)
            {
                top--;
            }
			break;
        case JNZ:
            if(stack[top] != 0)
            {
                pc = i.a;
            }
            if(!i.l)     // i.l 等于 0 时弹栈
            {
                top--;
            }
            break;
        case JG:
            top -=2;
            if (stack[top + 1] > stack[top + 2])
            {
                pc = i.a;
            }
            break;
        case JGE:
            top -= 2;
            if (stack[top + 1] >= stack[top + 2])
            {
                pc = i.a;
            }
            break;
        case JL:
            top -= 2;
            if (stack[top + 1] < stack[top + 2])
            {
                pc = i.a;
            }
            break;
        case JLE:
            top -=2;
            if(stack[top + 1] <= stack[top + 2])
            {
                pc = i.a;
            }
            break;
        case JE:
            top -= 2;
            if(stack[top + 1] == stack[top + 2])
            {
                pc = i.a;
            }
            break;
        case JNE:
            top -= 2;
            if(stack[top + 1] != stack[top + 2])
            {
                pc = i.a;
            }
            break;
		} // switch
	}
	while (pc);

	printf("End executing PL/0 program.\n");
} // interpret

//////////////////////////////////////////////////////////////////////
int main ()
{
	FILE* hbin;
	char s[80];
	int i;
	symset set, set1, set2;

	printf("Please input source file name: "); // get file name to be compiled
	scanf("%s", s);
	if ((infile = fopen(s, "r")) == NULL)
	{
		printf("File %s can't be opened.\n", s);
		exit(1);
	}

	phi = createset(SYM_NULL);
	relset = createset(SYM_EQU, SYM_NEQ, SYM_LES, SYM_LEQ, SYM_GTR, SYM_GEQ, SYM_NULL);

	// create begin symbol sets
	declbegsys = createset(SYM_CONST, SYM_VAR, SYM_PROCEDURE, SYM_NULL);
	statbegsys = createset(SYM_BEGIN, SYM_CALL, SYM_IF, SYM_WHILE, SYM_NULL);
	facbegsys = createset(SYM_IDENTIFIER, SYM_NUMBER, SYM_ARRAY, SYM_LPAREN, SYM_MINUS, SYM_NOT, SYM_BITNOT, SYM_ODD, SYM_NULL);

	err = cc = cx = ll = 0; // initialize global variables
	ch = ' ';
	kk = MAXIDLEN;
	table[0].kind =ID_PROCEDURE;
	table[0].number = 0;
    mask *mk;
    mk = (mask*) &table[0];
    mk->level = -1;

	getsym();

	set1 = createset(SYM_PERIOD, SYM_NULL);
	set2 = uniteset(declbegsys, statbegsys);
	set = uniteset(set1, set2);
	block(set);
	destroyset(set1);
	destroyset(set2);
	destroyset(set);
	destroyset(phi);
	destroyset(relset);
	destroyset(declbegsys);
	destroyset(statbegsys);
	destroyset(facbegsys);

	if (sym != SYM_PERIOD)
		error(9); // '.' expected.
	if (err == 0)
	{
		hbin = fopen("hbin.txt", "w");
		for (i = 0; i < cx; i++)
			fwrite(&code[i], sizeof(instruction), 1, hbin);
		fclose(hbin);
	}
	if (err == 0)
		interpret();
	else
		printf("There are %d error(s) in PL/0 program.\n", err);
	listcode(0, cx);
	return 0;
} // main

//////////////////////////////////////////////////////////////////////
// eof pl0.c
