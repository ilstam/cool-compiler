/*
 *  The scanner definition for COOL.
 */

/*
 *  Stuff enclosed in %{ %} in the first section is copied verbatim to the
 *  output, so headers and global definitions are placed here to be visible
 *  to the code in the file.  Don't remove anything that was here initially
 */
%{
#include <cool-parse.h>
#include <stringtab.h>
#include <utilities.h>

/* The compiler assumes these identifiers. */
#define yylval cool_yylval
#define yylex  cool_yylex

/* Max size of string constants */
#define MAX_STR_CONST 1025
#define YY_NO_UNPUT   /* keep g++ happy */

extern FILE *fin; /* we read from this file */

/* define YY_INPUT so we read from the FILE fin:
 * This change makes it possible to use this scanner in
 * the Cool compiler.
 */
#undef YY_INPUT
#define YY_INPUT(buf,result,max_size) \
	if ( (result = fread( (char*)buf, sizeof(char), max_size, fin)) < 0) \
		YY_FATAL_ERROR( "read() in flex scanner failed");

char string_buf[MAX_STR_CONST]; /* to assemble string constants */
char *string_buf_ptr;

extern int curr_lineno;
extern int verbose_flag;

extern YYSTYPE cool_yylval;

/*
 *  Add Your own definitions here
 */

#define MAX_STR_LEN 1024

int comment_level = 0;
std::string str;
bool null_in_str = false;

%}

/*
 * Define names for regular expressions here.
 */

DIGIT         [0-9]
ALPHANUM      [a-zA-Z0-9]

%START COMMENT
%START STRING

%%

 /*
  * NEW LINES
  */

<INITIAL,COMMENT>\n {
    curr_lineno++;
}

 /*
  * WHITESPACE
  */

[ \f\r\t\v]+ ;

 /*
  * NOTE:
  * We intentionally put rules for comments and strings on top of the file because strings
  * and comments may contain other tokens inside them as part of them. By putting those rules
  * first we make sure that they have priority over any other rules.
  */

 /*
  * COMMENTS
  */

<INITIAL>--.* {
    curr_lineno++;
}

<INITIAL,COMMENT>"(*" {
    comment_level++;
    BEGIN COMMENT;
}

<INITIAL,COMMENT>"*)" {
    comment_level--;

    if (comment_level == 0) {
        BEGIN INITIAL;
    } else if (comment_level == -1) {
        yylval.error_msg = "Unmatched *)";
        comment_level = 0;
        return ERROR;
    }
}

<COMMENT>[^\n(*]* {
    // This is the actual comment so we just ignore it.
    // It reads as "any char except \n, (, or *" any number of times while in a comment.
    // We have separate rules for matching (*, *) and \n.
}

<COMMENT>"("[^*] ; // just consume this

<COMMENT>"*"[^)] ; // just consume this

<COMMENT><<EOF>> {
    BEGIN INITIAL;

    cool_yylval.error_msg = "EOF in comment";
    return ERROR;
}

 /*
  *  STRINGS
  *
  *  String constants (C syntax)
  *  Escape sequence \c is accepted for all characters c. Except for
  *  \n \t \b \f, the result is c.
  *
  */

<INITIAL>\" {
    BEGIN STRING;
    str = "";
    null_in_str = false;
}

<STRING>[^"\n\0\\]* {
    str += yytext;
}

<STRING>\\(.|\n) {
    switch (yytext[1]) {
    case '\n':
        curr_lineno++;
        str.push_back('\n');
        break;
    case 'b':
        str.push_back('\b');
        break;
    case 't':
        str.push_back('\t');
        break;
    case 'n':
        str.push_back('\n');
        break;
    case 'f':
        str.push_back('\f');
        break;
    case '\0':
        null_in_str = true;
        break;
    default:
        str.push_back(yytext[1]);
        break;
    }
}

<STRING>\n {
    // This is an error since the new line is not escaped.
    // We assume the user forgot the close-quote and we resume lexing.
    curr_lineno++;

    BEGIN INITIAL;
    cool_yylval.symbol = stringtable.add_string((char *) str.c_str());
    str = "";

    yylval.error_msg = "Unterminated string constant";
    return ERROR;
}

<STRING>\0 {
    null_in_str = true;
}

<STRING>\" {
    BEGIN INITIAL;

    if (null_in_str) {
        yylval.error_msg = "String contains null character";
        return ERROR;
    }

    if (str.length() > MAX_STR_LEN) {
        yylval.error_msg = "String constant too long";
        return ERROR;
    }

    cool_yylval.symbol = stringtable.add_string((char *) str.c_str());
    return STR_CONST;
}

<STRING><<EOF>> {
    BEGIN INITIAL;

    cool_yylval.error_msg = "EOF in string constant";
    return ERROR;
}


 /*
  * KEYWORDS
  *
  * Keywords are case-insensitive except for the values true and false,
  * which must begin with a lower-case letter.
  */

(?i:class)    { return CLASS; }
(?i:else)     { return ELSE; }
(?i:fi)       { return FI; }
(?i:if)       { return IF; }
(?i:in)       { return IN; }
(?i:inherits) { return INHERITS; }
(?i:let)      { return LET; }
(?i:loop)     { return LOOP; }
(?i:pool)     { return POOL; }
(?i:then)     { return THEN; }
(?i:while)    { return WHILE; }
(?i:case)     { return CASE; }
(?i:esac)     { return ESAC; }
(?i:of)       { return OF; }
(?i:new)      { return NEW; }
(?i:isvoid)   { return ISVOID; }
(?i:not)      { return NOT; }

t(?i:rue)     {
    cool_yylval.boolean = 1;
    return BOOL_CONST;
}
f(?i:alse)    {
    cool_yylval.boolean = 0;
    return BOOL_CONST;
}

 /*
  * OPERATORS (arithmetic, comparison, dispatch)
  *
  * This section contains operators that solely consist of symbols.
  * Keyword operators are described in the keywords section.
  */

"=>"          { return DARROW; }
"<-"          { return ASSIGN; }
"<="          { return LE; }
[+-/*<=~.@]   { return yytext[0]; }

 /*
  * MISC TOKENS
  */

[:)(}{,;]    { return yytext[0]; }

 /*
  * INTEGERS
  */

{DIGIT}+ {
    cool_yylval.symbol = inttable.add_string(yytext);
    return INT_CONST;
}

 /*
  * IDENTIFIERS
  *
  * Type identifiers begin with a capital letter.
  * Object identifiers begin with a lower case letter.
  *
  */

[A-Z]({ALPHANUM}|_)* {
    cool_yylval.symbol = idtable.add_string(yytext);
    return TYPEID;
}

[a-z]({ALPHANUM}|_)* {
    cool_yylval.symbol = idtable.add_string(yytext);
    return OBJECTID;
}

%%
