%{
  var escopoAtual = 0;
  var tabelaSimbolos = [];
  var tabelaDefinicoes = {}; // Tabela para armazenar definições do pré-processador
  var temp = [];
  var tac = [];
  var erros = [];
  var arvores = [];
  var comment_flag = false;

  class AST {
      constructor(root) {
          this.root = root;
      }

      postorder(node) {
          if (node !== null) {
              this.postorder(node.leftChild);
              this.postorder(node.rightChild);
              console.log(node.type);
          }
      }

      inorder(node) {
          if (node !== null) {
              this.inorder(node.leftChild);
              tac.push(node.type);
              this.inorder(node.rightChild);
          }
      }
  }

  class Node {
      constructor(type, leftChild = null, rightChild = null) {
          this.type = type;
          this.leftChild = leftChild;
          this.rightChild = rightChild;
      }
  }

  function printPosOrder(node, deep = 0) {
      let indent = "    ".repeat(deep);  // Ajusta a indentação para cada nível
      let treeLine = indent + "|";
      
      if (node !== null && node !== undefined) {
          // Exibe o tipo do nó atual
          console.log(treeLine + "- " + (node.type || "undefined"));
          
          // Se houver filhos, imprimimos as relações
          if (node.leftChild !== null && node.leftChild !== undefined || 
              node.rightChild !== null && node.rightChild !== undefined) {
              
              if (node.leftChild !== null && node.leftChild !== undefined) {
                  console.log(treeLine + "   /         \\");
                  printPosOrder(node.leftChild, deep + 1); // Desenha o filho à esquerda
              }

              if (node.rightChild !== null && node.rightChild !== undefined) {
                  console.log(treeLine + "   \\         /");
                  printPosOrder(node.rightChild, deep + 1); // Desenha o filho à direita
              }
          }
      } else {
          console.log(treeLine + "- " + "undefined");
      }
  }

  function criarVariavel(tipo, nome, valor) {
      if (typeof valor === 'string') {
          const variavelExistente = tabelaSimbolos.find(dictAtual => dictAtual.id === valor);
          if (variavelExistente) {
              tabelaSimbolos.push({ tipo, id: nome, val: variavelExistente.val, escopo: escopoAtual });
          }
      } else {
          tabelaSimbolos.push({ tipo, id: nome, val: valor, escopo: escopoAtual });
      }
  }

  function verificaVariavel(id) {
      const variavel = tabelaSimbolos.find(variavel => variavel.id === id);
      if (!variavel) {
          erros.push("Variável '" + id + "' não declarada");
      }
  }

  function verificaTipos(varOne, varTwo) {
      if (typeof varOne === typeof varTwo) {
          return true;
      } else {
          erros.push(`${varOne} é de tipo diferente de ${varTwo}`);
      }
  }

  function criaTAC(temp, var1, var2, op) {
      const code = `${temp} = ${var1} ${op} ${var2}`;
      tac.push(code);
  }

  function criaTACIf(condicional, l) {
      tac.push(`if ${condicional} goto ${l}`);
  }

  function criaTACUnaryOp(op, var1, temp) {
      const code = `${temp} = ${op}${var1}`;
      tac.push(code);
  }

  function printaTAC() {
      tac.forEach((line, index) => {
          console.log(`${index}: ${line}`);
      });
  }

  function criaTemp() {
      return `temp${Math.floor(Math.random() * 1000)}`;
  }

  function criaNode(type, left, right) {
      return new Node(type, left, right);
  }
  
  function definirConstante(nome, valor) {
      tabelaDefinicoes[nome] = valor;
  }
  
  function obterValorDefinicao(nome) {
      return tabelaDefinicoes[nome];
  }
%}


%lex
%%

"//"[^\n]*        { /* Ignora completamente comentários de linha */ return; }
"/\\*"([^*]|"\\*"+[^*/])*"\\*"+"/" { /* Ignora completamente comentários de bloco */ return; }
"/*"(.|\n)*?"*/"  { /* Alternativa robusta para comentários de bloco */ return; }

\s+                                 {/* Ignorar espaços em branco */}
"#include"                          {console.log('Token INCLUDE'); return 'INCLUDE';}
"#define"                           {console.log('Token DEFINE'); return 'DEFINE';}
"stdio.h"                           {console.log('Token STDIO_H'); return 'STDIO_H';}
"stdlib.h"                          {console.log('Token STDLIB_H'); return 'STDLIB_H';}
"malloc"                            {console.log('Token MALLOC'); return 'MALLOC';}
"free"                              {console.log('Token FREE'); return 'FREE';}
"sizeof"                            {console.log('Token SIZEOF'); return 'SIZEOF';}
"void"                              {console.log('Token VOID'); return 'VOID';}
"return"                            {console.log('Token RETURN'); return 'RETURN';}
"printf"                            {console.log('Token PRINTF'); return 'PRINTF';}
"scanf"                             {console.log('Token SCANF'); return 'SCANF';}
"main"                              {console.log('Token MAIN'); return 'MAIN';}
"int"                               {console.log('Token INT'); return 'INT';}
"double"                            {console.log('Token DOUBLE'); return 'DOUBLE';}
"float"                             {console.log('Token FLOAT'); return 'FLOAT';}
"char"                              {console.log('Token CHAR'); return 'CHAR';}
"struct"                            {console.log('Token STRUCT'); return 'STRUCT';}
"union"                             {console.log('Token UNION'); return 'UNION';}
"enum"                              {console.log('Token ENUM'); return 'ENUM';}
"typedef"                           {console.log('Token TYPEDEF'); return 'TYPEDEF';}
"unsigned"                          {console.log('Token UNSIGNED'); return 'UNSIGNED';}
"signed"                            {console.log('Token SIGNED'); return 'SIGNED';}
"const"                             {console.log('Token CONST'); return 'CONST';}
"volatile"                          {console.log('Token VOLATILE'); return 'VOLATILE';}
"register"                          {console.log('Token REGISTER'); return 'REGISTER';}
"long"                              {console.log('Token LONG'); return 'LONG';}
"short"                             {console.log('Token SHORT'); return 'SHORT';}
"+="                                {console.log('Token ADD_ASSIGN'); return 'ADD_ASSIGN';}
"-="                                {console.log('Token SUB_ASSIGN'); return 'SUB_ASSIGN';}
"++"                                {console.log('Token INC'); return 'INC';}
"--"                                {console.log('Token DEC'); return 'DEC';}
"*"                                 {console.log('Token MUL'); return 'MUL';}
"*="                                {console.log('Token MUL_ASSIGN'); return 'MUL_ASSIGN';}
"/"                                 {console.log('Token DIV'); return 'DIV';}
"/="                                {console.log('Token DIV_ASSIGN'); return 'DIV_ASSIGN';}
"%"                                 {console.log('Token MOD'); return 'MOD';}
"%="                                {console.log('Token MOD_ASSIGN'); return 'MOD_ASSIGN';}
"+"                                 {console.log('Token SUM'); return 'SUM';}
"-"                                 {console.log('Token SUB'); return 'SUB';}
","                                 {console.log('Token COMMA'); return ',';}
";"                                 {console.log('Token SEMICOLON'); return ';';}
":"                                 {console.log('Token COLON'); return ':';}
"."                                 {console.log('Token DOT'); return '.';}
"("                                 {console.log('Token LPAREN'); return '(';}
")"                                 {console.log('Token RPAREN'); return ')';}
"{"                                 {console.log('Token LBRACE'); return '{';}
"}"                                 {console.log('Token RBRACE'); return '}';}
"["                                 {console.log('Token LBRACKET'); return '[';}
"]"                                 {console.log('Token RBRACKET'); return ']';}
"<="                                {console.log('Token LE'); return 'LE';}
">="                                {console.log('Token GE'); return 'GE';}
"<"                                 {console.log('Token LT'); return 'LT';}
">"                                 {console.log('Token GT'); return 'GT';}
"!="                                {console.log('Token NE'); return 'NE';}
"=="                                {console.log('Token EQ'); return 'EQ';}
"="                                 {console.log('Token ASSIGN'); return '=';}
"&&"                                {console.log('Token AND'); return 'AND';}
"||"                                {console.log('Token OR'); return 'OR';}
"!"                                 {console.log('Token NOT'); return 'NOT';}
"&"                                 {console.log('Token BITAND'); return 'BITAND';}
"if"                                {console.log('Token IF'); return 'IF';}
"switch"                            {console.log('Token SWITCH'); return 'SWITCH';}
"case"                              {console.log('Token CASE'); return 'CASE';}
"break"                             {console.log('Token BREAK'); return 'BREAK';}
"continue"                          {console.log('Token CONTINUE'); return 'CONTINUE';}
"default"                           {console.log('Token DEFAULT'); return 'DEFAULT';}
"else"                              {console.log('Token ELSE'); return 'ELSE';}
"while"                             {console.log('Token WHILE'); return 'WHILE';}
"for"                               {console.log('Token FOR'); return 'FOR';}
"do"                                {console.log('Token DO'); return 'DO';}
"NULL"                              {console.log('Token NULL'); return 'NULL';}

"%d"                                {console.log('Token FORMAT_D'); return 'FORMAT_D';}

\"([^\\\"]|\\.)*\"   {console.log('Token STRING_LIT:', yytext); return 'STRING_LIT';}
[a-zA-Z][a-zA-Z0-9_]*               {console.log('Token IDF'); return 'IDF';}
[0-9]*\.[0-9]+([eE][+-][0-9]+)?     {console.log('Token F_LIT'); return 'F_LIT';}
[0-9]+                              {console.log('Token INT_LIT'); return 'INT_LIT';}
"'[a-zA-Z0-9]'"                     {console.log('Token CHAR_LIT'); return 'CHAR_LIT';}
//"'"                                 {console.log('Token QUOTE'); return 'QUOTE';}
"#"                                 {console.log('Token HASH'); return '#';}
.                                   {console.log('Erro léxico: caractere [', yytext, '] não reconhecido.');}
<<EOF>>                             {console.log('Token EOF'); return 'EOF';}



%%

/lex


%start corpo

%%

/* Gramática melhorada */

/* Corpo principal da análise */
corpo
    : program EOF
    {
        console.log("Análise Sintática concluída com sucesso!\n");

        // Exibindo a Tabela de Símbolos
        console.log('Tabela de símbolos:\n', tabelaSimbolos);
        
        // Verificação de erros semânticos
        console.log('Análise Semântica\n');
        if (erros.length > 0) {
            console.log('Erros semânticos encontrados:\n', erros);
        } else {
            console.log('Sem erros semânticos encontrados.\n');
            
            // Se não houver erros, gera o código TAC
            console.log('Códigos Three Address Code (TAC) gerados:\n');
            printaTAC();
            console.log('');
        }

        // Gerando ASTs
        console.log('ASTs geradas: \n');
        arvores.forEach(arvore => {
            printPosOrder(arvore.root, 1);
        });
    }
    ;

program
    : preproc_directives statements_list
    {
        arvores.push(new AST($2.node));
    }
    | statements_list
    {
        arvores.push(new AST($1.node));
    }
    ;

preproc_directives
    : preproc_directive
    { $$ = { node: $1.node }; }
    | preproc_directives preproc_directive
    { $$ = { node: new Node('PREPROC_DIRECTIVES', $1.node, $2.node) }; }
    ;

preproc_directive
    : INCLUDE LT STDIO_H GT
    { $$ = { node: new Node('INCLUDE', new Node('STDIO_H')) }; }
    | INCLUDE LT STDLIB_H GT
    { $$ = { node: new Node('INCLUDE', new Node('STDLIB_H')) }; }
    | INCLUDE LT IDF DOT IDF GT
    { $$ = { node: new Node('INCLUDE', new Node($3 + '.' + $5)) }; }
    | DEFINE IDF valor_lit
    { 
        definirConstante($2, $3.value);
        $$ = { node: new Node('DEFINE', new Node($2), $3.node) }; 
    }
    | DEFINE IDF
    { 
        definirConstante($2, null);
        $$ = { node: new Node('DEFINE', new Node($2)) }; 
    }
    ;

acesso_membro
    : IDF '.' IDF
    {
        verificaVariavel($1);
        $$ = {
            node: new Node('MEMBER_ACCESS', new Node($1), new Node($3)),
            stringValue: $1 + '.' + $3
        };
        // Gera TAC para acesso a membro
        criaTAC($$.stringValue, $1, $3, 'MEMBER_ACCESS');
    }
    | IDF '->' IDF
    {
        verificaVariavel($1);
        $$ = {
            node: new Node('PTR_MEMBER_ACCESS', new Node($1), new Node($3)),
            stringValue: $1 + '->' + $3
        };
        // Gera TAC para acesso via ponteiro
        let tempDeref = criaTemp();
        criaTAC(tempDeref, '*', $1, 'DEREF');
        criaTAC($$.stringValue, tempDeref, $3, 'MEMBER_ACCESS');
    }
    ;

/* Definindo múltiplas declarações */
statements_list
    : statement
    {
        arvores.push(new AST($1.node));
    }
    | statements_list statement
    {
        // Adiciona cada declaração à lista de ASTs
        arvores.push(new AST($2.node));
    }
    ;

statement
    : exp_stmt
    { $$ = { node: $1.node, stringValue: $1.stringValue, value: $1.value }; }
    | BREAK ';'
    { $$ = { node: new Node('BREAK') }; }
    | CONTINUE ';'
    { $$ = { node: new Node('CONTINUE') }; }
    | if_stmt
    { $$ = { node: $1.node, stringValue: $1.stringValue }; }
    | loop_stmt
    { $$ = { node: $1.node, stringValue: $1.stringValue }; }
    | switch_stmt
    { $$ = { node: $1.node }; }
    | statement_composto
    { $$ = { node: $1.node }; }
    | function_definition
    { $$ = { node: $1.node }; }
    | function_prototype
    { $$ = { node: $1.node }; }
    | return_stmt
    { $$ = { node: $1.node }; }
    | struct_decl
    { $$ = { node: $1.node }; }
    | union_decl
    { $$ = { node: $1.node }; }
    | enum_decl
    { $$ = { node: $1.node }; }
    | declaracao_variavel ';'  /* Isso já deve estar incluído via exp_stmt */
    ;

function_definition
    : tipo_var MAIN '(' ')' statement_composto
    { $$ = { node: new Node('MAIN_FUNCTION', new Node($1), $5.node) }; }
    | tipo_var IDF '(' parameter_list ')' statement_composto
    { $$ = { node: new Node('FUNCTION', new Node($1), new Node($2), $4.node, $6.node) }; }
    | VOID IDF '(' parameter_list ')' statement_composto
    { $$ = { node: new Node('VOID_FUNCTION', new Node($2), $4.node, $6.node) }; }
    | tipo_var MAIN '(' parameter_list ')' statement_composto
    { $$ = { node: new Node('MAIN_FUNCTION', new Node($1), $4.node, $6.node) }; }
    | VOID MAIN '(' parameter_list ')' statement_composto
    { $$ = { node: new Node('VOID_MAIN_FUNCTION', $4.node, $6.node) }; }
    | VOID MAIN '(' ')' statement_composto
    { $$ = { node: new Node('VOID_MAIN_FUNCTION', $5.node) }; }
    ;

function_prototype
    : tipo_var IDF '(' parameter_list ')' ';'
    { $$ = { node: new Node('FUNCTION_PROTOTYPE', new Node($1), new Node($2), $4.node) }; }
    | tipo_var IDF '(' ')' ';'
    { $$ = { node: new Node('FUNCTION_PROTOTYPE', new Node($1), new Node($2)) }; }
    | VOID IDF '(' parameter_list ')' ';'
    { $$ = { node: new Node('VOID_FUNCTION_PROTOTYPE', new Node($2), $4.node) }; }
    | VOID IDF '(' ')' ';'
    { $$ = { node: new Node('VOID_FUNCTION_PROTOTYPE', new Node($2)) }; }
    ;

parameter_list
    : parameter
    { $$ = { node: $1.node }; }
    | parameter_list ',' parameter
    { $$ = { node: new Node('PARAMETER_LIST', $1.node, $3.node) }; }
    ;

parameter
    : tipo_var IDF
    { $$ = { node: new Node('PARAMETER', new Node($1), new Node($2)) }; }
    | tipo_var MUL IDF
    { $$ = { node: new Node('POINTER_PARAMETER', new Node($1), new Node($3)) }; }
    ;

return_stmt
    : RETURN expressao_aritmetica ';'
    { $$ = { node: new Node('RETURN', $2.node) }; }
    | RETURN ';'
    { $$ = { node: new Node('RETURN') }; }
    ;

statement_composto
    : '{' statements_list '}'
    { $$ = { node: new Node('BLOCK', $2.node) }; }
    | '{' '}'
    { $$ = { node: new Node('EMPTY_BLOCK') }; }
    ;

/* Declaração de variável, atribuição de valor, expressão condicional */
exp_stmt
    : declaracao_variavel ';' 
    {$$ = {node: $1.node, stringValue: $1.stringValue, value: $1.value}}
    | expressao_atribuicao ';'
    {$$ = {node: $1.node, stringValue: $1.stringValue}}
    | expressao_condicional ';' 
    {$$ = {node: $1.node, stringValue: $1.stringValue}}
    | function_call ';'
    {$$ = {node: $1.node, stringValue: $1.stringValue}}
    ;

function_call
    : IDF '(' argument_list ')'
    { $$ = { node: new Node('FUNCTION_CALL', new Node($1), $3.node), stringValue: criaTemp() }; }
    | IDF '(' ')'
    { $$ = { node: new Node('FUNCTION_CALL', new Node($1)), stringValue: criaTemp() }; }
    | MALLOC '(' expressao_aritmetica ')'
    { $$ = { node: new Node('MALLOC', $3.node), stringValue: criaTemp() }; }
    | FREE '(' expressao_aritmetica ')'
    { $$ = { node: new Node('FREE', $3.node), stringValue: criaTemp() }; }
    | SCANF '(' string_lit ',' BITAND IDF ')'
    { $$ = { node: new Node('SCANF', $3.node, new Node($6)), stringValue: criaTemp() }; }
    | SCANF '(' FORMAT_D ',' BITAND IDF ')'
    { $$ = { node: new Node('SCANF', new Node('FORMAT_D'), new Node($6)), stringValue: criaTemp() }; }
    | PRINTF '(' string_lit ')'
    { $$ = { node: new Node('PRINTF', $3.node), stringValue: criaTemp() }; }
    | PRINTF '(' string_lit ',' argument_list ')'
    { $$ = { node: new Node('PRINTF', $3.node, $5.node), stringValue: criaTemp() }; }
    ;

string_lit
    : STRING_LIT
    { $$ = { node: new Node('STRING', new Node($1)), stringValue: $1 }; }
    ;

argument_list
    : expressao_aritmetica
    { $$ = { node: $1.node, stringValue: $1.stringValue }; }
    | argument_list ',' expressao_aritmetica
    { $$ = { node: new Node('ARG_LIST', $1.node, $3.node), stringValue: $1.stringValue + ',' + $3.stringValue }; }
    ;

/* Gramática do IF */
if_stmt
    : IF '(' expressao_condicional ')' statement
    {
        $$ = {
            type: 'IF',
            node: new Node('IF', $3.node, $5.node)
        };
    }
    | IF '(' expressao_condicional ')' statement_composto
    {
        $$ = {
            type: 'IF',
            node: new Node('IF', $3.node, $5.node)
        };
    }
    | IF '(' expressao_condicional ')' statement_composto ELSE statement
    {
        $$ = {
            type: 'IF_ELSE',
            node: new Node('IF_ELSE', $3.node, $5.node, $7.node)
        };
    }
    | IF '(' expressao_condicional ')' statement_composto ELSE statement_composto
    {
        $$ = {
            type: 'IF_ELSE',
            node: new Node('IF_ELSE', $3.node, $5.node, $7.node)
        };
    }
    | IF '(' expressao_condicional ')' statement ELSE statement
    {
        $$ = {
            type: 'IF_ELSE',
            node: new Node('IF_ELSE', $3.node, $5.node, $7.node)
        };
    }
    | IF '(' expressao_condicional ')' statement ELSE statement_composto
    {
        $$ = {
            type: 'IF_ELSE',
            node: new Node('IF_ELSE', $3.node, $5.node, $7.node)
        };
    }
    | IF '(' expressao_condicional ')' statement_composto ELSE if_stmt
    {
        $$ = {
            type: 'IF_ELSE_IF',
            node: new Node('IF_ELSE_IF', $3.node, $5.node, $7.node)
        };
    }
    | IF '(' expressao_condicional ')' statement ELSE if_stmt
    {
        $$ = {
            type: 'IF_ELSE_IF',
            node: new Node('IF_ELSE_IF', $3.node, $5.node, $7.node)
        };
    }
    ;

/* Gramática do SWITCH */
switch_stmt
    : SWITCH '(' expressao_aritmetica ')' '{' case_list '}'
    { $$ = { node: new Node('SWITCH', $3.node, $6.node) }; }
    | SWITCH '(' cast_exp ')' '{' case_list '}'
    { $$ = { node: new Node('SWITCH', $3.node, $6.node) }; }
    | SWITCH '(' expressao_condicional ')' '{' case_list '}'
    { $$ = { node: new Node('SWITCH', $3.node, $6.node) }; }
    ;

/* Lista de cases em um switch */
case_list
    : case_item
    { $$ = { node: $1.node }; }
    | case_list case_item
    { $$ = { node: new Node('CASE_LIST', $1.node, $2.node) }; }
    ;

/* Um único case ou default */
case_item
    : CASE valor_lit ':' 
    { $$ = { node: new Node('CASE', $2.node, null) }; }
    | CASE valor_lit ':' statements_list
    { $$ = { node: new Node('CASE', $2.node, $4.node) }; }
    | DEFAULT ':' 
    { $$ = { node: new Node('DEFAULT', null) }; }
    | DEFAULT ':' statements_list
    { $$ = { node: new Node('DEFAULT', $3.node) }; }
    ;

/* Gramática do WHILE e do FOR */
loop_stmt
    : WHILE '(' expressao_condicional ')' statement_composto
    {
        $$ = {
            type: 'WHILE',
            stringValue: criaTemp(),
            node: new Node('WHILE', $3.node, $5.node)
        };
        criaTACIf($3.stringValue, temp.indexOf($5.stringValue));
    }
    | WHILE '(' expressao_condicional ')' statement
    {
        $$ = {
            type: 'WHILE',
            stringValue: criaTemp(),
            node: new Node('WHILE', $3.node, $5.node)
        };
        criaTACIf($3.stringValue, temp.indexOf($5.stringValue));
    }
    | FOR '(' exp_stmt exp_stmt expressao_atribuicao ')' statement
    {
        $$ = {
            type: 'FOR',
            node: new Node('FOR', $3.node, $4.node, $5.node, $7.node)
        };
        criaTACIf($3.stringValue, temp.indexOf($7.stringValue));
    }
    | FOR '(' exp_stmt exp_stmt expressao_atribuicao ')' statement_composto
    {
        $$ = {
            type: 'FOR',
            node: new Node('FOR', $3.node, $4.node, $5.node, $7.node)
        };
        criaTACIf($3.stringValue, temp.indexOf($7.stringValue));
    }
    | DO statement_composto WHILE '(' expressao_condicional ')' ';'
    {
        $$ = {
            type: 'DO_WHILE',
            node: new Node('DO_WHILE', $2.node, $5.node)
        };
        criaTACIf($5.stringValue, temp.indexOf($2.stringValue));
    }
    | DO statement WHILE '(' expressao_condicional ')' ';'
    {
        $$ = {
            type: 'DO_WHILE',
            node: new Node('DO_WHILE', $2.node, $5.node)
        };
        criaTACIf($5.stringValue, temp.indexOf($2.stringValue));
    }
    ;

/* Valor literal */
valor_lit
    : INT_LIT
    {
        $$ = {
            type: 'INT_LIT',
            value: parseInt($1),
            stringValue: $1,
            node: new Node('INT_LIT', new Node($1))
        };
    }
    | F_LIT
    {
        $$ = {
            type: 'F_LIT',
            value: parseFloat($1),
            stringValue: $1,
            node: new Node('F_LIT', new Node($1))
        };
    }
    | CHAR_LIT
    {
        $$ = {
            type: 'CHAR_LIT',
            value: $1.charCodeAt(1),
            stringValue: $1,
            node: new Node('CHAR_LIT', new Node($1))
        };
    }
    | STRING_LIT  // Adicione esta regra para strings
    {
        $$ = {
            type: 'STRING_LIT',
            value: $1,
            stringValue: $1,
            node: new Node('STRING_LIT', new Node($1))
        };
    }
    ;

/* Tipo da variável */
tipo_var
    : INT 
    {$$ = 'int';}
    | DOUBLE 
    {$$ = 'double';}
    | FLOAT 
    {$$ = 'float';}
    | CHAR 
    {$$ = 'char';}
    | UNSIGNED INT
    {$$ = 'unsigned int';}
    | SIGNED INT
    {$$ = 'signed int';}
    | LONG INT
    {$$ = 'long int';}
    | SHORT INT
    {$$ = 'short int';}
    | CONST INT
    {$$ = 'const int';}
    | VOLATILE INT
    {$$ = 'volatile int';}
    | REGISTER INT
    {$$ = 'register int';}
    | STRUCT IDF
    {$$ = 'struct ' + $2;}
    | UNION IDF
    {$$ = 'union ' + $2;}
    | ENUM IDF
    {$$ = 'enum ' + $2;}
    ;

/* Declaração de variável com ou sem inicialização, incluindo arrays e ponteiros */
declaracao_variavel
    : tipo_var lista_ids
    {
        $$ = {
            node: $2.node,
            value: $2.value,
            stringValue: $2.stringValue
        };
    }
    | tipo_var MUL lista_ids
    {
        $$ = {
            node: new Node('POINTER_DECL', new Node($1), $3.node),
            value: $3.value,
            stringValue: $3.stringValue
        };
    }
    | tipo_var QUOTE IDF QUOTE
    {
        // Support for character literal declarations (like 'const int 'A';')
        let varName = '_char_const_' + $3;
        criarVariavel($1, varName, $3.charCodeAt(0));
        $$ = {
            node: new Node('CHAR_CONST_DECL', new Node($1), new Node($3)),
            value: $3.charCodeAt(0),
            stringValue: varName
        };
    }
    | STRUCT IDF IDF '=' '{' struct_init_list '}'  // Declaração com inicialização
    {
        let tipo = 'struct ' + $2;
        criarVariavel(tipo, $3, $6.value);
        $$ = {
            node: new Node('STRUCT_INIT_DECL', new Node(tipo), new Node($3), $6.node),
            value: $6.value,
            stringValue: $3
        };
    }
    | UNION IDF IDF  // Declaração simples de union
    {
        $$ = {
            node: new Node('UNION_DECL', new Node('union ' + $2), new Node($3)),
            value: null,
            stringValue: $3
        };
        criarVariavel('union ' + $2, $3, null);
    }
    | UNION IDF IDF '=' '{' union_init_list '}' ';'  // Declaração com inicialização
    {
        $$ = {
            node: new Node('UNION_INIT_DECL', new Node('union ' + $2), new Node($3), $6.node),
            value: $6.value,
            stringValue: $3
        };
        criarVariavel('union ' + $2, $3, $6.value);
    }
    ;

union_init_list
    : valor_lit
    {
        $$ = {
            node: new Node('UNION_INIT_VALUE', $1.node),
            value: $1.value,
            stringValue: $1.stringValue
        };
    }
    ;

lista_ids
    : IDF 
    {
        criarVariavel($0, $1, null);
        $$ = {
            node: new Node('DECL', new Node($0), new Node($1)),
            value: null,
            stringValue: $1
        };
    }
    | CHAR_LIT
    {
        // Support for character literal constant declarations (like 'const int 'A';')
        let varName1 = '_char_const_' + $1.replace(/'/g, '');
        criarVariavel($0, varName1, $1.charCodeAt(1));
        $$ = {
            node: new Node('CHAR_CONST_DECL', new Node($0), new Node($1)),
            value: $1.charCodeAt(1),
            stringValue: varName1
        };
    }
    | QUOTE IDF QUOTE
    {
        // Support for character literal using quotes (like 'const int 'A';')
        let varName2 = '_char_const_' + $2;
        criarVariavel($0, varName2, $2.charCodeAt(0));
        $$ = {
            node: new Node('CHAR_CONST_DECL', new Node($0), new Node($2)),
            value: $2.charCodeAt(0),
            stringValue: varName2
        };
    }
    | IDF '=' expressao_aritmetica
    {
        criarVariavel($0, $1, $3.value);
        $$ = {
            node: new Node('=', new Node($1), $3.node),
            value: $3.value,
            stringValue: $3.stringValue
        };
    }
    | IDF '[' INT_LIT ']'
    {
        criarVariavel($0+'[]', $1, null);
        $$ = {
            node: new Node('ARRAY_DECL', new Node($0), new Node($1), new Node($3)),
            value: null,
            stringValue: $1
        };
    }
    | IDF '[' INT_LIT ']' '[' INT_LIT ']'  // Para arrays bidimensionais
    {
        criarVariavel($0+'[][]', $1, null);
        $$ = {
            node: new Node('ARRAY2D_DECL', new Node($0), new Node($1), new Node($3), new Node($6)),
            value: null,
            stringValue: $1
        };
    }
    | IDF '[' INT_LIT ']' '[' INT_LIT ']' '=' '{' array_init_list_2d '}'
    {
        criarVariavel($0+'[][]', $1, $9.value);
        $$ = {
            node: new Node('ARRAY2D_INIT', new Node($0), new Node($1), new Node($3), new Node($6), $9.node),
            value: $9.value,
            stringValue: $1
        };
    }
    | IDF '[' IDF ']'
    {
        // Verifica se o identificador é uma definição
        let valDef1 = obterValorDefinicao($3);
        if (valDef1 !== undefined) {
            criarVariavel($0+'[]', $1, null);
            $$ = {
                node: new Node('ARRAY_DECL', new Node($0), new Node($1), new Node(valDef1)),
                value: null,
                stringValue: $1
            };
        } else {
            erros.push("Identificador '" + $3 + "' não é uma constante definida");
            $$ = {
                node: new Node('ARRAY_DECL_ERROR'),
                value: null,
                stringValue: $1
            };
        }
    }
    | IDF '[' INT_LIT ']' '=' '{' array_init '}'
    {
        criarVariavel($0+'[]', $1, $7.value);
        $$ = {
            node: new Node('ARRAY_INIT', new Node($0), new Node($1), new Node($3), $7.node),
            value: $7.value,
            stringValue: $1
        };
    }
    | IDF '[' IDF ']' '=' '{' array_init '}'
    {
        // Verifica se o identificador é uma definição
        let valDef2 = obterValorDefinicao($5);
        if (valDef2 !== undefined) {
            criarVariavel($0+'[]', $1, $7.value);
            $$ = {
                node: new Node('ARRAY_INIT', new Node($0), new Node($1), new Node(valDef2), $7.node),
                value: $7.value,
                stringValue: $1
            };
        } else {
            erros.push("Identificador '" + $5 + "' não é uma constante definida");
            $$ = {
                node: new Node('ARRAY_INIT_ERROR'),
                value: null,
                stringValue: $1
            };
        }
    }

    | lista_ids ',' IDF
    {
        criarVariavel($0, $3, null);
        $$ = {
            node: new Node('MULTI_DECL', $1.node, new Node($3)),
            value: null,
            stringValue: $1.stringValue + ',' + $3
        };
    }
    | lista_ids ',' IDF '=' expressao_aritmetica
    {
        criarVariavel($0, $3, $5.value);
        $$ = {
            node: new Node('MULTI_DECL', $1.node, new Node('=', new Node($3), $5.node)),
            value: null,
            stringValue: $1.stringValue + ',' + $3
        };
    }
    | lista_ids ',' IDF '[' INT_LIT ']'
    {
        criarVariavel($0+'[]', $3, null);
        $$ = {
            node: new Node('MULTI_DECL', $1.node, new Node('ARRAY_DECL', new Node($0), new Node($3), new Node($5))),
            value: null,
            stringValue: $1.stringValue + ',' + $3
        };
    }
    | lista_ids ',' IDF '[' IDF ']'
    {
        // Verifica se o identificador é uma definição
        let valDef3 = obterValorDefinicao($5);
        if (valDef3 !== undefined) {
            criarVariavel($0+'[]', $3, null);
            $$ = {
                node: new Node('MULTI_DECL', $1.node, new Node('ARRAY_DECL', new Node($0), new Node($3), new Node(valDef3))),
                value: null,
                stringValue: $1.stringValue + ',' + $3
            };
        } else {
            erros.push("Identificador '" + $5 + "' não é uma constante definida");
            $$ = {
                node: new Node('ARRAY_DECL_ERROR'),
                value: null,
                stringValue: $1.stringValue + ',' + $3
            };
        }
    }
    | lista_ids ',' IDF '[' INT_LIT ']' '=' '{' array_init '}'
    {
        criarVariavel($0+'[]', $3, $9.value);
        $$ = {
            node: new Node('MULTI_DECL', $1.node, new Node('ARRAY_INIT', new Node($0), new Node($3), new Node($5), $9.node)),
            value: null,
            stringValue: $1.stringValue + ',' + $3
        };
    }
    | lista_ids ',' IDF '[' IDF ']' '=' '{' array_init '}'
    {
        // Verifica se o identificador é uma definição
        let valDef4 = obterValorDefinicao($5);
        if (valDef4 !== undefined) {
            criarVariavel($0+'[]', $3, $9.value);
            $$ = {
                node: new Node('MULTI_DECL', $1.node, new Node('ARRAY_INIT', new Node($0), new Node($3), new Node(valDef4), $9.node)),
                value: null,
                stringValue: $1.stringValue + ',' + $3
            };
        } else {
            erros.push("Identificador '" + $5 + "' não é uma constante definida");
            $$ = {
                node: new Node('ARRAY_INIT_ERROR'),
                value: null,
                stringValue: $1.stringValue + ',' + $3
            };
        }
    }
    
    ;

array_init
    : valor_lit
    {
        $$ = {
            node: new Node($1),
            value: [$1],
            stringValue: $1.stringValue
        };
    }
    | '{' array_init_list '}'  // Para arrays unidimensionais
    {
        $$ = {
            node: new Node('ARRAY_INIT_LIST', $2.node),
            value: $2.value,
            stringValue: $2.stringValue
        };
    }
    | '{' array_init_list_2d '}'  // Para arrays bidimensionais
    {
        $$ = {
            node: new Node('ARRAY2D_INIT_LIST', $2.node),
            value: $2.value,
            stringValue: $2.stringValue
        };
    }
    | array_init ',' valor_lit
    {
        $1.value.push($3);
        $$ = {
            node: new Node('ARRAY_ELEMS', $1.node, new Node($3)),
            value: $1.value,
            stringValue: $1.stringValue
        };
    }
    ;
array_init_list
    : array_init
    {
        $$ = {
            node: $1.node,
            value: [$1.value],
            stringValue: $1.stringValue
        };
    }
    | array_init_list ',' array_init
    {
        $1.value.push($3.value);
        $$ = {
            node: new Node('ARRAY2D_ELEMS', $1.node, $3.node),
            value: $1.value,
            stringValue: $1.stringValue
        };
    }
    ;
array_init_list_2d
    : '{' array_init_list '}'  // Uma linha da matriz
    {
        $$ = {
            node: new Node('ARRAY2D_ROW', $2.node),
            value: [$2.value],
            stringValue: $2.stringValue
        };
    }
    | array_init_list_2d ',' '{' array_init_list '}'  // Múltiplas linhas
    {
        $1.value.push($4.value);
        $$ = {
            node: new Node('ARRAY2D_ROWS', $1.node, $4.node),
            value: $1.value,
            stringValue: $1.stringValue
        };
    }
    ;
/* Expressão de atribuição */
expressao_atribuicao
    : IDF '=' expressao_aritmetica
    {
        verificaVariavel($1);
        $$ = {
            node: new Node('=', new Node($1), $3.node),
            stringValue: criaTemp()
        };
        criaTAC($$.stringValue, $1, $3.stringValue, '=');
    }
    | IDF '=' malloc_exp
    {
        verificaVariavel($1);
        $$ = {
            node: new Node('=', new Node($1), $3.node),
            stringValue: criaTemp()
        };
        criaTAC($$.stringValue, $1, $3.stringValue, '=');
    }
    | IDF '=' cast_exp
    {
        verificaVariavel($1);
        $$ = {
            node: new Node('=', new Node($1), $3.node),
            stringValue: criaTemp()
        };
        criaTAC($$.stringValue, $1, $3.stringValue, '=');
    }
    | IDF '=' QUOTE IDF QUOTE
    {
        verificaVariavel($1);
        $$ = {
            node: new Node('=', new Node($1), new Node('CHAR_LIT', new Node($4))),
            stringValue: criaTemp()
        };
        criaTAC($$.stringValue, $1, "'" + $4 + "'", '=');
    }
    | IDF '=' CHAR_LIT
    {
        verificaVariavel($1);
        $$ = {
            node: new Node('=', new Node($1), new Node('CHAR_LIT', new Node($3))),
            stringValue: criaTemp()
        };
        criaTAC($$.stringValue, $1, $3, '=');
    }
    | acesso_array '=' expressao_aritmetica
    {
        $$ = {
            node: new Node('=', $1.node, $3.node),
            stringValue: criaTemp()
        };
        criaTAC($$.stringValue, $1.stringValue, $3.stringValue, '=');
    }
    | expressao_in_decrement
    {
        $$ = {
            node: $1.node,
            stringValue: $1.stringValue
        };
    }
    | IDF '=' '{' struct_init_list '}'  // Inicialização de struct
    {
        verificaVariavel($1);
        $$ = {
            node: new Node('STRUCT_INIT', new Node($1), $4.node),
            stringValue: criaTemp()
        };
    }
    | IDF '=' '{' union_init_list '}'  /* Inicialização de union */
    {
        verificaVariavel($1);
        $$ = {
            node: new Node('UNION_INIT', new Node($1), $4.node),
            stringValue: criaTemp()
        };
    }
    | acesso_membro '=' expressao_aritmetica
    {
        $$ = {
            node: new Node('=', $1.node, $3.node),
            stringValue: criaTemp()
        };
        criaTAC($$.stringValue, $1.stringValue, $3.stringValue, '=');
    }
    ;

struct_init_list
    : expressao_aritmetica
    {
        $$ = {
            node: new Node('STRUCT_INIT_VALUE', $1.node),
            value: [$1.value],
            stringValue: $1.stringValue
        };
    }
    | string_lit  // Para strings na inicialização
    {
        $$ = {
            node: new Node('STRUCT_INIT_STRING', $1.node),
            value: $1.value,
            stringValue: $1.stringValue
        };
    }
    | struct_init_list ',' expressao_aritmetica
    {
        $1.value.push($3.value);
        $$ = {
            node: new Node('STRUCT_INIT_VALUES', $1.node, $3.node),
            value: $1.value,
            stringValue: $1.stringValue
        };
    }
    | struct_init_list ',' string_lit
    {
        $1.value.push($3.value);
        $$ = {
            node: new Node('STRUCT_INIT_VALUES', $1.node, $3.node),
            value: $1.value,
            stringValue: $1.stringValue
        };
    }
    ;

malloc_exp
    : MALLOC '(' expressao_aritmetica ')'
    {
        $$ = {
            node: new Node('MALLOC', $3.node),
            stringValue: criaTemp()
        };
        criaTAC($$.stringValue, 'MALLOC', $3.stringValue, 'CALL');
    }
    | MALLOC '(' IDF MUL SIZEOF '(' tipo_var ')' ')'
    {
        $$ = {
            node: new Node('MALLOC_SIZEOF', new Node($3), new Node($7)),
            stringValue: criaTemp()
        };
        criaTAC($$.stringValue, 'MALLOC_SIZEOF', $3 + '*sizeof(' + $7 + ')', 'CALL');
    }
    | MALLOC '(' IDF MUL SIZEOF '(' tipo_var MUL ')' ')'
    {
        $$ = {
            node: new Node('MALLOC_SIZEOF', new Node($3), new Node($7 + '*')),
            stringValue: criaTemp()
        };
        criaTAC($$.stringValue, 'MALLOC_SIZEOF', $3 + '*sizeof(' + $7 + '*)', 'CALL');
    }
    ;

cast_exp
    : '(' tipo_var ')' fator
      {
        $$ = {
            type: 'CAST',
            stringValue: criaTemp(),
            node: new Node('CAST', new Node($2), $4.node)
        };
        criaTAC($$.stringValue, $4.stringValue, $2, 'CAST');
      }
    ;

acesso_array
    : IDF '[' expressao_aritmetica ']'  // Acesso unidimensional
    {
        verificaVariavel($1);
        $$ = {
            node: new Node('ARRAY_ACCESS', new Node($1), $3.node),
            stringValue: criaTemp()
        };
        criaTAC($$.stringValue, $1, $3.stringValue, 'ARRAY_ACCESS');
    }
    | IDF '[' expressao_aritmetica ']' '[' expressao_aritmetica ']'  // Acesso bidimensional
    {
        verificaVariavel($1);
        $$ = {
            node: new Node('ARRAY2D_ACCESS', new Node($1), $3.node, $6.node),
            stringValue: criaTemp()
        };
        let tempIndex1 = criaTemp();
        let tempOffset = criaTemp();
        // Cálculo do offset para array bidimensional: base + (i * colunas + j) * tamanho_do_elemento
        criaTAC(tempIndex1, $3.stringValue, $6.stringValue, 'MUL');
        criaTAC(tempOffset, tempIndex1, 'sizeof(int)', 'MUL'); // Assumindo int por simplicidade
        criaTAC($$.stringValue, $1, tempOffset, 'ADD');
    }
    ;


expressao_in_decrement
    : IDF INC
    {
        verificaVariavel($1);
        $$ = {
            node: new Node('++', new Node($1)),
            stringValue: criaTemp()
        };
        criaTAC($$.stringValue, $1, '1', '++');
    }
    | IDF DEC
    {
        verificaVariavel($1);
        $$ = {
            node: new Node('--', new Node($1)),
            stringValue: criaTemp()
        };
        criaTAC($$.stringValue, $1, '1', '--');
    }
    | IDF ADD_ASSIGN expressao_aritmetica
    {
        verificaVariavel($1);
        $$ = {
            node: new Node('+=', new Node($1), $3.node),
            stringValue: criaTemp()
        };
        criaTAC($$.stringValue, $1, $3.stringValue, '+=');
    }
    | IDF SUB_ASSIGN expressao_aritmetica
    {
        verificaVariavel($1);
        $$ = {
            node: new Node('-=', new Node($1), $3.node),
            stringValue: criaTemp()
        };
        criaTAC($$.stringValue, $1, $3.stringValue, '-=');
    }
    | acesso_array ADD_ASSIGN expressao_aritmetica
    {
        $$ = {
            node: new Node('+=', $1.node, $3.node),
            stringValue: criaTemp()
        };
        criaTAC($$.stringValue, $1.stringValue, $3.stringValue, '+=');
    }
    | acesso_array SUB_ASSIGN expressao_aritmetica
    {
        $$ = {
            node: new Node('-=', $1.node, $3.node),
            stringValue: criaTemp()
        };
        criaTAC($$.stringValue, $1.stringValue, $3.stringValue, '-=');
    }
    ;

expressao_aritmetica
    : termo
    {   
        $$ = {
            type: $1.type,
            value: $1.value,
            stringValue: $1.stringValue,
            node: $1.node
        }
    }
    | expressao_aritmetica SUM termo
    {   
        $$ = {
            type: 'SUM',
            value: $1.value + $3.value,
            stringValue: criaTemp(),
            node: new Node('+', $1.node, $3.node)
        };
        criaTAC($$.stringValue, $1.stringValue, $3.stringValue, 'SUM');
    }
    | expressao_aritmetica SUB termo
    {   
        $$ = {
            type: 'SUB',
            value: $1.value - $3.value,
            stringValue: criaTemp(),
            node: new Node('-', $1.node, $3.node)
        };
        criaTAC($$.stringValue, $1.stringValue, $3.stringValue, 'SUB');
    }
    | sizeof_exp
    {
        $$ = {
            type: 'SIZEOF',
            value: $1.value,
            stringValue: $1.stringValue,
            node: $1.node
        }
    }
    ;

sizeof_exp
    : SIZEOF '(' tipo_var ')'
    {
        $$ = {
            type: 'SIZEOF',
            value: 0, // Valor padrão
            stringValue: criaTemp(),
            node: new Node('SIZEOF', new Node($3))
        };
        criaTAC($$.stringValue, 'SIZEOF', $3, 'UNARY');
    }
    | SIZEOF '(' tipo_var MUL ')'
    {
        $$ = {
            type: 'SIZEOF',
            value: 0, // Valor padrão
            stringValue: criaTemp(),
            node: new Node('SIZEOF', new Node($3 + '*'))
        };
        criaTAC($$.stringValue, 'SIZEOF', $3 + '*', 'UNARY');
    }
    ;

termo
    : fator
    {   
        $$ = {
            type: $1.type,
            value: $1.value,
            stringValue: $1.stringValue,
            node: $1.node
        }
    }
    | termo MUL fator
    {   
        $$ = {
            type: 'MUL',
            value: $1.value * $3.value,
            stringValue: criaTemp(),
            node: new Node('*', $1.node, $3.node)
        };
        criaTAC($$.stringValue, $1.stringValue, $3.stringValue, 'MUL');
    }
    | termo MUL sizeof_exp
    {   
        $$ = {
            type: 'MUL',
            value: 0, // Valor padrão
            stringValue: criaTemp(),
            node: new Node('MUL', $1.node, $3.node)
        };
        criaTAC($$.stringValue, $1.stringValue, $3.stringValue, 'MUL');
    }
    | termo DIV fator
    {   
        $$ = {
            type: 'DIV',
            value: $1.value / $3.value,
            stringValue: criaTemp(),
            node: new Node('/', $1.node, $3.node)
        };
        criaTAC($$.stringValue, $1.stringValue, $3.stringValue, 'DIV');
    }
    | termo MOD fator
    {   
        $$ = {
            type: 'MOD',
            value: $1.value % $3.value,
            stringValue: criaTemp(),
            node: new Node('%', $1.node, $3.node)
        };
        criaTAC($$.stringValue, $1.stringValue, $3.stringValue, 'MOD');
    }
    ;

fator
    : IDF
      {
        // Verificar se o identificador é uma constante definida
        let defValue = obterValorDefinicao($1);
        if (defValue !== undefined) {
            $$ = {
                type: 'DEFINE_CONST',
                value: defValue,
                stringValue: $1,
                node: new Node('DEFINE_CONST', new Node($1))
            };
        } else {
            // Se não for uma constante, então é uma variável normal
            verificaVariavel($1);
            $$ = {
                type: 'IDF',
                value: $1,
                stringValue: $1,
                node: new Node('IDF')
            };
        }
      }
    | valor_lit 
      {$$ = {type: $1.type, stringValue: $1.stringValue, node: $1.node, value: $1.value};}
    | '(' expressao_aritmetica ')'
      {$$ = $2;}
    | acesso_array
      {$$ = $1;}
    | SUB fator
      {
        $$ = {
            type: 'UNARY_MINUS',
            value: -$2.value,
            stringValue: criaTemp(),
            node: new Node('UNARY_MINUS', $2.node)
        };
        criaTACUnaryOp('-', $2.stringValue, $$.stringValue);
      }
    | cast_exp
      {$$ = $1;}
    | MUL fator
      {
        $$ = {
            type: 'DEREF',
            stringValue: criaTemp(),
            node: new Node('DEREF', $2.node)
        };
        criaTAC($$.stringValue, '*', $2.stringValue, 'DEREF');
      }
    | BITAND fator
      {
        $$ = {
            type: 'ADDR',
            stringValue: criaTemp(),
            node: new Node('ADDR', $2.node)
        };
        criaTAC($$.stringValue, '&', $2.stringValue, 'ADDR');
      }
    | sizeof_exp
      {$$ = $1;}
    | '(' tipo_var MUL ')' malloc_exp
    {
        $$ = {
            type: 'CAST_MALLOC',
            stringValue: criaTemp(),
            node: new Node('CAST_MALLOC', new Node($2 + '*'), $5.node)
        };
        criaTAC($$.stringValue, $5.stringValue, $2 + '*', 'CAST');
    }
    | acesso_membro
    { $$ = $1; }
    ;

expressao_condicional
    : expressao_or
    | NOT expressao_condicional
    {
        $$ = {
            type: 'NOT',
            value: !$2.value,
            stringValue: criaTemp(),
            node: new Node('NOT', $2.node)
        };
        criaTACUnaryOp('!', $2.stringValue, $$.stringValue);
    }
    | expressao_primaria operador_relacional expressao_primaria
    {
        $$ = {
            type: $2,
            value: $1.value + $2 + $3.value,
            stringValue: criaTemp(),
            node: new Node($2, $1.node, $3.node)
        };
        criaTAC($$.stringValue, $1.stringValue, $3.stringValue, $2);
    }
    | expressao_primaria operador_relacional SUB INT_LIT MUL IDF
    {
        $$ = {
            type: $2,
            value: $1.value + $2 + (-parseInt($4)),
            stringValue: criaTemp(),
            node: new Node($2, $1.node, new Node('UNARY_MINUS', new Node('MUL', new Node($4), new Node($6))))
        };
        let tempMulCond1 = criaTemp();
        criaTAC(tempMulCond1, $4, $6, 'MUL');
        let tempNegCond1 = criaTemp();
        criaTACUnaryOp('-', tempMulCond1, tempNegCond1);
        criaTAC($$.stringValue, $1.stringValue, tempNegCond1, $2);
    }
    | expressao_condicional AND expressao_condicional
    {
        $$ = {
            type: 'AND',
            value: $1.value && $3.value,
            stringValue: criaTemp(),
            node: new Node('AND', $1.node, $3.node)
        };
        criaTAC($$.stringValue, $1.stringValue, $3.stringValue, 'AND');
    }
    | expressao_condicional OR expressao_condicional
    {
        $$ = {
            type: 'OR',
            value: $1.value || $3.value,
            stringValue: criaTemp(),
            node: new Node('OR', $1.node, $3.node)
        };
        criaTAC($$.stringValue, $1.stringValue, $3.stringValue, 'OR');
    }
    | expressao_aritmetica operador_relacional expressao_aritmetica
    {
        $$ = {
            type: $2,
            value: $1.value + $2 + $3.value,
            stringValue: criaTemp(),
            node: new Node($2, $1.node, $3.node)
        };
        criaTAC($$.stringValue, $1.stringValue, $3.stringValue, $2);
    }
    | IDF operador_relacional expressao_aritmetica
    {
        verificaVariavel($1);
        $$ = {
            type: $2,
            value: $1 + $2 + $3.value,
            stringValue: criaTemp(),
            node: new Node($2, new Node($1), $3.node)
        };
        criaTAC($$.stringValue, $1, $3.stringValue, $2);
    }
    | expressao_aritmetica operador_relacional IDF
    {
        verificaVariavel($3);
        $$ = {
            type: $2,
            value: $1.value + $2 + $3,
            stringValue: criaTemp(),
            node: new Node($2, $1.node, new Node($3))
        };
        criaTAC($$.stringValue, $1.stringValue, $3, $2);
    }
    | IDF operador_relacional SUB INT_LIT MUL IDF
    {
        verificaVariavel($1);
        $$ = {
            type: $2,
            value: $1 + $2 + (-parseInt($4)),
            stringValue: criaTemp(),
            node: new Node($2, new Node($1), new Node('UNARY_MINUS', new Node('MUL', new Node($4), new Node($6))))
        };
        let tempMulCond2 = criaTemp();
        criaTAC(tempMulCond2, $4, $6, 'MUL');
        let tempNegCond2 = criaTemp();
        criaTACUnaryOp('-', tempMulCond2, tempNegCond2);
        criaTAC($$.stringValue, $1, tempNegCond2, $2);
    }
    | IDF EQ SUB INT_LIT MUL IDF
    {
        verificaVariavel($1);
        verificaVariavel($6);
        $$ = {
            type: 'EQ',
            value: $1 + '==' + (-parseInt($4)),
            stringValue: criaTemp(),
            node: new Node('==', new Node($1), new Node('UNARY_MINUS', new Node('MUL', new Node($4), new Node($6))))
        };
        let tempMulCond3 = criaTemp();
        criaTAC(tempMulCond3, $4, $6, 'MUL');
        let tempNegCond3 = criaTemp();
        criaTACUnaryOp('-', tempMulCond3, tempNegCond3);
        criaTAC($$.stringValue, $1, tempNegCond3, '==');
    }
    ;

expressao_or
    : expressao_and
    | expressao_or OR expressao_and
    {
        $$ = {
            type: 'OR',
            value: $1.value || $3.value,
            stringValue: criaTemp(),
            node: new Node('OR', $1.node, $3.node)
        };
        criaTAC($$.stringValue, $1.stringValue, $3.stringValue, 'OR');
    }
    ;

expressao_and
    : expressao_relacional_ou_termo
    | expressao_and AND expressao_relacional_ou_termo
    {
        $$ = {
            type: 'AND',
            value: $1.value && $3.value,
            stringValue: criaTemp(),
            node: new Node('AND', $1.node, $3.node)
        };
        criaTAC($$.stringValue, $1.stringValue, $3.stringValue, 'AND');
    }
    ;

expressao_relacional_ou_termo
    : expressao_primaria
    | expressao_relacional
    | '(' expressao_condicional ')'
    {
        $$ = $2;
    }
    | IDF EQ NULL
    {
        verificaVariavel($1);
        $$ = {
            type: 'EQ_NULL',
            value: false, // Valor padrão
            stringValue: criaTemp(),
            node: new Node('==', new Node($1), new Node('NULL'))
        };
        criaTAC($$.stringValue, $1, 'NULL', 'EQ');
    }
    | IDF EQ SUB INT_LIT
    {
        verificaVariavel($1);
        $$ = {
            type: 'EQ_NEG',
            value: false, // Valor padrão
            stringValue: criaTemp(),
            node: new Node('==', new Node($1), new Node('-' + $4))
        };
        criaTAC($$.stringValue, $1, '-' + $4, 'EQ');
    }
    | NOT fator
    {
        $$ = {
            type: 'NOT',
            value: !$2.value,
            stringValue: criaTemp(),
            node: new Node('NOT', $2.node)
        };
        criaTACUnaryOp('!', $2.stringValue, $$.stringValue);
    }
    | NOT '(' expressao_condicional ')'
    {
        $$ = {
            type: 'NOT',
            value: !$3.value,
            stringValue: criaTemp(),
            node: new Node('NOT', $3.node)
        };
        criaTACUnaryOp('!', $3.stringValue, $$.stringValue);
    }
    ;

expressao_primaria
    : expressao_aritmetica
    {
        $$ = $1;
    }
    | SUB INT_LIT MUL IDF
    {
        $$ = {
            type: 'UNARY_MINUS_MUL',
            value: -parseInt($2) * 1, // placeholder for IDF value
            stringValue: criaTemp(),
            node: new Node('UNARY_MINUS_MUL', new Node($2), new Node($4))
        };
        let tempMulPrim = criaTemp();
        criaTAC(tempMulPrim, $2, $4, 'MUL');
        criaTACUnaryOp('-', tempMulPrim, $$.stringValue);
    }
    ;

expressao_relacional
    : expressao_primaria operador_relacional expressao_primaria
    {
        $$ = {
            type: $2,
            value: $1.value + $2 + $3.value,
            stringValue: criaTemp(),
            node: new Node($2, $1.node, $3.node)
        };
        criaTAC($$.stringValue, $1.stringValue, $3.stringValue, $2);
    }
    | expressao_primaria operador_relacional SUB expressao_primaria
    {
        $$ = {
            type: $2,
            value: $1.value + $2 + (-$4.value),
            stringValue: criaTemp(),
            node: new Node($2, $1.node, new Node('UNARY_MINUS', $4.node))
        };
        let tempNegRel = criaTemp();
        criaTACUnaryOp('-', $4.stringValue, tempNegRel);
        criaTAC($$.stringValue, $1.stringValue, tempNegRel, $2);
    }
    | expressao_primaria operador_relacional SUB INT_LIT MUL IDF
    {
        $$ = {
            type: $2,
            value: $1.value + $2 + (-parseInt($4)),
            stringValue: criaTemp(),
            node: new Node($2, $1.node, new Node('UNARY_MINUS', new Node('MUL', new Node($4), new Node($6))))
        };
        let tempMulRel = criaTemp();
        criaTAC(tempMulRel, $4, $6, 'MUL');
        let tempNegRel2 = criaTemp();
        criaTACUnaryOp('-', tempMulRel, tempNegRel2);
        criaTAC($$.stringValue, $1.stringValue, tempNegRel2, $2);
    }
    | expressao_primaria operador_relacional SUB IDF
    {
        verificaVariavel($4);
        $$ = {
            type: $2,
            value: $1.value + $2 + ('-' + $4),
            stringValue: criaTemp(),
            node: new Node($2, $1.node, new Node('UNARY_MINUS', new Node($4)))
        };
        let tempNegRel3 = criaTemp();
        criaTACUnaryOp('-', $4, tempNegRel3);
        criaTAC($$.stringValue, $1.stringValue, tempNegRel3, $2);
    }
    ;

operador_relacional
    : LE | GE | EQ | NE | GT | LT
    {$$ = $1}
    ;

/* Adicionar as regras para struct, union e enum após a declaração de variáveis */

/* Structs */
struct_decl
    : STRUCT IDF '{' struct_member_list '}' ';'
    { $$ = { node: new Node('STRUCT_DECL', new Node($2), $4.node) }; }
    | STRUCT IDF '{' struct_member_list '}' IDF ';'
    { 
        $$ = { 
            node: new Node('STRUCT_WITH_VAR', new Node($2), $4.node, new Node($6)) 
        }; 
    }
    ;

struct_member_list
    : struct_member
    { $$ = { node: $1.node }; }
    | struct_member_list struct_member
    { $$ = { node: new Node('STRUCT_MEMBERS', $1.node, $2.node) }; }
    ;

struct_member
    : tipo_var IDF ';'
    { $$ = { node: new Node('STRUCT_MEMBER', new Node($1), new Node($2)) }; }
    | tipo_var IDF '[' INT_LIT ']' ';'
    { $$ = { node: new Node('STRUCT_ARRAY_MEMBER', new Node($1), new Node($2), new Node($4)) }; }
    ;

/* Unions */
union_decl
    : UNION IDF '{' struct_member_list '}' ';'  /* Definição de union */
    { 
        $$ = { 
            node: new Node('UNION_DEF', new Node($2), $4.node) 
        };
    }
    | UNION IDF '{' struct_member_list '}' IDF ';'  /* Definição com declaração */
    { 
        $$ = { 
            node: new Node('UNION_DEF_WITH_VAR', new Node($2), $4.node, new Node($6)) 
        };
        criarVariavel('union ' + $2, $6, null);
    }
    ;

/* Enums */
enum_decl
    : ENUM IDF '{' enum_member_list '}' ';'
    { $$ = { node: new Node('ENUM_DECL', new Node($2), $4.node) }; }
    | ENUM IDF '{' enum_member_list '}' IDF ';'
    { 
        $$ = { 
            node: new Node('ENUM_WITH_VAR', new Node($2), $4.node, new Node($6)) 
        }; 
    }
    ;

enum_member_list
    : enum_member
    { $$ = { node: $1.node }; }
    | enum_member_list ',' enum_member
    { $$ = { node: new Node('ENUM_MEMBERS', $1.node, $3.node) }; }
    ;

enum_member
    : IDF
    { $$ = { node: new Node('ENUM_MEMBER', new Node($1)) }; }
    | IDF '=' INT_LIT
    { $$ = { node: new Node('ENUM_MEMBER_VALUE', new Node($1), new Node($3)) }; }
    ;
