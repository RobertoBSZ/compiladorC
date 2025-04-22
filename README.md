# Documentação do Projeto: Compilador C Simplificado com Jison

Este projeto implementa um compilador para uma linguagem baseada em C, utilizando a biblioteca [Jison](https://zaach.github.io/jison/) para gerar o analisador léxico e sintático. O compilador realiza análise léxica, sintática e suporta estruturas complexas como ponteiros, arrays, estruturas de controle e diretivas do pré-processador.

---

## ✨ Funcionalidades

- **Análise Léxica** com suporte a:
  - Tipos primitivos (`int`, `float`, `char`, etc.)
  - Operadores (`+`, `-`, `*`, `/`, `=`, `==`, `!=`, `<=`, `>=`, etc.)
  - Estruturas (`if`, `else`, `while`, `for`, `do-while`, `switch-case`, etc.)
  - Funções (`main`, definição, protótipo, chamadas)
  - Ponteiros (`*`, `->`)
  - Arrays (`[]`, acesso e inicialização)
  - Pré-processamento (`#define`, `#include`)
  - Comentários de linha e bloco

- **Análise Sintática**
  - Reconhecimento da estrutura de programas C
  - Suporte a blocos compostos `{}` e expressões compostas
  - Geração de árvores sintáticas abstratas (ASTs)

- **Geração de Código**
  - Código intermediário no formato **TAC (Three Address Code)**
  - Geração de temporários (`temp1`, `temp2`, etc.)
  - Suporte a operações aritméticas, lógicas e de controle


---

## 📁 Estrutura do Projeto

```bash
├── analisador.js               # Script principal de execução
├── compilador.js              # Gerado a partir do arquivo .txt via Jison
├── compilador.jison  # Arquivo fonte com gramática Jison
├── entradas/
│   └── entrada_03.txt         # Exemplos de código fonte a serem analisados

```

## 🚀 Como Executar
- Instale o Jison globalmente (se ainda não tiver):

```bash
Copiar
Editar
npm install -g jison
```

- Compile a gramática:

```bash
Copiar
Editar
jison compiladorVersaoFinalAgoraVai.txt -o compilador.js
```

- Rode o analisador:

```bash
Copiar
Editar
node analisador.js entradas/entrada_03.txt
```

- Saída esperada:

Tabela de símbolos

Erros Léxicos e Sintáticos (se houverem)

## 🧠 Exemplos de Suporte
```c
#define TAM 10

int main() {
  int x = 5;
  float y;
  x += 10;
  if (x > 0) {
    printf("positivo");
  } else {
    printf("negativo");
  }
  return 0;
}
```

Esse código seria analisado com geração de TAC e AST para cada instrução.

## 🧱 Componentes Técnicos
Jison para definição de:

**Léxico**: via expressões regulares com tokens detalhados.

**Gramática**: extensa, cobrindo expressões, controle de fluxo, declarações, funções, etc.

#### Funções Utilitárias

```criaTemp(), criaTAC(), verificaVariavel(), criarVariavel()```, entre outras.

Classe Node e AST para manipulação de árvores sintáticas.

## ⚠️ Tratamento de Erros
Léxicos: caracteres inválidos são reportados com mensagem personalizada.

Sintáticos: erros detalhados com linha, token esperado e encontrado.

Semânticos: variáveis não declaradas, tipos incompatíveis, usos inválidos de ponteiros, etc.

## 🔮 Possíveis Melhorias Futuras
Suporte a arquivos .h externos e múltiplos arquivos

Geração de código em linguagem de máquina simulada

Interface gráfica para visualização de AST

Otimizações em TAC

Integração com WebAssembly

## 👨‍💻 Desenvolvido por

André Prado de Oliveira, Lucas Zanon, Roberto Bastos

Disciplina: Compiladores

Turma: CC8M

Ano: 2025

