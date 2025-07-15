# Lupita: A Linguagem de Programação Intuitiva para Iniciantes em Desenvolvimento Web.

## Resumo

A Lupita é uma linguagem de programação desenvolvida com o propósito de atender um público iniciante em programação, com o objetivo em facilitar a compreensão dos conceitos através de uma linguagem que não cause dificuldades para uma pessoa que esteja iniciando, com foco em pessoas que queiram seguir a área de desenvolvimento web.

## Implementação

O desenvolvimento da Lupita seguiu uma abordagem estruturada: inicialmente, a gramática da linguagem foi projetada utilizando BNF (Backus-Naur Form). Em seguida, como uma linguagem interpretada, a Lupita foi implementada em **Haskell**, utilizando **Alex** para a construção do analisador léxico e **Parsec** para definir as estruturas sintáticas e semânticas.

## Uso do interpretador

### Instalação

1. **Instalar o GHCup**: Inicialmente é recomendado instalar o GHCup, que é a forma mais fácil de instalar ferramentas como o GHC (compilador do Haskell) entre outras:

```
curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
```

Durante a instalação, o GHCup fará algumas perguntas. Na maioria dos casos, você pode aceitar as opções padrão pressionando `Enter`.

2. **Instalar GHC e Cabal**: Uma vez que o GHCup esteja instalado, você pode usá-lo para instalar o `GHC` (Glasgow Haskell Compiler) e o `Cabal` (gerenciador de pacotes padrão do Haskell):

```
ghcup install ghc
ghcup install cabal
```

3. **Instalar Alex**: Agora faz-se necessário instalar o `Alex` que é um gerador de analisadores léxicos (scanners) para Haskell:

```
cabal install alex
```

**Dica**: Caso tenha problemas na instalação do `alex` no Linux baseado em Debian como o Ubuntu, recomenda-se rodar os seguintes comandos:

```
sudo apt update
sudo apt install libgmp-dev
```

4. **Verificar Instalações**: Para confirmar que tudo foi instalado corretamente basta rodar os seguintes comandos:

```
ghc --version
cabal --version
alex --version
```

### Compilação

1. **Compilação do Lexer (Analisador Léxico)**

Para compilar os tokens da linguagem (estrutura léxica):

```sh
alex Lexer.x
```

2. **Compilação do Parser (Analisador Sintático e Semântico)**

Para compilar o parser (estrutura sintática e semântica):

```sh
ghc Parser.hs -o parser
```

Compilação de todo programa:

```sh
alex Lexer.x && ghc Parser.hs -o parser
```

### Execução

Depois de compilar os arquivos, você pode executar o interpretador Lupita especificando o caminho para o seu arquivo de programa (.pe) (há alguns exemplos de programas na pasta `/tasks`):

```sh
# Executando um programa Lupita
# Substitua <arquivo.pe> pelo caminho do seu programa Lupita
./parser <arquivo.pe>

# Exemplo: Executar o programa '1.pe' localizado na pasta 'tasks'
./parser tasks/1.pe
```
