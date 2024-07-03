# Paradigmas da Computação

### Haskell

Para compilar o programa, execute o comando no diretório `haskell/`:

```bash
ghc -o kojun_solver *.hs
```
 execute o comando:

```bash
./kojun_solver
```

ou usando a Haskell Stack:

```
stack ghc -- Main.hs Games.hs Types.hs -o output && ./output 
```

### Lisp

Para compilar o programa, execute o comando no diretório `lisp/`:

```
rlwrap sbcl --script main.lisp
```

### Prolog

Para executar o programa, execute o comando no diretório `prolog/`:

```bash
swipl -s kojun.pl
```
    
execute o comando:

```bash
?- main_6x6.
?- main_10x10.
?- main_17x17.
```
    