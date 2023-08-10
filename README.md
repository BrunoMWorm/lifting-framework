### Generate the Frontend from the .cf Grammar definition

```sh
bnfc --haskell --name-space=Language.Frontend --output=src ./src/Language/Language.cf
```

### Compiling and running one of the interpreters

You must have CUDD library installed. Instructions can be found [on this mirror](https://github.com/ivmai/cudd).


```sh
cabal build
cabal run < src/Language/Examples/Fibonacci.lng
```