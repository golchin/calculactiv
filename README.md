## Noms et prénoms
- Maganga Mouloungui Renata Prismell
- Golchin Kharazi Mehri

## Description de ce qui a été accompli
Tout les modules de la première partie ont été implémenté, les operators comme (Addition, Subtraction, Multiplication, Division, Exponentiation, Sine) et les commandes comme (quit, help, set, unset, vars). Les gestions des exceptions ont été effectué dans les cas nécessaires.

## Sources
Pour la comprehension et l'Utilisation de Parser:
- http://pageperso.lif.univ-mrs.fr/~luigi.santocanale/teaching/PF/
- livre "Real World Haskell" de Donald Bruce Stewart, Bryan O'Sullivan, John Goerzen
- http://learnyouahaskell.com/
- https://stackoverflow.com/
- http://jakewheat.github.io/intro_to_parsing/#getting-started
- https://hspec.github.io

## Descriptions des objectifs du project
# Calculactiv
Calculactiv est une simple ligne de commande Interactive écris en Haskell et permettant de manipuler des données mathématiques.

## Pré-requis
Afin de compiler et exécuter le programme Calculactiv, il faut utiliser GHC ou Stack.

## Compiler
Après avoir installé GHC, taper make en ligne de commande pour compiler tout le projet.

```
$ make
$ ./calculactiv
```

ou utiliser Stack comme suivant.

```
$ stack build
$ stack exec calculactiv-exe
```

## Tests exécution
Avant exécution le test unitaire, il faut installer "hspec" en utilisant Cabal.
```
$ cabal install hspec
```

Ensuite, exécuter tout les test unitaire par la commande suivante.

```
$ make test
```

ou utiliser Stack comme suivant.

```
$ stack test
```

## Nettoyage
Vous pouvez nettoyer tout le désordre, exécuter la commande suivante.

```
$ make clean
```
