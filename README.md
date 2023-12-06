# logique
Projet de Logique


###Commande terminal
```bash
swipl
```
###Commande dans swipl
```bash
[prog].
set_echo.
```



##Liste d'équations à tester
True : 
```bash
unifie([a?=a]).
unifie([X ?= a]).
unifie([X ?= f(a,b)]).
unifie([f(X, b) ?= f(a, Y)]).
unifie([f(X, g(Y)) ?= f(Z, g(b))]).
unifie([f(X, Y) ?= f(a, g(X))]).
unifie([f(X, [a, b, c]) ?= f(a, Y)]).
```

False :
```bash
unifie([a ?= b]).
unifie([f(a, b) ?= g(a, b)]).
unifie([f(a, b) ?= f(a)]).
unifie([X ?= f(X)]).
```
