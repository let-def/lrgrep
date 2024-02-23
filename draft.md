# LRGrep: engineering error messages for LR parsers

Le plan: 
1) On introduit les concepts et on construit une intuition des erreurs des
grammaires hors-contexte et des automates LR(0).

2) On présente le DSL pour caractériser ces erreurs.

3) On illustre avec quelques exemples pratique.

4) On généralise l'approche à des automates réalistes : LR(1) et ses variantes,
avec résolution de conflits et optimisation.

5) On montre comment implémenter cette méthode, en partant d'un automate LR et
d'une spécification et en générant un automate fini pour reconnaître une
situation.

6) Le tooling qui va avec : comment concevoir des messages d'erreur en pratique ?
- interprète
- tests de couverture et d'accessibilité
- problème : comment présenter les résultats, ... ?

J'ai envisagé trois axes pour l'article :
1. technique, pour l'essentiel expliquer les techniques de manipulation d'automates, passant de LR à des automates finis
  => avantage: il faudra bien l'écrire, et c'est le coeur nécessaire pour rendre cette approche possible
  => inconvénient: sans doute trop technique, ce sera très aride de faire rentrer ça dans la limite, peu intéressant pour l'audience
2. pratique: présenter le DSL, les outils, et survoler la théorie, les applications et l'implémentation
  => avantage: approprié pour SLE, on peut construire un narratif qui montre les applications potentielles
  => inconvénient: impossible à reproduire sans la partie technique, peu d'exemples pour illustrer ça à ce jour (seulement la grammaire OCaml)
3. pratique, mais en construisant le narratif en partie autour d'OCaml : on part du problème des erreurs de syntaxe, on construit la solution;
   par rapport à 2., la partie DSL, outils, connexion à la théorie LR restent inchangées, mais on les motive directement avec des exemples concrets tirés d'OCaml;
   l'article reste "langage-agnostic", OCaml n'est qu'une prétexte pour introduire les problèmes qui devraient se généraliser bien;
   on fait aussi remarquer qu'il s'agit d'une mise en bouche, le gros du sujet sera à retrouver dans la thèse

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-refresh-toc -->
**Table of Contents**

- [Introduction](#introduction)
- [Context Free Grammars, Canonical LR(0) and right-sentential forms](#context-free-grammars-canonical-lr0-and-right-sentential-forms)
  - [Example](#example)
  - [Failure space](#failure-space)
- [Error specifications](#error-specifications)
  - [Pattern built on regular expressions](#pattern-built-on-regular-expressions)
    - [Symbols or states as atoms](#symbols-or-states-as-atoms)
    - [Rightmost derivation operator](#rightmost-derivation-operator)
    - [Captures](#captures)
      - [Disambiguating captures](#disambiguating-captures)
    - [Specifying items](#specifying-items)
      - [Filtering with items](#filtering-with-items)
    - [Examples](#examples)
  - [Error clauses](#error-clauses)
    - [Ordered match to disambiguate](#ordered-match-to-disambiguate)
    - [Partial clauses](#partial-clauses)
    - [Lookahead constraints](#lookahead-constraints)
  - [Full example](#full-example)
- [Generalizing to non-canonical automata](#generalizing-to-non-canonical-automata)
- [Implementation](#implementation)
  - [Dérivée d'Antimirov](#dérivée-dantimirov)

<!-- markdown-toc end -->

## Introduction

Les parsers LR c'est bien, mais il est difficile de générer des messages d'erreur.
Les industriels du coup préfèrent souvent des parsers écrits à la main.

Donc il faut pouvoir générer des messages d'erreur: d'abord avec Merr, puis avec Menhir.
Il est important de raisonner sur l'exhaustivité, de donner des exemples pour
aider l'auteur, et de pouvoir contrôler les réductions.

Mais ces approches dépendent trop de l'automate: la notion d'erreur est définie
au niveau d'un état, cela peut-être trop fin (plusieurs états => même erreur),
ou pas assez précis (information contextuelle inaccessible). Par exemple
CompCert doit aussi extraire des informations de la pile du parseur pour
compléter un message (d'après Gasche, à vérifier). Enfin c'est fragile en
présence de changements dans la grammaire (beaucoup de travail de maintenance). 

LRGrep propose d'analyser la pile de l'automate pour produire des messages, et
permet l'accès aux informations sémantiques déjà calculées pour enrichir les
messages. Il fonctionne à partir d'un ensemble de règles qui viennent complétées
la grammaire et qui sont exprimées sur les formes sententielles-droites; en
terme purement grammaticaux donc, sans avoir à faire référence à l'automate.

## Context Free Grammars, Canonical LR(0) and right-sentential forms 

On présente brièvement les grammaires hors-contexte, les dérivations et les formes sententielles.

On s'intéresse aux préfixes viables des formes sententielles droites : c'est un
langage régulier, et il est au coeur de la construction LR(0) puisque l'automate
canonique reconnaît précisément ce langage. 

En fait, les parsers LR(0) "direct" n'ont pas besoin d'automate et travaille
directement sur la forme sententielle.  L'automate est utilisé en pratique pour
rendre les choses performantes et déceler statiquement les ambiguités LR (qui se
traduisent par du non-déterminisme dans l'automate). La forme sententielle
représentée par la pile est alors annotée avec les états de l'automate
correspondant à chaque symbole pour accélérer le processus (mais pas
nécessairement, voir par exemple "Discriminating Reverse LR").

Les variantes de LR raffinent cet automate avec des informations
complémentaires, mais il reste qu'on peut toujours extraire un préfixe viable
d'une forme sententielle droite d'une pile LR.

Enfin, un petit mot sur les /items/, car on y fera référence après : il
est important de mentionner que tout état de l'automate est identifié par un
item set, et que les items LR(0) sont composées d'une production "pointée". On
peut donc également caractériser les préfixes des formes sententielles en terme
d'items LR(0) (sans pour autant faire référence à l'automate, cela reste une
construction purement grammaticale).

_Note_: c'était un inquiétude des gens de Catala, en particulier de Louis
Gesbert si je me souviens bien, de vraiment rester au niveau discursif de la
grammaire (et je le comprends bien). Je ne sais pas s'il est important
d'insister là-dessus.

### Example

Un exemple pour illustrer les formes sententielles-droites, les préfixes viables, et les items LR(0).

### Failure space

Maintenant que l'on sait à quoi ressemble les formes sententielles droites, il faut
voir comment elles peuvent échouer.

A priori, une entrée incorrecte est tout mot en dehors du langage.
La grammaire donne de la structure (la dérivation) aux mots du langage, quelle
structure peut-on donner au complémentaire du langage?

Les grammaires LR ont la propriété du préfixe viable : le symbole qui déclenche
l'erreur est le premier qui fait que l'entrée consommée n'est préfixe d'aucun
mot du langage.

On peut donc structurer l'ensemble des entrées erronnées en terme de préfixe viable et de symbole provoquant l'erreur.
Si viable(u) = exists v. uv \in L, on construit l'ensemble {(u, t) | viable(u) /\ ~viable(ut)}.

Enfin, puisque les $u$ sont des préfixes viables, on peut calculer le préfixe viable d'une forme
sententielle droite qui leur correspond.

On obtient un nouvel ensemble {(\alpha, t)} qui est lui-même régulier. Cela
revient à annoter l'ensemble des préfixes viables.

On s'intéresse seulement aux formes "initiales", avant toute réduction.

## Error specifications

La spécification des erreurs est très proche de la spécification d'un lexer (comme lex, ml-ulex, ...).

Les règles sont spécifiées comme une liste ordonnée des clauses associant un
motif et une action sémantique à exécuter quand le motif est reconnu.

Notre prototype étant implémenté en OCaml, l'action sémantique est une
expression OCaml, mais du point de vue de LRgrep c'est une chaîne de caractères
sans signification particulière.

### Pattern built on regular expressions

Tout comme pour les générateurs de lexeurs, les motifs sont construits dans un
dialecte d'expressions rationnelles. Les expressions rationnelles de LRgrep ont
toujours une double interprétation : l'auteur de la grammaire pensera en terme
de formes sententielles, quand l'implémentation analysera directement les états
de l'automate LR.

Voici le langage des patterns :

e ::= atom
    | e1e2
    | e1 | e2
    | e^*
    | [ e ]
    | / item

#### Symbols or states as atoms
 
The regular expressions are built using grammar symbols as alphabet (both terminal and nonterminals).

This brings a slight ambiguity over the meaning of a non-terminal: in an error
specification, a non-terminal denotes only itself and not the set of sentences
it can expands to. 

In the automaton interpretation, a symbol denotes the set of states having this symbol as 

#### Rightmost derivation operator

Les expressions rationnelles seules permettent de caractériser des piles, mais ignorent tout de la grammaire.
La forme sententielle construite par un automate LR est issue de deux opérations : shift et reduce.
Concaténer des expressions est suffisant pour reproduire l'effet du shift, mais que faire pour reduce?

La réduction a pour effet d'inverser la relation de dérivation, remplaçant un
préfixe viable de forme sententielle droite par un autre en substituant le
/handle/ le plus à droite par un non-terminal.

Pour comprendre l'opérateur `[e]`, procédons deux temps:
- l'expression e est d'abord interprétée vers un ensemble de suffixes de piles
- chacun de ses suffixes est complété par la cloture par cette relation derivation-1

L'effet sur un non-terminal est facilement décrit par une grammaire linéaire droite.
Sur l'exemple de l'arithmétique, [E]:
 [F] -> n
 [E] -> [F] | E + [F] | ( E )

On prend chaque règle définissant E, et on remplace les non-terminaux en suffixe par d'autres occurrences de [E].

#### Captures

e ::= x=atom
    | x=[e]

##### Disambiguating captures

e ::= [[e]]
    | x=[[e]]
    | e**
    
#### Specifying items

Parfois il est pratique de pouvoir restreindre l'analyse aux états contenant un item spéficique.
Notamment, un item permet de restreindre le préfixe accepté en fonction des suffixes potentiels.

Par exemple, pour caractériser les parenthèses non fermées, on s'intéressera aux
piles qui acceptent une parenthèse fermante comme suffixe.

```
[_ / e: ( e . )]

```

En terme d'automate, l'interprétation de `/` est directe: on limite les états à ceux de l'automate contenant cet _item_ dans leur itemset. 

Il est beaucoup moins immédiat de donner une interprétation directement en terme
de formes sententielles. Il s'agit toutefois d'une application immédiate de
l'approche "Discrminating Reverse" : on veut identifier un itemset en analysant
un suffixe d'une pile de symboles.

Cependant, nous pensons que la référence à un item est assez intuitive,
l'intention de l'auteur est assez immédiatement véhiculée. Même s'il faut avoir
recours à un automate pour l'interpréter formellement, ceci est un détail
d'implémentation qui ne devrait pas _leaker_.

##### Filtering with items 

Un dernier point à éclaircir : alors que les atomes identifient un état qui doit
exister sur la pile (ils "consomment" de l'entrée), les items agissent comme des
filtres : ils restreignent les entrées acceptées. Nous avons expérimenté avec
plusieurs approches et celle-ci nous a semblé la plus flexible.

Premièrement, les deux constructions qui consomment de la pile peuvent être
capturées, et le contenu capturé est très clair syntaxiqument. Il est moins
visual de comprendre la valeur capturée par un item. Par exemple pour `x=(E :
E + F .)`, x capturerait la valeur sémantique associée au non-terminal f à
gauche du `+`. Dans le formalisme que nous avons retenu, ce motif s'écrit `x=F /
E : E + F .`, qui sépare l'action de capture (x capture un non-terminal f) des
restrictions sur la situation (un f apparaissant à droite de +)`

Enfin, l'auteur peut restreindre un message d'erreur à une situation combinant
plusieurs items, en chaînant les occurrences de `/`.

#### Examples

### Error clauses

Une spécification des erreurs d'une grammaire est une séquence de clause de la forme:

rule ::= clause 
       | clause '|' rule

clause ::= e {action}
         | ...

#### Ordered match to disambiguate

Fréquemment, plusieurs clauses seront candidates dans une situation donnée.

Dans ce cas, l'analyseur appliquera la première clause, dans l'ordre des
déclarations.

En conséquence, les clauses d'une spécification ont tendance à être ordonnées des
plus spécifiques (qui s'appliquent dans une situation bien précise) aux plus
génériques (des cas très peu précis qui rattrapent toutes les situations
restantes).

#### Partial clauses

clause ::= e partial {action}

#### Lookahead constraints

Finally, a clause can be constrained to apply only when certain lookahead tokens caused the failure.

```
clause ::= e @ lookaheads {action}
         | e partial @ lookaheads {action}
```

For instance, this rule is taken from our prototype specification on OCaml grammar:

```
| _ / . expr @ MODULE { ... }
```

The clause catches the case where a user tried to use the `module` keyword in a
context where an term expression is expected. This is a common error that
beginners do: the grammar allows injecting module expressions inside a term
expression, but the construction has to be parenthesized.

### Full example





## Generalizing to non-canonical automata

En pratique il y a souvent des conflits qui sont résolus avec des annotations de précédences de Yacc.
Cela se traduit par des transitions supprimées de l'automate, qui n'est donc pas aussi régulier que l'automate LR(0).
L'automate inclue aussi un certain nombres d'optimisations : default & spurrious
reductions. Un échec pourra donc détecté être détecté un peu plus tard.

Il faut revisiter les notions que l'on a introduite en terme grammaticaux pour
les calculer directement sur un automate.

Attention cependant: si notre technique s'adapte à un automate LR(1) quel que
soit la famille d'algorithme qui l'a construit (LALR, SLR, Pager's Minimal
Automaton, IELR, ...), le matching à l'execution suppose que l'on part de la
dernière pile "valide" précédant le token qui a provoqué l'erreur.

Cela peut être problématique pour les implémentations impératives qui ont pu
détruire des informations avant de procéder à l'analyse. 
Une solution simple est de lancer une seconde analyse de l'entrée en s'arrêtant
à la fin du préfixe valide.

## Implementation

### Antimirov derivative

### Generating the automaton

#### Lr1 stack approximation

#### Lrc stack approximation

#### Lrce stack approximation

## Tooling: development workflow

### Interpreter with reduction sequences

### Coverage checks
    
