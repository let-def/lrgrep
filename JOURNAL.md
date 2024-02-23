# Wed Dec 13 13:10:52 JST 2023

- Une réduction qui ne consomme rien (mais avec un éventuel look-ahead) devrait matcher immédiatement.
  => il semble que ce soit déjà le cas ;)
- Changer la construction des LazyNFA et LazyDFA:
  - inutile de mettre en cache les dérivations, on va les re-découvrir systématiquement.
  - peut-être spécialiser la gestion des réductions "wildcard" 

# Fri Feb  3 22:06:40 JST 2023

1) En observant les items des états accessibles en cas
d'erreur, j'ai l'impression que les items les plus rares sont les plus
intéressants.

Par exemple, pour les expressions arithmétiques, comme c'est une construction
récursive, dans l'itemset des états qui la reconnaissance on va avoir de
nombreuses occurrences de certains items: [expr . + expr], [expr . * expr],
etc.

En revanche, l'item [( expr . )] apparaît beaucoup moins fréquemment. Pourtant
il est très intéressant: on peut dire qu'on attend une parenthèse fermante,
alors que les items devant un opérateur binaire... On ne peut pas dire grand
chose dessus, il n'est pas très utile de rappeler à l'utilisateur qu'il peut
mettre un nouvel opérateur pour continuer l'analyse!

2) Faire remarquer à Jonathan Coates que son pattern [^ table_entry: expr .]
est peut-être fragile. On ne sait pas ce qu'on attend après table_entry, alors
qu'intuitivement un item d'erreur pourrait généralement avoir la forme 
[prefix . EXPECTATION] pour suggérer la continuation.

Ici, le contexte est évidemment véhiculé par le lhs "table_entry:". 
Mais c'est un invariant "implicite". Peu problable que l'outil d'énumération
des erreurs puisse trouver ça !

C'est sans doute une bonne approche, mais intéressante à mentionner.

3) La suite pour LRGrep :

- outil d'énumération d'erreurs et de couverture
- matching sur le token de lookahead également!
- revisiter la syntaxe des items ?
- capture des locations pendant les réductions
