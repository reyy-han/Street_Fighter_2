# Street Fighter PAF


## Projet PAF (Programmation Avancée Fonctionnelle)

Le but du projet était de réaliser une refonte du jeu Street Fighter 2 à l'aide du langage fonctionel Haskell.

### Fonctionnalités:

Les deux joueurs peuvent : 
- bouger à droite et à gauche
- sauter par dessus leur adversaire
- faire un coup de pied
- faire un coup de point lorsque leur jauge est pleine (en rouge)

A chaque coup reçu, le joueur perd un peu de vie.
Lorsque l'un des joueurs n'a plus de vie, le jeu se termine et on affiche le gagnant.

### Motricité des joueurs

Ryu (à gauche au départ)

|              | Button              |
|--------------|---------------------|
| Move Left    | <kbd>left</kbd>     |
| Move right   | <kbd>right</kbd>    |
| Move Up      | <kbd>up</kbd>       |
| Kick         | <kbd>down</kbd>     |
| Punch        | <kbd>enter</kbd>    |

Ken (à droite)

|              | Button              |
|--------------|---------------------|
| Move Left    | <kbd>Q</kbd>        |
| Move right   | <kbd>D</kbd>        |
| Move Up      | <kbd>Z</kbd>        |
| Kick         | <kbd>S</kbd>        |
| Punch        | <kbd>space</kbd>    |


### Vue du Jeu


![paf-jeu-init](https://user-images.githubusercontent.com/79942403/194407208-a1c26ba6-93a1-4ebe-96d2-ab5193e216ba.png)
figure 1 : Vue sur le jeu au démarrage

![jeu_paf_coup_pied](https://user-images.githubusercontent.com/79942403/194407576-5de5b5b3-48ad-40fe-ba21-03765ef6b026.png)
figure 2 : Ryu frappe Ken avec son pied (Kick)


![paf-jeu-jauge-remplie](https://user-images.githubusercontent.com/79942403/194407737-3408ff3e-f443-45fd-bf30-1f85fea3b6da.png)
figure 3 : Les jauges sont remplies


![jeu-paf-coup-de-poing-punch](https://user-images.githubusercontent.com/79942403/194408024-d2b9f4d0-c38d-4ba4-abb3-9f27cdfc761a.png)
figure 4 : Ken utilise sa jauge et fait son coup de poing special contre Ryu


![jeu-paf-hitbox-mouv-](https://user-images.githubusercontent.com/79942403/194409630-58f73674-898c-4efd-9236-6721187ac6e6.png)
figure 5 : les deux joueurs ne peuvent pas se superposer


![jeu-paf-gameover](https://user-images.githubusercontent.com/79942403/194410104-7ba4f66e-283b-4d83-a0d9-19f3fb8bdadf.png)
figure 6 : le jeu est fini, le gagnant est Ken

