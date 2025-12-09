from affichage_grille import *
from creer_grille import *
from jeton_lache import *
from alignement_jetons import *
from difficulte_2 import *

def coup_possible_ameliore(grille, joueur):
  """
  Détermine le meilleur coup possible pour un joueur en prenant en compte les mouvements de l'adversaire pour bloquer ses tentatives de victoire.

  Arguments :
  - grille : list[list[str]] - Une grille représentée par une liste de listes où chaque élément est une valeur représentant un emplacement dans la grille.
  - joueur : str - Le caractère représentant le joueur ("o" ou "*").

  Retour :
  - int : L'indice de la colonne recommandée pour jouer.

  Preconditions : joueur valide
  postconditions : 1 <= coup_possible <= 7
  """
  if joueur == 'o': adversaire = '*'
  elif joueur == '*': adversaire = 'o'
  else : return "AssertionError"
  coup_adversaire = conseil_colonne(grille, adversaire, []) 
  colonne_interdite = []  
  lacher_jeton(coup_adversaire, grille, adversaire)
  if jetons_alignes(grille, adversaire):
    supprimer_jeton(grille, coup_adversaire)
    return coup_adversaire  
  supprimer_jeton(grille, coup_adversaire) 
  coup_possible = conseil_colonne(grille, joueur, [])
  lacher_jeton(coup_possible, grille, joueur)
  coup_adversaire = conseil_colonne(grille, adversaire, [])
  lacher_jeton(coup_adversaire, grille, adversaire)
  while jetons_alignes(grille, adversaire):
    supprimer_jeton(grille, coup_possible)
    supprimer_jeton(grille, coup_adversaire)
    colonne_interdite.append(coup_possible)
    coup_possible = conseil_colonne(grille, joueur, colonne_interdite)
    lacher_jeton(coup_possible, grille, joueur)
    coup_adversaire = conseil_colonne(grille, adversaire, [])
    lacher_jeton(coup_adversaire, grille, joueur)
  supprimer_jeton(grille, coup_possible)
  supprimer_jeton(grille, coup_adversaire)
  assert coup_possible >= 1 and coup_possible <= 7
  return coup_possible