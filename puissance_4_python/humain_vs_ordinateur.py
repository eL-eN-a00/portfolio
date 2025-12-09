from creer_grille import *
from alignement_jetons import *
from possibilite_coup import *
from jeton_lache import * 
from difficulte_2 import *
from difficulte_3 import coup_possible_ameliore
#afficher si victoire
def jouer_vs_ordinateur(grille):
  joueur, jetons_joueur, jetons_ordinateur, fin, issue_partie = True, 21, 21, False, ""
  colonnes_possibles = [1, 2, 3, 4, 5, 6, 7]
  while not fin :
    affichage = afficher_grille1(grille)
    print(affichage)
    if joueur  :
      colonne = int(input("Quelle colonne ? "))
      while colonne not in colonnes_possibles and coup_possible(colonne, grille) :
        colonne = int(input("Je ne comprends pas, quelle colonne ? "))
      lacher_jeton(colonne, grille, '*')
      jetons_joueur -= 1
      joueur = False
      if jetons_alignes(grille, '*') : fin, issue_partie = True, "VICTOIRE DU JOUEUR"
      elif jetons_joueur == 0 : fin, issue_partie = True, "MATCH NUL"
    else :
      joueur = True
      colonne = coup_possible_ameliore(grille, 'o')
      print(colonne)
      lacher_jeton(colonne, grille, 'o')
      jetons_ordinateur -= 1
      if jetons_alignes(grille, 'o') : fin, issue_partie = True, "DEFAITE DU JOUEUR"
      elif jetons_ordinateur == 0 : fin, issue_partie = True, "MATCH NUL"
  affichage = afficher_grille1(grille)
  print(affichage)
  print(issue_partie)