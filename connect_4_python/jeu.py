from affichage_grille import *
from alignement_jetons import *
from creation_grille import *
from difficulte_1 import *
from difficulte_2 import *
from difficulte_3 import *
from jeton_lache import *
from possibilite_coup import *
from time import *

def jeu():
  """
  Fonction principale pour lancer le jeu Puissance 4, permettant aux joueurs de choisir le mode de jeu, le niveau de difficulté et le style d'affichage.
  """
  grille = creer_grille()
  print("Puissance 4 by Elena \nChoisissez votre affichage : 1 2 3 4 ou 5")
  numero = int(input())
  while numero not in [1, 2, 3, 4, 5]:
    print("Puissance 4 by Elena \nChoisissez votre affichage : 1 2 3 4 ou 5")
    numero = int(input())
  print("Choisissez votre mode de jeu : \n Si vous voulez jouer contre un ami (la famille tu connais), tapez 1 \nSi vous voulez jouer contre un ordinateur (bon courage), tapez 2 \nSi vous voulez que deux ordinateurs s'affrontent (carrément en loges VIP), tapez 3")
  entree = int(input())
  while entree not in [1, 2, 3]:
    print("Je n'ai pas compris")
    print("Puissance 4 by Elena \nChoisissez votre mode de jeu : \nSi vous voulez jouer contre un ami (la famille tu connais), tapez 0 \nSi vous voulez jouer contre un ordinateur (bon courage), tapez 1 \nSi vous voulez que deux ordinateurs s'affrontent (carrément en loges VIP), tapez 2")
    entree = int(input())
  if entree == 1:
    jouer_vs_humain(grille, numero) 
  else:
    print("Quel niveau de difficulté voulez-vous pour l'ordinateur ? \nFacile, tapez 1 \nMoyen, tapez 2 \nDifficile, tapez 3")
    difficulte = int(input())
    while difficulte not in [1, 2, 3]:
      print("Je n'ai pas compris")
      print("Quel niveau de difficulté voulez-vous ? \nFacile, tapez 1 \nMoyen, tapez 2 \nDifficile, tapez 3")
      difficulte = int(input())
    if entree == 2:
      jouer_vs_ordi(grille, difficulte, numero) 
    else:
        ordi_vs_ordi(grille, difficulte, numero) 

def jouer_vs_humain(grille, numero):
  """
  Permet à deux joueurs humains de jouer l'un contre l'autre dans le jeu Puissance 4.

  Arguments :
  - grille : list[list[str]] - Une grille représentée par une liste de listes où chaque élément est une valeur représentant un emplacement dans la grille.
  - numero : int - Le numéro de l'affichage choisi pour la grille.

  Retour :
  - Aucun (affiche l'état du jeu et le résultat final de la partie).

  pre-conditions : 1 <= numero <= 5
  """
  assert numero in [1, 2, 3, 4, 5]
  joueur, jetons_jaune, jetons_rouge, fin, issue_partie = '*', 21, 21, False, ""
  colonnes_possibles = [1, 2, 3, 4, 5, 6, 7]
  while fin == False:
      print(affichage(grille, numero))  
      if joueur == '*':
          print("TOUR DU JOUEUR JAUNE")
          colonne = int(input("Quelle colonne ? "))
          while colonne not in colonnes_possibles or not coup_possible(colonne, grille):
              colonne = int(input("Je ne comprends pas, quelle colonne ? "))

          lacher_jeton(colonne, grille, '*')  
          jetons_jaune -= 1
          if jetons_alignes(grille, joueur):
              fin, issue_partie = True, "VICTOIRE DU JOUEUR JAUNE"
          elif jetons_jaune == 0:  
              fin, issue_partie = True, "MATCH NUL"
          joueur = 'o'  
      else:
          print("TOUR DU JOUEUR ROUGE")
          colonne = int(input("Quelle colonne ? "))
          while colonne not in colonnes_possibles or not coup_possible(colonne, grille):
              colonne = int(input("Je ne comprends pas, quelle colonne ? "))

          lacher_jeton(colonne, grille, 'o')  
          jetons_rouge -= 1
          if jetons_alignes(grille, joueur):
              fin, issue_partie = True, "VICTOIRE DU JOUEUR ROUGE"
          elif jetons_rouge == 0: 
              fin, issue_partie = True, "MATCH NUL"
          joueur = '*' 
  print(affichage(grille, numero)) 
  print(issue_partie) 


def jouer_vs_ordi(grille, difficulte, numero):
  """
  Permet à un joueur humain de jouer contre l'ordinateur dans le jeu Puissance 4.

  Arguments :
  - grille : list[list[str]] - Une grille représentée par une liste de listes où chaque élément est une valeur représentant un emplacement dans la grille.
  - difficulte : int - Le niveau de difficulté de l'ordinateur (1, 2 ou 3).
  - numero : int - Le numéro de l'affichage choisi pour la grille.

  Retour :
  - Aucun (affiche l'état du jeu et le résultat final de la partie).

  preconditions  : 1 <= numero <= 5, 1 <= difficulte <= 3
  """
  assert numero in [1, 2, 3, 4, 5]
  assert difficulte in [1, 2, 3]
  joueur, jetons_jaunes, jetons_rouges, fin, issue_partie = '*', 21, 21, False, ""
  colonnes_possibles = [1, 2, 3, 4, 5, 6, 7]
  while fin == False:
    print(affichage(grille, numero))  
    if joueur == '*':
      print("TOUR DU JOUEUR JAUNE")
      colonne = int(input("Quelle colonne ? "))
      while colonne not in colonnes_possibles or not coup_possible(colonne, grille):
        colonne = int(input("Je ne comprends pas, quelle colonne ? "))
      lacher_jeton(colonne, grille, '*')  
      jetons_jaunes -= 1
      if jetons_alignes(grille, joueur):
        fin, issue_partie = True, "VICTOIRE DU JOUEUR JAUNE"
      elif jetons_jaunes == 0:  
        fin, issue_partie = True, "MATCH NUL"
      joueur = 'o'  
    else:
      print("TOUR DU JOUEUR ROUGE")
      if difficulte == 1:
        colonne = liste_colonnes()  
      elif difficulte == 2:
        colonne = conseil_colonne(grille, 'o', [])  
      else:
        colonne = coup_possible_ameliore(grille, 'o')  
      lacher_jeton(colonne, grille, 'o')  
      jetons_rouges -= 1
      if jetons_alignes(grille, joueur):
        fin, issue_partie = True, "VICTOIRE DU JOUEUR ROUGE"
      elif jetons_rouges == 0: 
        fin, issue_partie = True, "MATCH NUL"
      joueur = '*'
  print(affichage(grille, numero)) 
  print(issue_partie)  

from time import sleep  

def ordi_vs_ordi(grille, difficulte, numero):
  """
  Simule un affrontement entre deux joueurs contrôlés par l'ordinateur dans le jeu Puissance 4.

  Arguments :
  - grille : list[list[str]] - Une grille représentée par une liste de listes où chaque élément est une valeur représentant un emplacement dans la grille.
  - difficulte : int - Le niveau de difficulté des ordinateurs (1, 2 ou 3).
  - numero : int - Le numéro de l'affichage choisi pour la grille.

  Retour :
  - Aucun (affiche l'état du jeu et le résultat final de la partie).
  preconditions  : 1 <= numero <= 5, 1 <= difficulte <= 3
  """
  assert numero in [1, 2, 3, 4, 5]
  assert difficulte in [1, 2, 3]
  joueur, jetons_jaunes, jetons_rouges, fin, issue_partie = '*', 21, 21, False, ""
  colonnes_possibles = [1, 2, 3, 4, 5, 6, 7]
  while fin == False:
    print(affichage(grille, numero)) 
    sleep(2)  
    if joueur == '*':
      if difficulte == 1:
        colonne = liste_colonnes()  
      elif difficulte == 2:
        colonne = conseil_colonne(grille, joueur, [])  
      else:
        colonne = coup_possible_ameliore(grille, joueur)  
      lacher_jeton(colonne, grille, joueur)  
      jetons_jaunes -= 1
      if jetons_alignes(grille, joueur):
        fin, issue_partie = True, "VICTOIRE DU JOUEUR JAUNE"
      elif jetons_jaunes == 0:  
        fin, issue_partie = True, "MATCH NUL"
      joueur = 'o'  
    else:
      if difficulte == 1:
        colonne = liste_colonnes()  
      elif difficulte == 2:
        colonne = conseil_colonne(grille, joueur, [])  
      else:
        colonne = coup_possible_ameliore(grille, joueur)  

      lacher_jeton(colonne, grille, joueur)  
      jetons_rouges -= 1
      if jetons_alignes(grille, joueur):
        fin, issue_partie = True, "VICTOIRE DU JOUEUR ROUGE"
      elif jetons_rouges == 0:  
        fin, issue_partie = True, "MATCH NUL"
      joueur = '*'

  print(affichage(grille, numero)) 
  print(issue_partie)  