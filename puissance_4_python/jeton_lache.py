from possibilite_coup import *
def lacher_jeton(indice_colonne, grille, joueur) :
  """
  obj : lâcher un jeton. Lorsqu'un joueur joue, il lâche un jeton de
  sa couleur (o/*) au dessus d'une colonne de la grille (indice
  donné). En raison de la force gravitationnelle, le jeton tombe dans
  la grille jusqu'à ce qu'il soit arrêté par un autre jeton ou le fond de la grille
  IN : indice_colonne int ; grille -> liste de sous-listes de string sauf la dernière qui est de int
  OUT : None (mais modifie la grille)
  préconditions : 1 <= indice_colonne <= 7 (+ exercice 4)
  """
  if not coup_possible(indice_colonne, grille) : return
  assert indice_colonne >= 1 and indice_colonne <= 7, "indice non correct"
  assert joueur in ('o','*'), "joueur non valide"
  indice_ligne = 0
  while grille[indice_ligne][indice_colonne - 1] == " " :
    indice_ligne += 1
  grille[indice_ligne - 1][indice_colonne - 1] = joueur
  return indice_ligne - 1