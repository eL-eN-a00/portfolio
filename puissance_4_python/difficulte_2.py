def supprimer_jeton(grille, colonne):
  """
Supprime un jeton dans une colonne spécifique de la grille.

Arguments :
- grille : list[list[str]] - Une grille représentée par une liste de listes où chaque élément
          est une valeur représentant un emplacement dans la grille.
- colonne : int - L'indice de la colonne où le jeton doit être supprimé (entre 1 et 7).

Retour :
- Aucun (modifie la grille en place).

préconditions : 1 <= colonne <= 7
"""
  assert colonne >= 1 and colonne <= 7, "indice non correct"
  indice_ligne = 0  
  while grille[indice_ligne][colonne - 1] == " ":
    indice_ligne += 1 
  grille[indice_ligne][colonne - 1] = " "

from difficulte_1 import *
from alignement_jetons import *
from jeton_lache import *
from possibilite_coup import *

def conseil_colonne(grille, joueur, col_interdite):
  """
  Suggère une colonne à jouer pour un joueur donné en analysant les possibilités de gain dans chaque colonne.

  Arguments :
  - grille : list[list[str]] - Une grille représentée par une liste de listes où chaque élément est une valeur représentant un emplacement dans la grille.
  - joueur : str - Le caractère représentant le joueur ("o" ou "*").
  - col_interdite : list[int] - Liste d'indices de colonnes interdites.

  Retour :
  - int : L'indice de la colonne suggérée pour jouer.

  préconditions : grille et joueur valide
  postconditions : 1 <= alignement <= 7
  """
  assert len(grille) == 7, "grille2 non valide"  
  assert joueur in ('o', '*'), "joueur non valide" 
  max_alignement = []  
  if joueur not in grille[5]:
    return liste_colonnes() 
  for colonne in range(len(grille)): 
    assert len(grille[colonne]) == 7, "grille2 non valide"
    align_colonne = []  
    if coup_possible(colonne + 1, grille) and colonne not in col_interdite:
      ligne = lacher_jeton(colonne + 1, grille, joueur)  
      align_colonne.append(verif_comb_horizontal(grille, joueur, ligne))
      align_colonne.append(verif_comb_vertical(grille, joueur, colonne)) 
      align_colonne.append(verif_comb_diagonal1(grille, joueur)) 
      align_colonne.append(verif_comb_diagonal2(grille, joueur))
      max_alignement.append(maximum_liste(align_colonne)[0])  
      supprimer_jeton(grille, colonne + 1) 
    else:
      max_alignement.append(-1)
  alignement = maximum_liste(max_alignement)[1] + 1 
  assert alignement >= 1 and alignement <= 7
  return alignement 