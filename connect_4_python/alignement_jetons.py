def maximum_liste(liste):
  """
Trouve la valeur maximale dans une liste donnée et retourne cette valeur ainsi que son indice.

Arguments :
- liste : list - Une liste de valeurs.

Retour :
- tuple : Un tuple contenant la valeur maximale et son indice dans la liste.

préconditions : liste de type list et liste non vide
"""
  assert type(liste) == list and liste != []
  max_val, indice, longueur = liste[0], 0, len(liste)  
  for i in range(1, longueur): 
    if liste[i] > max_val: 
      max_val, indice = liste[i], i 
  return max_val, indice 
 
def verif_comb_horizontal(grille, joueur, ligne):
  """
  vérifie la présence d'une combinaison horizontale du symbole du joueur dans une ligne spécifique de la grille

  Arguments :
  - grille : list[list[str]] - une grille représentée par une liste de listes où chaque élément est une valeur représentant un emplacement dans la grille
  - joueur : str - le caractère représentant le joueur ("*" ou 'o')
  - ligne : int - l'indice de la ligne à vérifier dans la grille

  Retour :
  - int : le nombre de jetons alignés horizontalement pour le joueur dans la ligne spécifiée

  préconditions : joueur et ligne valide
  postconditions : 0 <= alignement <= 4
  """

  assert joueur in ('o', '*'), "joueur non valide"
  assert ligne >= 0 and ligne <= 6, "ligne non valide"
  compteur, i, align_max = 0, 0, [] 
  while i <= 6 and compteur < 4: 
    if grille[ligne][i] == joueur : 
      compteur += 1 
    else :
      if compteur != 0 : align_max.append(compteur)
      compteur = 0 
    i += 1
  if compteur != 0 : align_max.append(compteur)
  if align_max == [] : return 0
  alignement = maximum_liste(align_max)[0]
  assert alignement >= 0 and alignement <= 4
  return alignement

def verif_comb_vertical(grille, joueur, colonne):
  """
Vérifie la présence d'une combinaison verticale du symbole du joueur dans une colonne spécifique de la grille.

Arguments :
- grille : list[list[str]] - Une grille représentée par une liste de listes où chaque élément est une valeur représentant un emplacement dans la grille.
- joueur : str - Le caractère représentant le joueur ("X" ou "O").
- colonne : int - L'indice de la colonne à vérifier dans la grille.

Retour :
- int : Le nombre de jetons alignés verticalement pour le joueur dans la colonne spécifiée.

préconditions : joueur et colonne valide
postconditions : 0 <= alignement <= 4
"""
  assert joueur in ('o', '*'), "joueur non valide"
  assert colonne >= 0 and colonne <= 6, "colonne non valide"
  compteur, i, align_max = 0, 0, []
  while i <= 5 and compteur < 4 : 
    if grille[i][colonne] == joueur:
      compteur += 1  
    else:
      if compteur != 0 : align_max.append(compteur)
      compteur = 0  
    i += 1 
  if compteur != 0 : align_max.append(compteur)
  if align_max == [] : return 0
  alignement = maximum_liste(align_max)[0]
  assert alignement >= 0 and alignement <= 4
  return alignement  

def verif_comb_diagonal1(grille, joueur):
  """
  Vérifie la présence d'une combinaison diagonale ascendante vers la droite du symbole du joueur dans une grille à partir d'une colonne spécifique.

  Arguments :
  - grille : list[list[str]] - Une grille représentée par une liste de listes où chaque élément est une valeur représentant un emplacement dans la grille.
  - joueur : str - Le caractère représentant le joueur ("*" ou "o").
  - colonne : int - L'indice de la colonne à partir de laquelle vérifier la diagonale ascendante droite.

  Retour :
  - int : Le nombre de jetons alignés en diagonale ascendante droite pour le joueur à partir de la colonne spécifiée.

  préconditions : joueur valide
  postconditions : 0 <= alignement <= 9
  """

  assert joueur in ('o', '*'), "joueur non valide"
  compteur, ligne, colonne, align_max = 0, 0, 0, [] 
  while compteur < 4 and ligne <= 5:  
    compteur = 0
    if grille[ligne][colonne] == joueur:
      ind_l, ind_c = ligne + 1, colonne + 1
      compteur += 1  
      while ind_l <= 5 and ind_c <= 6 and grille[ind_l][ind_c] == joueur:
        compteur += 1  
        ind_l += 1
        ind_c += 1
      align_max.append(compteur)
    else:
      compteur = 0  
    if colonne == 6 : 
      colonne = 0
      ligne += 1
    else : colonne += 1

  if compteur != 0 : align_max.append(compteur)
  if align_max == [] : return 0
  alignement = maximum_liste(align_max)[0]
  assert alignement >= 0
  return alignement

def verif_comb_diagonal2(grille, joueur):
  """
  Vérifie la présence d'une combinaison diagonale ascendante vers la gauche du symbole du joueur dans une grille à partir d'une colonne spécifique.

  Arguments :
  - grille : list[list[str]] - Une grille représentée par une liste de listes où chaque élément est une valeur représentant un emplacement dans la grille.
  - joueur : str - Le caractère représentant le joueur ("*" ou "o").
  - colonne : int - L'indice de la colonne à partir de laquelle vérifier la diagonale ascendante gauche.

  Retour :
  - int : Le nombre de jetons alignés en diagonale ascendante gauche pour le joueur à partir de la colonne spécifiée.

  préconditions : joueur valide
  postconditions : 0 <= alignement <= 4
  """

  assert joueur in ('o', '*'), "joueur non valide"
  compteur, ligne, colonne, align_max = 0, 0, 6, [] 
  while compteur < 4 and ligne <= 5: 
    compteur = 0
    if grille[ligne][colonne] == joueur:
      ind_l, ind_c = ligne + 1, colonne - 1
      compteur += 1  
      while ind_l <= 5 and ind_c >= 0 and grille[ind_l][ind_c] == joueur:
        compteur += 1  
        ind_l += 1
        ind_c -= 1
      align_max.append(compteur)
    else:
      compteur = 0  
    if colonne == 0 : 
      colonne = 6
      ligne += 1
    else : colonne -= 1 

  if compteur != 0 : align_max.append(compteur)
  if align_max == [] : return 0
  alignement = maximum_liste(align_max)[0]
  assert alignement >= 0 and alignement <= 4
  return alignement

def jetons_alignes(grille, joueur):
  """
Vérifie si le joueur a aligné quatre jetons (symboles) horizontalement, verticalement ou en diagonale dans une grille.

Arguments :
- grille : list[list[str]] - Une grille représentée par une liste de listes où chaque élément
          est une valeur représentant un emplacement dans la grille.
- joueur : str - Le caractère représentant le joueur ("o" ou "*").

Retour :
- bool : True si le joueur a aligné quatre jetons, False sinon.

préconditions : grille et joueur valide
"""

  assert len(grille) == 7, "grille non valide"  
  assert joueur in ('o', '*'), "joueur non valide"  
  if verif_comb_diagonal1(grille, joueur) >= 4 or verif_comb_diagonal2(grille, joueur) >= 4 :
    return 4
  for i in range(len(grille)):  
    assert len(grille[i]) == 7, "grille non valide"  
    if (
        verif_comb_horizontal(grille, joueur, i) >= 4
        or verif_comb_vertical(grille, joueur, i) >= 4
    ):
        return True 
  return False 