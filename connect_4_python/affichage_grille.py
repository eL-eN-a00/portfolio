from creation_grille import creer_grille 
grille = creer_grille()


def afficher_grille1(grille):
  """
  Affiche une représentation visuelle de la grille donnée.
  
Arguments : 
- grille : list[list[str]] - une grille représentée par une liste de listes où chaque élément est une valeur à afficher dans la grille.
Retour : 
- str : une chaîne de caractères représentant visuellement la grille
"""
  
  affichage = "" 
  for ligne in grille :
    for colonne in ligne :
      affichage += " " + str(colonne) + " "
    affichage += " \n"
  return affichage

def afficher_grille2(grille) :
  """
  Affiche une représentation visuelle de la grille donnée avec une ligne de séparation en bas
  
  Arguments : 
  - grille : list[list[str]] - une grille représentée par une liste de listes où chaque élément est une valeur à afficher dans la grille.
  Retour : 
  - str : une chaîne de caractères représentant visuellement la grille avec une ligne de séparation
  """
  
  affichage, longueur = "", len(grille) - 1 
  for ligne in range(longueur) :
    for colonne in range(len(grille[ligne])) :
      affichage += " " + str(grille[ligne][colonne]) + " "
    affichage += " \n" 
  affichage += " - - - - - - - "
  return affichage

def afficher_grille3(grille) :
  """
  Affiche une représentation visuelle de la grille donnée avec des indices de lignes et une ligne supplémentaire en haut et en bas
   
  Arguments : 
  - grille : list[list[str]] - une grille représentée par une liste de listes où chaque élément est une valeur à afficher dans la grille.
  
  Retour : 
  - str : une chaîne de caractères représentant visuellement la grille avec des indices de ligne en miroir
  """
  affichage, longueur = "  ", len(grille) 
  for colonne in range(len(grille[6])) :
    affichage += "  " + str(grille[6][colonne]) + ""
  affichage += "\n"
  for ligne in range(longueur) :
    affichage += str(6-ligne) + "  " 
    for colonne in range(len(grille[ligne])) :
      affichage += " " + str(grille[ligne][colonne]) + " "
    affichage += "  " + str(6-ligne) + "\n"
  return affichage

def afficher_grille4(grille) :
  """
  Affiche une représentation visuelle de la grille donnée avec des indices de lignes sous forme de lettres

  Arguments : 
  - grille : list[list[str]] - une grille représentée par une liste de listes où chaque élément est une valeur à afficher dans la grille.

  Retour : 
  - str : une chaîne de caractères représentant visuellement la grille avec des indices de ligne en lettres
  """
  affichage = "   1  2  3  4  5  6  7   \n" 
  lettres = ["F", "E", "D", "C", "B", "A", " "] 
  longueur = len(grille)
  for ligne in range(longueur) :
    affichage += lettres[ligne] + " " 
    for colonne in range(len(grille[ligne])) :
      affichage += " " + str(grille[ligne][colonne]) + " " 
    affichage += lettres[ligne] + " \n" 
  return affichage

def afficher_grille5(grille) :
  """
  Affiche une représentation visuelle élaborée de la grille donnée avec des bordures, des indices de lignes et de colonnes

  Arguments : 
  - grille : list[list[str]] - une grille représentée par une liste de listes où chaque élément est une valeur à afficher dans la grille.

  Retour : 
  - str : une chaîne de caractères représentant visuellement la grille avec des bordures et des indices
  """
  numeros = "    1   2   3   4   5   6   7    \n" 
  affichage = numeros 
  cases = "  |---|---|---|---|---|---|---| \n" 
  longueur = len(grille)-1 
  for ligne in range(longueur) :
    affichage += cases + str(6 - ligne) + " " 
    for colonne in range(len(grille[ligne])) :
      affichage += "| " + str(grille[ligne][colonne]) + " " 
    affichage += "| " + str(6-ligne) +  "\n" 
  affichage += "  +---+---+---+---+---+---+---+ \n" + numeros
  return affichage

def affichage(grille, numero) :
  """
  Selecteur pour différentes fonctions d'affichage de grille en fonction du numéro donné.

  Arguments :
  - grille : list[list[str]] - une grille représentée par une liste de listes où chaque élément est une valeur à afficher dans la grille
  - numero : int - un numéro indiquant quelle fonction d'affichage de grille utiliser

  Retour : 
  - str : une chaîne de caractères représentant visuellement la grille en utilisant la fonction spécifiée

  Préconditions : 1 <= numero <= 5
  """
  assert numero in [1, 2, 3, 4, 5], "numéro non valide"
  if numero == 1 : 
    return afficher_grille1(grille)
  elif numero == 2 : 
    return afficher_grille2(grille)
  elif numero == 3 : 
    return afficher_grille3(grille)
  elif numero == 4 : 
    return afficher_grille4(grille)
  else : 
    return afficher_grille5(grille)