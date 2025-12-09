def creer_grille():
  """Spécification: 
  obj:
  - créer une grille de taille 7*7 
  - créer une liste de sous-listes: 
    - les sous-listes 1 à 6 seront des chaînes de caractères
    - la 7ème sous-liste sera constituée d'entiers (int)
  - définir la variable grille pour pouvoir s'en servir à l'exercice 3
  out: grille de taille 7*7 
  post-condition: la grille doit être de taille 7*7"""
  grille = [[" "," "," "," "," "," "," "],
      [" "," "," "," "," "," "," "],
      [" "," "," "," "," "," "," "],
      [" "," "," "," "," "," "," "],
      [" "," "," "," "," "," "," "],
      [" "," "," "," "," "," "," "],
      [1, 2, 3, 4, 5, 6, 7]]
  longueur = len(grille)
  assert longueur == 7 and type(grille) == list, "grille non correcte"
  for i in range(longueur) :
    assert len(grille[i]) == longueur and type(grille[i]) == list, "grille non valide"
  return grille