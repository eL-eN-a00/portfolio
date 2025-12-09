def coup_possible(colonne, grille) : 
  """
  Spécification
  obj : décider si un coup est possible. On vérifiera si dans la première sous-liste, 
  la valeur à l'indice donné est ' '.
  IN : ind -> int (l'indice doit être un entier); grille -> liste de sous-listes de string sauf la dernière qui est de int
  OUT : booléen (true or false)
  Préconditions : 1 <= ind <= 7 (l'indice doit être compris entre 1 et 7 inclus))
  postconditions : None"""
  assert colonne <= 7 and colonne >= 1, "indice non correct"
  if grille[0][colonne - 1] == " " : return True
  return False