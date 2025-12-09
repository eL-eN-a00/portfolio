from random import *
def liste_colonnes():
  """Spécifications:

  Objectif:
  définir la liste qui prend valeurs de 1 à 6, ensuite,                                           OUT
  choisir un indice aléatoirement  dans cette liste, cela nous donnera la colonne à jouer         OUT

  
  """
  liste_colonnes = [1,2,3,4,5,6,7]
  random = randint(0,6)
  return liste_colonnes[random]