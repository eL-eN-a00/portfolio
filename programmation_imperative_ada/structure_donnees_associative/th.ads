with LCA;
-- Définition de tables de hachage pour accéder à la SDA.
generic
    type T_Cle is private;
    type T_Valeur is private;
	T_Taille : Integer;
   -- fonction qui à partir d'une clé calcule la position de sa valeur (valeur de hachage) dans le tableau de hachage.
    with function Fonction_Hachage(Cle : in T_Cle) return Integer;

package TH is
    
    package LCA_type is
      new LCA (T_Cle => T_Cle, T_Valeur => T_Valeur);
    use LCA_type;

	type T_THachage is limited private;

	-- Initialiser une table de hachage. La table de hachage est vide.
	procedure Initialiser(TH: out T_THachage) with
		Post => Est_Vide (TH);


	-- Détruire une table de hachage. Elle ne devra plus être utilisée.
	procedure Detruire (TH : in out T_THachage);


	-- Est-ce qu'une table de hachage est vide ?
	function Est_Vide (TH : in T_THachage) return Boolean;


	-- Obtenir le nombre d'éléments d'une Sda. 
	function Taille (TH : in T_THachage) return Integer with
		Post => Taille'Result >= 0
            and (Taille'Result = 0) = Est_Vide (TH);
    
   
	-- Enregistrer une valeur associée à un indice dans une Sda dans une table de hachage grâce à une fonction de hachage
	procedure Enregistrer (TH : in out T_THachage ; Cle : in T_Cle ; Valeur : in T_Valeur ) with
		Post => Cle_Presente (TH, Cle) and (La_Valeur (TH, Cle) = Valeur)   -- valeur insérée
				and (not (Cle_Presente (TH, Cle)'Old) or Taille (TH) = Taille (TH)'Old)
				and (Cle_Presente (TH, Cle)'Old or Taille (TH) = Taille (TH)'Old + 1);


	-- Supprimer la valeur associée à une Clé dans un tableau de hachage.
	-- Exception : Cle_Absente_Error si Clé n'est pas utilisée dans le tableau de hachage
	procedure Supprimer (TH : in out T_THachage ; Cle : in T_Cle) with
		Post =>  Taille (TH) = Taille (TH)'Old - 1 -- un élément de moins
			and not Cle_Presente (TH, Cle);         -- la clé a été supprimée


	-- Savoir si une Clé est présente dans une table de hachage.
	function Cle_Presente (TH : in T_THachage ; Cle : in T_Cle) return Boolean;


	-- Obtenir la valeur associée à une Cle dans le tableau de hachage.
	-- Exception : Cle_Absente_Error si Clé n'est pas utilisée dans le tableau de hachage
	function La_Valeur (TH : in T_THachage ; Cle : in T_Cle) return T_Valeur;


	-- Appliquer un traitement (Traiter) pour chaque couple d'un tableau de hachage.
	generic
		with procedure Traiter (Cle : in T_Cle; Valeur: in T_Valeur);
	procedure Faire_Pour_Chaque (TH : in T_THachage);


	-- Afficher le tableau de hachage en révélant sa structure interne.
	generic
		with procedure Afficher_Cle (Cle : in T_Cle);
		with procedure Afficher_Donnee (Valeur : in T_Valeur);
	procedure Afficher_Debug (TH : in T_THachage);


private
    
    type T_THachage is array (1..T_Taille) of T_LCA;
    
end TH;
