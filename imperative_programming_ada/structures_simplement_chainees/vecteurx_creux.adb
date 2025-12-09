with Ada.Text_IO;                 use Ada.Text_IO;
with Ada.Integer_Text_IO;         use Ada.Integer_Text_IO;
with Ada.Float_Text_IO;           use Ada.Float_Text_IO;
with Ada.Unchecked_Deallocation;

package body Vecteurs_Creux is


	procedure Free is
		new Ada.Unchecked_Deallocation (T_Cellule, T_Vecteur_Creux);


    procedure Initialiser (V : out T_Vecteur_Creux) is
	begin
		V := Null;
	end Initialiser;


	procedure Detruire (V: in out T_Vecteur_Creux) is
	begin
        if not Est_Nul(V) then
            Detruire(V.all.Suivant);
            Free(V);
        else
            Null;
        end if;

	end Detruire;


	function Est_Nul (V : in T_Vecteur_Creux) return Boolean is
	begin
		return V = Null;
	end Est_Nul;


    function Composante_Recursif (V : in T_Vecteur_Creux ; Indice : in Integer) return Float is
        Resultat_Trouve : Float;
	begin
        Resultat_Trouve := 0.0;
        if not Est_Nul(V) and then V.all.Indice = Indice then
           Resultat_Trouve := V.all.Valeur;
        elsif not Est_Nul(V) then
            Resultat_Trouve := Composante_Recursif(V.Suivant, Indice);
        else
            Null;
        end if;
        return Resultat_Trouve;
	end Composante_Recursif;


	function Composante_Iteratif (V : in T_Vecteur_Creux ; Indice : in Integer) return Float is
        Courant : T_Vecteur_Creux;
        Resultat_Trouve : Float;
        Est_Trouve : Boolean;
    begin
        Resultat_Trouve := 0.0;
        Courant := V;
        Est_Trouve := False;
        while not Est_Nul(Courant) and not Est_Trouve loop
            if Courant.all.Indice = Indice then
                Resultat_Trouve := Courant.all.Valeur;
                Est_Trouve := True;
            else
                Courant := Courant.Suivant;
            end if;
        end loop;
        return Resultat_Trouve;
	end Composante_Iteratif;


	procedure Modifier (V : in out T_Vecteur_Creux ;
				       Indice : in Integer ;
                     Valeur : in Float ) is
        Courant: T_Vecteur_Creux;
        Precedent: T_Vecteur_Creux;
        Temp: T_Vecteur_Creux;
    begin
        Courant := V;
        Precedent := Null;
        while not Est_Nul(Courant) and then (Courant.all.Indice /= Indice  and Courant.all.Indice > Indice) loop
            Precedent := Courant;
            Courant := Courant.Suivant;
        end loop;
        if Est_Nul(Courant) then
            Courant := new T_Cellule;
            Courant.all.Indice := Indice;
            Courant.all.Valeur := Valeur;
            Courant.all.Suivant := Null;
            Precedent.all.Suivant := Courant;
        elsif Courant.all.Indice = Indice then
            Courant.all.Valeur := Valeur;
        elsif Courant.all.Indice < Indice then
            Temp := new T_Cellule;
            Temp.all.Indice := Indice;
            Temp.all.Valeur := Valeur;
            Temp.all.Suivant := Courant;
            Precedent.all.Suivant := Temp;
        else
            null;
        end if;
	end Modifier;


	function Sont_Egaux_Recursif (V1, V2 : in T_Vecteur_Creux) return Boolean is
	begin
		return False;	-- TODO : à changer
	end Sont_Egaux_Recursif;


	function Sont_Egaux_Iteratif (V1, V2 : in T_Vecteur_Creux) return Boolean is
	begin
		return False;	-- TODO : à changer
	end Sont_Egaux_Iteratif;


    procedure Additionner (V1 : in out T_Vecteur_Creux; V2 : in T_Vecteur_Creux) is
        V3 : T_Vecteur_Creux;
        Compteur : Integer;
        Courant_V1 : T_Vecteur_Creux;
        Courant_V2 : T_Vecteur_Creux;
    begin
        Initialiser(V3);
        Compteur := 1;
        Courant_V1 := V1;
        Courant_V2 := V2;
        while not Est_Nul(Courant_V1) and not Est_Nul(Courant_V2) loop
            Modifier(V3, Compteur, Courant_V1.all.Valeur + Courant_V2.all.Valeur);
            Compteur := Compteur + 1;
            Courant_V1 := Courant_V1.Suivant;
            Courant_V2 := Courant_V2.Suivant;
        end loop;
        while not Est_Nul(Courant_V1) loop
            Modifier(V3, Compteur, Courant_V1.all.Valeur);
            Compteur := Compteur + 1;
        end loop;
        while not Est_Nul(Courant_V2) loop
            Modifier(V3, Compteur, Courant_V2.all.Valeur);
            Compteur := Compteur + 1;
        end loop;
	end Additionner;


    function Norme2 (V : in T_Vecteur_Creux) return Float is
        Courant_V : T_Vecteur_Creux;
        Norme : Float;
    begin
        Courant_V := V;
        if not Est_Nul(Courant_V) then
        --    Courant_V.all.Valeur := Courant_V.all.Valeur^2;
            null;
        end if;
        return 0.0;
	end Norme2;


	Function Produit_Scalaire (V1, V2: in T_Vecteur_Creux) return Float is
	begin
		return 0.0;	-- TODO : à changer
	end Produit_Scalaire;


	procedure Afficher (V : T_Vecteur_Creux) is
	begin
		if V = Null then
			Put ("--E");
		else
			-- Afficher la composante V.all
			Put ("-->[ ");
			Put (V.all.Indice, 0);
			Put (" | ");
			Put (V.all.Valeur, Fore => 0, Aft => 1, Exp => 0);
			Put (" ]");

			-- Afficher les autres composantes
			Afficher (V.all.Suivant);
		end if;
	end Afficher;


	function Nombre_Composantes_Non_Nulles (V: in T_Vecteur_Creux) return Integer is
	begin
		if V = Null then
			return 0;
		else
			return 1 + Nombre_Composantes_Non_Nulles (V.all.Suivant);
		end if;
	end Nombre_Composantes_Non_Nulles;


end Vecteurs_Creux;
