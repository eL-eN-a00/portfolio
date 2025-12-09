with Ada.Text_IO;            use Ada.Text_IO;
with SDA_Exceptions;         use SDA_Exceptions;
with Ada.Unchecked_Deallocation;

package body LCA is

	procedure Free is
		new Ada.Unchecked_Deallocation (Object => T_Struct, Name => T_LCA);


	procedure Initialiser(Sda: out T_LCA) is
	begin
		Sda := Null;
	end Initialiser;


	function Est_Vide (Sda : T_LCA) return Boolean is
	begin
		return Sda = Null;
	end;


	procedure Detruire (Sda : in out T_LCA) is
	begin
        if not Est_Vide(Sda) then
            Detruire(Sda.all.Pointeur);
            Free(Sda);
        else
            Null;
        end if;
	end Detruire;


    function Taille (Sda : in T_LCA) return Integer is
        Compte : Integer;
        Courant : T_LCA;
	begin
        Compte := 0;
        Courant := Sda;
        while not Est_Vide(Courant) loop
            Compte := Compte + 1;
            Courant := Courant.all.Pointeur;
        end loop;
        return Compte;
	end Taille;


    function Cle_Presente (Sda : in T_LCA ; Cle : in T_Cle) return Boolean is
        Courant: T_LCA;
        Est_Presente: Boolean;
    begin
        Courant := Sda;
        Est_Presente := False;
        while not Est_Vide(Courant) and not Est_Presente loop
            if Courant.all.Cle = Cle then
                Est_Presente := True;
            else
                Courant := Courant.all.Pointeur;
            end if;
        end loop;
        return Est_Presente;
	end Cle_Presente;


    function La_Valeur (Sda : in T_LCA ; Cle : in T_Cle) return T_Valeur is
        Courant : T_LCA;
        Resultat_Trouve : T_Valeur;
        Est_Trouve : Boolean;
	begin
        Courant := Sda;
        Est_Trouve := False;
        while not Est_Vide(Courant) and not Est_Trouve loop
            if Courant.all.Cle  = Cle then
                Resultat_Trouve := Courant.all.Valeur;
                Est_Trouve := True;
            else
                null;
            end if;
            if not Est_Trouve then
                Courant := Courant.all.Pointeur;
            else
                null;
            end if;
        end loop;
        if Est_Trouve then
            return Resultat_Trouve;
        else
            raise Cle_Absente_Error;
        end if;
	end La_Valeur;


	procedure Enregistrer (Sda : in out T_LCA ; Cle : in T_Cle ; Valeur : in T_Valeur) is
	begin
		Enregistrer_Recursif(Sda, Cle, Valeur);
	end Enregistrer;


    procedure Enregistrer_Iteratif (Sda : in out T_LCA ; Cle : in T_Cle ; Valeur : in T_Valeur) is
        Courant : T_LCA;
        Preced : T_LCA;
        Nouveau : T_LCA;
        Est_Trouve : Boolean;
    begin
        Courant := Sda;
        Preced := Null;
        Est_Trouve := False;
        while not Est_Vide(Courant) and not Est_Trouve loop
            if Courant.all.Cle = Cle then
                Courant.all.Valeur := Valeur;
                Est_Trouve := True;
            else
                Preced := Courant;
                Courant := Courant.all.Pointeur;
            end if;
        end loop;
        if not Est_Trouve then
            Nouveau := new T_Struct;
            Nouveau.all.Cle := Cle;
            Nouveau.all.Valeur := Valeur;
            Nouveau.all.Pointeur := Null;
            if Est_Vide(Preced) then
                Sda := Nouveau;
            else
                Preced.all.Pointeur := Nouveau;
            end if;
        end if;
	end Enregistrer_Iteratif;


	procedure Enregistrer_Recursif (Sda : in out T_LCA ; Cle : in T_Cle ; Valeur : in T_Valeur) is
	begin
        if Est_Vide(Sda) then
            Sda := new T_Struct;
            Sda.all.Cle := Cle;
            Sda.all.Valeur := Valeur;
            Sda.all.Pointeur := Null;
      elsif Sda.all.Cle = Cle then
         Sda.all.Valeur := Valeur;
      else
         Enregistrer_Recursif (Sda.Pointeur, Cle, Valeur);
      end if;
	end Enregistrer_Recursif;


	procedure Supprimer (Sda : in out T_LCA ; Cle : in T_Cle) is
	begin
		Supprimer_Recursif(Sda, Cle);
	end Supprimer;


	procedure Supprimer_Iteratif (Sda : in out T_LCA ; Cle : in T_Cle) is
      Courant : T_LCA;
      Precedent : T_LCA;
	begin
      Courant := Sda;
      Precedent := Null;
      while not Est_Vide(Courant) and then Courant.all.Cle /= Cle loop
         Precedent := Courant;
         Courant := Courant.Pointeur;
      end loop;
      if Est_Vide(Courant) then
         raise Cle_Absente_Error;
      end if;
      if Est_Vide(Precedent) then
         Sda := Courant.Pointeur;
      else
         Precedent.Pointeur := Courant.Pointeur;
      end if;
      Courant.Pointeur := Null;
	end Supprimer_Iteratif;


	procedure Supprimer_Recursif (Sda : in out T_LCA ; Cle : in T_Cle) is
     Temp : T_LCA;
	begin
		if Est_Vide(Sda) then
            raise Cle_Absente_Error;
        elsif Sda.all.Cle = Cle then
            Temp := Sda;
            Sda := Sda.Pointeur;
            Temp.Pointeur := Null;
            Temp := Null;
        else
            Supprimer_Recursif (Sda.Pointeur, Cle);
        end if;
	end Supprimer_Recursif;


	procedure Faire_Pour_Chaque (Sda : in T_LCA) is
	begin
        if not Est_Vide(Sda) then
            begin
               Traiter(Sda.Cle, Sda.Valeur);
            exception
               when others => null;
            end;
            Faire_Pour_Chaque(Sda.Pointeur);
        else
            null;
        end if;
	end Faire_Pour_Chaque;


    procedure Afficher_Debug (Sda : in T_LCA) is
        Courant : T_LCA;
    begin
        Courant := Sda;
        while not Est_Vide(Courant) loop
            Put("-->[");
            Afficher_Cle(Courant.Cle);
            Put(" : ");
            Afficher_Donnee(Courant.Valeur);
            Put("]");
            Courant := Courant.Pointeur;
        end loop;
        Put_Line("--E");
	end Afficher_Debug;


end LCA;
