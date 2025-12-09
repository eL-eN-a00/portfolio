with Ada.Text_IO;            use Ada.Text_IO;
with Ada.Integer_Text_IO;   use Ada.Integer_Text_IO;

package body TH is
   -- procedure Free is
		--new Ada.Unchecked_Deallocation (Object => T_Struct, Name => T_LCA);


	procedure Initialiser(TH: out T_THachage) is
    begin
        for Indice in 1..T_Taille loop
            LCA_type.Initialiser(TH(Indice));
        end loop;
	end Initialiser;


   function Est_Vide (TH : T_THachage) return Boolean is
        Est_Nul : Boolean;
        Indice : Integer;
   begin
        Est_Nul := True;
        Indice := 1;
        while Indice /= T_Taille and Est_Nul loop
            if not LCA_type.Est_Vide(TH(Indice)) then
                Est_Nul := False;
            else
                Indice := Indice + 1;
            end if;
        end loop;
        return Est_Nul;
	end Est_Vide;


	procedure Detruire (TH : in out T_THachage) is
	begin
		for Indice in 1..T_Taille loop
            LCA_type.Detruire(TH(Indice));
        end loop;
	end Detruire;


    function Taille (TH : in T_THachage) return Integer is
        Taille_Finale : Integer;
    begin
        Taille_Finale := 0;
	    for Indice in 1..T_Taille loop
            Taille_Finale := Taille_Finale + LCA_type.Taille(TH(Indice));
        end loop;
        return Taille_Finale;
	end Taille;


	function Cle_Presente (TH : in T_THachage ; Cle : in T_Cle) return Boolean is
     begin
        return LCA_type.Cle_Presente(TH(Fonction_Hachage(Cle)), Cle);
	end Cle_Presente;
    
   function La_Valeur (TH : in T_THachage; Cle : in T_Cle) return T_Valeur is
     begin
        return LCA_type.La_Valeur(TH(Fonction_Hachage(Cle)), Cle);
	end La_Valeur;


	procedure Enregistrer (TH : in out T_THachage; Cle : in T_Cle ; Valeur : in T_Valeur) is
	begin
      LCA_type.Enregistrer(TH(Fonction_Hachage (Cle)), Cle, Valeur);
	end Enregistrer;


	procedure Supprimer (TH : in out T_THachage ; Cle : in T_Cle) is
	begin
		LCA_type.Supprimer(TH(Fonction_Hachage(Cle)), Cle);
	end Supprimer;


	procedure Faire_Pour_Chaque (TH : in T_THachage) is
      procedure Faire_Pour_Chaque_LCA is new LCA_type.Faire_Pour_Chaque(Traiter);
	begin
		 for Indice in 1..T_Taille loop
            Faire_Pour_Chaque_LCA(TH(Indice));
         end loop;
	end Faire_Pour_Chaque;


	procedure Afficher_Debug (TH : in T_THachage) is
      procedure Afficher_Debug_LCA is new LCA_type.Afficher_Debug(Afficher_Cle, Afficher_Donnee);
	begin
		for Indice in 0..T_Taille-1 loop
            Put("Indice : ");
            Put(Indice, 1);
            Put(" : ");
            Afficher_Debug_LCA(TH(Indice + 1));
            New_Line;
         end loop;
	end Afficher_Debug;

    
 end TH;




