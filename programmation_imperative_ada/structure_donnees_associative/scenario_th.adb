with Ada.Text_IO;           use Ada.Text_IO;
with Ada.Integer_Text_IO;   use Ada.Integer_Text_IO;
with Ada.Float_Text_IO;     use Ada.Float_Text_IO;
with Sda_Exceptions;		use Sda_Exceptions;
with TH;

-- Exemple simple d'utilisation d'une TH.
-- On pourra utiliser le type Integer pour les clés et le type Float
-- pour les valeurs.
procedure Scenario_TH is

   TTTaille : constant Integer := 11;

   function Hash(Cle : in Integer) return Integer is
   begin
      return (abs(Cle) mod TTTaille) + 1;
   end Hash;

   package TH_Int_Float is new TH(
      T_Cle            => Integer,
      T_Valeur         => Float,
      T_Taille         => TTTaille,
      Fonction_Hachage => Hash
   );
   use TH_Int_Float;

   procedure Afficher_Cle(Cle : in Integer) is
   begin
      Put(Cle, 0);
   end Afficher_Cle;

   procedure Afficher_Valeur(Valeur : in Float) is
   begin
      Put(Valeur, 0, 2);
   end Afficher_Valeur;

   procedure Afficher_Couple(Cle : in Integer; Valeur : in Float) is
   begin
      Put("[clé=");
      Put(Cle, 0);
      Put(", valeur=");
      Put(Valeur, 0, 2);
      Put("]");
      New_Line;
   end Afficher_Couple;

   procedure Afficher_Tous is new Faire_Pour_Chaque(Afficher_Couple);
   procedure Afficher_Debug2 is new Afficher_Debug(Afficher_Cle, Afficher_Valeur);

   Ma_TH : T_THachage;
   V : Float;

begin
	
   Put_Line("=== Scénario d'utilisation de TH ===");
   New_Line;

   Put_Line("1. Initialisation de la table");
   Initialiser(Ma_TH);
   Put(" Est vide ?");
   Put_Line(Boolean'Image(Est_Vide(Ma_TH)));
   Put(" Taille : ");
   Put(Taille(Ma_TH), 0);
   New_Line;
   Afficher_Debug2(Ma_TH);
   New_Line;

   Put_Line("2. Enregistrement de quelques couples (Cle, Valeur)");
   Enregistrer(Ma_TH, 10, 3.14);
   Enregistrer(Ma_TH, 20, 2.71);
   Enregistrer(Ma_TH, 5, 1.41);
   Enregistrer(Ma_TH, 15, 9.81);
   Enregistrer(Ma_TH, 33, 6.67);
   Put("Taille après ajouts : ");
   Put(Taille(Ma_TH), 0);
   New_Line;
   Afficher_Debug2(Ma_TH);
   New_Line;

   Put_Line("3. Recherche de clés");
   Put("La clé 10 est présente ? ");
   Put_Line(Boolean'Image(Cle_Presente(Ma_TH, 10)));
   if Cle_Presente (Ma_TH, 10) then
      Put("Valeur associée à 10 : ");
      Put(La_Valeur(Ma_TH, 10), 0);
      New_Line;
   else
      null;
   end if;
   Put("La clé 100 est présente ? ");
   Put_Line(Boolean'Image(Cle_Presente(Ma_TH, 100)));
   Afficher_Debug2(Ma_TH);
   New_Line;

   Put_Line("4. Mise à jour de la valeur pour la clé 20");
   Put(" Ancienne valeur : ");
   Put(La_Valeur(Ma_TH, 20));
   New_Line;
   Enregistrer(Ma_TH, 20, 5.55);
   Put(" Nouvelle valeur : ");
   Put(La_Valeur(Ma_TH, 20));
   New_Line;
   Put(" Taille (inchangée) : ");
   Put(Taille(Ma_TH), 0);
   New_Line;
   Afficher_Debug2(Ma_TH);
   New_Line;

   Put_Line("5. Affichage de tous les couples : ");
   Afficher_Tous(Ma_TH);
   New_Line;

   Put_Line("6. Suppression de la clé 5");
   Supprimer(Ma_TH, 5);
   Put(" La clé 5 est encore présente ? ");
   Put_Line(Boolean'Image(Cle_Presente(Ma_TH, 5)));
   Put(" Taille après suppression : ");
   Put(Taille(Ma_TH), 0);
   New_Line;
   Afficher_Debug2(Ma_TH);
   New_Line;

   Put_Line("7. Test d'exception : accès à une clé absente");
   begin
      V := La_Valeur(Ma_TH, 999);
      Put(" Valeur trouvée : ");
      Put(V, 0);
      New_Line;
   exception
      when Cle_Absente_Error => Put_Line("Exception capturée : Cle_Absente_Error");
   end;

   Detruire(Ma_TH);
   Put_Line("TH détruite.");

end Scenario_TH;
