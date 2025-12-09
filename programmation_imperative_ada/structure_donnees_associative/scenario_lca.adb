with Ada.Text_IO;           use Ada.Text_IO;
with Ada.Integer_Text_IO;   use Ada.Integer_Text_IO;
with Ada.Float_Text_IO;     use Ada.Float_Text_IO;
with Sda_Exceptions;		use Sda_Exceptions;
with lca;

-- Exemple simple d'utilisation d'une LCA.
procedure Scenario_LCA is

    package SDA_Int_Float is
            new lca (T_Cle => Integer, T_Valeur => Float);
    use SDA_Int_Float;

    procedure Afficher_Cle_Debug(Cle : Integer) is
    begin
      Put(Cle, 0);
    end Afficher_Cle_Debug;

    procedure Afficher_Donnee_Debug(Valeur : Float) is
    begin
      Put(Valeur, 0, 2);
    end Afficher_Donnee_Debug;

    procedure Debug is
      new Afficher_Debug(Afficher_Cle_Debug, Afficher_Donnee_Debug);

    procedure Afficher_Paire(Cle : Integer; Valeur : Float) is
    begin
      Put("[clé=");
      Put(Cle, 0);
      Put(", valeur=");
      Put(Valeur, 0, 2);
      Put("]");
      New_Line;
    end Afficher_Paire;

    procedure Pour_Chaque is
      new Faire_Pour_Chaque(Afficher_Paire);
    
    Sda : T_LCA;
begin
   Put_Line("Scénario d'utilisation de la SDA");

   Initialiser(Sda);
   Put("Sda initialisée, vide = ");
   Put(Boolean'Image(Est_Vide(Sda)));
   New_Line;

   Enregistrer(Sda, 10, 1.1);
   Enregistrer_Iteratif (Sda, 20, 2.2);
   Enregistrer_Recursif (Sda, 30, 3.3);
   Put_Line("Trois éléments enregistrés.");
   Put("Taille = ");
   Put(Taille(Sda), 0);
   New_Line;
   Debug(Sda);

   Put("Clé 20 présente ? ");
   Put(Boolean'Image(Cle_Presente(Sda, 20)));
   New_Line;
   Put("Valeur associée à 30 = ");
   Put(La_Valeur(Sda, 30));
   New_Line;

   Enregistrer (Sda, 20, 99.9);
   Put_Line("Valeur de la clé 20 mise à jour.");
   Debug(Sda);

   Supprimer(Sda, 10);
   Put_Line("Suppression clé 10.");
   Debug(Sda);
   Supprimer_Iteratif(Sda, 30);
   Put_Line("Supression clé 30 (itératif).");
   Debug(Sda);
   Supprimer_Recursif (Sda, 20);
   Put_Line("Supression clé 20 (récursive)");
   Debug(Sda);
   Put("Sda vide ? ");
   Put(Boolean'Image(Est_Vide(Sda)));
   New_Line;

   Enregistrer(Sda, 1, 10.0);
   Enregistrer (Sda, 2, 20.0);
   Enregistrer (Sda, 3, 30.0);
   Put_Line("Parcours avec Faire_Pour_Chaque : ");
   Pour_Chaque(Sda);

   Put_Line("--- Test d'exception : Cle_Absente_Error ---");
   begin
      Put_Line("Tentative de supression de la clé 999...");
      Supprimer(Sda, 999);
      Put_Line("ERREUR : l'exception aurait dû être levée ! ");
   exception
      when Cle_Absente_Error => Put_Line("Exception capturée : Cle_Absente_Error");
   end;
   New_Line;

   Detruire (Sda);
   Put_Line("Sda détruite.");
end Scenario_LCA;
