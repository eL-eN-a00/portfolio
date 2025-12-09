with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with LCA;

-- Exemple simple d'utilisation d'une LCA.

procedure Sujet_LCA is

   package LCA_String_Int is 
      new LCA (T_Cle => Unbounded_String, T_Valeur => Integer);
   use LCA_String_Int;

   procedure Afficher_Cle(Cle : Unbounded_String) is
   begin
      Put('"');
      Put(To_String(Cle));
      Put('"');
   end Afficher_Cle;

   procedure Afficher_Valeur(Valeur : Integer) is
   begin
      Put(Valeur, 0);
   end Afficher_Valeur;

   procedure Debug is
      new Afficher_Debug (Afficher_Cle, Afficher_Valeur);
   
   Sda : T_LCA;

begin

   Put_Line("Début du scénario minimal LCA");
   Initialiser(Sda);
   Enregistrer(Sda, To_Unbounded_String("un"), 1);
   Enregistrer(Sda, To_Unbounded_String("deux"), 2);
   Put_Line("Structure interne de la LCA :");
   Debug(Sda);

end Sujet_LCA;