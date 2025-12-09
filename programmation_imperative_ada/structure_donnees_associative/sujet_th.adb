with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with TH;

procedure Sujet_TH is
   
   function Hash(C : Unbounded_String) return Integer is
   begin
      return To_String(C)'Length mod 11 + 1;
   end Hash;

   package TH_String_Int is new TH(
      T_Cle => Unbounded_String, 
      T_Valeur => Integer, 
      T_Taille => 11, 
      Fonction_Hachage => Hash
   );
   use TH_String_Int;

   procedure Afficher_Cle(C : in Unbounded_String) is
   begin
      Put(To_String(C));
   end Afficher_Cle;

   procedure Afficher_Donnee(V : in Integer) is
   begin
      Put(V, 0);
   end Afficher_Donnee;

   procedure Debug_Afficher is new Afficher_Debug(Afficher_Cle, Afficher_Donnee);

   Table : T_THachage;

   begin

      Initialiser(Table);

      Enregistrer(Table, To_Unbounded_String("un"), 1);
      Enregistrer(Table, To_Unbounded_String("deux"), 2);
      Enregistrer(Table, To_Unbounded_String("trois"), 3);
      Enregistrer(Table, To_Unbounded_String("quatre"), 4);
      Enregistrer(Table, To_Unbounded_String("cinq"), 5);
      Enregistrer(Table, To_Unbounded_String("quatre-vingt-dix-neuf"), 99);
      Enregistrer(Table, To_Unbounded_String("vingt-et-un"), 21);

      Debug_Afficher(Table);

      Detruire(Table);

   end Sujet_TH;