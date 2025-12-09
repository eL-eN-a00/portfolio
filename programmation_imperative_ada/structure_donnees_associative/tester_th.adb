with Ada.Text_IO; use Ada.Text_IO;
with SDA_Exceptions; use SDA_Exceptions;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with TH;

procedure Tester_TH is

   generic 
      type K is private;
      type V is private;
      K1, K2, K3, K4, K5 : K;
      V1, V2, V3, V4 : V;
      with package P_TH is new TH (
         T_Cle => K, 
         T_Valeur => V, 
         T_Taille => 11, 
         Fonction_Hachage => <>);
      with function "+" (Left, Right : V) return V;
   
   package Testeur_TH is
      procedure Tester_Tout;
      procedure Tester_Presque_Tout;
      procedure Tester_Traiter;
   end Testeur_TH;

   package body Testeur_TH is

      procedure Tester_Tout is
      begin
         Tester_Presque_Tout;
         Tester_Traiter;
      end Tester_Tout;

      procedure Tester_Presque_Tout is
         THab : P_TH.T_THachage;
      begin

         P_TH.Initialiser(THab);
         pragma Assert(P_TH.Est_Vide(THab));
         pragma Assert(P_TH.Taille(THab) = 0);

         P_TH.Enregistrer(THab, K1, V1);
         P_TH.Enregistrer(THab, K2, V2);
         P_TH.Enregistrer(THab, K3, V3);
         P_TH.Enregistrer(THab, K4, V4);

         pragma Assert(P_TH.Taille(Thab) = 4);

         pragma Assert(P_TH.La_Valeur(THab, K1) = V1);
         pragma Assert(P_TH.La_Valeur(THab, K2) = V2);
         pragma Assert(P_TH.La_Valeur(THab, K3) = V3);
         pragma Assert(P_TH.La_Valeur(THab, K4) = V4);

         P_TH.Supprimer(THab, K4);
         pragma Assert(P_TH.Taille(THab) = 3);
         pragma Assert(not P_TH.Cle_Presente(THab, K4));

         begin
            P_TH.Supprimer(THab, K5);
            pragma Assert(False);
         exception
            when Cle_Absente_Error => null;
         end;
      
         P_TH.Detruire(THab);
            
      end Tester_Presque_Tout;

      procedure Tester_Traiter is
         THab : P_TH.T_THachage;
         Somme : V;
         procedure Sommer(C : K; Vv : V) is
            pragma Unreferenced(C);
         begin
            Somme := Somme + Vv;
         end Sommer;
         procedure FPQ is new P_TH.Faire_Pour_Chaque(Traiter => Sommer);
      begin
         P_TH.Initialiser(THab);
         P_TH.Enregistrer(THab, K1, V1);
         P_TH.Enregistrer(THab, K2, V2);
         P_TH.Enregistrer(THab, K3, V3);
         Somme := V1;
         FPQ(THab);
         P_TH.Detruire(THab);   
      end Tester_Traiter;
   
   end Testeur_TH;

   function Hash(C: Unbounded_String) return Integer is
   begin
      return To_String(C)'Length mod 11 + 1;
   end Hash;

    package TH_String_Integer is new TH(
      T_Cle => Unbounded_String, 
      T_Valeur => Integer, 
      T_Taille => 11, 
      Fonction_Hachage => Hash
   );
   
   package Testeur_TH_String_Integer is
      new Testeur_TH(
         K  => Unbounded_String,
         V  => Integer,
         K1 => To_Unbounded_String("un"),
         K2 => To_Unbounded_String("deux"),
         K3 => To_Unbounded_String("trois"),
         K4 => To_Unbounded_String("quatre"),
         K5 => To_Unbounded_String("cinq"),
         V1 => 1,
         V2 => 2,
         V3 => 3,
         V4 => 4,
         P_TH => TH_String_Integer,
         "+" => "+");
   
begin

   Testeur_TH_String_Integer.Tester_Tout;
   Put_Line("Testeur TH : OK");
   
end Tester_TH;
