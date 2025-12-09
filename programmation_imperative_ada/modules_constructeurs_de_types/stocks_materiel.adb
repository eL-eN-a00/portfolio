--with Ada.Text_IO;          use Ada.Text_IO;
--with Ada.Integer_Text_IO;  use Ada.Integer_Text_IO;

-- Auteur: 
-- Gérer un stock de matériel informatique.
--
package body Stocks_Materiel is

    procedure Creer (Stock : out T_Stock) is
    begin
        Stock.taille := 0;
    end Creer;


    function Nb_Materiels (Stock: in T_Stock) return Integer is
    begin
        return Stock.taille;
    end;


    procedure Enregistrer (
            Stock        : in out T_Stock;
            Numero_Serie : in     Integer;
            Nature       : in     T_Nature;
            Annee_Achat  : in     Integer
                          ) is
    begin
      Stock.Taille := Stock.Taille + 1;
      Stock.Materiels(Stock.Taille) := (Numero_Serie, Nature, Annee_Achat, True);
    end;
    
    function Nb_Materiels_HS (Stock : in T_Stock) return Integer is
      Resultat : Integer;
    begin
      Resultat := 0;
      for i in 1..Stock.Taille loop
         -- Invariant : Resultat = nb de materiels HS dans Stock (1..i)
         if not Stocks.Materiels (i).En_Etat then
            Resultat := Resultat + 1;
         else
            null;
         end if;
      end loop;
      return Resultat;
     end Nb_Materiels_HS;

     -- Obtenir l'indice d'un matériel dans un stock à partir de son numéro de série. 
     --  Retourne 1 de plus que le nombre de matériels si le numéro de série ne correspond à aucun matériel dans le stock
     -- Paramètres :
      -- Stock : le stock à considérer
      -- Numero_Serie : le numéro de série du matériel recherché
    -- Assure Indice'Result >= 1 and Indice'Result <= Nb_Materiels(Stock) + 1
    function Indice_Materiel(Stock : in T_Stock; Numero_Serie : Integer) return Integer with
      post => Indice_Materiel'Result >= 1 and Indice_Materiel'Result <= Nb_Materiels (Stock) + 1
    is
      Indice : Integer;
    begin
      Indice := 1;
      while Indice <= Stock.Taille and Stock.Materiels(Indice).Nb_Serie /= Numero_Serie loop
         Indice := Indice + 1;
      end loop;
      return Indice;
    end Indice_Materiel;

    function Est_Dans_Stock (Stock : in T_Stock; Numero_Serie : in Integer) return Boolean is
    begin
      return Indice_Materiel (Stock, Numero_Serie) <= Stock.Taille;
    end Est_Dans_Stock;

    function Est_En_Marche(Stock : in T_Stock ; Numero_Serie : in Integer) return Boolean is
    begin
      return Stock.Materiels (Indice_Materiel (Stock, Numero_Serie)).En_Etat;
    end Est_En_Marche;

    procedure Modifier_Etat_Materiel(Stock : in out T_Stock; Numero_Serie : Integer; En_Marche : in Boolean) is
    begin
      Stock.Materiels(Indice_Materiel(Stock, Numero_Serie)).En_Etat := En_Marche;
    end Modifier_Etat_Materiel;

    procedure Supprimer_Un_Materiel(Stock : in out T_Stock; Numero_Serie : in Integer) is
      Indice : Integer;
    begin
      Indice := Indice_Materiel(Stock, Numero_Serie);
      if Indice <= Stock.Taille then
         Stock.Materiels(Indice) := Stock.Materiels(Stock.Taille);
         Stock.Taille := Stock.Taille - 1;
      else
         null;
      end if;
    end Supprimer_Un_Materiel;

    function Nb_Materiels_Anciens (Stock : in T_Stock; Annee : in Integer) return Integer is
      Resultat : Integer;
    begin
      Resultat := 0;
      for Indice in 1..Stock.Taille loop
         if Stock.Materiels (Indice).Annee <= Annee then
            Resultat := Resultat + 1;
         end if;
      end loop;
      return Resultat;
    end Nb_Materiels_Anciens;

end Stocks_Materiel;
