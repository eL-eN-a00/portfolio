
-- Auteur: 
-- Gérer un stock de matériel informatique.

package Stocks_Materiel is


    CAPACITE : constant Integer := 10;      -- nombre maximum de matériels dans un stock
    
    type T_Nature is (UNITE_CENTRALE, DISQUE, ECRAN, CLAVIER, IMPRIMANTE);

    type T_Stock is limited private;

    -- Créer un stock vide.
    --
    -- paramètres
    --     Stock : le stock à créer
    --
    -- Assure
    --     Nb_Materiels (Stock) = 0
    --
    procedure Creer (Stock : out T_Stock) with
        Post => Nb_Materiels (Stock) = 0;
    
    -- Obtenir le nombre de matériels dans le stock Stock
    -- Paramètres : stock : le stock dont on veut obtenir la taille
    -- Assure : Résultat >= 0 et Résultat <= Capacité
    function Nb_Materiels (Stock: in T_Stock) return Integer with
         Post => Nb_Materiels'Result >= 0 and Nb_Materiels'Result <= CAPACITE;

    -- Enregistrer un nouveau matériel dans le stock. Il est en fonctionnement. Le stock ne doit pas être plein.
    -- Paramètres :
      -- Stock : le stock à compléter
      -- Numéro_Série : le numéro de série du nouveau matériel
      -- Nature : la nature du nouveau matériel
      -- Annee_Achat : l'année d'achat du nouveau matériel
    -- Nécessite Nb_Materiels (Stock) < CAPACITE
    -- Assure
      -- Nouveau matériel ajouté
      -- Nb_Materiels (Stock) = Nb_Materiels (Stock)'Avant + 1
    procedure Enregistrer(Stock : in out T_Stock; Numero_Serie : in Integer; Nature : In Integer; Annee_Achat : in Integer) with
      Pre => Nb_Materiels (Stock) < CAPACITE,
      Post => Nb_Materiels (Stock) = Nb_Materiels (Stock)'Old + 1;
   
    -- Obtenir le nombre de matériels hors d'usage dans le stock Stock
    -- Paramètres : le stock à considérer
    -- Assure Résultat >= 0 et Résultat <= Nb_Materiels (Stock)
    function Nb_Materiels_HS (Stock : in T_Stock) return Integer with
      post => 0 <= Nb_Materiels_HS'Result and Nb_Materiels_HS'Result <= Nb_Materiels (Stock);

    -- Savoir si un numéro de série correspond bien à un matériel de stock.
    -- Paramètres : 
      -- Stock : le stock à considérer
      -- Numero_Serie : le numéro de série du matériel cherché
    function Est_Dans_Stock(Stock : in T_Stock; Numero_Serie : in Integer) return Boolean;

    -- Savoir si un matériel du stock est en état de marche
    -- Paramètres : 
      -- Stock : le stock à considérer
      -- Numero_Serie : le numéro de série du matériel recherché
    -- Nécessite Est_Dans_Stock (Stock, Numero_Serie)
    function Est_En_Marche(Stock : in T_Stock; Numero_Serie : in Integer) return Boolean;

    -- Modifier l'état d'un matériel du stock. Le matériel est identifié par son numéro de série.
    -- Paramètres :
      -- Stock : le stock à considérer
      -- Numero_Serie : le numéro de série du matériel à considérer
      -- En_Marche : Est-ce que le matériel est en état de marche ?
    -- Nécessite Est_Dans_Stock (Stock, Numero_Serie)
    -- Assure Etat_Materiel (Stock, Numero_Serie) = En_Marche
    procedure Modifier_Etat_Materiel(Stock : in out T_Stock; Numero_Serie : Integer; En_Marche : in Boolean) with
      pre => Est_Dans_Stock (Stock, Numero_Serie);
   
    -- Supprimer d'un stock un matériel. Le matériel est identifié par son numéro de série. Si le matériel n'est pas dans le stock, le matériel reste inchangé.
    -- Paramètres :
      -- Stock : le stock à considérer
      -- Numero_Serie : le numéro de série du matériel à considérer
    -- Assure not Est_Dans_Stock (Stock, Numero_Serie)
    procedure Supprimer_Un_Materiel (Stock : in out T_Stock; Numero_Serie : in Integer) with
      post => not Est_Dans_Stock (Stock, Numero_Serie);
   
    -- Obtenir le nombre de matériels d'un stock achetés avant une certaine date
    -- Paramètres :
      -- Stock : le stock à considérer
      -- Annee : l'année avant laquelle les matériels sont anciens
    -- Assure Nb_Materiels_Anciens'Result >= 0 and Nb_Materiels_Anciens'Result <= Capacité
    function Nb_Materiels_Anciens(Stock : in T_Stock; Annee : in Integer) return Integer with
      post => Nb_Materiels_Anciens'Result >= 0 and Nb_Materiels_Anciens'Result <= Nb_Materiels (Stock);

    -- afficher les matériels du stock
    -- Paramètre : Stock : le stock à considérer
    procedure Afficher_Un_Materiel(Stock : in T_Stock; Numero_Serie : in Integer);
    
    private
      type T_Materiel is -- Informations concernant un matériel
         record
            Nb_Serie : Integer; -- numéro de série
            Nature : T_Nature; -- la nature de l'équipement
            Annee : Integer; -- année d'acquisition
            En_Etat : Boolean; -- est-ce qu'il est en état de marche ?
         end record;
      
      type T_Tableau_Materiel is array(1..CAPACITE) of T_Materiel;

      type T_Stock is -- le stock de matériel
         record
            Materiels : T_Tableau_Materiel;
            Taille : Integer;
         end record;
end Stocks_Materiel;

