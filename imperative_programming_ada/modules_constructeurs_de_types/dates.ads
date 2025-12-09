-- Correspond à la spécification du module Dates

-- Spécification d'un module Dates très simplifié.
--

package Dates is

    type T_Mois is (JANVIER, FEVRIER, MARS, AVRIL, MAI, JUIN, JUILLET,
                    AOUT, SEPTEMBRE, OCTOBRE, NOVEMBRE, DECEMBRE);
    -- Le type T_Mois ne peut pas être déclaré privé car ses valeurs doivent être connues des utilisateurs du module puisqu'elles doivent être passées en paramètre de Initialiser.
    -- Sinon, il aurait fallu ajouter d'autres opérations qui permettent d'obtenir une donnée de type T_Mois, par exemple à partir d'un entier de 1 à 12.

    type T_Date is limited private;
    -- Si limited_private, on perd l'affectation entre T_Date et l'égalité (et la différence)

    -- Initialiser une date à partir du jour, du mois et de l'année.
    --
    -- Paramètres :
    --     Date : la date à initialiser
    --     Jour : la valeur du jour
    --     Mois : la valeur du mois
    --     Annee : la valeur de l'année
    --
    --  Nécessite :
    --     Jour/Mois/Annee constituent une date valide
    --
    --  Assure
    --     Le_Jour (Date) = Jour
    --     Le_Mois (Date) = Mois
    --     L_Annee (Date) = Annee
    --
    procedure Initialiser ( Date  : out T_Date  ;
                            Jour  : in  Integer ;
                            Mois  : in  T_Mois  ;
                            Annee : in  Integer )
    with
        Pre => Annee >= 0 and Jour >= 1 and Jour <= 31, -- simplifiée !
        Post => Le_Jour (Date) = Jour and Le_Mois (Date) = Mois and L_Annee (Date) = Annee;

    -- Afficher une date sous la forme jj/mm/aaaa
    procedure Afficher (Date : in T_Date);

    -- Obtenir le mois d'une date.
    -- Paramètres
    --     Date : la date dont on veut obtenir le moi
    function Le_Mois (Date : in T_Date) return T_Mois;

    -- Obtenir le jour d'une date.
    -- Paramètres
    --     Date : la date dont on veut obtenir le jour
    function Le_Jour (Date : in T_Date) return Integer;

    -- Obtenir l'année d'une date.
    -- Paramètres
    --     Date : la date dont on veut obtenir l'année
    function L_Annee (Date : in T_Date) return Integer;


private

    type T_Date is
        record
            Jour : Integer;
            Mois : T_Mois;
            Annee : Integer;
            -- Invariant
            --    Annee > 0
            --    Jour >= 1
            --    Jour <= Nombre_Jours (Mois, Annee)
        end record;

end Dates;
