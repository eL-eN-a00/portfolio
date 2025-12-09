with Ada.Text_IO;
use  Ada.Text_IO;
with Dates;
use Dates;

-- Lignes refusées par le compilateur avant limited private: 21, 32, 33
-- Lignes refusées par le compilateur après limited private: 21, 32, 33, 38, 43
procedure Exemple_Dates_Erreurs is
    Une_Date : T_Date;
    Mois_Suivant : T_Mois;
    Autre_Date : T_Date;
begin
    -- Initialiser une date
    Initialiser (Une_Date, 1, OCTOBRE, 2018);

    -- L'afficher
    Afficher (Une_Date);
    New_Line;

    -- Afficher un enter sur 2 positions
    -- On n'a pas le droit d'utiliser la procédure Afficher_Deux_Positions car elle n'apparaît pas dans la spécification du module.
    -- Même si cette procédure est bien définie dans le corps du sous-programme, on ne peut pas l'utiliser à l'extérieur du module.
    -- C'est le principe du masquage d'information.
    Afficher_Deux_Positions (2);
    New_Line;

    -- Afficher le mois suivant de Une_Date
    Mois_Suivant := T_Mois'succ (Le_Mois (Une_Date));
    Put ("Mois suivant : ");
    Put (T_Mois'Image (Mois_Suivant));
    New_Line;
    -- OK car le type T_Mois est accessible de l'utilisateur.

    -- Modifier directement la date
    -- On n'a pas le droit d'utiliser les champs (attributs) d'une variable de type T_Date car le type étant privé, on n'a pas accès à sa définition qui est donnée dans la partie privée du module, toujours le masquage d'information.
    -- On ne peut donc pas mettre un point "." après la variable "Une_Date" car ne connaissant pas la définition de son type, on ne peut pas savoir si c'est un enregistrement ou non.
    Une_Date.jour := 15; 
    Une_Date.Mois := Mois_Suivant; 
    Afficher (Une_Date);
    New_Line;

    -- Illustrer les opérations possibles sur T_Date, type privé
    Autre_Date := Une_Date; -- on ne peut pas affilier à une autre variable un type T_Date préexistant
    Put ("Autre date : ");
    Afficher (Autre_Date);
    New_Line;

    if Autre_Date = Une_Date then -- on ne peut pas faire d'opérations de vérifications d'égalité avec un type limited private
        Put_Line ("Ce sont les mêmes dates !");
    else
        Put_Line ("Les dates sont différentes !");
    end if;

end Exemple_Dates_Erreurs;
