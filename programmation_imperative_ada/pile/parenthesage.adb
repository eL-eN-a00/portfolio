with Piles;

procedure Parenthesage is


    -- L'indice dans la chaîne Meule de l'élément Aiguille.
    -- Si l'Aiguille n'est pas dans la Meule, on retroune Meule'Last + 1.
    Function Index (Meule : in String; Aiguille: Character) return Integer with
        Post => Meule'First <= Index'Result and then Index'Result <= Meule'Last + 1
            and then (Index'Result > Meule'Last or else Meule (Index'Result) = Aiguille)
    is
        Indice : integer;
        longueur : Integer;
        Index_return : integer;
    begin
        longueur := Meule'Last;
        Index_return := longueur + 1;
        Indice := Meule'First;
        while Index_return = longueur + 1 and Indice <= longueur loop
            if Meule(Indice) = Aiguille then
                Index_return := Indice;
            else
                Indice := Indice + 1;
            end if;
        end loop;
        return Index_return;
    end Index;


    -- Programme de test de Index.
    procedure Tester_Index is
        ABCDEF : constant String := "abcdef";
    begin
        pragma Assert (1 = Index (ABCDEF, 'a'));
        pragma Assert (3 = Index (ABCDEF, 'c'));
        pragma Assert (6 = Index (ABCDEF, 'f'));
        pragma Assert (7 = Index (ABCDEF, 'z'));
        pragma Assert (4 = Index (ABCDEF (1..3), 'z'));
        pragma Assert (3 = Index (ABCDEF (3..5), 'c'));
        pragma Assert (5 = Index (ABCDEF (3..5), 'e'));
        pragma Assert (6 = Index (ABCDEF (3..5), 'a'));
        pragma Assert (6 = Index (ABCDEF (3..5), 'g'));
    end;


    -- Vérifier les bon parenthésage d'une Chaîne (D).  Le sous-programme
    -- indique si le parenthésage est bon ou non (Correct : R) et dans le cas
    -- où il n'est pas correct, l'indice (Indice_Erreur : R) du symbole qui
    -- n'est pas appairé (symbole ouvrant ou fermant).
    --
    -- Exemples
    --   "[({})]"  -> Correct
    --   "]"       -> Non Correct et Indice_Erreur = 1
    --   "((()"    -> Non Correct et Indice_Erreur = 2
    --
    procedure Verifier_Parenthesage (Chaine: in String ; Correct : out Boolean ; Indice_Erreur : out Integer) is
        Ouvrants : Constant String := "([{";
        Fermants : Constant String := ")]}";
        package Piles_Ouvrants is
            new Piles(Chaine'Last, Character);
        use Piles_Ouvrants;
        package Piles_Indices is
            new Piles(Chaine'Last, Integer);
        use Piles_Indices;
        Pile_Ouvr : Piles_Ouvrants.T_Pile;
        Pile_Ind : Piles_Indices.T_Pile;
        longueur : Integer;
        Indice : Integer;
    begin
        Initialiser(Pile_Ouvr);
        Initialiser(Pile_Ind);

        longueur := Chaine'Last;
        Indice := Chaine'First;
        Correct := True;
        while Correct and Indice <= longueur loop
            if Index(Ouvrants, Chaine(Indice)) <= Ouvrants'Last then
                Empiler(Pile_Ouvr, Chaine(Indice));
                Empiler(Pile_Ind, Indice);
                Indice := Indice + 1;
            elsif Index(Fermants, Chaine(Indice)) <= Fermants'Last then
                if not Est_Vide(Pile_Ouvr) and then Index(Fermants, Chaine(Indice)) = Index(Ouvrants, Sommet(Pile_Ouvr)) then
                    Depiler(Pile_Ouvr);
                    Depiler(Pile_Ind);
                    Indice := Indice + 1;
                else
                    Correct := False;
                    Indice_Erreur := Indice;
                end if;
            else
                Indice := Indice + 1;
            end if;
        end loop;
        if Correct and not Est_Vide(Pile_Ouvr) then
            Correct := False;
            Indice_Erreur := Index(Chaine, Sommet(Pile_Ouvr));
        else
            null;
        end if;

    end Verifier_Parenthesage;


    -- Programme de test de Verifier_Parenthesage
    procedure Tester_Verifier_Parenthesage is
        Exemple1 : constant String(1..2) :=  "{}";
        Exemple2 : constant String(11..18) :=  "]{[(X)]}";

        Indice : Integer;   -- Résultat de ... XXX
        Correct : Boolean;
    begin
        Verifier_Parenthesage ("(a < b)", Correct, Indice);
        pragma Assert (Correct);

        Verifier_Parenthesage ("([{a}])", Correct, Indice);
        pragma Assert (Correct);

        Verifier_Parenthesage ("(][{a}])", Correct, Indice);
        pragma Assert (not Correct);
        pragma Assert (Indice = 2);

        Verifier_Parenthesage ("]([{a}])", Correct, Indice);
        pragma Assert (not Correct);
        pragma Assert (Indice = 1);

        Verifier_Parenthesage ("([{}])}", Correct, Indice);
        pragma Assert (not Correct);
        pragma Assert (Indice = 7);

        Verifier_Parenthesage ("([{", Correct, Indice);
        pragma Assert (not Correct);
        pragma Assert (Indice = 3);

        Verifier_Parenthesage ("([{}]", Correct, Indice);
        pragma Assert (not Correct);
        pragma Assert (Indice = 1);

        Verifier_Parenthesage ("", Correct, Indice);
        pragma Assert (Correct);

        Verifier_Parenthesage (Exemple1, Correct, Indice);
        pragma Assert (Correct);

        Verifier_Parenthesage (Exemple2, Correct, Indice);
        pragma Assert (not Correct);
        pragma Assert (Indice = 11);

        Verifier_Parenthesage (Exemple2(12..18), Correct, Indice);
        pragma Assert (Correct);

        Verifier_Parenthesage (Exemple2(12..15), Correct, Indice);
        pragma Assert (not Correct);
        pragma Assert (Indice = 14);
    end Tester_Verifier_Parenthesage;

begin
    Tester_Index;
    Tester_Verifier_Parenthesage;
end Parenthesage;
