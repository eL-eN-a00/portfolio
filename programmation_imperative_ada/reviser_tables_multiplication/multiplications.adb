--------------------------------------------------------------------------------
--  Auteur   : Elena CORTINAS GARCIA
--------------------------------------------------------------------------------

with Ada.Text_IO;           use Ada.Text_IO;
with Ada.Integer_Text_IO;   use Ada.Integer_Text_IO;
With Alea;

procedure Multiplications is
package Mon_Alea is
		new Alea (1, 10);  -- générateur de nombre dans l'intervalle [1, 10]
	use Mon_Alea;
        
        Table : Integer;            -- la table qui va être révisée
        Nombre_Erreurs : Integer;   -- le nombre d'erreurs de multiplication de l'utilisateur lors de la révision de la dite table
        Nombre : Integer;           -- le nombre aléatoire choisi pour faire une multiplication de la table
        Ancien_Nombre : Integer := 0; -- pour éviter la répétition (extension 1)
        Resultat : Integer;         -- le résultat de la multiplication de Table par Nombre
        Bonnes_Reponses : Integer;  -- le nombre de bonnes réponses lors de la révision de la dite table
        Continuer : Character;      -- Est-ce que l'utilisateur souhaite continuer ou non
        Nombre_Multiplications : constant Integer := 10; -- le nombre de multiplications faites lors de la révision de la table
        Borne_Max_Table : constant Integer := 10; -- on veut réviser les tables que de 0 à 10
begin

        -- Réviser ses tables de multiplication
        loop
                -- Réviser une table de multiplication

                -- Demander à l'utilisateur la table à réviser
                Put("Table à réviser : ");
                Get(Table);
                while Table < 0 or Table > Borne_Max_Table loop
                        Put_Line("Impossible. La table doit être entre 0 et 10.");
                        Put("Table à réviser : ");
                        Get(Table);
                end loop;
                
                Nombre_Erreurs := 0;
                For Indice in 1 .. Nombre_Multiplications loop

                        -- Demander à l'utilisateur le résultat d'une multiplication de la table

			-- Tirage aléatoire sans répétition immédiate (extension 1)
			loop
				Get_Random_Number(Nombre);
				exit when Nombre /= Ancien_Nombre;
			end loop;
			Ancien_Nombre := Nombre;

                        -- Afficher la multiplication à calculer
                        New_Line;
                        Put("M(");
                        Put(Indice, 1);
                        Put(")");
                        Put(Table, 2);
                        Put(" *");
                        Put(Nombre, 2);
                        Put(" = ");

                        Get(Resultat);

                        -- Traiter la réponse de l'utilisateur
                        if Resultat /= (Table * Nombre) then
                                Put_Line("Mauvaise réponse.");
                                Nombre_Erreurs := Nombre_Erreurs + 1;
                        else
                                Put_Line("Bravo.");
                        end if;

                end loop;
                
                -- Afficher un message
                case Nombre_Erreurs is
                        when 0 => Put_Line("Aucune erreur. Excellent.");
                        when 1 => Put_Line("Une seule erreur. Très bien.");
                        when 10 => Put_Line("Tout est faux. Volontaire ?");

                        when 6..9 => Put_Line("Seulement ");
                        Bonnes_Reponses := 10 - Nombre_Erreurs;
                        Put(Bonnes_Reponses, 1);
                        Put(" bonnes réponses. Il faut encore apprendre la table de ");
                        Put(Table, 1);
			when 2 | 3 => Put(Nombre_Erreurs, 1); -- extension 2
			Put(" erreurs. Bien.");
                        when others => Put(Nombre_Erreurs, 1);
                        Put(" erreurs. Il faut encore travailler la table de ");
                        Put(Table, 1);
               end case;

               -----------------------------------------------------------------------------------------

               -- Demander à l'utilisateur la suite
	       New_Line;
               Put("On continue (o/n)? ");
               Get(Continuer);
               Skip_Line;
               exit when Continuer /= 'o' and  Continuer /= 'O';
               end loop;
end Multiplications;
