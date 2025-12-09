with Ada.Text_IO;           use Ada.Text_IO;
with SDA_Exceptions; 		use SDA_Exceptions;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
	--! Les Unbounded_String ont une capacité variable, contrairement aux String
	--! pour lesquelles une capacité doit être fixée.
with LCA;

procedure Tester_LCA is

	generic
		type K is private;	-- le type des clés
		type V is private;	-- le type des valeurs
		K1, K2, K3, K4, K5 : K;	-- 5 clés différentes
		V1, V2, V3, V4 : V;		-- 4 valeurs différentes
		with package P_LCA is new LCA (K, V);
	package Testeur is

		-- Vérifier que les clés sont différentes
		 pragma Assert (K1 /= K2 and K1 /= K3 and K1 /= K4 and K1 /= K5
				and K2 /= K3 and K2 /= K4 and K2 /= K5
				and K3 /= K4 and K3 /= K5
				and K4 /= K5);

		-- Vérifier que les valeurs sont différentes
		 pragma Assert (V1 /= V2 and V1 /= V3 and V1 /= V4
				and V2 /= V3 and V2 /= V4
				and V3 /= V4);

		-- Lancer tous les tests.
		generic
			with function "+" (Gauche, Droite : in V) return V;
		procedure Tester_Tout;

		-- Tester Initialiser, Est_Vide, Taille et Detruire (partiel).
		procedure Tester_Initialiser;

		-- Tester l'opérations Enregistrer.
		procedure Tester_Enregistrer;

		-- Tester l'opération La_Valeur.
		procedure Tester_La_Valeur;

		-- Tester Supprimer.
		procedure Tester_Supprimer;

		-- Tester Faire_Pour_Chaque.
		generic
			with function "+" (Gauche, Droite : in V) return V;
		procedure Tester_Faire_Pour_Chaque;

	end Testeur;


	package body Testeur is
		use P_LCA;


		procedure Tester_Tout is
			procedure Tester_Faire_Pour_Chaque_Instancie is
				new Tester_Faire_Pour_Chaque ("+");

		begin
			Tester_Initialiser;
			Tester_Enregistrer;
			Tester_La_Valeur;
			Tester_Supprimer;
			Tester_Faire_Pour_Chaque_Instancie;

			Put_Line ("Tests : OK.");
		end Tester_Tout;


		--! Ici on définit des types qui correspondent à des pointeurs
		--! sur des sous-programmes.  Un tel type permet d'avoir une variable
		--! ou un paramètre de type pointeur sur sous-programme ce qui constitue
		--! une alternative à un paramètre de généricité de type sous-programme.
		--! Contrairement à la généricité, il faut passer en paramètre le
		--! sous-programme à utiliser à chaque appel.
		--
		type SP_Enregistrer is
			access procedure (LCA : in out T_LCA; Cle : in K; Valeur : in V);

		type SP_Supprimer is
			access procedure (LCA : in out T_LCA; Cle : in K);


		-- Initialiser une LCA avec (K1, V1), etc. en vérifiant l'état
		-- de la LCA à chaque étape.
		-- Pour enregistrer les éléments, on utilise la procédure Enregistrer
		-- passée en paramètre qui par défaut est celle du paquetage P_LCA.
		-- On pourra donc appeler cette procédure avec la version itérative
		-- ou récursive.
		procedure Creer_LCA(LCA : out T_LCA ;
						Enregistrer : SP_Enregistrer := P_LCA.Enregistrer'Access)
		is
		begin
			Initialiser (LCA);
			pragma assert (Est_Vide (LCA));
			pragma assert (Taille (LCA) = 0);
			pragma assert (not Cle_Presente (LCA, K1));
			pragma assert (not Cle_Presente (LCA, K2));
			pragma assert (not Cle_Presente (LCA, K3));
			pragma assert (not Cle_Presente (LCA, K4));

			Enregistrer (LCA, K1, V1);
			pragma assert (not Est_Vide (LCA));
			pragma assert (Taille (LCA) = 1);
			pragma assert (Cle_Presente (LCA, K1));
			pragma assert (not Cle_Presente (LCA, K2));
			pragma assert (not Cle_Presente (LCA, K3));
			pragma assert (not Cle_Presente (LCA, K4));
			pragma assert (La_Valeur (LCA, K1) = V1);

			Enregistrer (LCA, K2, V2);
			pragma assert (not Est_Vide (LCA));
			pragma assert (Taille (LCA) = 2);
			pragma assert (Cle_Presente (LCA, K1));
			pragma assert (Cle_Presente (LCA, K2));
			pragma assert (not Cle_Presente (LCA, K3));
			pragma assert (not Cle_Presente (LCA, K4));
			pragma assert (La_Valeur (LCA, K1) = V1);
			pragma assert (La_Valeur (LCA, K2) = V2);

			Enregistrer (LCA, K3, V3);
			pragma assert (not Est_Vide (LCA));
			pragma assert (Taille (LCA) = 3);
			pragma assert (Cle_Presente (LCA, K1));
			pragma assert (Cle_Presente (LCA, K2));
			pragma assert (Cle_Presente (LCA, K3));
			pragma assert (not Cle_Presente (LCA, K4));
			pragma assert (La_Valeur (LCA, K1) = V1);
			pragma assert (La_Valeur (LCA, K2) = V2);
			pragma assert (La_Valeur (LCA, K3) = V3);

			Enregistrer (LCA, K4, V4);
			pragma assert (not Est_Vide (LCA));
			pragma assert (Taille (LCA) = 4);
			pragma assert (Cle_Presente (LCA, K1));
			pragma assert (Cle_Presente (LCA, K2));
			pragma assert (Cle_Presente (LCA, K3));
			pragma assert (Cle_Presente (LCA, K4));
			pragma assert (La_Valeur (LCA, K1) = V1);
			pragma assert (La_Valeur (LCA, K2) = V2);
			pragma assert (La_Valeur (LCA, K3) = V3);
			pragma assert (La_Valeur (LCA, K4) = V4);
		end Creer_LCA;


		procedure Tester_Initialiser is
			LCA: T_LCA;
		begin
			Put ("Tester_Initialiser : ");
			Initialiser (LCA);
			pragma assert (Est_Vide (LCA));
			pragma assert (Taille (LCA) = 0);
			Detruire (LCA);
			Put_Line ("OK");
		end Tester_Initialiser;


		procedure Tester_Enregistrer (Enregistrer : in SP_Enregistrer ; Nom: in String) is
			LCA: T_LCA;
		begin
			Put ("Tester_" & Nom & " : ");

			-- Tester l'ajout d'élément non déjà présents
			Creer_LCA (LCA);

			-- Tester l'ajout d'un élément présent => remplacement
			Enregistrer (LCA, K1, V4);
			pragma assert (Taille (LCA) = 4);
			pragma assert (La_Valeur (LCA, K1) = V4);
			pragma assert (La_Valeur (LCA, K2) = V2);
			pragma assert (La_Valeur (LCA, K3) = V3);
			pragma assert (La_Valeur (LCA, K4) = V4);

			Enregistrer (LCA, K2, V1);
			pragma assert (Taille (LCA) = 4);
			pragma assert (La_Valeur (LCA, K1) = V4);
			pragma assert (La_Valeur (LCA, K2) = V1);
			pragma assert (La_Valeur (LCA, K3) = V3);
			pragma assert (La_Valeur (LCA, K4) = V4);

			Enregistrer (LCA, K4, V2);
			pragma assert (Taille (LCA) = 4);
			pragma assert (La_Valeur (LCA, K1) = V4);
			pragma assert (La_Valeur (LCA, K2) = V1);
			pragma assert (La_Valeur (LCA, K3) = V3);
			pragma assert (La_Valeur (LCA, K4) = V2);

			Enregistrer (LCA, K3, V2);
			pragma assert (Taille (LCA) = 4);
			pragma assert (La_Valeur (LCA, K1) = V4);
			pragma assert (La_Valeur (LCA, K2) = V1);
			pragma assert (La_Valeur (LCA, K3) = V2);
			pragma assert (La_Valeur (LCA, K4) = V2);

			Detruire (LCA);

			Put_Line ("OK");
		end Tester_Enregistrer;

		procedure Tester_Enregistrer is
		begin
			Tester_Enregistrer (P_LCA.Enregistrer_Iteratif'Access, "Enregistrer_Iteratif");
			Tester_Enregistrer (P_LCA.Enregistrer_Recursif'Access, "Enregistrer_Recursif");
			Tester_Enregistrer (P_LCA.Enregistrer'Access, "Enregistrer");
		end Tester_Enregistrer;


		procedure Tester_La_Valeur is

			-- Vérifier que l'exception Cle_Absente_Error se produit.
			procedure Verifier_Avec_Cle_Absente (LCA : in out T_LCA ; Cle: in K) is 
				Valeur : V;
			begin
				pragma Unreferenced (Valeur);
				Valeur := La_Valeur (LCA, Cle);
				pragma assert (false); -- pas de Cle_Absente_Error
			exception
				when Cle_Absente_Error =>
					null;
				when others =>
					pragma assert (false);	-- Pas la bonne exception
			end Verifier_Avec_Cle_Absente;

			LCA : T_LCA;
		begin
			Put ("Tester_La_Valeur : ");

			-- Tester des clés absentes
			Initialiser (LCA);
			Verifier_Avec_Cle_Absente (LCA, K1);
			Detruire (LCA);

			Creer_LCA (LCA);
			Verifier_Avec_Cle_Absente (LCA, K5);

			-- Tester des clés présentes
			pragma assert (La_Valeur (LCA, K1) = V1);
			pragma assert (La_Valeur (LCA, K2) = V2);
			pragma assert (La_Valeur (LCA, K3) = V3);
			pragma assert (La_Valeur (LCA, K4) = V4);
			Put_Line ("OK");
		end Tester_La_Valeur;


		procedure Tester_Supprimer_Element_Absent (
				Supprimer : in SP_Supprimer ;
				Nom : in String
		) is

			procedure Verifier_Supprimer_Absent (LCA : in out T_LCA ; Cle: in K) is
			begin
				Supprimer (LCA, Cle);
				pragma Assert (false);	-- une exception aurait dû se produire
			exception
				when Cle_Absente_Error =>
					null;
				when others =>
					pragma Assert (false);	-- Mauvaise exception
			end Verifier_Supprimer_Absent;

			LCA : T_LCA;
		begin
			Put ("Tester_" & Nom & "_Element_Absent : ");

			Initialiser (LCA);
			Verifier_Supprimer_Absent (LCA, K5);
			Verifier_Supprimer_Absent (LCA, K2);
			Detruire (LCA);

			Initialiser (LCA);
			Enregistrer (LCA, K1, V1);
			Verifier_Supprimer_Absent (LCA, K5);
			Verifier_Supprimer_Absent (LCA, K2);
			Verifier_Supprimer_Absent (LCA, K4);

			Enregistrer (LCA, K3, V3);
			Verifier_Supprimer_Absent (LCA, K5);
			Verifier_Supprimer_Absent (LCA, K2);
			Verifier_Supprimer_Absent (LCA, K4);
			Detruire (LCA);

			Creer_LCA (LCA);
			Verifier_Supprimer_Absent (LCA, K5);
			Detruire (LCA);

			Put_Line ("OK");
		end Tester_Supprimer_Element_Absent;


		procedure Tester_Supprimer_Element_Existant (
				Supprimer : in SP_Supprimer ;
				Nom : in String
		) is
			LCA: T_LCA;
		begin
			Put ("Tester_" & Nom & "_Element_Existant : ");

			-- Supprimer le dernier ajouté
			Creer_LCA (LCA);
			Supprimer (LCA, K4);
			pragma assert (Taille (LCA) = 3);
			pragma assert (Cle_Presente (LCA, K1));
			pragma assert (Cle_Presente (LCA, K2));
			pragma assert (Cle_Presente (LCA, K3));
			pragma assert (not Cle_Presente (LCA, K4));
			Detruire (LCA);

			-- Supprimer le premier ajouté
			Creer_LCA (LCA);
			Supprimer (LCA, K1);
			pragma assert (Taille (LCA) = 3);
			pragma assert (not Cle_Presente (LCA, K1));
			pragma assert (Cle_Presente (LCA, K2));
			pragma assert (Cle_Presente (LCA, K3));
			pragma assert (Cle_Presente (LCA, K4));
			Detruire (LCA);

			-- Supprimer le deuxième ajouté
			Creer_LCA (LCA);
			Supprimer (LCA, K2);
			pragma assert (Taille (LCA) = 3);
			pragma assert (Cle_Presente (LCA, K1));
			pragma assert (not Cle_Presente (LCA, K2));
			pragma assert (Cle_Presente (LCA, K3));
			pragma assert (Cle_Presente (LCA, K4));
			Detruire (LCA);

			-- Supprimer le troisième ajouté
			Creer_LCA (LCA);
			Supprimer (LCA, K3);
			pragma assert (Taille (LCA) = 3);
			pragma assert (Cle_Presente (LCA, K1));
			pragma assert (Cle_Presente (LCA, K2));
			pragma assert (not Cle_Presente (LCA, K3));
			pragma assert (Cle_Presente (LCA, K4));
			Detruire (LCA);

			Put_Line ("OK");
		end Tester_Supprimer_Element_Existant;


		procedure Tester_Supprimer (
				Supprimer : in SP_Supprimer ;
				Nom : in String
		) is
		begin
			Tester_Supprimer_Element_Absent (Supprimer, Nom);
			Tester_Supprimer_Element_Existant (Supprimer, Nom);
			Put ("Tester_" & Nom & " : ");
			Put_Line ("OK");
		end Tester_Supprimer;


		procedure Tester_Supprimer is
		begin
			Tester_Supprimer (P_LCA.Supprimer_Iteratif'Access, "Supprimer_Iteratif");
			Tester_Supprimer (P_LCA.Supprimer_Recursif'Access, "Supprimer_Recursif");
			Tester_Supprimer (P_LCA.Supprimer'Access, "Supprimer");
		end Tester_Supprimer;



		procedure Tester_Faire_Pour_Chaque is

			Somme: V;	-- Variable utiliser pour Sommer ou Sommer_Sauf

			procedure Sommer (Cle: K; Valeur: V) is
				pragma Unreferenced (Cle);
			begin
				Somme := Somme + Valeur;
			end;

			Exception_Locale: Exception;

			Sauf : V;	-- utilisé par Sommer_Sauf

			procedure Sommer_Sauf (Cle: K; Valeur: V) is
				pragma Unreferenced (Cle);
			begin
				if Valeur = Sauf then
					raise Exception_Locale;
				end if;

				Somme := Somme + Valeur;
			end;


			procedure Sommer is
				new Faire_Pour_Chaque (Sommer);
			procedure Sommer_Sauf is
				new Faire_Pour_Chaque (Sommer_Sauf);

			LCA: T_LCA;
		begin
			Put ("Tester_Faire_Pour_Chaque : ");

			-- Faire la somme des éléments d'une LCA
			Creer_LCA (LCA);
			Somme := V1;	-- On ne connait pas l'élément neutre de V
			Sommer (LCA);
			pragma assert (Somme = V1 + V1 + V2 + V3 + V4);

			Somme := V1;
			Sauf := V1; 
			Sommer_Sauf (LCA);
			pragma assert (Somme = V1 + V2 + V3 + V4);

			Somme := V1;
			Sauf := V2; 
			Sommer_Sauf (LCA);
			pragma assert (Somme = V1 + V1 + V3 + V4);

			Somme := V1;
			Sauf := V4; 
			Sommer_Sauf (LCA);
			pragma assert (Somme = V1 + V1 + V2 + V3);

			Somme := V1;
			Sauf := V1; 
			Enregistrer (LCA, K4, V1);
			Sommer_Sauf (LCA);
			pragma assert (Somme = V1 + V2 + V3);

			Somme := V1;
			Enregistrer (LCA, K3, V1);
			Sommer_Sauf (LCA);
			pragma assert (Somme = V1 + V2);

			Somme := V1;
			Enregistrer (LCA, K2, V1);
			Sommer_Sauf (LCA);
			pragma assert (Somme = V1);

			Detruire (LCA);
			Put_Line ("OK");
		exception
			when Exception_Locale =>
				Put_Line("Relire les explications sur l'opération 9 d'une LCA.");
				pragma assert (False);
		end Tester_Faire_Pour_Chaque;


	end Testeur;

	package LCA_String_Integer is
		new LCA (T_Cle => Unbounded_String, T_Valeur => Integer);

	-- Surcharge l'opérateur unaire "+" pour convertir une String
	-- en Unbounded_String.
	-- Cette astuce permet de simplifier l'initialisation
	-- de cles un peu plus loin.
	function "+" (Item : in String) return Unbounded_String
		renames To_Unbounded_String;

	package Testeur_String_Integer is
		new Testeur(
				Unbounded_String, Integer,
				+"un", +"deux", +"trois", +"cinq", +"six",
				1, 2, 3, 4,
				LCA_String_Integer);

	procedure Tester_Tout_Integer is
		new Testeur_String_Integer.Tester_Tout ("+");



begin
	Tester_Tout_Integer;

	New_Line;
	Put_Line ("Tous les tests ont réussi.");
end Tester_LCA;
