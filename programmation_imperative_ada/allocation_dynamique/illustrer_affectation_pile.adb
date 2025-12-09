with Piles;
with Ada.Text_IO;            use Ada.Text_IO;

-- Montrer le risque d'autoriser l'affectation entre variables dont le type
-- est une structure chaînée.
procedure Illustrer_Affectation_Pile is
	package Pile is
		new Piles (Character);
	use Pile;

	procedure Afficher is
		new Pile.Afficher (Put);

	P1, P2 : T_Pile;
begin
	-- construire la pile P1
	Initialiser (P1);
	Empiler (P1, 'A');
	Empiler (P1, 'B');
	Afficher (P1); New_Line;   -- XXX Qu'est ce qui s'affiche ?   "[ A, B >"

	P2 := P1;                  -- XXX Conseillé ? non on n'aime pas que 2 pointeurs pointent au même endroit
	pragma Assert (P1 = P2);

     Depiler (P2);              -- XXX Quel effet ?   dépile A chez P2 et P1
	Afficher (P2); New_Line;   -- XXX Qu'est ce qui s'affiche ? "[ A > "
	Afficher (P1); New_Line;   -- XXX Qu'est ce qui s'affiche ? rien car P1 pointe sur un truc nonexistant
	-- XXX Que donne l'exécution avec valgrind ?   erreur car rien n'est free

    Depiler (P1);	-- XXX correct ?   je pense pas il faut détruire
end Illustrer_Affectation_Pile;
