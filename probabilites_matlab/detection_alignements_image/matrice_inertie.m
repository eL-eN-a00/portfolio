% Fonction matrice_inertie 

function [M_inertie,C] = matrice_inertie(E,G_norme_E)

    x = E(:, 1);
    y = E(:, 2);
    somme = sum(G_norme_E);
    x_barre = sum(G_norme_E.*x)/somme;
    y_barre = sum(G_norme_E.*y)/somme;
    C = [x_barre, y_barre];
    M_inertie = [sum(G_norme_E.*(x-x_barre).*(x-x_barre)), sum(G_norme_E.*(x-x_barre).*(y - y_barre)) 
; sum(G_norme_E.*(x-x_barre).*(y - y_barre)), sum(G_norme_E.*(y-y_barre).*(y-y_barre))]/somme;

end