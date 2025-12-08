% Fonction E_recursif 

function [E,pixels_contour,G_somme] = E_recursif(E,pixels_contour,G_somme,i,j,voisins,G_x,G_y,card_max,cos_alpha)
    
    pixels_contour(i, j) = 0; % je viens de le visiter
    k = 1; % numéro de voisin
    while (k <= size(voisins, 1)) && (size(E,1) < card_max) % processus des 8 voisins, et logique, arêtes pas trop grandes
        i_voisink = i + voisins(k, 1);
        j_voisink = j + voisins(k, 2); % coordonnées de voisin
        if pixels_contour(i_voisink, j_voisink)
            G_voisin = [G_x(i_voisink, j_voisink), G_y(i_voisink, j_voisink)];
            G_voisin_normalise = G_voisin/norm(G_voisin);
            G_somme_normalise = G_somme/norm(G_somme);
            if G_voisin_normalise*G_somme_normalise' > cos_alpha % ligne * colonne
                E = [E; i_voisink, j_voisink];
                G_somme = G_somme + G_voisin;
                [E, pixels_contour, G_somme] = E_recursif(E, pixels_contour, G_somme, i_voisink, j_voisink, voisins, G_x, G_y, card_max, cos_alpha);
            end
        end
        k = k + 1; % passe au voisin suivant
    end

end