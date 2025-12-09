% Fonction calcul_G_et_R_moyen 

function [G, R_moyen, distances] = calcul_G_et_R_moyen(x_donnees_bruitees,y_donnees_bruitees)

    G = [mean(x_donnees_bruitees); mean(y_donnees_bruitees)];
    distances = sqrt((G(1) - x_donnees_bruitees).^2 + (G(2) - y_donnees_bruitees).^2);
    R_moyen = mean(distances);
    

end