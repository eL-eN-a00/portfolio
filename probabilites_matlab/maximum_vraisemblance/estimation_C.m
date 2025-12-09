% Fonction estimation_C 

function C_estime = estimation_C(x_donnees_bruitees,y_donnees_bruitees,tirages_C,R_moyen)

    ecarts_x = x_donnees_bruitees - tirages_C(1, :);
    ecarts_y = y_donnees_bruitees - tirages_C(2, :);
    distances = sqrt(ecarts_x.^2 + ecarts_y.^2) ;
    somme = sum((distances-R_moyen).^2);
    [~, ind_min] = min(somme);
    C_estime = tirages_C(:, ind_min);

end