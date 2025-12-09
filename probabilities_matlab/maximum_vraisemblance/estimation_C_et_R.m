% Fonction estimation_C_et_R 

function [C_estime, R_estime] = estimation_C_et_R(x_donnees_bruitees,y_donnees_bruitees,tirages_C,tirages_R)

    ecarts_x = x_donnees_bruitees - tirages_C(1, :);
    ecarts_y = y_donnees_bruitees - tirages_C(2, :);
    distances = sqrt(ecarts_x.^2 + ecarts_y.^2) ;
    somme = sum((distances-tirages_R).^2);
    [~, ind_min] = min(somme);
    C_estime = tirages_C(:, ind_min);
    R_estime = tirages_R(:, ind_min);

end