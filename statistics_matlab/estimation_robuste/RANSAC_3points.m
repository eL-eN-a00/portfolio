% Fonction RANSAC_3points

function [C_estime,R_estime] = RANSAC_3points(x_donnees_bruitees,y_donnees_bruitees,parametres)

    % Parametres de l'algorithme RANSAC :
    S_ecart = parametres(1); % seuil pour l'ecart
    S_prop = parametres(2); % seuil pour la proportion
    k_max = parametres(3); % nombre d'iterations
    n_tirages = parametres(4); 
    n_donnees = size(x_donnees_bruitees,1);
    ecart_moyen_min = Inf;
    
    [G, R, ~] = calcul_G_et_R_moyen(x_donnees_bruitees, y_donnees_bruitees);
    [tirages_C, tirages_R] = tirages_aleatoires_uniformes(n_tirages, G, R);
    for p = 1:k_max
        ind_3_points = randperm(n_donnees,3);
        x_3_points = x_donnees_bruitees(ind_3_points);
        y_3_points = y_donnees_bruitees(ind_3_points);
        [C_3p, R_3p] = estim_param_cercle_3points(x_3_points, y_3_points);
        distances = sqrt((x_donnees_bruitees - C_3p(1)).^2 + (y_donnees_bruitees - C_3p(2)).^2); 
        ecarts = abs(distances - R_3p);
        indices_ecarts = ecarts < S_ecart;
        if sum(indices_ecarts)/n_donnees > S_prop
            x_points_proches = x_donnees_bruitees(indices_ecarts);
            y_points_proches = y_donnees_bruitees(indices_ecarts);
            [C_estime_2, R_estime_2, ecart_moyen_2] = estimation_C_et_R(x_points_proches, y_points_proches, tirages_C, tirages_R);
            if ecart_moyen_2 < ecart_moyen_min
                ecart_moyen_min = ecart_moyen_2;
                C_estime = C_estime_2;
                R_estime = R_estime_2;
            end
        end
    end    

end