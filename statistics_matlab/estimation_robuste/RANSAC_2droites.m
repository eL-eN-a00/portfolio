% Fonction RANSAC_2droites 

function [rho_F_estime,theta_F_estime] = RANSAC_2droites(rho,theta,parametres)

    % Parametres de l'algorithme RANSAC :
    S_ecart = parametres(1); % seuil pour l'ecart
    S_prop = parametres(2); % seuil pour la proportion
    k_max = parametres(3); % nombre d'iterations
    n_donnees = length(rho);
    ecart_moyen_min = Inf;

    for i = 1:k_max
        ind_2_droites = randperm(n_donnees, 2);
        rho_2_droites = rho(ind_2_droites);
        theta_2_droites = theta(ind_2_droites);
        [rho_F_1, theta_F_1, ~] = estim_param_F(rho_2_droites, theta_2_droites);
        ecarts = abs(rho - rho_F_1*cos(theta - theta_F_1));
        indices_ecarts = ecarts < S_ecart;
        if sum(indices_ecarts)/n_donnees > S_prop
            rho_droites_proches = rho(indices_ecarts);
            theta_droites_proches = theta(indices_ecarts);
            [rho_F_2, theta_F_2, ecart_moyen_2] = estim_param_F(rho_droites_proches, theta_droites_proches);
            if ecart_moyen_2 < ecart_moyen_min
                ecart_moyen_min = ecart_moyen_2;
                rho_F_estime = rho_F_2;
                theta_F_estime = theta_F_2;
            end
        end
    end    

end