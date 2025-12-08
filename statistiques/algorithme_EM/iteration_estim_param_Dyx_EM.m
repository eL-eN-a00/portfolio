% Fonction iteration_estim_param_Dyx_EM

function [probas_classe_1,proportion_1,a_1,b_1,probas_classe_2,proportion_2,a_2,b_2] =...
         iteration_estim_param_Dyx_EM(x_donnees_bruitees,y_donnees_bruitees,sigma,...
         proportion_1,a_1,b_1,proportion_2,a_2,b_2)

    % Calcul des probas
    % Calcul des proportions (moyennes des probas)
    % Calcul des paramètres (a1, b1) et (a2, b2) par moindres carrés
    % pondérés

    [probas_classe_1,probas_classe_2] = probabilites_classe(x_donnees_bruitees,y_donnees_bruitees,sigma,...
                                                            a_1,b_1,proportion_1,a_2,b_2,proportion_2);

    proportion_1 = mean(probas_classe_1);
    proportion_2 = mean(probas_classe_2);

    [a_1, b_1] = estim_param_Dyx_MCP(x_donnees_bruitees,y_donnees_bruitees,probas_classe_1);
    [a_2, b_2] = estim_param_Dyx_MCP(x_donnees_bruitees,y_donnees_bruitees,probas_classe_2);

end