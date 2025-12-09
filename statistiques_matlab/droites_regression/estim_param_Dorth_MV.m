% Fonction estim_param_Dorth_MV 

function [theta_Dorth,rho_Dorth] = ...
         estim_param_Dorth_MV(x_donnees_bruitees,y_donnees_bruitees,tirages_theta)

    [~, ~, x_c, y_c] = centrage_des_donnees(x_donnees_bruitees, y_donnees_bruitees);
    expression = sum((x_c*cos(tirages_theta') + y_c*sin(tirages_theta')).^2, 1);
    [~, indice_min] = min(expression);
    theta_Dorth = tirages_theta(indice_min);
    rho_Dorth = mean(x_donnees_bruitees*cos(theta_Dorth) + y_donnees_bruitees*sin(theta_Dorth));

end