% Fonction estim_param_Dyx_MC2 (exercice_2bis.m)

function [a_Dyx,b_Dyx,coeff_r2] = ...
                   estim_param_Dyx_MC2(x_donnees_bruitees,y_donnees_bruitees)

	cov = mean(x_donnees_bruitees.*y_donnees_bruitees) - mean(x_donnees_bruitees)*mean(y_donnees_bruitees);
    var_x = mean(x_donnees_bruitees.^2) - mean(x_donnees_bruitees).^2;
    var_y = mean(y_donnees_bruitees.^2) - mean(y_donnees_bruitees).^2;
    r = cov/sqrt(var_x*var_y);
    a_Dyx = r*sqrt(var_y/var_x);
    b_Dyx = mean(y_donnees_bruitees) - a_Dyx*mean(x_donnees_bruitees);
    coeff_r2 = r*r;
    
end