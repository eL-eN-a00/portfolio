% Fonction estim_param_Dyx_MV (exercice_1.m)

function [a_Dyx,b_Dyx,residus_Dyx] = ...
           estim_param_Dyx_MV(x_donnees_bruitees,y_donnees_bruitees,tirages_psi)

    [~, ~, x_donnees_bruitees_centrees, y_donnees_bruitees_centrees] = centrage_des_donnees(x_donnees_bruitees, y_donnees_bruitees);
    expression = sum((y_donnees_bruitees_centrees - x_donnees_bruitees_centrees*tan(tirages_psi')).^2, 1);
    [~, indice_psi_min] = min(expression);
    a_Dyx = tan(tirages_psi(indice_psi_min));
    b_Dyx = mean(y_donnees_bruitees - a_Dyx*x_donnees_bruitees);
    residus_Dyx = y_donnees_bruitees - (a_Dyx*x_donnees_bruitees + b_Dyx);
   
end