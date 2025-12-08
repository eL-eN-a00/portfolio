% Fonction estim_param_Dyx_MV_2droites 

function [a_Dyx_1,b_Dyx_1,a_Dyx_2,b_Dyx_2] = ... 
         estim_param_Dyx_MV_2droites(x_donnees_bruitees,y_donnees_bruitees,sigma, ...
                                     tirages_G_1,tirages_psi_1,tirages_G_2,tirages_psi_2)    

    residus1 = (y_donnees_bruitees - tirages_G_1(2, :)) - tan(tirages_psi_1).*(x_donnees_bruitees - tirages_G_1(1, :));
    residus2 = (y_donnees_bruitees - tirages_G_2(2, :)) - tan(tirages_psi_2).*(x_donnees_bruitees - tirages_G_2(1, :));
    residus1carre = residus1.^2;
    residus2carre = residus2.^2;
    fonc = exp(-residus1carre/(2*(sigma^2))) + exp(-residus2carre/(2*(sigma^2)));
    logvraisemblance = sum(log(fonc), 1);
    [~, indice_max] = max(logvraisemblance);
    a_Dyx_1 = tan(tirages_psi_1(indice_max));
    a_Dyx_2 = tan(tirages_psi_2(indice_max));
    b_Dyx_1 = tirages_G_1(2, indice_max) - a_Dyx_1*tirages_G_1(1, indice_max);
    b_Dyx_2 = tirages_G_2(2, indice_max) - a_Dyx_2*tirages_G_2(1, indice_max);

end