% Fonction estim_param_Dyx_MC 

function [a_Dyx,b_Dyx] = ...
                   estim_param_Dyx_MC(x_donnees_bruitees,y_donnees_bruitees)

    A = [x_donnees_bruitees, ones(size(x_donnees_bruitees))];
    B = y_donnees_bruitees;
    X = A\B;
    a_Dyx = X(1);
    b_Dyx = X(2);
    
end