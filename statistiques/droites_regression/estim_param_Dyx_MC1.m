% Fonction estim_param_Dyx_MC1 

function [a_Dyx,b_Dyx,coeff_R2] = ...
                   estim_param_Dyx_MC1(x_donnees_bruitees,y_donnees_bruitees)

   A = ones(length(x_donnees_bruitees), 2);
   A(:, 1) = x_donnees_bruitees';
   B = y_donnees_bruitees;
   X = A\B;
   a_Dyx = X(1);
   b_Dyx = X(2);
   SCR = sum((y_donnees_bruitees - a_Dyx*x_donnees_bruitees - b_Dyx).^2);
   SCT = sum((y_donnees_bruitees - mean(y_donnees_bruitees)).^2);
   coeff_R2 = 1 - SCR/SCT;
   
end