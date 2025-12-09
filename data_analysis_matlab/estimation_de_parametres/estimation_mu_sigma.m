function [moyennes,ecarts_types] = estimation_mu_sigma(liste_parametres)
    
    moyennes = mean(liste_parametres, 2);
    variances = (liste_parametres - moyennes).*(liste_parametres - moyennes);
    ecarts_types = sqrt(variances);

end