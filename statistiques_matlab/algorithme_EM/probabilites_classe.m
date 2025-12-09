% Fonction probabilites_classe

function [probas_classe_1,probas_classe_2] = probabilites_classe(x_donnees_bruitees,y_donnees_bruitees,sigma,...
                                                                 a_1,b_1,proportion_1,a_2,b_2,proportion_2)

    residus1 = y_donnees_bruitees - a_1*x_donnees_bruitees - b_1;
    residus2 = y_donnees_bruitees - a_2*x_donnees_bruitees - b_2;
    vraisemblance1 = proportion_1*exp(-(residus1.^2)/(2*sigma.^2));
    vraisemblance2 = proportion_2*exp(-(residus2.^2)/(2*sigma.^2));
    probas_classe_1 = vraisemblance1./(vraisemblance1+vraisemblance2);
    probas_classe_2 = vraisemblance2./(vraisemblance2+vraisemblance1);

end