% Fonction histogramme_normalise

function [vecteur_Imin_a_Imax,vecteur_frequences] = histogramme_normalise(I)

    I_vect = I(:);
    Imin = min(I_vect);
    Imax = max(I_vect);
    vecteur_Imin_a_Imax = Imin:Imax;    %N valeurs
    occurences = histcounts(I_vect,[vecteur_Imin_a_Imax, Imax+1]);
    vecteur_frequences = occurences/sum(occurences);
end