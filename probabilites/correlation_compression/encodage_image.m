% Fonction encodage_image

function [I_encodee,dictionnaire,hauteur_I,largeur_I] = encodage_image(I)

    [vecteur_Imin_a_Imax,vecteur_frequences] = histogramme_normalise(I);
    dictionnaire = huffmandict(vecteur_Imin_a_Imax,vecteur_frequences);
    I_encodee = huffmanenco(I(:),dictionnaire);
    [hauteur_I, largeur_I] = size(I);
end