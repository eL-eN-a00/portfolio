% Fonction reconstruction_image 

function I_reconstruite = reconstruction_image(I_encodee,dictionnaire,hauteur_I,largeur_I)

    I_decodee = huffmandeco(I_encodee,dictionnaire);
    I_decorrelee = reshape(I_decodee, [hauteur_I, largeur_I]);
    I_reconstruite = cumsum(I_decorrelee, 2);

end