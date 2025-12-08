% Fonction tirages_aleatoires_uniformes

function [tirages_angles,tirages_G] = tirages_aleatoires_uniformes(n_tirages,taille)

    % Tirages aleatoires d'angles : moyenne = 0 / demi-repartition = pi/2
    tirages_angles = pi*(rand(1, n_tirages) - 0.5);
    % Tirages aleatoires de points pour se trouver sur la droite (sur [-20,20])
    tirages_G = 40*(rand(2, n_tirages) - 0.5);

end