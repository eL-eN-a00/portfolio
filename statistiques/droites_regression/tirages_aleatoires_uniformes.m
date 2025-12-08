% Fonction tirages_aleatoires (exercice_1.m)

function tirages_angles = tirages_aleatoires_uniformes(n_tirages)
    
    tirages_angles = -(pi/2) + (pi/2)*rand(n_tirages, 1);

end