% Fonction tirages_aleatoires 

function [tirages_C,tirages_R] = tirages_aleatoires_uniformes(n_tirages,G,R_moyen)
    
    tirages_C = G + (rand(2, n_tirages)-0.5)*2*R_moyen;
    tirages_R = R_moyen + (rand(1, n_tirages)-0.5)*R_moyen;

end