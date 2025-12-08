% Fonction calcul_proba 

function [x_min,x_max,probabilite] = calcul_proba(E_nouveau_repere,p)

    x_min = min(E_nouveau_repere(:, 1));
    x_max = max(E_nouveau_repere(:, 1));
    deltax = x_max - x_min;
    deltay = max(E_nouveau_repere(:, 2)) - min(E_nouveau_repere(:, 2));
    N = round(deltax + deltay);
    n = size(E_nouveau_repere, 1);
    probabilite = 1 - binocdf(n-1, N, p);

end