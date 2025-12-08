function [y_inf,y_sup] = silhouette_aleatoire(x,moyennes,ecarts_types,beta_0,gamma_0)

% Degre de la courbe de Bezier :
d = (length(moyennes)+1)/2;

% Tirage aleatoire des 2d-1 parametres (beta_k^j, gamma_k^j, epsilon^j) :
points_de_controle = moyennes+ecarts_types.*randn(size(moyennes));

% Calcul de y_inf a partir de beta_k^j et epsilon^j :
y_inf = courbe_bezier(x,[beta_0 ; points_de_controle(1:d-1) ; points_de_controle(end)]);

% Calcul de y_sup a partir de gamma_k^j et epsilon^j :
y_sup = courbe_bezier(x,[gamma_0 ; points_de_controle(d:end)]);
