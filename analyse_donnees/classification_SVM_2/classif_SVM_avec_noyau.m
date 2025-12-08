function Y_pred = classif_SVM_avec_noyau(X,sigma,X_VS,Y_VS,Alpha_VS,c)

nb_donnees = size(X,1);
Y_pred = zeros(nb_donnees,1);

% Calcul de la matrice de Gram pour tous les vecteurs de support X_vs_i / tous les vecteurs X_i
K = calcul_noyau(X_VS,X,sigma);

for i = 1:nb_donnees
	Y_pred(i) = sign(sum(Alpha_VS.*Y_VS.*K(:,i))-c);
end
