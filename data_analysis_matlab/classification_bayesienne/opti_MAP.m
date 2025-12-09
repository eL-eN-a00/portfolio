function [liste_pourcent_MAP,pourcent_MAP_max,p1_max] = opti_MAP(X,Y,valeurs_p1,mu_1,Sigma_1,mu_2,Sigma_2)

    Y_pred_Max = classif_MAP(X,valeurs_p1,mu_1,Sigma_1,mu_2,Sigma_2);

end