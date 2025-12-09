function [pourcent_total,pourcent_fibrome,pourcent_melanome] = eval_classif(Y_pred,Y)

% Pourcentage de fibromes bien classes :
nb_bonnes_classif_fibrome = sum(Y == 1 & Y_pred == 1);
nb_donnees_fibrome = sum(Y == 1);
pourcent_fibrome = 100*nb_bonnes_classif_fibrome/nb_donnees_fibrome;

% Pourcentage de melanomes bien classes : 
nb_bonnes_classif_melanome = sum(Y ~= 1 & Y_pred ~= 1);
nb_donnees_melanome = sum(Y ~= 1);
pourcent_melanome = 100*nb_bonnes_classif_melanome/nb_donnees_melanome;

% Pourcentage total d'elements bien classes :
nb_bonnes_classif = sum(Y == Y_pred);
nb_donnees = size(Y,1);
pourcent_total = 100*nb_bonnes_classif/nb_donnees;
