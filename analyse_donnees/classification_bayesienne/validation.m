clear;
close all;
clc;

load apprentissage_statistique;
load classification_maximum_posteriori;

% Recuperation des colonnes de X correspondant aux deux 
% caracteristiques choisies de l'ensemble de test :
X_test = X_test(:,[ind_carac_1 ind_carac_2]);
nb_donnees_test = size(Y_test,1);

%------------------------------------------------------------------------------------------
% Classification de l'ensemble de test par maximum de vraisemblance
%------------------------------------------------------------------------------------------

% Grille de classification par maximum de vraisemblance entre les 2 classes :
Y_pred_MV_grille = classif_MV(X_grille,mu_1,Sigma_1,mu_2,Sigma_2);
Y_pred_MV_grille = reshape(Y_pred_MV_grille,length(graduation_carac_2),length(graduation_carac_1));

% Affichage de la grille du maximum de vraisemblance :
figure('Name','Classification par maximum de vraisemblance',...
	'Position',[0.02*L,0.1*H,0.47*L,0.65*H],...
	'Color',[0.7 0.75 0.85]);
imagesc(graduation_carac_1,graduation_carac_2,Y_pred_MV_grille);
carte_couleurs = [0.5 0.5 1; 1 0.5 0.5];
colormap(carte_couleurs);
axis xy;
axis(limites_affichage);
title('Classification par MV (test)');
xlabel(nom_carac_1);
ylabel(nom_carac_2);
set(gca,'FontSize',20);
hold on;    
    
% Classification dans l'ensemble de test :
Y_test_pred_MV = classif_MV(X_test,mu_1,Sigma_1,mu_2,Sigma_2);

% Affichage des points de l'ensemble de test :
[f_ok,m_ok] = affichage_points_classes(X_test,Y_test,Y_test_pred_MV);

% Calcul des bonnes classifications :
[pourcent_MV,pourcent_fibrome,pourcent_melanome] = eval_classif(Y_test_pred_MV,Y_test);

% Affichage du taux de bonnes classifications :
title({'Classification par MV (test)' [num2str(pourcent_MV,'%.1f') '% de bonnes classifications']});
legend([f_ok,m_ok],['Fibromes bien classés (' num2str(pourcent_fibrome,'%.1f') '%)'],...
	['Melanomes bien classés (' num2str(pourcent_melanome,'%.1f') '%)'],'Location','NorthWest');
set(gca,'FontSize',20);

%------------------------------------------------------------------------------------------
% Classification de l'ensemble de test par maximum a posteriori
%------------------------------------------------------------------------------------------

% Grille de classification par maximum a posteriori entre les 2 classes :
Y_pred_MAP_grille = classif_MAP(X_grille,p1_max,mu_1,Sigma_1,mu_2,Sigma_2);
Y_pred_MAP_grille = reshape(Y_pred_MAP_grille,length(graduation_carac_2),length(graduation_carac_1));

% Affichage de la grille du maximum a posteriori :
figure('Name','Classification par maximum a posteriori',...
	'Position',[0.51*L,0.1*H,0.47*L,0.65*H],...
	'Color',[0.7 0.75 0.85]);
imagesc(graduation_carac_1,graduation_carac_2,Y_pred_MAP_grille)
carte_couleurs = [0.5 0.5 1; 1 0.5 0.5];
colormap(carte_couleurs);
axis xy;
axis(limites_affichage);
title(['Classification par MAP avec p_1 = ' num2str(p1_max,'%.2f') ' (test)']);
xlabel(nom_carac_1);
ylabel(nom_carac_2);
set(gca,'FontSize',20);
hold on;    

% Classification dans l'ensemble de test :
Y_test_pred_MAP = classif_MAP(X_test,p1_max,mu_1,Sigma_1,mu_2,Sigma_2);

% Affichage des points de l'ensemble de test :
[f_ok,m_ok] = affichage_points_classes(X_test,Y_test,Y_test_pred_MAP);

% Calcul des bonnes classifications :
[pourcent_MAP,pourcent_fibrome,pourcent_melanome] = eval_classif(Y_test_pred_MAP,Y_test);

% Affichage du taux de bonnes classifications :
title({['Classification par MAP avec p_1 = ' num2str(p1_max,'%.2f') ' (test)'] ...
	[num2str(pourcent_MAP,'%.1f') '% de bonnes classifications']});
xlabel(nom_carac_1);
ylabel(nom_carac_2);
legend([f_ok,m_ok],['Fibromes bien classés (' num2str(pourcent_fibrome,'%.1f') '%)'],...
	['Melanomes bien classés (' num2str(pourcent_melanome,'%.1f') '%)'],'Location','NorthWest');
set(gca,'FontSize',20);
