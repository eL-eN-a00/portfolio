clear;
close all;
clc;

load exercice_1;

%------------------------------------------------------------------------------------------
% Classification de l'ensemble d'apprentissage par maximum de vraisemblance
%------------------------------------------------------------------------------------------

% Grille de classification par maximum de vraisemblance entre les 2 classes :
Y_pred_MV_grille = classif_MV(X_grille,mu_1,Sigma_1,mu_2,Sigma_2);
Y_pred_MV_grille = reshape(Y_pred_MV_grille,length(graduation_carac_2),length(graduation_carac_1));

% Affichage de la grille du maximum de vraisemblance :
figure('Name','Classification par maximum de vraisemblance',...
	'Position',[0.02*L,0.1*H,0.47*L,0.65*H],...
	'Color',[0.7 0.75 0.85]);
imagesc(graduation_carac_1,graduation_carac_2,Y_pred_MV_grille)
carte_couleurs = [0.5 0.5 1; 1 0.5 0.5];
colormap(carte_couleurs);
axis xy;
axis(limites_affichage);
xlabel(nom_carac_1);
ylabel(nom_carac_2);
set(gca,'FontSize',20);
hold on;

% Classification dans l'ensemble d'apprentissage :
Y_app_pred_MV = classif_MV(X_app,mu_1,Sigma_1,mu_2,Sigma_2);

% Affichage des points de l'ensemble d'apprentissage :
[f_ok,m_ok] = affichage_points_classes(X_app,Y_app,Y_app_pred_MV);

% Calcul des bonnes classifications :
[pourcent_MV,pourcent_fibrome,pourcent_melanome] = eval_classif(Y_app_pred_MV,Y_app);

% Affichage du taux de bonnes classifications :
title({'Classification par MV (apprentissage)' [num2str(pourcent_MV,'%.1f') '% de bonnes classifications']});
legend([f_ok,m_ok],['Fibromes bien classés (' num2str(pourcent_fibrome,'%.1f') '%)'],...
	['Mélanomes bien classés (' num2str(pourcent_melanome,'%.1f') '%)'],'Location','NorthWest');
set(gca,'FontSize',20);
