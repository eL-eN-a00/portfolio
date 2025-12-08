clear;
close all;
clc;

% Parametres pour l'affichage des donnees :
taille_ecran = get(0,'ScreenSize');
L = taille_ecran(3);
H = taille_ecran(4);

% Donnees d'apprentissage non filtrees :
load donnees_carac;
X_app = X_app(:,1:2);
Y_app(Y_app == 2) = -1;			% Changement du label pour utiliser le SVM

% Donnees de test :
X_test = X_test(:,1:2);
Y_test(Y_test == 2) = -1;		% Changement du label pour utiliser le SVM

sigma = 0.0037;

%--------------------------------------------------------------------------
% Creation de la grille pour l'affichage des classes predites
%--------------------------------------------------------------------------

% Parametres d'affichage :
pas = 0.002;
marge = 0.005;
graduation_carac_1 = min(min(X_app(:,1)))-marge:pas:max(max(X_app(:,1)))+marge;
graduation_carac_2 = min(min(X_app(:,2)))-marge:pas:max(max(X_app(:,2)))+marge;
limites_affichage = [graduation_carac_1(1) graduation_carac_1(end) ...
                     graduation_carac_2(1) graduation_carac_2(end)];
nom_carac_1 = 'Compacité';
nom_carac_2 = 'Contraste';

% SVM avec noyau :
[X_VS,Y_VS,Alpha_VS,c,code_retour] = SVM_noyau(X_app,Y_app,sigma);

% L'optimisation converge-t-elle :
if code_retour ~= 1
	return;
end

% Classification d'une grille reguliere par le SVM (pour affichage des zones de couleur) :
[grille_carac1,grille_carac2] = meshgrid(graduation_carac_1,graduation_carac_2);
X_grille = [grille_carac1(:),grille_carac2(:)];
Y_pred_grille = classif_SVM_avec_noyau(X_grille,sigma,X_VS,Y_VS,Alpha_VS,c);
Y_pred_grille = reshape(Y_pred_grille,length(graduation_carac_2),length(graduation_carac_1));

% Affichage des classes predites par le SVM sur les donnees d'apprentissage :
figure('Name','Classification par SVM (données d''apprentissage)',...
       'Position',[0.02*L,0.1*H,0.47*L,0.7*H],'Color',[0.7 0.75 0.85]);
imagesc(graduation_carac_1,graduation_carac_2,Y_pred_grille),
carte_couleurs = [1 0.5 0.5; 0.5 0.5 1];
colormap(carte_couleurs);
axis xy;
axis(limites_affichage);
xlabel(nom_carac_1);
ylabel(nom_carac_2);
set(gca,'FontSize',20);
hold on;

%--------------------------------------------------------------------------
% Classification et affichage des donnees d'apprentissage
%--------------------------------------------------------------------------

% Classification des donnees d'apprentissage :
Y_app_pred = classif_SVM_avec_noyau(X_app,sigma,X_VS,Y_VS,Alpha_VS,c);
[pourcent_total,pourcent_fibrome,pourcent_melanome] = eval_classif(Y_app_pred,Y_app);

% Affichage des points de l'ensemble d'apprentissage :
[f_ok,m_ok] = affichage_points_classes(X_app,Y_app,Y_app_pred);

title(['Bonnes classifications (données d''aprentissage) : ' ...
	num2str(pourcent_total,'%.1f') '%']);
legend([f_ok,m_ok],...
	['Fibromes bien classés (' num2str(pourcent_fibrome,'%.1f') '%)'],...
	['Mélanomes bien classés (' num2str(pourcent_melanome,'%.1f') '%)'],...
	'Location','NorthWest');
set(gca,'FontSize',20);

%--------------------------------------------------------------------------
% Classification et affichage des donnees de test
%--------------------------------------------------------------------------

% Affichage des classes predites par le SVM sur les donnees de test :
figure('Name','Classification par SVM (données de test)',...
       'Position',[0.52*L,0.1*H,0.47*L,0.7*H],'Color',[0.7 0.75 0.85]);
imagesc(graduation_carac_1,graduation_carac_2,Y_pred_grille),
carte_couleurs = [1 0.5 0.5; 0.5 0.5 1];
colormap(carte_couleurs);
axis xy;
axis(limites_affichage);
xlabel(nom_carac_1);
ylabel(nom_carac_2);
set(gca,'FontSize',20);
hold on;

% Classification des donnees de test :
Y_test_pred = classif_SVM_avec_noyau(X_test,sigma,X_VS,Y_VS,Alpha_VS,c);
[pourcent_total,pourcent_fibrome,pourcent_melanome] = eval_classif(Y_test_pred,Y_test);

% Affichage des points de l'ensemble de test :
[f_ok,m_ok] = affichage_points_classes(X_test,Y_test,Y_test_pred);

title(['Bonnes classifications (données de test) : ' ...
	num2str(pourcent_total,'%.1f') '%']);
legend([f_ok,m_ok],...
	['Fibromes bien classés (' num2str(pourcent_fibrome,'%.1f') '%)'],...
	['Mélanomes bien classés (' num2str(pourcent_melanome,'%.1f') '%)'],...
	'Location','NorthWest');
set(gca,'FontSize',20);
