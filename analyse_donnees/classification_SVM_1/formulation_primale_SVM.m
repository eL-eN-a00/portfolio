clear;
close all;
clc;

% Parametres pour l'affichage des donnees :
taille_ecran = get(0,'ScreenSize');
L = taille_ecran(3);
H = taille_ecran(4);

% Donnees filtrees :
load donnees_carac_filtrees;
X_app = X_app(:,1:2);			% Seules les deux premieres caracteristiques sont utilisees
Y_app(Y_app == 2) = -1;			% Changement du label pour utiliser le SVM
Y_app = Y_app*0.9;
nb_donnees_app = size(Y_app,1);

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

% SVM (forme primale) :
[X_VS,w,c,code_retour] = SVM_primal(X_app,Y_app);

% L'optimisation converge-t-elle :
if code_retour ~= 1
	return;
end

% Classification d'une grille reguliere par le SVM (pour affichage des zones de couleur) :
[grille_carac1,grille_carac2] = meshgrid(graduation_carac_1,graduation_carac_2);
X_grille = [grille_carac1(:),grille_carac2(:)];
Y_pred_grille = sign(X_grille*w-c);
Y_pred_grille = reshape(Y_pred_grille,length(graduation_carac_2),length(graduation_carac_1));

% Affichage des classes predites par le SVM :
figure('Name','Classification par SVM',...
	'Position',[0.02*L,0.1*H,0.47*L,0.7*H],'Color',[0.7 0.75 0.85]);
imagesc(graduation_carac_1,graduation_carac_2,Y_pred_grille);
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
Y_app_pred = sign(X_app*w-c);
[pourcent_total,pourcent_fibrome,pourcent_melanome] = eval_classif(Y_app_pred,Y_app/0.9);

% Affichage des points de l'ensemble d'apprentissage :
[f_ok,m_ok] = affichage_points_classes(X_app,Y_app/0.9,Y_app_pred);

% Les vecteurs de support sont entoures en noir :
vs = plot(X_VS(:,1),X_VS(:,2),'ko','MarkerSize',20,'LineWidth',3);

title('Données séparables - SVM (forme primale)')
legend([f_ok,m_ok,vs],...
       ['Fibromes bien classés (' num2str(pourcent_fibrome,'%.1f') '%)'],...
       ['Mélanomes bien classés (' num2str(pourcent_melanome,'%.1f') '%)'],...
       'Vecteurs de support','Location','NorthWest');
set(gca,'FontSize',20);
