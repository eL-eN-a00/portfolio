clear;
close all;
clc;

load exercice_1;

% Valeurs de l'a priori testees :
valeurs_p1 = 0:0.02:1;

% Comptage des images correctement classees :
[liste_pourcent_MAP,pourcent_MAP_max,p1_max] = opti_MAP(X_app,Y_app,valeurs_p1,mu_1,Sigma_1,mu_2,Sigma_2);

% Recuperation du pourcentage de bonnes classification par le maximum de vraisemblance (p1 = p2 = 0.5)
pourcent_MV = liste_pourcent_MAP(valeurs_p1 == 0.5);

% Trace du pourcentage de bonnes classifications en fonction de p_1 :
figure('Name','Recherche de l''a priori optimal',...
	'Position',[0.1*L,0.1*H,0.8*L,0.65*H],...
	'Color',[0.7 0.75 0.85]);
plot(valeurs_p1,liste_pourcent_MAP,'r','LineWidth',2,'HandleVisibility','off');
hold on;
plot([0.5 0.5],[50 pourcent_MV],'b--','LineWidth',2);
plot([p1_max p1_max],[50 pourcent_MAP_max],'b','LineWidth',2);
axis([0 1 50 100]);
grid on;

title(['Recherche de l''a priori optimal : p_1 = ' num2str(p1_max,'%.2f')]);
xlabel('Probabilité a priori de la classe 1');
ylabel('Pourcentage de bonnes classifications');
legend('MV','MAP');
set(gca,'FontSize',20);

