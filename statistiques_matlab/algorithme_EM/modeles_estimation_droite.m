exemple;

% Nombre de tirages aleatoires pour la partie vraisemblance :
n_tirages = 100;

% Estimation de la droite de regression par le maximum de vraisemblance :
tirages_theta = tirages_aleatoires_uniformes(n_tirages,taille); % FONCTION A CODER
[a_Dyx_MV,b_Dyx_MV] = estim_param_Dyx_MV(x_donnees_bruitees,y_donnees_bruitees,tirages_theta); % FONCTION A CODER

% Creation de la droite estimee par le maximum de vraisemblance :
x_Dyx_MV = [-taille;x_donnees_bruitees;taille];
y_Dyx_MV = a_Dyx_MV*x_Dyx_MV+b_Dyx_MV;

% Affichage des droites :
plot(x_Dyx_MV,y_Dyx_MV,'b','LineWidth',3);
axis(bornes);
legend(' Droites_{VT} ', ...
       ' Donnees', ...
       ' Droite_{MV}', ...
       'Location','Best');
axis equal;
xlim([-taille taille]);
ylim([-taille taille]);
set(gca,'FontSize',15);
hx = xlabel('$x$','FontSize',30);
set(hx,'Interpreter','Latex');
hy = ylabel('$y$','FontSize',30);
set(hy,'Interpreter','Latex');
grid on;

% Fonction estim_param_Dyx_MV (exercice_1.m)
function [a_Dyx,b_Dyx] = estim_param_Dyx_MV(x_donnees_bruitees,y_donnees_bruitees,tirages_psi)

    % Centrage des donnees :
    [x_G, y_G, x_donnees_bruitees_centrees, y_donnees_bruitees_centrees] = ...
               centrage_des_donnees(x_donnees_bruitees,y_donnees_bruitees);

    % Estimation par maximum de vraisemblance :
    n_tirages = size(tirages_psi,2);
    residus = repmat(y_donnees_bruitees_centrees,1,n_tirages) - ...
                     x_donnees_bruitees_centrees*tan(tirages_psi);
    SCR = sum(residus.^2);
    [~,indice_min] = min(SCR);
    psi_Dyx = tirages_psi(indice_min);
    a_Dyx = tan(psi_Dyx);
    b_Dyx = y_G - a_Dyx*x_G;
    
end

% Fonction centrage_des_donnees (exercice_1.m)
function [x_G, y_G, x_donnees_bruitees_centrees, y_donnees_bruitees_centrees] = ...
                centrage_des_donnees(x_donnees_bruitees,y_donnees_bruitees)
     
    % Centrage des donnees :
    x_G = mean(x_donnees_bruitees);
    y_G = mean(y_donnees_bruitees);
    x_donnees_bruitees_centrees = x_donnees_bruitees - x_G;
    y_donnees_bruitees_centrees = y_donnees_bruitees - y_G;     
     
end