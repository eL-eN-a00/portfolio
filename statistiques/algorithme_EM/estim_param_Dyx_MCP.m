% Fonction estim_param_Dyx_MCP

function [a_Dyx,b_Dyx] = estim_param_Dyx_MCP(x_donnees_bruitees,y_donnees_bruitees,probas_classe)

    A = [x_donnees_bruitees.*probas_classe, ones(size(x_donnees_bruitees)).*probas_classe];
    B = y_donnees_bruitees.*probas_classe;
    X = A\B;
    a_Dyx = X(1);
    b_Dyx = X(2);
    
end