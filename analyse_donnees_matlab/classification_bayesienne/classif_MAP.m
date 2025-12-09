function Y_pred_MAP = classif_MAP(X,p1,mu_1,Sigma_1,mu_2,Sigma_2)

    Y_pred_MAP = zeros(length(X), 1);
    modele_V1 = vraisemblances(X, mu_1, Sigma_1);
    modele_V2 = vraisemblances(X, mu_2, Sigma_2);
    for indice = 1:length(X)
        if modele_V1(indice, 1) > modele_V2(indice, 1)
            Y_pred_MAP(indice) = modele_V1(indice, 1) + p1;
        else
            Y_pred_MAP(indice) = modele_V2(indice, 1) + 1 - p1;
        end
    end  
end