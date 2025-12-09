function modele_V = vraisemblances(X,mu,Sigma)

    modele_V = zeros(length(X), 1);
    inv_Sigma = inv(Sigma);
    for indice = 1:length(X)
        modele_V(indice) = exp(-0.5*(X(indice, :) - mu)*inv_Sigma*(X(indice, :) - mu)')/(2*pi*sqrt(det(Sigma)));
    end

end