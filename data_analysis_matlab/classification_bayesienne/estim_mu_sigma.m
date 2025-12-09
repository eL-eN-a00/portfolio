function [mu,Sigma] = estim_mu_Sigma(X)

    mu = mean(X);
    Sigma = ((X - mu)'*(X - mu))/length(X);

end