function [correlation,contraste] = correlation_contraste(X)

    moyenne = mean(X);
    n = size(X, 1);
    Xc = X - repmat(moyenne, n, 1);
    sigma = 1/n*(Xc'*Xc);
    variances = diag(sigma);
    correlation_RV = sigma(1,2)./(sqrt(sigma(1,1)*sigma(2,2)));
    correlation_RB = sigma(1,3)./(sqrt(sigma(1,1)*sigma(3,3)));
    correlation_BV = sigma(2,3)./(sqrt(sigma(3,3)*sigma(2,2)));
    correlation = [correlation_RV, correlation_RB, correlation_BV];
    contraste = variances/sum(variances);

end