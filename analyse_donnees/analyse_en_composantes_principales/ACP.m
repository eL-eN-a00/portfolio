function [C,P] = ACP(X)

    moyenne = mean(X);
    n = size(X, 1);
    Xc = X - repmat(moyenne, n, 1);
    sigma = 1/n*(Xc'*Xc);
    [W, D] = eig(sigma);
    [~, indices] = sort(diag(D), 'descend');
    P = W(:, indices);
    C = Xc*P;

end