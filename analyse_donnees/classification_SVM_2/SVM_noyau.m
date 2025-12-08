function [X_VS,Y_VS,Alpha_VS,c,code_retour] = SVM_noyau(X,Y,sigma)

    
    Aeq = Y';
    beq = 0;
    lb = zeros(length(Y), 1);
    f = -ones(length(Y), 1);
    H = calcul_noyau(X, X, sigma);
    H = diag(Y)*H*diag(Y);
    [Alpha, ~, code_retour] = quadprog(H, f, [], [], Aeq, beq, lb);
    Indices_VS = (Alpha > 1e-6);
    X_VS = X(Indices_VS, :);
    Y_VS = Y(Indices_VS);
    Alpha_VS = Alpha(Indices_VS);
    c = sum(Alpha_VS.*Y_VS.*calcul_noyau(X_VS, X_VS(1, :), sigma)) - 1/Y_VS(1);

end