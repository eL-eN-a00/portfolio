function [X_VS,w,c,code_retour] = SVM_dual(X_app, Y_app)

    Z = diag(Y_app);
    H = Z*X_app*X_app'*Z;
    f = -ones(length(Y_app), 1);
    Aeq = Y_app';
    beq = 0;
    lb = zeros(length(Y_app), 1);
    [alpha, ~, code_retour] = quadprog(H, f, [], [], Aeq, beq, lb);
    Indices_VS = (alpha > 1e-6);
    X_VS = X_app(Indices_VS, :);
    Y_VS = Y_app(Indices_VS);
    alpha_VS = alpha(Indices_VS);
    w = sum(alpha_VS.*Y_VS.*X_VS);
    c = X_VS(1, :)*w' - 1/Y_VS(1);
    w = w';

end