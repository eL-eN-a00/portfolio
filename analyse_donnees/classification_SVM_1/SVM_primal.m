function [X_VS,w,c,code_retour] = SVM_primal(X_app,Y_app)
    
    H = eye(3, 3);
    H(3, 3) = 0;
    f = zeros(3, 1);
    n = size(X_app, 1);
    A = zeros(n, 3);
    A(:, 1:2) = -X_app;
    A(:, 3) = ones(n, 1);
    Yi = repmat(Y_app, 1, 3);
    A = Yi.*A;
    B = -ones(n, 1);
    [X, ~, code_retour] = quadprog(H, f, A, B);
    w = X(1:2);
    c = X(3);
    Indices_VS = find(Y_app.*(X_app*w - c) - 1 < 1e-6);
    X_VS = X_app(Indices_VS, :);

end