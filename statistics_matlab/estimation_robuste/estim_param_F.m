% Fonction estim_param_F 

function [rho_F,theta_F,ecart_moyen] = estim_param_F(rho,theta)

    A = [cos(theta), sin(theta)];
    B = rho;
    X = A\B;
    rho_F = sqrt(X(1)^2 + X(2)^2);
    theta_F = atan2(X(2), X(1));
    ecart_moyen = sum(A*X - B)/length(A);

end