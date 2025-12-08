function [beta, norm_grad_f_beta, f_beta, norm_delta, nb_it, exitflag] ...
          = Algo_Gauss_Newton(residu, J_residu, beta0, option)

%
% GN resout par l'algorithme de Gauss-Newton les problemes aux moindres carres
% Min 0.5||r(beta)||^2
% beta \in \IR^p
%
% Paramètres en entrés
% --------------------
% residu : fonction qui code les résidus
%          r : \IR^p --> \IR^n
% J_residu : fonction qui code la matrice jacobienne
%            Jr : \IR^p --> real(n,p)
% beta0 : point de départ
%         real(p)
% option(1) : Tol_abs, tolérance absolue
%             real
% option(2) : Tol_rel, tolérance relative
%             real
% option(3) : n_itmax, nombre d'itérations maximum
%             integer
%
% Paramètres en sortie
% --------------------
% beta      : beta
%             real(p)
% norm_gradf_beta : ||gradient f(beta)||
%                   real
% f_beta : f(beta)
%          real
% r_beta : r(beta)
%          real(n)
% norm_delta : ||delta||
%              real
% nbit : nombre d'itérations
%        integer
% exitflag   : indicateur de sortie
%              integer entre 1 et 4
% exitflag = 1 : ||gradient f(beta)|| < max(Tol_rel||gradient f(beta0)||,Tol_abs)
% exitflag = 2 : |f(beta^{k+1})-f(beta^k)| < max(Tol_rel|f(beta^k)|,Tol_abs)
% exitflag = 3 : ||delta)|| < max(Tol_rel delta^k),Tol_abs)
% exitflag = 4 : nombre maximum d'itérations atteint
%      
% ---------------------------------------------------------------------------------

    beta = beta0;
    nb_it = 0;
    norm_grad_f_beta = 0;
    exitflag = 4;
    norm_grad_f_beta0 = norm(J_residu(beta0)'*residu(beta0));
    f_beta = 0;

    while nb_it <= option(3) && exitflag == 4
        r_beta = residu(beta);
        J_beta = J_residu(beta);
        beta_ancien = beta;
        beta = beta - (J_beta'*J_beta)\J_beta'*r_beta;
        delta = beta0 - beta;
        norm_delta = norm(delta);
        nb_it= nb_it + 1;
        f_beta_ancien = f_beta;
        f_beta = 0.5*norm(r_beta);
        norm_grad_f_beta = norm(J_beta'*r_beta);
        Tol_abs = option(1);
        Tol_rel = option(2);
        if norm_grad_f_beta <= max(Tol_rel*norm_grad_f_beta0, Tol_abs)
            exitflag = 1;
        elseif abs(f_beta - f_beta_ancien) <= max(Tol_rel*abs(f_beta), Tol_abs)
            exitflag = 2;
        elseif norm(beta - beta_ancien) <= max(Tol_rel*norm(beta), Tol_abs)
            exitflag = 3;
        end
    end

end
