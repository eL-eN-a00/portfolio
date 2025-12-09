function [beta, norm_grad_f_beta, f_beta, norm_delta, nb_it, exitflag] ...
          = Algo_Newton(Hess_f_C14, beta0, option)

%
% Newton resout par l'algorithme de Newton les problemes aux moindres carres
% Min 0.5||r(beta)||^2
% beta \in R^p
%
% Parametres en entrees
% --------------------
% Hess_f_C14 : fonction qui code la hessiennne de f
%              Hess_f_C14 : R^p --> matrice (p,p)
%              (la fonction retourne aussi le residu et la jacobienne)
% beta0  : point de départ
%          real(p)
% option(1) : Tol_abs, tolérance absolue
%             real
% option(2) : Tol_rel, tolérance relative
%             real
% option(3) : nitimax, nombre d'itérations maximum
%             integer
%
% Parametres en sortie
% --------------------
% beta      : beta
%             real(p)
% norm_gradf_beta : ||gradient f(beta)||
%                   real
% f_beta    : f(beta)
%             real
% res       : r(beta)
%             real(n)
% norm_delta : ||delta||
%              real
% nbit       : nombre d'itérations
%              integer
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
        if norm_grad_f_beta <= max(sqrt(eps)*norm_grad_f_beta0, sqrt(eps))
            exitflag = 1;
        elseif abs(f_beta - f_beta_ancien) <= max(sqrt(eps)*abs(f_beta), sqrt(eps))
            exitflag = 2;
        elseif norm(beta - beta_ancien) <= max(sqrt(eps)*norm(beta), sqrt(eps))
            exitflag = 3;
        end
    end

end
