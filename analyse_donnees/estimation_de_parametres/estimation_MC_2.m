function parametres = estimation_MC_2(d,x,y_inf,y_sup)

    A_av = ones(size(x, 1), d);
    for k=1:d
        A_av(:, k) = vecteur_bernstein(x, d, k);
    end
    A_ap = A_av(:, 1:end-1);
    A_apd = A_av(:, end);
    A_finale = zeros(size(x, 1)*2, (2*d-1));
    A_finale(1:size(x, 1), 1:d-1) = A_ap ;
    A_finale(size(x, 1)+1:size(x, 1)*2 , d:2*d-2) = A_ap;
    A_finale(1:size(x, 1), end) = A_apd;
    A_finale(size(x, 1)+1:size(x, 1)*2 , end) = A_apd;
    b = ones(size(x, 1)*2, 1);
    b(1:size(x, 1)) = y_inf-y_inf(1)*vecteur_bernstein(x, d, 0);
    b(size(x, 1)+1:size(x, 1)*2) = y_sup-y_sup(1)*vecteur_bernstein(x, d, 0);
    parametres = A_finale\b;

end