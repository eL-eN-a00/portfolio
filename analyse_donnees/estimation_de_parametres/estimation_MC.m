function parametres = estimation_MC(d,x,y)

    A = ones(size(x, 1), d);
    for k=1:d
        A(:, k) = vecteur_bernstein(x, d, k);
    end
    b = y-y(1)*vecteur_bernstein(x, d, 0);
    parametres = A\b;

end