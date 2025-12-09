% Fonction parametres_correlation

function [r,a,b] = parametres_correlation(Vd,Vg)

    moy_Vg = mean(Vg);
    moy_Vd = mean(Vd);
    var_Vg = mean(Vg.^2) - moy_Vg^2;
    var_Vd = mean(Vd.^2) - moy_Vd^2;
    cov = mean(Vd.*Vg) - moy_Vg*moy_Vd;
    r = cov/(sqrt(var_Vg*var_Vd));
    a = cov/var_Vd;
    b = moy_Vg - a*moy_Vd;
end