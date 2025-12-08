% Fonction vectorisation_par_colonne 

function [Vd,Vg] = vectorisation_par_colonne(I)

    IVg = I(:, 1:end-1);
    IVd = I(:, 2:end);
    Vg = IVg(:);
    Vd = IVd(:);

end