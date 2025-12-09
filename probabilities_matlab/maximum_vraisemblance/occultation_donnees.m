% Fonction occultation_donnees (donnees_occultees.m)

function [x_donnees_bruitees_visibles, y_donnees_bruitees_visibles] = occultation_donnees(x_donnees_bruitees, y_donnees_bruitees, theta_donnees_bruitees, thetas)
    
    if thetas(1) < thetas(2)
        ind_theta_vis = theta_donnees_bruitees > thetas(1) & theta_donnees_bruitees < thetas(2);
    else
        ind_theta_vis = theta_donnees_bruitees > thetas(1) | theta_donnees_bruitees < thetas(2);
    end
    x_donnees_bruitees_visibles = x_donnees_bruitees(ind_theta_vis);
    y_donnees_bruitees_visibles = y_donnees_bruitees(ind_theta_vis);

end