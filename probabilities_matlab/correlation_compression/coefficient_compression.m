% Fonction coefficient_compression 

function coeff_comp = coefficient_compression(signal_non_encode,signal_encode)

   nb_bits_sans_compression = 8*length(signal_non_encode);
   nb_bits_avec_compression = length(signal_encode);
   coeff_comp = nb_bits_sans_compression/nb_bits_avec_compression ;

end