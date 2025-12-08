function K = calcul_noyau(Xi,Xj,sigma)

ni = size(Xi,1);
nj = size(Xj,1);
distances_carre = zeros(ni,nj);
for k = 1:size(Xi,2)
	distances_carre = distances_carre+(repmat(Xi(:,k),1,nj)-repmat(Xj(:,k)',ni,1)).^2);
end;
K = exp(-distances_carre/(2*sigma^2));
end;