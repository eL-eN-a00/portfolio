function [f_ok,m_ok] = affichage_points_classes(X,Y,Y_pred)

nb_donnees = size(Y,1);

% Affichage des points :
for i = 1:nb_donnees
	if Y(i) == 1
		if Y_pred(i) == 1
			f_ok = plot(X(i,1),X(i,2),'bx','MarkerSize',10,'LineWidth',3);
		else
			plot(X(i,1),X(i,2),'kx','MarkerSize',10,'LineWidth',3);
            end
	else
		if Y_pred(i) == 1
			plot(X(i,1),X(i,2),'ko','MarkerSize',10,'LineWidth',3);
		else
			m_ok = plot(X(i,1),X(i,2),'ro','MarkerSize',10,'LineWidth',3);
		end
	end
end
