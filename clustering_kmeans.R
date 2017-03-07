station_x_date_mod7j <- read.table(file = './Sortie/station_x_date_mod7j.csv',sep = ';',dec = ',',header = T)
stations_gps <- read.table(file = './Sortie/stations_gps.csv',sep = ';',dec = ',',header = T)

cluster <- lapply(1:10,function(.x) kmeans(station_x_date_mod7j[ ,2:ncol(station_x_date_mod7j)],.x,iter.max = 50))
representation_kmeans <- lapply(1:10,function(.x) merge(data.frame(number = station_x_date_mod7j$number,
																																	 cluster = cluster[[.x]]$cluster,
																																	 station_x_date_mod7j[,-1]), 
																												stations_gps, by="number", all.x=F))

### representation graphique des kmeans en fonction du nombre de groupe (stocker dans map_plot_i_cluster)
# tracer le graphique grid a : grid.draw(a)
for(i in 2:10){
	ifelse(is.null(dev.list()),1,dev.off()) # nettoye l'ecran pour enregistrer la carte avec les groupes 
	PlotOnStaticMap(carte, lat=representation_kmeans[[i]][,'lat'], lon=representation_kmeans[[i]][,'lon'], 
									pch=16, cex=1, 
									col=brewer.pal(10, 'Paired')[as.numeric(representation_kmeans[[i]]$cluster)])
	grid.echo()
	eval(parse(text = paste('map_plot_',i,'_cluster <- grid.grab()',sep = '')))
	dev.off()
}

# grid.draw(map_plot_2_cluster)
# grid.draw(map_plot_3_cluster)
# grid.draw(map_plot_4_cluster)
# grid.draw(map_plot_5_cluster)
# grid.draw(map_plot_6_cluster)
# grid.draw(map_plot_7_cluster)
# grid.draw(map_plot_8_cluster)
# grid.draw(map_plot_9_cluster)
# grid.draw(map_plot_10_cluster)

### profil par classe

moyenne_par_classe <- function(x){
	liste_variable_temps <- names(representation_kmeans[[x]])[which(substr(names(representation_kmeans[[x]]),1,1)=='T')]
	liste_moyenne <- paste('M', substring(liste_variable_temps,2)," = mean(",liste_variable_temps,",na.rm=T)",sep = '') %>%
		paste(collapse = ', ')
	chaine_dplyr <-  paste('res <- representation_kmeans[[',x,']] %>% group_by(cluster) %>% summarise(',liste_moyenne,')',sep = '')
	eval(parse(text =chaine_dplyr))
	return(res)
}

profil_par_classe <- lapply(1:10,moyenne_par_classe)

# tracer profil de classe en fonction du nombre de classes
for(i in 2:10){
	print(i)
	ifelse(is.null(dev.list()),1,dev.off())
	couleur <- brewer.pal(10, 'Paired')
	n <- ncol(profil_par_classe[[i]])
	xx <- seq_along(names(profil_par_classe[[i]])[-1])
	plot(xx,as.data.frame(profil_par_classe[[i]][1,2:n]),type = 'l',
			 col = couleur[1],ylim = c(0,1),
			 ylab = 'proportion de velib dans la classe',
			 xlab = '')
	for(j in 2:i){
		lines(xx,as.data.frame(profil_par_classe[[i]][j,2:n]),type = 'l',
					col = couleur[j])  
	}
	legend(450,1,legend = 1:i,col = couleur[1:i],pch = 15)
	grid.echo()
	eval(parse(text = paste('profil_de_classe_',i,'_cluster <- grid.grab()',sep = '')))
	dev.off()
}

# grid.draw(profil_de_classe_2_cluster)
# grid.draw(profil_de_classe_3_cluster)
# grid.draw(profil_de_classe_4_cluster)
# grid.draw(profil_de_classe_5_cluster)
# grid.draw(profil_de_classe_6_cluster)
# grid.draw(profil_de_classe_7_cluster)
# grid.draw(profil_de_classe_8_cluster)
# grid.draw(profil_de_classe_9_cluster)
# grid.draw(profil_de_classe_10_cluster)

# tracer profil de classe avec les membre de la classe
l <- 5L
tracer_profil_membres <- function(nb_classes, classe, nb_membre){
	ifelse(is.null(dev.list()),1,dev.off())
	couleur <- brewer.pal(10, 'Paired')
	n <- ncol(representation_kmeans[[nb_classes]])-3
	plot(xx,as.data.frame(profil_par_classe[[nb_classes]][classe,2:n]),type = 'l',col = couleur[classe],ylim = c(0,1),
			 ylab = 'proportion de velib dans la classe',
			 xlab = '')
	for(j in sample(which(representation_kmeans[[nb_classes]]$cluster==classe),nb_membre)){
			lines(xx,representation_kmeans[[nb_classes]][j,2:n + 1], col = 'lightgray')	
	}
	lines(xx,as.data.frame(profil_par_classe[[nb_classes]][classe,2:n]), col = couleur[k])
	grid.echo()
	eval(parse(text = paste('profil_',nb_classes,'_cluster_classe_',classe,' <- grid.grab()',sep = '')))
}

for(i in 2:10){
	for (k in 1:i){
		tracer_profil_membres(i,k,l)
		dev.off()
	}
}

# grid.draw(profil_6_cluster_classe_1)
# grid.draw(profil_6_cluster_classe_2)
# grid.draw(profil_6_cluster_classe_3)
# grid.draw(profil_6_cluster_classe_4)
# grid.draw(profil_6_cluster_classe_5)
# grid.draw(profil_6_cluster_classe_6)
tracer_profil_membres(6,2,l)

for(i in 2:10){
  write.table(x = representation_kmeans[[i]][,c('number','cluster')],
              file = paste('./Sortie/clustering_',i,'_classes_mod7j.csv',sep = ''),
              sep = ';',dec = ',',row.names = F)
}
