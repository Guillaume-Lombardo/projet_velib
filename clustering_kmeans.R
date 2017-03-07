station_x_date_mod7j <- read.table(file = './Sortie/station_x_date_mod7j.csv',sep = ';',dec = ',',header = T)
stations_gps <- read.table(file = './Sortie/stations_gps.csv',sep = ';',dec = ',',header = T)

cluster <- lapply(1:10,function(.x) kmeans(station_x_date_mod7j[ ,2:ncol(station_x_date_mod7j)],.x,iter.max = 50))
representation_kmeans <- lapply(1:10,function(.x) merge(data.frame(number = station_x_date_mod7j$number,
																																	 cluster = cluster[[.x]]$cluster,
																																	 station_x_date_mod7j[,-1]), 
																												stations_gps, by="number", all.x=F))

### representation graphique des kmeans en fonction du nombre de groupe
# tracer le graphique grid a : grid.draw(a)
for(i in 2:10){
	ifelse(is.null(dev.list()),1,dev.off()) # nettoye l'ecran pour enregistrer la carte avec les groupes 
	PlotOnStaticMap(carte, lat=representation_kmeans[[i]][,'lat'], lon=representation_kmeans[[i]][,'lon'], 
									pch=16, cex=1, 
									col=brewer.pal(i, 'Paired')[as.numeric(representation_kmeans[[i]]$cluster)])
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
