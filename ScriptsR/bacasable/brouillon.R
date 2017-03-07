i <- 6

n <- ncol(profil_par_classe[[i]])
xx <- seq_along(names(profil_par_classe[[i]])[-1])
plot(xx,as.data.frame(profil_par_classe[[i]][1,2:n]),type = 'l',col = 1,ylim = c(0,1),ylab = 'proportion de velib dans la classe')
for(j in 2:i){
  lines(xx,as.data.frame(profil_par_classe[[i]][j,2:n]),type = 'l',col = j,ylim = c(0,1))  
}

grid.draw(profil_de_classe_6_cluster)
grid.draw(profil_de_classe_5_cluster)


grid.draw(profil_6_cluster_classe_1)
grid.draw(profil_6_cluster_classe_2)
grid.draw(profil_6_cluster_classe_3)
grid.draw(profil_6_cluster_classe_4)
grid.draw(profil_6_cluster_classe_5)
grid.draw(profil_6_cluster_classe_6)


for(i in 2:10){
  ifelse(is.null(dev.list()),1,dev.off())
  PlotOnStaticMap(carte, lat=representation_kmeans2[[i]][,'lat'], lon=representation_kmeans2[[i]][,'lon'], pch=16, cex=1, col=brewer.pal(max(i,3), 'Spectral')[as.numeric(representation_kmeans2[[i]]$cluster)])
  grid.echo()
  eval(parse(text = paste('map_plot_',i,'_cluster2 <- grid.grab()',sep = '')))
  dev.off()
}
grid.draw(map_plot_6_cluster2)
grid.draw(map_plot_6_cluster)

plot(1:6,col = brewer.pal(max(6), 'Spectral'))

ifelse(is.null(dev.list()),1,dev.off())
plot(x = 1:12,y = sapply(1:12,function(.x) cluster2[[.x]]$betweenss/cluster2[[.x]]$totss), type = 'l',ylim = c(0,1),ylab = 'part de la variance expliqué par les groupes', xlab = 'nombre de classe')
abline(h=(0:5)/5)
abline(v=(0:6)*2)
grid.echo()
Quantite_inter_sur_totale <- grid.grab()
dev.off()

moyenne_par_classe2 <- function(x){
  liste_variable_temps2 <- names(representation_kmeans2[[x]])[which(substr(names(representation_kmeans2[[x]]),1,1)=='T')]
  liste_moyenne2 <- paste('M', substring(liste_variable_temps2,2)," = mean(",liste_variable_temps2,")",sep = '') %>%
    paste(collapse = ', ')
  chaine_dplyr2 <-  paste('representation_kmeans2[[',x,']] %>% group_by(cluster) %>% summarise(',liste_moyenne2,')',sep = '')
  res <- eval(parse(text =chaine_dplyr2))
  return(res)
}

profil_par_classe2 <- lapply(1:12,moyenne_par_classe2)
i <- 6 
ifelse(is.null(dev.list()),1,dev.off())
n <- ncol(profil_par_classe2[[i]])
xx <- seq_along(names(profil_par_classe2[[i]])[-1])
plot(xx,as.data.frame(profil_par_classe2[[i]][1,2:n]),type = 'l',col = 1,ylim = c(0,1),ylab = 'proportion de velib dans la classe')
for(j in 2:i){
  lines(xx,as.data.frame(profil_par_classe2[[i]][j,2:n]),type = 'l',col = j,ylim = c(0,1))  
}
grid.echo()
eval(parse(text = paste('profil_de_classe2_',i,'_cluster <- grid.grab()',sep = '')))

df_test <- data.frame(cluster = cluster2[[6]]$cluster, nom_jour = stations_x_data_mod_j$identifiant_jour,
                      number = as.numeric(substr(stations_x_data_mod_j$identifiant_jour,5,15)),
                      jour = substr(stations_x_data_mod_j$identifiant_jour,1,3)) %>%
  arrange(number) %>%
  merge(y = stations[,c('number','lat','lon')], by="number", all.x=T)

df_test_melt <- melt(data = df_test,id.vars = c('number','jour'), measure.vars = 'cluster')
df_test_semaine <- dcast(data = df_test_melt,formula = number~jour,fun.aggregate = mean)
df_test_semaine <- df_test_semaine %>% mutate(nombre_classe_semaine = rank(c(J_0,J_1,J_2,J_3,J_4,J_5,J_6)))


PlotOnStaticMap(carte, 
                lat=df_test[,'lat'], lon=df_test[,'lon'], pch=16, cex=1, col=brewer.pal(i, 'Spectral')[as.numeric(df_test$cluster)])
