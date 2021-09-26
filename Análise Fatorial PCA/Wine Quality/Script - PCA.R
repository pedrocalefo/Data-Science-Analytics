###########################################
#
#Nome: Pedro Calefo Richena
#Data:26/09/2021
#Análise:Clustering
#Dados:Red Wine Quality
#
###########################################


pacotes <- c("tidyverse","knitr","kableExtra","cluster","factoextra","gridExtra",
             "PerformanceAnalytics","reshape2","rayshader","psych","pracma",
             "polynom","rqPen","ggrepel",'scatterplot3d','plotly','dplyr')

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T) 
} else {
  sapply(pacotes, require, character = T) 
}

#----- Carregando e padronizando dados -------#

wine_quality <- read.csv('winequality-red.csv')


wine_quality_scale <- scale(wine_quality)

summary(wine_quality_scale)

#----- Fazendo análise fatorial (PCA) -------#

rho <- cor(wine_quality_scale)

cortest.bartlett(R = rho)

rho %>% 
  melt() %>% 
  ggplot() +
  geom_tile(aes(x = Var1, y = Var2, fill = value)) +
  geom_text(aes(x = Var1, y = Var2, label = round(x = value, digits = 3)),
            size = 4) +
  labs(x = NULL,
       y = NULL,
       fill = "Correlações") +
  scale_fill_gradient2(low = "dodgerblue4", 
                       mid = "white", 
                       high = "brown4",
                       midpoint = 0) +
  theme(panel.background = element_rect("white"),
        panel.grid = element_line("grey95"),
        panel.border = element_rect(NA),
        legend.position = "bottom",
        axis.text.x = element_text(angle = 0))


afpc <- prcomp(wine_quality_scale)
summary(afpc)

#Vizualizando o % de variancia capturada por cada eigenvector.

data.frame(afpc$rotation) %>%
  mutate(var = names(wine_quality)) %>% 
  melt(id.vars = "var") %>%
  mutate(var = factor(var)) %>%
  ggplot(aes(x = var, y = value, fill = var)) +
  geom_bar(stat = "identity", color = "black") +
  facet_wrap(~variable) +
  labs(x = NULL, y = NULL, fill = "Legenda:") +
  scale_fill_viridis_d() +
  theme_bw()

ggplotly(
  fviz_eig(X = afpc,
           ggtheme = theme_bw(), 
           barcolor = "black", 
           barfill = "dodgerblue4",
           linecolor = "darkgoldenrod3")
)

k <- sum((afpc$sdev ^ 2) > 1) #Selecionando qtde de fatores pelos criterios de Kaiser. Raiz latente

cargas_fatoriais <- afpc$rotation[, 1:k] %*% diag(afpc$sdev[1:k])

data.frame(cargas_fatoriais) %>%
  rename(F1 = X1,
         F2 = X2,
         F3 = X3,
         F4 = X4) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = T, 
                font_size = 12)

data.frame(rowSums(cargas_fatoriais ^ 2)) %>%
  rename(comunalidades = 1) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = T, 
                font_size = 12)

#Verificando o % das comunalidades obtidas com a utilização dos fatores.
data.frame(cargas_fatoriais) %>%
  rename(F1 = X1,
         F2 = X2,
         F3 = X3,
         F4 = X4) %>%
  mutate(Comunalidades = rowSums(cargas_fatoriais ^ 2)) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = T, 
                font_size = 12)

scores_fatoriais <- t(afpc$rotation)/afpc$sdev 
colnames(scores_fatoriais) <- colnames(wine_quality_scale)

fatores <- list()
for(i in 1:4){
  fatores[[i]] <- rowSums(x = sweep(x = wine_quality, 
                                    MARGIN = 2, 
                                    STATS = scores_fatoriais[i,], 
                                    FUN = `*`))
}

fatores_df <- data.frame((sapply(X = fatores, FUN = c)))
fatores_df

colnames(fatores_df) <- c('F1','F2','F3','F4')



#----- Fazendo análise de cluster (K-means) -------#




#Verificando quantidade de grupos pelo método Elbow

fviz_nbclust(fatores_df, FUN = hcut, method = "wss")

cluster.k4 <- kmeans(fatores_df, centers = 4)

grupo_wine_kmeans4 <- data.frame(cluster.k4$cluster)

wine_quality_cluster <- cbind(fatores_df, grupo_wine_kmeans4)
view(wine_quality_cluster)

#visualizando em cores os clusters


p<-plot_ly(fatores_df, x=~F1, y=~F2, 
             z=~F3,color=~wine_quality_cluster$cluster.k4.cluster) %>%
  add_markers(size=1.5) %>% 
  layout(title = "Red Wine Quality - Cluster")
print(p)

