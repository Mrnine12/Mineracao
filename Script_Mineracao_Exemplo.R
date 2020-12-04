library(dplyr)
library(ggplot2)

## Base

ENEM_BA = read.csv("BA_ENEM_2019.csv", sep = ";")

colSums(is.na(ENEM_BA))
str(ENEM_BA)

## NA's NOTAS

ENEM_BA = ENEM_BA[complete.cases(ENEM_BA$NU_NOTA_CH),]
ENEM_BA = ENEM_BA[complete.cases(ENEM_BA$NU_NOTA_MT),]

## Descritiva

library(geobr)

ggplot(ENEM_BA, aes(x = NU_IDADE)) + 
  geom_histogram(aes(y = ..density..), colour = "black", fill = "white", binwidth = 2) + 
  labs(x = "Idade", y = "Densidade") + theme_bw()

ggplot(ENEM_BA) + aes(x = factor(TP_COR_RACA)) + 
  geom_bar(fill = "white", width = 0.5, color = "black") + theme_bw() +
  scale_x_discrete("Cor/Raça", labels = c("Não Declarado", "Branca", "Preta", "Parda",
                                          "Amarela", "Indígena")) +
  scale_y_continuous(name = "Inscritos (em milhares)", labels = c(0,50,100,150))

prop.table(table(ENEM_BA$TP_SEXO))
prop.table(table(ENEM_BA$TP_COR_RACA))
prop.table(table(ENEM_BA$TP_ANO_CONCLUIU))
prop.table(table(ENEM_BA$TP_ESCOLA))
prop.table(table(ENEM_BA$IN_TREINEIRO))
prop.table(table(ENEM_BA$Q006))

df_MEAN = ENEM_BA %>% group_by(CO_MUNICIPIO_RESIDENCIA) %>% 
  summarise(CN = mean(NU_NOTA_CN), CH = mean(NU_NOTA_CN), MT = mean(NU_NOTA_MT),
            LC = mean(NU_NOTA_LC), REDACAO = mean(NU_NOTA_REDACAO)) %>% data.frame()

df_MEAN$MEAN = rowMeans(df_MEAN[,2:6])

muni = read_municipality(code_muni = "BA", year = 2019)

muni = left_join(muni, df_MEAN, by = c("code_muni" = "CO_MUNICIPIO_RESIDENCIA"))

ggplot(muni) + geom_sf(aes(fill = MEAN), color = "black", size = .15) +
  scale_fill_distiller("", type = "seq", palette = "Reds", direction = 1) + 
  theme(panel.background = element_blank(), axis.ticks = element_blank(),
        panel.grid.major = element_line(color = "transparent"), axis.text = element_blank())

## K-Means

library(amap)
library(GGally)

# Agrupamento por municipios

df_group = ENEM_BA %>% group_by(CO_MUNICIPIO_RESIDENCIA) %>% 
  summarise(CN = mean(NU_NOTA_CN), CH = mean(NU_NOTA_CN), MT = mean(NU_NOTA_MT),
            LC = mean(NU_NOTA_LC), REDACAO = mean(NU_NOTA_REDACAO)) %>% data.frame()

# Busca pelo melhor numero de grupos

V = NULL

for(i in 2:10){
  K = i
  KMEANS = Kmeans(df_group[,2:6], K, nstart = 25, iter.max = 100)
  #  cat("running", i, ".. \n")
  V[i] = sum(KMEANS$size*KMEANS$withinss)
}

plot(1:10, V, type = 'b', ylab = "W(C)", xlab = "Clusters")

# Modelo Final

KMEANS = Kmeans(df_group[,2:6], 4, nstart = 25, method = "euclidean", iter.max = 100)

df_group$grupo = as.factor(KMEANS$cluster)

ggpairs(df_group, columns = 2:6, ggplot2::aes(colour = grupo))

## Agrupamentos Hierarquico

df_group_2 = ENEM_BA %>% group_by(CO_MUNICIPIO_RESIDENCIA) %>% 
  summarise(CN = mean(NU_NOTA_CN), CH = mean(NU_NOTA_CN), MT = mean(NU_NOTA_MT),
            LC = mean(NU_NOTA_LC), REDACAO = mean(NU_NOTA_REDACAO)) %>% data.frame()

# Distancia

dd = dist(df_group_2[,2:6], method = "euclidean")

# Selecao de metodo de linkage

metodos = c("single", "complete", "average")
correlacao = NULL

for(i in 1:3){
  cluster = hclust(dd, method = metodos[i])
  coph = cophenetic(cluster)
  correlacao[i] = cor(dd, coph)
}

data.frame(Ligacao = c("Simples", "Completa", "Media"), Correlacao = correlacao)

# Numero de clusters

library(dendextend)

den_k = find_k(as.dendrogram(hclust(dd, method = "complete")))
plot(den_k)

# Modelo Final

library(factoextra)

dendograma = hclust(dd, method = "complete")

fviz_dend(dendograma, cex = 0.5, k = 4, color_labels_by_k = FALSE, rect = TRUE, main = "", show_labels = F)

df_group_2$grupos = as.factor(cutree(dendograma, k = 4))

ggpairs(df_group_2, columns = 2:6, ggplot2::aes(colour = grupos))

## Arvores de Decisão

library(rpart)
library(rpart.plot)

# Regressao

dt_data = data.frame(ENEM_BA[,3:9], RENDA = ENEM_BA$Q006,
                     MEDIA = rowMeans(ENEM_BA[,10:14]))

modelo = rpart(MEDIA ~ ., data = dt_data)

plotcp(modelo)

indice = sample(1:nrow(dt_data), size = nrow(dt_data)*0.9)
treino = dt_data[indice,]
teste = dt_data[-indice,]

modelo = rpart(MEDIA ~ ., data = treino, control = rpart.control(cp = 0.005))
predicoes = predict(modelo, newdata = teste)

metricas = function(predito, real){
  
  erro = predito - real
  
  RMSE = function(error){
    sqrt(mean(error^2))
  } # Raiz do erro quadrático médio
  
  MAE = function(error){
    mean(abs(error))
  } # Erro Absoluto Médio
  
  MAPE = function(real, predito){
    mean(abs((real - predito)/real))
  } # Erro Percentual Absoluto Médio
  
  array(c(RMSE(erro), MAE(erro), MAPE(real, predito)), dim = c(1,3),
        dimnames = list(NULL, c("RMSE","MAE","MAPE")))
  
} # Medidas para modelos de regressao

metricas(predito = predicoes, real = teste$MEDIA)

rpart.plot(modelo, box.palette = "RdBu", shadow.col = "gray", nn = TRUE)

# Classificacao

dt_data_2 = data.frame(ENEM_BA[,3:9], RENDA = ENEM_BA$Q006,
                     MEDIA = rowMeans(ENEM_BA[,10:14]))

dt_data_2[dt_data_2$MEDIA < 500,]$CATEG = "Baixa"
dt_data_2[dt_data_2$MEDIA >= 500 & dt_data_2$MEDIA <= 700,]$CATEG = "Media"
dt_data_2[dt_data_2$MEDIA > 700,]$CATEG = "Alta"

dt_data_2 = dt_data_2[,-9]

indice = sample(1:nrow(dt_data_2), size = nrow(dt_data_2)*0.9)
treino = dt_data_2[indice,]
teste = dt_data_2[-indice,]

modelo = rpart(factor(CATEG) ~ ., data = treino, control = rpart.control(cp = 0.005))
predicoes = predict(modelo, teste, type = "class")
cm = table(teste$CATEG, predicoes)

sum(diag(cm))/sum(cm)

rpart.plot(modelo, box.palette = "RdBu", shadow.col = "gray", nn = TRUE)
