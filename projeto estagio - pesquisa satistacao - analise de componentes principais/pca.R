suppressMessages(library(ggplot2)) 
suppressMessages(library(ggfortify))
suppressMessages(library(ggbiplot))

library(readr)
dados <- read_delim("~/Projeto Estagio/pesquisa satisfacao/pesquisa satisfacao/dados_simples2.txt", 
                    ";", escape_double = FALSE, trim_ws = TRUE)

temp=as.data.frame(dados[,-1])

for(j in 1:length(temp[1,])){
  temp[is.na(temp[,j]),j]=mean(temp[,j],na.rm = T)
}

temp=as.data.frame(temp, row.names.data.frame=c(dados[,1]))

acpcor=prcomp(temp, scale = TRUE)

summary(acpcor)

g1=autoplot(acpcor, label = TRUE, label.size = 1,
                   loadings = TRUE, loadings.label = TRUE, loadings.label.size  = 3)
plotly::ggplotly(g1)

source("ggcreeplot.R")
g2=ggscreeplot(acpcor)
plotly::ggplotly(g2)


