---
  title: "Análise dos dados de Consolidação Final"
author: 
  - name: "Fellipe Gomes"
affliliation: Universidade Federal Fluminense
date: "25 de outubro de 2017"
output: 
  html_document:
  toc: true
toc_float: true
geometry: margin=lin


---
  
  ```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)

#Pacotes utilizados:
library(dplyr)
library(R2OpenBUGS)
library(ggplot2)
library(GGally)
library(coda)
library(lattice)
library(rpart)
library(rpart.plot)
library(gridExtra)
library(ROCR)
library(tabplot)
library(knitr)
library(kableExtra)
library(lubridate)
library(RSNNS)
library(caret)
library(FSelector)   #Metodos para selecionar os atributos
library(RSNNS)
library(nnet)
library(devtools)
library(scales)
library(reshape)
library(rattle)

#Dados que serão utilizados:
base=readRDS("modelo.rds")
basefator=readRDS("modelofator.rds")
data=readRDS("suicidio.rds")
base=base[,-c(4,5,6,12,13,16)]
dados=na.omit(base)

glimpse(base)
glimpse(dados)
glimpse(basefator)
basefator$condenado=as.factor(basefator$condenado)
basefator$condenado=relevel(basefator$condenado, "Sim")
```

## Introdução

Como havia uma riqueza de informação, este relatório tem como finalidade servir de sugestão algumass etapas.

Inicialmente optou-se por havaliar e modelar a probabilidade de um indivíduo ser condenado. Como o bando de dados conta com três tipos de condenação, primeiramente observou-se a sua distribuição, vejamos:
  
  ```{r, echo=F}
Condenado=c(na.omit(c(data$condenado_art33,data$condenado_art35,data$condenado_art37)))
Artigo=c(rep("33", length(na.omit(data$condenado_art33))),rep("35", length(na.omit(data$condenado_art35))),rep("37", length(na.omit(data$condenado_art37))))
c=data.frame(cbind(Condenado,Artigo))
levels(c$Condenado)=c("Nao", "Sim")

ggplot(data=c,aes( x=Condenado, fill=Artigo))+geom_bar(position="dodge")
```

Em que cada artigo significa:
  
  * *Art.33*:  Importar, exportar, remeter, preparar, produzir, fabricar, adquirir, vender, expor à venda, oferecer, ter em depósito, transportar, trazer consigo, guardar, prescrever, ministrar, entregar a consumo ou fornecer drogas, ainda que gratuitamente, sem autorização ou em desacordo com determinação legal ou regulamentar:.

* *Art.35*: Associarem-se duas ou mais pessoas para o fim de praticar, reiteradamente ou não, qualquer dos crimes previstos nos arts. 33, caput e § 1o, e 34.

* *Art.35*: Colaborar, como informante, com grupo, organização ou associação destinados à prática de qualquer dos crimes previstos nos arts. 33, caput e § 1o, e 34 desta Lei.

Como o maior número de condenações foi pela inflação do artigo 33, segue uma sugestão:
  
  * Primeiramente propoe-se o estudo de alguns modelos que podem ser utilizados para explicar o comportamento e fazer predições sobre fatores que influenciam para a condenação de indivíduos que inflingiram os artigos 33.

* Em seguinda utilizar um dos modelos que podem ser utilizados para explicar o comportamento e/ou fazer predições sobre fatores que influenciam para a condenação de indivíduos que inflingiram os artigos 35 após avaliar a abordagem que pode seguir o estudo.

* E por fim, elaborar o último modelo que sera utilizados para explicar o comportamento e/ou fazer predições sobre fatores que influenciam para a condenação de indivíduos que inflingiram os artigos 37 após avaliar a abordagem que pode seguir o estudo.

Como a maioria das pessoas foi internada por infligir o artigo 33,este relatório conta com variáveis específicamente selecionadas para tentar explicar o comportamento de indivíduos que tiveram tal desfecho.

Portanto, para estimar a probabilidade do i-ésimo indivídio que compôe essa amostra ser condenado como:
  
  > $Y_i =$ 1 se foi condenado ; 0 se não foi condenado


## Dados para avaliar a probabilidade de um indivíduo ser condenado pelo artigo 33

Após longa análise exploratória e estudo do banco de dados, foram selecionadas as seguintes variáveis:
  
  ## Banco de dados:
  
  ```{r,echo=F}
kable(basefator, "html")%>%
  kable_styling()%>%
  scroll_box(width = "1000px", height = "500px")
```


### Distribuição do bando de dados:

A seguir confere-se a distribuição do bando de dados sendo a condenação pelo artigo 33:
  
  ```{r, echo=F}
tableplot(na.omit(basefator), sortCol = condenado)
```

Pode-se observar inicialmente que o número de homens que compôe a amostra é elevado. A maioria dos casos de condenação não possuía reincidente, nem testemunha de defesa ou acusação. Houveram diversas ocorrências em flagrante e algumas ocorrencias haviam muita quantidade.

A maior parte da amostra não confessou e a não foi aplicada a súmula 70.

Por fim podemos notar que a maioria das pessoas que compoe a amostra selecionada para estudar o comportamento do desfecho, não foram condenada pelo artigo 33.

### Observando as correlações:

Vejamos como se esses dados estão correlacionados:
  
  ```{r,echo=F}
g1=ggcorr(dados[,],method = c("pairwise", "spearman"), nbreaks = 9)
g2=ggcorr(dados[,],method = c("pairwise", "spearman"), nbreaks = 12)
gridExtra::grid.arrange(g1,g2,ncol=2)
```

Podemos observar que nenhuma das variáveis apresentou um número  superior a 0,30 ou inferior a -0,30 para correlação de Spearman, o que indica que são fracamente correlacionadas.

### Gráfico de Dispersão e modelo logístico do desfecho para cada variavel explicativa

Além disso, podemos visualizar os gráficos de dispersão com ajustes de regressões logísticas de cada variável para explicar a probabilidade de ser condenado, veja:
  
  ```{r, echo=F}
#Funcao de correlacoes
my_fn <- function(data, mapping, method="glm", ...){
  p <- ggplot(data = data, mapping = mapping) + 
    geom_point() + 
    geom_smooth(method=method, ...)
  p
}
ggpairs(dados[,-c(7:12)], lower = list(continuous = my_fn))
ggpairs(dados[,-c(1:6)], lower = list(continuous = my_fn))

```

Essas variáveis podem ser vistas de forma mais detalhada na análise descritiva a seguir.

## Estatística descritiva

A seguir o resumo dos dados da tabela:
  
  ```{r,echo=F}
na.omit(basefator[,-2])%>%
  summary()%>%
  t()%>%
  kable( "html")%>%
  kable_styling()

```

De foram visual:
  
  ```{r,echo=F, warning=F}


g1=ggplot() + geom_bar(aes(x = juiz, fill = juiz),position = 'dodge', data = na.omit(basefator), stat="count")

g2=ggplot() + geom_bar(aes(x = sexo, fill = sexo),position = 'dodge', data = na.omit(basefator), stat="count")

g3=ggplot() + geom_bar(aes(x = quantidade_de_reus, fill = quantidade_de_reus),position = 'dodge', data = na.omit(basefator), stat="count")

g4=ggplot() + geom_bar(aes(x = reincidente, fill = reincidente),position = 'dodge', data = na.omit(basefator), stat="count")

g5=ggplot() + geom_bar(aes(x = favela, fill = favela),position = 'dodge', data = na.omit(basefator), stat="count")

g6=ggplot() + geom_bar(aes(x = testemunha_def, fill = testemunha_def),position = 'dodge', data = na.omit(basefator), stat="count")

g7=ggplot() + geom_bar(aes(x = testemunha_acus, fill = testemunha_acus),position = 'dodge', data = na.omit(basefator), stat="count")

g8=ggplot() + geom_bar(aes(x = flagrante, fill = flagrante),position = 'dodge', data = na.omit(basefator), stat="count")

g9=ggplot() + geom_bar(aes(x = muita_quantidade, fill = muita_quantidade),position = 'dodge', data = na.omit(basefator), stat="count")

g10=ggplot() + geom_bar(aes(x = condicao_pessoal, fill = condicao_pessoal),position = 'dodge', data = na.omit(basefator), stat="count")

g11=ggplot() + geom_bar(aes(x = sumula, fill = sumula),position = 'dodge', data = na.omit(basefator), stat="count")

g12=ggplot() + geom_bar(aes(x = confissao, fill = confissao),position = 'dodge', data = na.omit(basefator), stat="count")

gridExtra::grid.arrange(g1,g2,g3,g4,ncol=2)
gridExtra::grid.arrange(g5,g6,g7,g8,ncol=2)
gridExtra::grid.arrange(g9,g10,g11,g12,ncol=2)
```


### Para os que foram condenados:

```{r, echo=F}
a=by(basefator[,-2],basefator$condenado, summary)

a$Sim%>%
  t()%>%
  kable( "html")%>%
  kable_styling()
```

### Para os que nao foram condenados:

```{r, echo=F}
a=by(basefator[,-2],basefator$condenado, summary)

a$Nao%>%
  t()%>%
  kable( "html")%>%
  kable_styling()
```

Visualmente:
  
  ```{r,echo=F, warning=F}


g1=ggplot() + geom_bar(aes(x = juiz, fill = condenado),position = 'dodge', data = na.omit(basefator), stat="count")

g2=ggplot() + geom_bar(aes(x = sexo, fill = condenado),position = 'dodge', data = na.omit(basefator), stat="count")

g3=ggplot() + geom_bar(aes(x = quantidade_de_reus, fill = condenado),position = 'dodge', data = na.omit(basefator), stat="count")

g4=ggplot() + geom_bar(aes(x = reincidente, fill = condenado),position = 'dodge', data = na.omit(basefator), stat="count")

g5=ggplot() + geom_bar(aes(x = favela, fill = condenado),position = 'dodge', data = na.omit(basefator), stat="count")

g6=ggplot() + geom_bar(aes(x = testemunha_def, fill = condenado),position = 'dodge', data = na.omit(basefator), stat="count")

g7=ggplot() + geom_bar(aes(x = testemunha_acus, fill = condenado),position = 'dodge', data = na.omit(basefator), stat="count")

g8=ggplot() + geom_bar(aes(x = flagrante, fill = condenado),position = 'dodge', data = na.omit(basefator), stat="count")

g9=ggplot() + geom_bar(aes(x = muita_quantidade, fill = condenado),position = 'dodge', data = na.omit(basefator), stat="count")

g10=ggplot() + geom_bar(aes(x = condicao_pessoal, fill = condenado),position = 'dodge', data = na.omit(basefator), stat="count")

g11=ggplot() + geom_bar(aes(x = sumula, fill = condenado),position = 'dodge', data = na.omit(basefator), stat="count")

g12=ggplot() + geom_bar(aes(x = confissao, fill = condenado),position = 'dodge', data = na.omit(basefator), stat="count")

gridExtra::grid.arrange(g1,g2,g3,g4,ncol=2)
gridExtra::grid.arrange(g5,g6,g7,g8,ncol=2)
gridExtra::grid.arrange(g9,g10,g11,g12,ncol=2)

```

## Modelos propostos

A seguir serão propostos diversos modelos e diversos tipos de abordagem do problema e no final a sugestão do modelo que melhor avalie os dados apresentados quanto ao desfecho da primeira etapa do estudo.

### Modelo de Regressão linear logístico:

Primeiramente foi ajustado um modelo logístico para estimar a probabilidade do indivíduo: ser condenado por inflingir o artigo 33 (sim=1; não=0).

Inicialmente o modelo será proposto com as 12 variáveis do banco de dados. Após selecionar o modelo pelo critério de seleção do menor AIC será checado a capacidade do modelo de perdizer o desfecho e seus coeficientes serão interperetados.

```{r,echo=F}
set.seed(25-10-2017)

#TEste
#Criando indices para treino e teste
amostra=sample(1:2, dim(basefator)[1], replace=T, prob=c(0.9,1-0.9))
train=basefator[amostra==1,]
test=basefator[amostra==2,]
modelo = glm(data=na.omit(train), formula = condenado ~ . , family = binomial("logit"))

```

#### Treino 

Foram separados 90% da amostra para elaborar o modelo e 10 % da amostra ficaram guardados para conferir os valores da matriz de confusão ao final do ajuste.

```{r,echo=F, warning=F}
library(MASS)
step = stepAIC(modelo, direction = "both")
#Modelo selecionado:
summary(step)
```

O modelo selecionado segundo o critério de seleção AIC é composto pelas variáveis "quantidade_de_reus", "sexo", "favela", "testemunha_acus", "flagrante", "muita_quantidade" e "confissao". Todas eçlas apresentaram p-valor menor do que 0.01 para o teste de Wald, o que implica que todas elas estão significativamente relacionadas com a chance de ser condenado por inflingir o artigo 33.

#### Teste

Com os 10% dos valores aleatóriamente selecionados para não ser utilizados na confecção do modelo foram utilizados para fazer as predições e podermos avaliar a matriz de confusão e suas medidas de ajuste.

```{r,echo=F}
predRL = predict(step, newdata = na.omit(test)%>%dplyr::select(c("quantidade_de_reus", "sexo", "favela", "testemunha_acus", "flagrante", "muita_quantidade", "confissao")), type = "response")
a=ifelse(na.omit(test)%>%dplyr::select(condenado)=="Sim",1,0)
b=ifelse(predRL>0.5,1,0)
confusionMatrix(a,b)
```

O modelo teve um ajuste que proporcionou uma acurácia ruim. Houveram muitos falsos positivos e isto pode ser extremamente preocupante se levarmos em conta que 78% da amostra não foi condenado e este modelo condenaria.

Antes de interpretar este modelo, vejamos como se comportaram outros algorítmos de aprendizado de maquina para resolver este problema.


### Arvores de Decisões (sem cross validation)

A **Árvore de desições** é uma das técnicas mais populares de mineração de dados, mais comumente utilizada para resolver a tarefa de classificação, consiste em uma coleção de nós internos e nós folhas, organizados em um modelo hierárico.

A construção desde modelo, ou seja, a construção da árvore, é realizada por meio de um algorítmo que iterativamente analisa os atributos descritivos de um conjunto de dados previamente rotulado, contituindo o processo de aprendizado do modelo classificador.

#### Treino

Dado isso, om os 63,2% dos valores foram aleatóriamente selecionados para ser utilizados na confecção do modelo.

O critério de seleção de atributos será o Índice de Gini, que é um critério baseado em impureza para analisar as diferenças entre as distribuições de probabilidade dos valores dos atributos de classes (rótulos).

No contexto de classificação, o conceito de **inmpureza** está relacionado com a variabilidade de classes presentes em uma partição do conjunto de dados. Quanto mais classes diferentes existirem na partição, mais impura ela será. Como só possuímos 2 classificações (sim=1; não=0), esse critério foi o escolhido. 

```{r, echo=F}
set.seed(22-10-2017)
amostra=sample(1:2, 2997, replace=T, prob=c(0.632,1-0.632))
train=basefator[amostra==1,]
test=basefator[amostra==2,]

modelo_ad = rpart(data=as.data.frame(train), formula = condenado ~ . , method="class",control = rpart.control(minsplit=1),parms = list(split="gini"))
plot = rpart.plot(modelo_ad, 2, 3)

```

Esta ilustração pode ser facilmente interpretada.

#### Teste

```{r, echo=F}
predictions = predict(modelo_ad, test[,-13])
a=ifelse(test%>%dplyr::select(condenado)=="Sim",1,0)
b=ifelse(predictions[,1]>0.5,1,0)
confusionMatrix(a,b)
```

Ao observar a sua matriz de confusão, podemos observar que obteve melhor acurácia quando comparado ao modelo de regressão logística.

Porém este modelo apresentou elevado número de falsos negativos. O que pode ser muito ruim se levarmos em conta o elevado número de criminosos que não seriam condenados.


### Arvore de decisoes (com cross validation)

Uma outra abordagem para a validação deste modelo é através do cross-validarion.

Nesta abordagem, todos os exemplares farão aprte, em algum momento, do conjutno de dados usado no teste do modelo preditivo.

Para implementar essa situação, o conjunto de dados seá dividido em K subgrupos disjutos, com alocação aleatória de exemplares para cada  subconjunto.


```{r, echo=F}
#k - fold cress validation
trainControl = trainControl(method="cv", number=10, classProbs = T)

#Treina uma arvore de decisao
tree = train(condenado ~. , data = na.omit(basefator), method="rpart", trControl=trainControl)

#O modelo sera:
rattle::fancyRpartPlot(tree$finalModel)

```

Este modelo também pode ser facilmente interpretável.


```{r, echo=F}
#Verifica o resultado da validacao cruzada
print(tree)
```

Para verificar as medidas de avaliação, podemos notar que o modelo final selecionado teve acurácia bem próxima do modelo anterior.

### Redes neurais:

Este modelo está aqui a atítulo de curiosidade.

Também foi implementada uma tentativa com escolhas arbitrárias de parâmetros para a implementação de um modelo de redes neurais e os resultados podem ser conferidos a seguir:
  
  #### Treino:
  
  ```{r, echo=F}

#Começando a organizar os dados:
base=as.matrix(dados)
X= base[,-13]
Y=decodeClassLabels(base[,13])

#Conjuntos de treinamentos devem ser criados:
d_separado=splitForTrainingAndTest(X,Y,ratio=0.1)

#Comando para normalizar os dados:
d_normalizado=normTrainingAndTestSet(d_separado, dontNormTargets = TRUE, type="0_1")

#Funcao de Treinamento;
#Obs.: quantidade de camadas escondidas e o numero de neuronios foram escolhas arbitrarias
modelo_mlp= mlp(d_separado$inputsTrain,
                d_separado$targetsTrain,
                size=c(3),
                maxit=1000,
                initFunc = "Randomize_Weights",
                #learnFunc = "Std_Backpropagation",
                learnFuncParams = c(0.2),
                hiddenActFunc = "Act_Logistic",
                shufflePatterns = T,
                linOut = T,
                inputsTest = d_separado$inputsTest,
                targetsTest = d_separado$targetsTest);modelo_mlp

#Plotagem para curva de aprendizado:
plot(modelo_mlp$IterativeFitError, type="n", main="Curva de Aprendizagem", xlab="Iteração", ylab="Erro médio quadrado", cex.lab=1.5)
lines(modelo_mlp$IterativeFitError,col="1", lwd=3, cex=2) #Erro quadrado medio a cada iteração para o conjunto de treinamento
lines(modelo_mlp$IterativeTestError, col="2", lwd=3, cex=2) ##Erro quadrado medio a cada iteração para o conjunto de teste
legend("top", c("Treinamento", "Teste"), lty=c(1,1), col=c(1,2)) #Clicar para legenda aparecer

#importando a funcao do Github
library(devtools)
source_url('https://gist.githubusercontent.com/fawda123/7471137/raw/466c1474d0a505ff044412703516c34f1a4684a5/nnet_plot_update.r')

plot.nnet(modelo_mlp)


```

#### Teste

```{r, echo=F}

predictions = predict(modelo_mlp,d_separado$inputsTest)[,1]
a=d_separado$targetsTest[,1]
b=ifelse(predictions>0.5,1,0);length(b)
confusionMatrix(a,b)
```

Teve um comportamento rasoável.

Este modelo está aqui apenas para ilustração, não será implementado.

### Modelo Logístico Bayesiano

Por fim, a última abordagem será bayesiana. Aqui ainda teremos o mesmo desfecho porém teremos que abordar o problema de maneira diferente.

Primeiro temos que escrever o modelo na linguagem BUGS. Nele vamos declarar a verossimilhança como um modelo generalizado da família binomial e com uma função de ligação logística.

Teremos de estimar os coeficientes B0+Bi, i =1,...,12  nesse caso, e como priori, optaremos (apesar das controvérsias na estatística bayesiana) simplesmente partir do pressuposto que não existe relação nenhuma, então vamos começar achando que B0=0 e todos Bi=0, não existe relação nem intercepto a priori.

```{r, eval=F, echo=F, warning=F}
library(dplyr)
library(R2OpenBUGS)
library(ggplot2)
library(GGally)
library(coda)
library(lattice)
library(rpart)
library(rpart.plot)

#Testando modelos
getwd()
#Conjunto de dados:
base=readRDS("modelo.rds")
#saveRDS(basefator, "modelo.rds")

dados=na.omit(base)
glimpse(dados)
dados=dados[,-c(4,5,6,12,13,16)]




```

#### O modelo no R em linguagem Bugs

```{r,eval=F}
### Analise Bayesiana usando Openbugs
# Escrevendo o modelo de regressão linear
sink("linreg.txt")
cat("
    model {
    
    # Priori
    
    B0 ~ dnorm(0,0.001)
    B1 ~ dnorm(0,0.001)
    B2 ~ dnorm(0,0.001)
    B3 ~ dnorm(0,0.001)
    B4 ~ dnorm(0,0.001)
    B5 ~ dnorm(0,0.001)
    B6 ~ dnorm(0,0.001)
    B7 ~ dnorm(0,0.001)
    B8 ~ dnorm(0,0.001)
    B9 ~ dnorm(0,0.001)
    B10 ~ dnorm(0,0.001)
    B11 ~ dnorm(0,0.001)
    B12 ~ dnorm(0,0.001)
    
    # Verossimilhança
    for (i in 1:n) {
    y[i] ~ dbern(p[i])
    logit(p[i]) <- B0+B1*x1[i]+B2*x2[i]+B3*x3[i]+B4*x4[i]+B5*x5[i]+B6*x6[i]+B7*x7[i]+B8*x8[i]+B9*x9[i]+B10*x10[i]+B11*x11[i]+B12*x12[i]
    }
    
    }
    ",fill=TRUE)
sink()


#Conjunto de dados:
glimpse(dados)

# Junte os dados em uma lista
win.data <- list(x1=dados$juiz, x2=dados$quantidade_de_reus, x3=dados$sexo, x4=dados$favela, x5=dados$reincidente, x6=dados$testemunha_def, x7=dados$testemunha_acus, x8=dados$flagrante, x9=dados$muita_quantidade, x10=dados$condicao_pessoal, x11=dados$confissao, x12=dados$sumula,y=dados$condenado, n=length(dados$condenado))

# Função de inicialização
inits <- function(){ list(B0=rnorm(1), B1=rnorm(1),B2=rnorm(1),B3=rnorm(1), B4=rnorm(1),B5=rnorm(1),B6=rnorm(1), B7=rnorm(1),B8=rnorm(1),B9=rnorm(1),B10=rnorm(1), B11=rnorm(1),B12=rnorm(1) )}

# Escolha os parametros que quer estimar
params <- c("B0","B1","B2","B3","B4","B5","B6","B7","B8","B9","B10","B11","B12" )

```

Após ter definido todas as variáveis para inciar o modelo, foram estabelecidas as seguintes características para o MCMC:
  
  * Serão realizadas 3 cadeias
* Cada cadeia tem tamanho 1500
* Serão descartadas 500 simulação de cada cadeia

Portanto:
  
  ```{r,eval=F}
# Caracteristicas do MCMC
nc = 3 #Numero de cadeias
ni=3000 #Tamanho da cadeira
nb=1000 #Numero de simulação que serão descartadas
#nt=10 #Thinning rate

```

#### Amostrador

Iniciando o amostrador obtemos o seguinte resultado:
  
  ```{r, eval=F}
# Inicie o Amostrador

modelo1.bayes <-bugs(data = win.data,
                     inits = inits,
                     parameters =params,
                     model = "linreg.txt",
                     #n.thin = nt,
                     n.chains = nc,
                     n.burnin = nb,
                     n.iter = ni,
                     debug=F)
print(modelo1.bayes)
plot(modelo1.bayes)

```

#### Resultados:

![](C:/Users/Fellipe/Dropbox/Estatistica Aplicada II/Dados criminais consultoria individual/Apresentacao/print_modelo.png)

Como podemos observar, alguns coeficientes apresentaram valores bem próximos do zero se considerarmos um nível de confiança de 95%.

Esta afirmação pode ser conferida visualmente através da seguinte representação:
  
  ![Estimativas do modelo](C:/Users/Fellipe/Dropbox/Estatistica Aplicada II/Dados criminais consultoria individual/Apresentacao/1.png)

#### Interpretação


Executando esses comandos teremos acesso a mais informações deste modelo:
  
  ```{r, eval=F}
coda.bayes <-bugs(data = win.data, inits = inits, parameters = params,
                  model = "linreg.txt",
                  #n.thin = nt,
                  #n.chains = nc,
                  n.burnin = nb,
                  n.iter = ni,debug=F,codaPkg = T
)

out.coda <- read.bugs(coda.bayes)

```

#### Convergência

```{r,eval=F}
library(coda)
xyplot(out.coda)
```

![Convergência](C:/Users/Fellipe/Dropbox/Estatistica Aplicada II/Dados criminais consultoria individual/Apresentacao/2.png)

Aqui podemos observar que houve convergência das estimativas.

#### Densidades das estimativas

```{r,eval=F}
densityplot(out.coda)
```

![Densidades](C:/Users/Fellipe/Dropbox/Estatistica Aplicada II/Dados criminais consultoria individual/Apresentacao/3.png)

Com esta ilustração podemos constar as distribuições de cada um dos parâmetros estimados.

#### Auto correlacao

```{r,eval=F}
acfplot(out.coda)
```

![Autocorrelações](C:/Users/Fellipe/Dropbox/Estatistica Aplicada II/Dados criminais consultoria individual/Apresentacao/4.png)

Através autocorrelações podemos ver que o comportamento da variável sexo e do intercepto que não foi como o desejável

#### Razoes de chances e interpretação:

```{r, eval=F}
out.summary <- summary(out.coda,q=c(0.025, 0.975))
out.summary$stat[1:10,"Mean", drop=FALSE]
out.summary$q[1:10, ]

#Razoes de chance 
OR1=exp(out.summary$stat[1:10,"Mean", drop=FALSE])
ICbeta1 <- out.summary$q[1:10, ]
ICOR1=exp(ICbeta1)
round((cbind(OR1, ICOR1)),4)
glimpse(dados)
```

![Razão de chances](C:/Users/Fellipe/Dropbox/Estatistica Aplicada II/Dados criminais consultoria individual/Apresentacao/or.png)


Como podemos observar:
  
  * Indivíduos julgados pelo juiz 1 tem uma chance de ser condenado por inflingir o artigo 33 73,15% menor do que os indivíduos que não foram condenados por inflingir o artigo 33.
* A chance do indivíduo ser condenado por inflingir o artigo 33 diminui em 96,77% ao aumentar em 1 ano a idade.
* Indivíduos do sexo masculino tem uma chance de ser condenado por inflingir o artigo 33 41,91% menor do que os indivíduos que não foram condenados por inflingir o artigo 33.
* Indivíduos da favela tem uma chance de ser condenado por inflingir o artigo 33 77,25% menor do que os indivíduos que não foram condenados por inflingir o artigo 33.
* Indivíduos reincidentes tem uma chance de ser condenado por inflingir o artigo 33 24,6% maior do que os indivíduos que não foram condenados por inflingir o artigo 33.
* Indivíduos com testemunha de defesa tem uma chance de ser condenado por inflingir o artigo 33 40,79% menor do que os indivíduos que não foram condenados por inflingir o artigo 33.
* Indivíduos com testemunha de acusação tem uma chance de ser condenado por inflingir o artigo 33 42,66% maior do que os indivíduos que não foram condenados por inflingir o artigo 33.
* Indivíduos pegos em flagranteo tem uma chance de ser condenado por inflingir o artigo 33 17,93% maior do que os indivíduos que não foram condenados por inflingir o artigo 33.
* Indivíduos com muita quantidade tem uma chance de ser condenado por inflingir o artigo 33 20,93% maior do que os indivíduos que não foram condenados por inflingir o artigo 33.
* Indivíduos tiveram condição pessoal levada em conta tem uma chance de ser condenado por inflingir o artigo 33 3 vezes maior do que os indivíduos que não foram condenados por inflingir o artigo 33.
* Indivíduos que confessaram tem uma chance de ser condenado por inflingir o artigo 33 2,04% maior do que os indivíduos que não foram condenados por inflingir o artigo 33.
* Indivíduos que tiveram a súmula 70 aplicada tem uma chance de ser condenado por inflingir o artigo 33 79,26% maior do que os indivíduos que não foram condenados por inflingir o artigo 33.

## Conclusão

Dado os resultados semelhantes em cada um dos modelos, sugere-se que utilize os resultados da interptretação do modelo logistimo bayesiano e apreveitar o poder de predição do modelo de árvore de decisões sem o cross validation.

Aguardo contato caso tenha interesse em proceguir com as próximas etapas para avaliar o comportamento da amostra segundo a condenação por inflingir o artigo 35 e 37.