#Pacotes que serao utilizados:
require(tm)       #Pacote de para text mining
require(caret)    #Pacote para matriz de confusao
require(wordcloud)#Pacote para nuvem de palavras
require(readxl)   #Pacote para leitura de dados excel
require(e1071)    #PAcote para rodar o modelo naive bayes

#Leitura da base de dados:
dados <- read_excel("monitoramento_redes_sociais_classificacao.xls")  #Necessario estar no diretorio do banco de dados
names(dados) #Conferindo variaveis presentes no banco de dados

#Variaveis que serao utilizadas:
x=dados[,c("Reclamação")]#Explicativa
y=dados[,c("Classe")]# Resposta

#Criando o corpus para o tratamento das variaveis com pacote library(tm): 
corpus <- Corpus(DataframeSource(x))

#Para remmover as stopwords:
corpus = tm_map(corpus, removeWords, stopwords("portuguese"))

#Para remover excessos de espaços em branco:
corpus = tm_map(corpus, stripWhitespace)

#Para remover pontuacao:
corpus = tm_map(corpus, removePunctuation)

#Para remover numeros:
corpus = tm_map(corpus, removeNumbers)

#Para colocar todas as letras como minusculas:
corpus = tm_map(corpus, content_transformer(tolower))

#Criando a matrix de termos:
corpus_tf=TermDocumentMatrix(corpus, control = list(minWordLength=1,minDocFreq=1))

# #Transformando em matrix, permitindo a manipulacao:
# matrix=as.matrix(corpus_tf)

#Caso tenha interesse em normalizar os dados:
corpus_tf_idf = weightTfIdf(corpus_tf,normalize = T)

#Transformando em matrix, permitindo a manipulacao:
matrix=as.matrix(corpus_tf_idf)
  
#Criando a base de dados:

base=data.frame(y,t(matrix)) #Primeira coluna com a classe, a partir da segunda coluna são as variaveis
base[,1]=as.factor(base[,1]) #A classificacao deve ser um fator


#Criando o modelo: ------------------------------------------------------

#Separando em dados de treino e de teste:
set.seed(43)
amostra=sample(1:2, 10, replace=T, prob=c(0.6,1-0.6))
train=base[amostra==1,]
test=base[amostra==2,]

#Criando o modelo:
modelo=naiveBayes(train[,-1] ,train[,1])

#Fazendo uma predicao:
previsao=predict(modelo, test[,-1], type="class");previsao
resposta=test[,1]

#Funcao matriz de confusao do pacote caret
caret::confusionMatrix(previsao,resposta)


# AED ---------------------------------------------------------------------

wordcloud(corpus,
          max.words = 100,   #numero maximo de palavras
          random.order = F,   #organiza de forma aleatoria
          colors = rainbow(8), #definindo cores
          rot.per=0.5,         #para desenhar as palavras 50% em cada eixo (horizontal e vertical)
          use.r.layout=T #dar uma variada melhor no layout
)


#Existem dois modos como os dados serao apresentados:
# TermDocumentMatrix DocumentTermMatrix

#Criando a matrix de termos:
corpus_tf=TermDocumentMatrix(corpus, control = list(minWordLength=1,minDocFreq=1))

#PAra ficar mais facil manipular os dados:
matriz = as.matrix(corpus_tf)

#organizar os dados de forma decrescente
matriz = sort(rowSums(matriz), decreasing=T)

#criando um data.frame para a matriz
matriz = data.frame(word=names(matriz), freq = matriz)

#Vejamos os primeiros 100 registros:
head(matriz, n=100)

#Vejamos visualmente:
barplot(head(matriz[,2]), names.arg = head(matriz[,1]))
