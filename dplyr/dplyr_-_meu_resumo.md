dplyr
================
Fellipe Gomes
8 de outubro de 2017

Conteúdo:
=========

-   Seleção de linhas e colunas;
-   Criação e transformação de variáveis;
-   Formatos de variáveis e conversões (data, numérico, texto, etc.);
-   Ordenamento;
-   Merge básico;
-   Estatísticas agregadas;
-   Subsetting com comandos tradicionais do R.

------------------------------------------------------------------------

Leitura da base de dados
========================

Inicialmente vamos fazer a importação da base suicidios.

``` r
base=read.csv("suicidios.csv", header=T, sep=";")
head(base)
```

    ##   suicidio_paf Microrregiao idade id_legal trab_armado sexo raca
    ## 1            0        11001    26        1           0    2    1
    ## 2            0        11001    34        1           0    1    4
    ## 3            0        11001    49        1           0    1    4
    ## 4            1        11001    34        1           0    1    1
    ## 5            0        11001    20        1           0    1    4
    ## 6            0        11001    30        1           0    1    1
    ##   estado_civil escolaridade
    ## 1            1            6
    ## 2            1            3
    ## 3            1            2
    ## 4            6            6
    ## 5            1            6
    ## 6            2            3

Tratando as variáveis qualitativas:

``` r
#Transformando a variável suicidio por arma de fogo em um factor
base$suicidio_paf = factor(base$suicidio_paf, labels=c("Não","Sim"))

#Transformando a variável Microrregiao em um factor
base$Microrregiao = factor(base$Microrregiao)

#Transformando a variável Escolaridade em um ordered
base$escolaridade = ordered(base$escolaridade,levels=c(1,2,3,4,5,6), labels=c("Analfabeto","1 a 3","4 a 7","8 a 11", "12 ou mais","Desconhecido"))

#Transformando a variável estado civil em um factor
base$estado_civil = factor(base$estado_civil, labels=c("Solteiro","União estavel","Casado","Viuvo", "Divorciado","Desconhecido"))

#Transformando a variável raca em um factor
base$raca = factor(base$raca, labels=c("Branco","Negro","Amarelo","Pardo", "Indigena","Desconhecido"))

#Transformando a variável sexo em um factor
base$sexo = factor(base$sexo, labels=c("Masculino","Feminino"))

#Transformando a variável id_legal em um factor
base$id_legal = factor(base$id_legal, labels=c("Não","Sim"))

#Transformando a variável trab_armado em um factor
base$trab_armado = factor(base$trab_armado, labels=c("Não","Sim"))
```

Vejamos um resumo dos dados:

``` r
summary(base)
```

    ##  suicidio_paf  Microrregiao       idade        id_legal    trab_armado
    ##  Não:72017    35061  : 5511   Min.   : 20.00   Não: 7405   Não:73165  
    ##  Sim:11971    33018  : 2619   1st Qu.: 29.00   Sim:76583   Sim:10823  
    ##               43026  : 2507   Median : 40.00                          
    ##               31030  : 2058   Mean   : 42.28                          
    ##               23016  : 1808   3rd Qu.: 52.00                          
    ##               41037  : 1364   Max.   :103.00                          
    ##               (Other):68121                                           
    ##         sexo                 raca              estado_civil  
    ##  Masculino:67379   Branco      :47084   Solteiro     :38037  
    ##  Feminino :16609   Negro       : 4475   União estavel:30025  
    ##                    Amarelo     :  396   Casado       : 3936  
    ##                    Pardo       :26821   Viuvo        : 4971  
    ##                    Indigena    :  438   Divorciado   : 1220  
    ##                    Desconhecido: 4774   Desconhecido : 5799  
    ##                                                              
    ##        escolaridade  
    ##  Analfabeto  : 4988  
    ##  1 a 3       :13596  
    ##  4 a 7       :17149  
    ##  8 a 11      :10124  
    ##  12 ou mais  : 5289  
    ##  Desconhecido:32842  
    ## 

Pacote dplyr
============

O pacote dplyr é um dos pacotes mais poderosos e populares do R, desenvolvido por Hadley Wickham.

O dplyr é um poderoso pacote R para manipular, limpar e resumir dados não estruturados. Em suma, faz a exploração de dados e manipulação de dados de forma fácil e rápida no R.

``` r
# Carregando o pacote dplyr
library(dplyr)
```

    ## Warning: package 'dplyr' was built under R version 3.4.2

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

Selecionando n linhas aleatorias: função sample\_n(.)
-----------------------------------------------------

``` r
# Selecionando 3 linhas aleatoriamente
sample_n(base,3)
```

    ##       suicidio_paf Microrregiao idade id_legal trab_armado      sexo
    ## 37480          Não        35003    51      Sim         Não Masculino
    ## 36376          Não        33018    41      Sim         Não Masculino
    ## 39017          Não        35014    28      Sim         Não Masculino
    ##         raca  estado_civil escolaridade
    ## 37480 Branco  Desconhecido Desconhecido
    ## 36376  Pardo União estavel        1 a 3
    ## 39017 Branco      Solteiro       8 a 11

Removendo linhas duplicadas: função distinct(.)
-----------------------------------------------

### Baseado em todas as variáveis

``` r
#Calculando a dimensao da base
dim(base)
```

    ## [1] 83988     9

``` r
# excluindo linhas iguais
base2=distinct(base)
dim(base2)
```

    ## [1] 71923     9

### Baseado em uma variável

``` r
# excluindo linhas que possuem Microrregiao igual
base3=distinct(base,Microrregiao)
dim(base3)
```

    ## [1] 557   1

### Baseado em mais de uma variável

``` r
# excluindo linhas que possuem idade e escolatridade iguais
base4=distinct(base,idade, escolaridade)
dim(base4)
```

    ## [1] 468   2

Selecionando colunas (variáveis): função select(.)
--------------------------------------------------

``` r
# Selecionando a variavel idade e  todas as variáveis de trabalho armado até raca na base
base5=select(base,idade, trab_armado:raca)
head(base5)
```

    ##   idade trab_armado      sexo   raca
    ## 1    26         Não  Feminino Branco
    ## 2    34         Não Masculino  Pardo
    ## 3    49         Não Masculino  Pardo
    ## 4    34         Não Masculino Branco
    ## 5    20         Não Masculino  Pardo
    ## 6    30         Não Masculino Branco

``` r
# Selecionando todas as variaveis com exceção de idade e idade legal
base6=select(base,-c(idade,id_legal))
head(base6)
```

    ##   suicidio_paf Microrregiao trab_armado      sexo   raca  estado_civil
    ## 1          Não        11001         Não  Feminino Branco      Solteiro
    ## 2          Não        11001         Não Masculino  Pardo      Solteiro
    ## 3          Não        11001         Não Masculino  Pardo      Solteiro
    ## 4          Sim        11001         Não Masculino Branco  Desconhecido
    ## 5          Não        11001         Não Masculino  Pardo      Solteiro
    ## 6          Não        11001         Não Masculino Branco União estavel
    ##   escolaridade
    ## 1 Desconhecido
    ## 2        4 a 7
    ## 3        1 a 3
    ## 4 Desconhecido
    ## 5 Desconhecido
    ## 6        4 a 7

``` r
# Selecionando todas as variaveis cujo nome inicia com e
base7=select(base,starts_with("e")) #também ends_with() e contains().
head(base7)
```

    ##    estado_civil escolaridade
    ## 1      Solteiro Desconhecido
    ## 2      Solteiro        4 a 7
    ## 3      Solteiro        1 a 3
    ## 4  Desconhecido Desconhecido
    ## 5      Solteiro Desconhecido
    ## 6 União estavel        4 a 7

#### Podem ser úteis também ends\_with() e contains().

Reordenando as as colunas das variáveis: função select(.)
---------------------------------------------------------

``` r
# reorganiza o data frame, iniciando com a variável Microrregiao e depois as demais
base8=select(base,Microrregiao,everything())
head(base8)
```

    ##   Microrregiao suicidio_paf idade id_legal trab_armado      sexo   raca
    ## 1        11001          Não    26      Sim         Não  Feminino Branco
    ## 2        11001          Não    34      Sim         Não Masculino  Pardo
    ## 3        11001          Não    49      Sim         Não Masculino  Pardo
    ## 4        11001          Sim    34      Sim         Não Masculino Branco
    ## 5        11001          Não    20      Sim         Não Masculino  Pardo
    ## 6        11001          Não    30      Sim         Não Masculino Branco
    ##    estado_civil escolaridade
    ## 1      Solteiro Desconhecido
    ## 2      Solteiro        4 a 7
    ## 3      Solteiro        1 a 3
    ## 4  Desconhecido Desconhecido
    ## 5      Solteiro Desconhecido
    ## 6 União estavel        4 a 7

Renomeando variáveis: função rename(.)
--------------------------------------

``` r
# Renomeando a variável Microrregiao para micro
base9 = rename(base8,Micro = Microrregiao)
head(base9)
```

    ##   Micro suicidio_paf idade id_legal trab_armado      sexo   raca
    ## 1 11001          Não    26      Sim         Não  Feminino Branco
    ## 2 11001          Não    34      Sim         Não Masculino  Pardo
    ## 3 11001          Não    49      Sim         Não Masculino  Pardo
    ## 4 11001          Sim    34      Sim         Não Masculino Branco
    ## 5 11001          Não    20      Sim         Não Masculino  Pardo
    ## 6 11001          Não    30      Sim         Não Masculino Branco
    ##    estado_civil escolaridade
    ## 1      Solteiro Desconhecido
    ## 2      Solteiro        4 a 7
    ## 3      Solteiro        1 a 3
    ## 4  Desconhecido Desconhecido
    ## 5      Solteiro Desconhecido
    ## 6 União estavel        4 a 7

Selecionando um subconjunto de linhas que satisfazem uma ou mais condições: função filter(.)
--------------------------------------------------------------------------------------------

``` r
# Selecionando somente os indivíduos do sexo masculino
base9 = filter(base, sexo == "Masculino")
head(base9)
```

    ##   suicidio_paf Microrregiao idade id_legal trab_armado      sexo   raca
    ## 1          Não        11001    34      Sim         Não Masculino  Pardo
    ## 2          Não        11001    49      Sim         Não Masculino  Pardo
    ## 3          Sim        11001    34      Sim         Não Masculino Branco
    ## 4          Não        11001    20      Sim         Não Masculino  Pardo
    ## 5          Não        11001    30      Sim         Não Masculino Branco
    ## 6          Não        11001    42      Sim         Não Masculino Branco
    ##    estado_civil escolaridade
    ## 1      Solteiro        4 a 7
    ## 2      Solteiro        1 a 3
    ## 3  Desconhecido Desconhecido
    ## 4      Solteiro Desconhecido
    ## 5 União estavel        4 a 7
    ## 6      Solteiro        4 a 7

``` r
# Selecionando somente os indivíduos do sexo masculino e branco
base10 = filter(base, sexo == "Masculino" & raca == "Branco")
head(base10)
```

    ##   suicidio_paf Microrregiao idade id_legal trab_armado      sexo   raca
    ## 1          Sim        11001    34      Sim         Não Masculino Branco
    ## 2          Não        11001    30      Sim         Não Masculino Branco
    ## 3          Não        11001    42      Sim         Não Masculino Branco
    ## 4          Não        11001    54      Sim         Não Masculino Branco
    ## 5          Não        11001    55      Sim         Não Masculino Branco
    ## 6          Sim        11001    33      Sim         Não Masculino Branco
    ##    estado_civil escolaridade
    ## 1  Desconhecido Desconhecido
    ## 2 União estavel        4 a 7
    ## 3      Solteiro        4 a 7
    ## 4         Viuvo        4 a 7
    ## 5      Solteiro Desconhecido
    ## 6      Solteiro        1 a 3

``` r
# Selecionando somente os indivíduos com escolaridade de 1 a 3 anos e 4 a 7 anos
base11 = filter(base, escolaridade %in% c("1 a 3","4 a 7"))
head(base11)
```

    ##   suicidio_paf Microrregiao idade id_legal trab_armado      sexo   raca
    ## 1          Não        11001    34      Sim         Não Masculino  Pardo
    ## 2          Não        11001    49      Sim         Não Masculino  Pardo
    ## 3          Não        11001    30      Sim         Não Masculino Branco
    ## 4          Não        11001    42      Sim         Não Masculino Branco
    ## 5          Não        11001    28      Sim         Não  Feminino  Negro
    ## 6          Não        11001    54      Sim         Não Masculino Branco
    ##    estado_civil escolaridade
    ## 1      Solteiro        4 a 7
    ## 2      Solteiro        1 a 3
    ## 3 União estavel        4 a 7
    ## 4      Solteiro        4 a 7
    ## 5      Solteiro        4 a 7
    ## 6         Viuvo        4 a 7

``` r
# Selecionando indivíduos que ou são homens solteiros ou são mulheres casadas
base12 = filter(base, (estado_civil=="Solteiro" & sexo =="Masculino") | (estado_civil=="Casado" & sexo =="Feminino") )
head(base12)
```

    ##   suicidio_paf Microrregiao idade id_legal trab_armado      sexo   raca
    ## 1          Não        11001    34      Sim         Não Masculino  Pardo
    ## 2          Não        11001    49      Sim         Não Masculino  Pardo
    ## 3          Não        11001    20      Sim         Não Masculino  Pardo
    ## 4          Não        11001    42      Sim         Não Masculino Branco
    ## 5          Não        11001    45      Sim         Não Masculino  Pardo
    ## 6          Não        11001    26      Sim         Não Masculino  Pardo
    ##   estado_civil escolaridade
    ## 1     Solteiro        4 a 7
    ## 2     Solteiro        1 a 3
    ## 3     Solteiro Desconhecido
    ## 4     Solteiro        4 a 7
    ## 5     Solteiro Desconhecido
    ## 6     Solteiro Desconhecido

Ordene seus data frames: função arrange(.)
------------------------------------------

``` r
# Ordenando os dados pela variável idade de forma crescente
base13 = arrange(base, idade )
head(base13)
```

    ##   suicidio_paf Microrregiao idade id_legal trab_armado      sexo
    ## 1          Não        11001    20      Sim         Não Masculino
    ## 2          Não        11001    20      Não         Não  Feminino
    ## 3          Não        11001    20      Não         Não Masculino
    ## 4          Não        11001    20      Sim         Não Masculino
    ## 5          Sim        11001    20      Sim         Não Masculino
    ## 6          Não        11001    20      Não         Não Masculino
    ##           raca estado_civil escolaridade
    ## 1        Pardo     Solteiro Desconhecido
    ## 2        Pardo     Solteiro       8 a 11
    ## 3        Pardo     Solteiro        4 a 7
    ## 4        Negro Desconhecido Desconhecido
    ## 5 Desconhecido Desconhecido Desconhecido
    ## 6        Pardo     Solteiro Desconhecido

``` r
# Ordenando os dados pela variável idade e escolaridade
base14 = arrange(base, idade ,escolaridade)
head(base14)
```

    ##   suicidio_paf Microrregiao idade id_legal trab_armado      sexo
    ## 1          Não        12004    20      Sim         Não Masculino
    ## 2          Não        12005    20      Sim         Não Masculino
    ## 3          Não        13001    20      Não         Não Masculino
    ## 4          Não        14001    20      Não         Não  Feminino
    ## 5          Não        14001    20      Sim         Não Masculino
    ## 6          Não        15007    20      Sim         Não Masculino
    ##           raca  estado_civil escolaridade
    ## 1 Desconhecido      Solteiro   Analfabeto
    ## 2       Branco      Solteiro   Analfabeto
    ## 3     Indigena      Solteiro   Analfabeto
    ## 4     Indigena União estavel   Analfabeto
    ## 5        Pardo      Solteiro   Analfabeto
    ## 6        Pardo      Solteiro   Analfabeto

``` r
# Ordenando os dados pela variável idade de forma decrescente
base15 = arrange(base, desc(idade) )
head(base15)
```

    ##   suicidio_paf Microrregiao idade id_legal trab_armado      sexo   raca
    ## 1          Não        52007   103      Sim         Não Masculino Branco
    ## 2          Não        22014   100      Sim         Não Masculino Branco
    ## 3          Não        33018   100      Sim         Não  Feminino Branco
    ## 4          Não        41011   100      Sim         Não Masculino Branco
    ## 5          Não        15017    99      Sim         Não Masculino  Pardo
    ## 6          Não        25022    99      Sim         Não Masculino  Pardo
    ##   estado_civil escolaridade
    ## 1     Solteiro Desconhecido
    ## 2       Casado   Analfabeto
    ## 3       Casado        1 a 3
    ## 4 Desconhecido Desconhecido
    ## 5       Casado   Analfabeto
    ## 6       Casado Desconhecido

Crie uma nova variável: função mutate(.) e transmute(.)
-------------------------------------------------------

``` r
# Criando a variável Idade ao quadrado
base16 = mutate(base, idade2 = idade**2 )
head(base16)
```

    ##   suicidio_paf Microrregiao idade id_legal trab_armado      sexo   raca
    ## 1          Não        11001    26      Sim         Não  Feminino Branco
    ## 2          Não        11001    34      Sim         Não Masculino  Pardo
    ## 3          Não        11001    49      Sim         Não Masculino  Pardo
    ## 4          Sim        11001    34      Sim         Não Masculino Branco
    ## 5          Não        11001    20      Sim         Não Masculino  Pardo
    ## 6          Não        11001    30      Sim         Não Masculino Branco
    ##    estado_civil escolaridade idade2
    ## 1      Solteiro Desconhecido    676
    ## 2      Solteiro        4 a 7   1156
    ## 3      Solteiro        1 a 3   2401
    ## 4  Desconhecido Desconhecido   1156
    ## 5      Solteiro Desconhecido    400
    ## 6 União estavel        4 a 7    900

``` r
# Criando a variável UF e a variável Grandes regiões
base17 = mutate(base, UF = substring(Microrregiao,1,2), Grandes.regioes = substring(UF,1,1) )
head(base17)
```

    ##   suicidio_paf Microrregiao idade id_legal trab_armado      sexo   raca
    ## 1          Não        11001    26      Sim         Não  Feminino Branco
    ## 2          Não        11001    34      Sim         Não Masculino  Pardo
    ## 3          Não        11001    49      Sim         Não Masculino  Pardo
    ## 4          Sim        11001    34      Sim         Não Masculino Branco
    ## 5          Não        11001    20      Sim         Não Masculino  Pardo
    ## 6          Não        11001    30      Sim         Não Masculino Branco
    ##    estado_civil escolaridade UF Grandes.regioes
    ## 1      Solteiro Desconhecido 11               1
    ## 2      Solteiro        4 a 7 11               1
    ## 3      Solteiro        1 a 3 11               1
    ## 4  Desconhecido Desconhecido 11               1
    ## 5      Solteiro Desconhecido 11               1
    ## 6 União estavel        4 a 7 11               1

``` r
# Se você quiser somente manter as variáveis criadas
base18 = transmute(base,UF = substring(Microrregiao,1,2), Grandes.regioes = substring(UF,1,1) )
head(base18)
```

    ##   UF Grandes.regioes
    ## 1 11               1
    ## 2 11               1
    ## 3 11               1
    ## 4 11               1
    ## 5 11               1
    ## 6 11               1

Resumindo variáveis: função summarize(.)
----------------------------------------

``` r
# Calculando a media e a mediana da variável idade
summarise(base,  media.idade = mean(idade), mediana.idade = median(idade))
```

    ##   media.idade mediana.idade
    ## 1    42.28133            40

Resumindo variáveis por grupo: função group\_by(.) e summarize(.)
-----------------------------------------------------------------

``` r
# Calculando a media da variável idade para as combinações entre sexo, escolaridade e estado civil e a frequencia de indivíduos em cada combinação
grupo = group_by(base, sexo, escolaridade, estado_civil)
resultado=summarise(grupo,  media.idade = mean(idade),frequencia=n())
resultado
```

    ## # A tibble: 72 x 5
    ## # Groups:   sexo, escolaridade [?]
    ##         sexo escolaridade  estado_civil media.idade frequencia
    ##       <fctr>        <ord>        <fctr>       <dbl>      <int>
    ##  1 Masculino   Analfabeto      Solteiro    41.21746       1890
    ##  2 Masculino   Analfabeto União estavel    55.91661       1547
    ##  3 Masculino   Analfabeto        Casado    71.80420        286
    ##  4 Masculino   Analfabeto         Viuvo    54.76404         89
    ##  5 Masculino   Analfabeto    Divorciado    42.89286         84
    ##  6 Masculino   Analfabeto  Desconhecido    46.79832        119
    ##  7 Masculino        1 a 3      Solteiro    34.96684       5277
    ##  8 Masculino        1 a 3 União estavel    52.21151       4430
    ##  9 Masculino        1 a 3        Casado    68.39024        574
    ## 10 Masculino        1 a 3         Viuvo    51.04595        457
    ## # ... with 62 more rows

``` r
# Calculando a media da variável idade para as combinações entre idade legal, escolaridade e estado civil e a frequencia de indivíduos em cada combinação
grupo = group_by(base, id_legal, escolaridade, estado_civil)
resultado=summarise(grupo,  media.idade = mean(idade),frequencia=n())
resultado
```

    ## # A tibble: 70 x 5
    ## # Groups:   id_legal, escolaridade [?]
    ##    id_legal escolaridade  estado_civil media.idade frequencia
    ##      <fctr>        <ord>        <fctr>       <dbl>      <int>
    ##  1      Não   Analfabeto      Solteiro    22.18705        139
    ##  2      Não   Analfabeto União estavel    22.23077         13
    ##  3      Não   Analfabeto         Viuvo    22.00000          2
    ##  4      Não   Analfabeto    Divorciado    21.50000          4
    ##  5      Não   Analfabeto  Desconhecido    23.25000          4
    ##  6      Não        1 a 3      Solteiro    22.02111        758
    ##  7      Não        1 a 3 União estavel    22.30233         43
    ##  8      Não        1 a 3        Casado    22.50000          2
    ##  9      Não        1 a 3         Viuvo    23.00000          1
    ## 10      Não        1 a 3    Divorciado    22.09091         11
    ## # ... with 60 more rows

Operador pipe: %&gt;%
---------------------

Realiza múltiplas ações sem guardar os passos intermediários.

``` r
# Selecionando as variáveis idade e idade legal
base18 = base %>% select(idade,id_legal)
head(base18)
```

    ##   idade id_legal
    ## 1    26      Sim
    ## 2    34      Sim
    ## 3    49      Sim
    ## 4    34      Sim
    ## 5    20      Sim
    ## 6    30      Sim

``` r
base19 = base %>% select(-estado_civil) %>% filter(sexo=="Masculino") %>% group_by(raca,escolaridade) %>% summarise(maximo=max(idade),media=mean(idade))
head(base19 , n=10)
```

    ## # A tibble: 10 x 4
    ## # Groups:   raca [2]
    ##      raca escolaridade maximo    media
    ##    <fctr>        <ord>  <dbl>    <dbl>
    ##  1 Branco   Analfabeto    100 54.51308
    ##  2 Branco        1 a 3     95 48.30461
    ##  3 Branco        4 a 7     95 43.06727
    ##  4 Branco       8 a 11     98 39.19390
    ##  5 Branco   12 ou mais     95 42.32459
    ##  6 Branco Desconhecido    103 44.94032
    ##  7  Negro   Analfabeto     95 46.59059
    ##  8  Negro        1 a 3     87 39.25240
    ##  9  Negro        4 a 7     84 36.65531
    ## 10  Negro       8 a 11     81 34.29712

Aplicando funções em linhas
---------------------------

``` r
base20 = base16 %>%
  rowwise() %>% mutate(Max= max(idade:idade2)) %>%
  select(idade,idade2,Max)
head(base20)
```

    ## # A tibble: 6 x 3
    ##   idade idade2   Max
    ##   <int>  <dbl> <int>
    ## 1    26    676   676
    ## 2    34   1156  1156
    ## 3    49   2401  2401
    ## 4    34   1156  1156
    ## 5    20    400   400
    ## 6    30    900   900

------------------------------------------------------------------------

Leitura dos dados
=================

Vamos fazer a importação da base Populacao por micro sexo e idade (base.pop), da base Municipios\_2010 (base.mun) e da base Num de obitos 2010 (base.obitos).

``` r
base.pop=read.csv("Populacao por micro sexo e idade.txt", header=TRUE, sep="")

library(readxl)
base.mun <- read_excel("~/Estatistica Aplicada II/Dplyr/Municipios_2010.xlsx")

base.obitos <- read_excel("~/Estatistica Aplicada II/Dplyr/Municipios_2010.xlsx")
```

Vejamos o banco de dados:

``` r
#Cabeçalho da base com as informações sobre a população
head(base.pop)
```

    ##   Microrregiao SEXO IDADE.CAT tot.pop
    ## 1        11001    0         0  153264
    ## 2        11001    0         1  110853
    ## 3        11001    1         0  159442
    ## 4        11001    1         1  116761
    ## 5        11002    0         0   20878
    ## 6        11002    0         1   13927

``` r
dim(base.pop)
```

    ## [1] 2232    4

``` r
#Cabeçalho da base com as informações sobre os municípios
head(base.mun)
```

    ## # A tibble: 6 x 8
    ##      UF  Nome_UF Mesorregião         Nome_Meso Microrregiao
    ##   <chr>    <chr>       <dbl>             <chr>        <dbl>
    ## 1    11 Rondônia        1102 Leste Rondoniense        11006
    ## 2    11 Rondônia        1102 Leste Rondoniense        11003
    ## 3    11 Rondônia        1102 Leste Rondoniense        11008
    ## 4    11 Rondônia        1102 Leste Rondoniense        11006
    ## 5    11 Rondônia        1102 Leste Rondoniense        11008
    ## 6    11 Rondônia        1102 Leste Rondoniense        11008
    ## # ... with 3 more variables: Nome_Micro <chr>, codmun <dbl>,
    ## #   Nome_Munic <chr>

``` r
dim(base.mun)
```

    ## [1] 5565    8

``` r
#Cabeçalho da base com as informações sobre os números de óbitos
head(base.obitos)
```

    ## # A tibble: 6 x 8
    ##      UF  Nome_UF Mesorregião         Nome_Meso Microrregiao
    ##   <chr>    <chr>       <dbl>             <chr>        <dbl>
    ## 1    11 Rondônia        1102 Leste Rondoniense        11006
    ## 2    11 Rondônia        1102 Leste Rondoniense        11003
    ## 3    11 Rondônia        1102 Leste Rondoniense        11008
    ## 4    11 Rondônia        1102 Leste Rondoniense        11006
    ## 5    11 Rondônia        1102 Leste Rondoniense        11008
    ## 6    11 Rondônia        1102 Leste Rondoniense        11008
    ## # ... with 3 more variables: Nome_Micro <chr>, codmun <dbl>,
    ## #   Nome_Munic <chr>

``` r
dim(base.obitos)
```

    ## [1] 5565    8

Combinando duas bases de dados
------------------------------

O pacote dplyr possui um conjunto de funções que nos auxiliam a combinar dos data frames do nosso interesse.

``` r
#Criando a base b1
b1 <- data.frame(ID = c(1010, 2010, 3010, 4010, 5010),
                  W = c('a', 'b', 'c', 'd', 'e'),
                  X = c(1, 1, 0, 0, 1),
                  Y=rnorm(5))

b1
```

    ##     ID W X          Y
    ## 1 1010 a 1  0.5547243
    ## 2 2010 b 1 -0.9396278
    ## 3 3010 c 0  1.6046311
    ## 4 4010 d 0  0.9160843
    ## 5 5010 e 1 -1.3944780

``` r
#Criando a base b2

b2 <- data.frame(ID = c(1010, 7010, 3010, 6010, 8010),
                  A = c('z', 'b', 'k', 'd', 'l'),
                  B = c(1, 2, 3, 0, 4),
                  C =rnorm(5))

b2
```

    ##     ID A B          C
    ## 1 1010 z 1 -1.3319855
    ## 2 7010 b 2  1.1171702
    ## 3 3010 k 3 -0.7481325
    ## 4 6010 d 0  0.3039678
    ## 5 8010 l 4  1.0115219

``` r
#Criando a base b3

b3 <- data.frame(Identificacao = c(1010, 5010,2541),
                  Z =rnorm(3),
                  W =c("Rio","São Paulo","Niteroi"))

b3
```

    ##   Identificacao          Z         W
    ## 1          1010  1.4099525       Rio
    ## 2          5010 -0.6073574 São Paulo
    ## 3          2541 -0.4028271   Niteroi

### inner\_join

``` r
# Função inner_join: Combina as duas bases incluindo todas as variáveis de ambas as bases e todas as linhas comuns as duas bases
merge1 = inner_join(b1,b2,by="ID")
merge1
```

    ##     ID W X         Y A B          C
    ## 1 1010 a 1 0.5547243 z 1 -1.3319855
    ## 2 3010 c 0 1.6046311 k 3 -0.7481325

``` r
merge2 = inner_join(b1,b3,by=c("ID"="Identificacao"))
merge2
```

    ##     ID W.x X          Y          Z       W.y
    ## 1 1010   a 1  0.5547243  1.4099525       Rio
    ## 2 5010   e 1 -1.3944780 -0.6073574 São Paulo

### left\_join

``` r
# Função left_join: Combina as duas bases incluindo todas as variáveis de ambas as bases e todas as linhas da base a esquerda
merge3 = left_join(b1,b2,by="ID")
merge3
```

    ##     ID W X          Y    A  B          C
    ## 1 1010 a 1  0.5547243    z  1 -1.3319855
    ## 2 2010 b 1 -0.9396278 <NA> NA         NA
    ## 3 3010 c 0  1.6046311    k  3 -0.7481325
    ## 4 4010 d 0  0.9160843 <NA> NA         NA
    ## 5 5010 e 1 -1.3944780 <NA> NA         NA

### right\_join

``` r
# Função right_join: Combina as duas bases incluindo todas as variáveis de ambas as bases e todas as linhas da base a direita
merge4 = right_join(b1,b2,by="ID")
merge4
```

    ##     ID    W  X         Y A B          C
    ## 1 1010    a  1 0.5547243 z 1 -1.3319855
    ## 2 7010 <NA> NA        NA b 2  1.1171702
    ## 3 3010    c  0 1.6046311 k 3 -0.7481325
    ## 4 6010 <NA> NA        NA d 0  0.3039678
    ## 5 8010 <NA> NA        NA l 4  1.0115219

### full\_join

``` r
# Função full_join: Combina as duas bases incluindo todas as variáveis de ambas as bases e todas as linhas de ambas as bases
merge5 = full_join(b1,b2,by="ID")
merge5
```

    ##     ID    W  X          Y    A  B          C
    ## 1 1010    a  1  0.5547243    z  1 -1.3319855
    ## 2 2010    b  1 -0.9396278 <NA> NA         NA
    ## 3 3010    c  0  1.6046311    k  3 -0.7481325
    ## 4 4010    d  0  0.9160843 <NA> NA         NA
    ## 5 5010    e  1 -1.3944780 <NA> NA         NA
    ## 6 7010 <NA> NA         NA    b  2  1.1171702
    ## 7 6010 <NA> NA         NA    d  0  0.3039678
    ## 8 8010 <NA> NA         NA    l  4  1.0115219

### semi\_join

``` r
# Função semi_join: Combina as duas bases incluindo as variáveis da basea a esquerda e todas as linhas comuns as duas bases
merge6 = semi_join(b1,b2,by="ID")
merge6
```

    ##     ID W X         Y
    ## 1 1010 a 1 0.5547243
    ## 2 3010 c 0 1.6046311

### anti\_join

``` r
# Função anti_join: Combina as duas bases incluindo as variáveis da base a esquerda e todas as linhas que não são comuns as duas bases
merge7 = anti_join(b1,b2,by="ID")
merge7
```

    ##     ID W X          Y
    ## 1 2010 b 1 -0.9396278
    ## 2 4010 d 0  0.9160843
    ## 3 5010 e 1 -1.3944780

Combinando dados verticalmente
------------------------------

``` r
#Criando a base d1
d1 <- data.frame(ID = c(1010, 2010, 3010, 4010, 5010),
                  W = c('a', 'b', 'a', 'b', 'e'),
                  X = c(1, 1, 0, 0, 1),
                  Y=c(3,6,3,5,7))

d1
```

    ##     ID W X Y
    ## 1 1010 a 1 3
    ## 2 2010 b 1 6
    ## 3 3010 a 0 3
    ## 4 4010 b 0 5
    ## 5 5010 e 1 7

``` r
#Criando a base d2

d2 <- data.frame(ID = c(1010, 2010, 5010),
                  W = c('a', 'b', 'e'),
                  X = c(1, 1, 1),
                  Y=c(3,6,7))

d2
```

    ##     ID W X Y
    ## 1 1010 a 1 3
    ## 2 2010 b 1 6
    ## 3 5010 e 1 7

``` r
#Criando a base d3

d3 <- data.frame(ID = c(3210, 2011, 1017),
                  W = c('b', 'e', "a"),
                  X = c(1,0,1),
                  Y=c(3,5,4))

d3
```

    ##     ID W X Y
    ## 1 3210 b 1 3
    ## 2 2011 e 0 5
    ## 3 1017 a 1 4

``` r
#Criando a base d4

d4 <- data.frame(ID = c(3210, 2011, 1017),
                  idade = c(10,20,32),
                  Sexo = c(1,0,1))

d3
```

    ##     ID W X Y
    ## 1 3210 b 1 3
    ## 2 2011 e 0 5
    ## 3 1017 a 1 4

### Juntando por linhas comuns com intersect

``` r
#Criando uma base com as linhas comus as duas bases
intersect(d1,d2)
```

    ##     ID W X Y
    ## 1 1010 a 1 3
    ## 2 2010 b 1 6
    ## 3 5010 e 1 7

### Juntando todas as linhas com union

``` r
#Criando uma base unindo todas as linhas das duas bases
union(d1,d3)
```

    ##     ID W X Y
    ## 1 5010 e 1 7
    ## 2 2011 e 0 5
    ## 3 3010 a 0 3
    ## 4 3210 b 1 3
    ## 5 2010 b 1 6
    ## 6 1017 a 1 4
    ## 7 1010 a 1 3
    ## 8 4010 b 0 5

### Base com linhas distintas nas duas bases com setdiff

``` r
#Criando uma base com as linhas distintas nas duas bases
setdiff(d1,d3)
```

    ##     ID W X Y
    ## 1 1010 a 1 3
    ## 2 2010 b 1 6
    ## 3 3010 a 0 3
    ## 4 4010 b 0 5
    ## 5 5010 e 1 7

### Empilhando duas bases uma sobre a outra com rbind

``` r
#Empilhando duas bases, uma em cima da outra
rbind(d1,d3)
```

    ##     ID W X Y
    ## 1 1010 a 1 3
    ## 2 2010 b 1 6
    ## 3 3010 a 0 3
    ## 4 4010 b 0 5
    ## 5 5010 e 1 7
    ## 6 3210 b 1 3
    ## 7 2011 e 0 5
    ## 8 1017 a 1 4

### Empilhando duas bases lado a lado com cbind

``` r
#Empilhando duas bases, uma ao lado da outra
cbind(d3,d4)
```

    ##     ID W X Y   ID idade Sexo
    ## 1 3210 b 1 3 3210    10    1
    ## 2 2011 e 0 5 2011    20    0
    ## 3 1017 a 1 4 1017    32    1
