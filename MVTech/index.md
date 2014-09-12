---
title       : Machine Learning com R
subtitle    : Human Activity Recognition | MV Tech Day
author      : Taurã Figueiredo
job         : Coordenador de CRM
framework   : io2012        # {io2012, html5slides, shower, dzslides, ...}
highlighter : highlight.js  # {highlight.js, prettify, highlight}
hitheme     : tomorrow      # 
widgets     : []            # {mathjax, quiz, bootstrap}
mode        : selfcontained # {standalone, draft}
knit        : slidify::knit2slides
---

## Introdução

"Using devices such as Jawbone Up, Nike FuelBand, and Fitbit it is now possible to collect a large amount of data about personal activity relatively inexpensively. These type of devices are part of the quantified self movement – a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. One thing that people regularly do is quantify how much of a particular activity they do, but they rarely quantify how well they do it. In this project, your goal will be to use data from accelerometers on the belt, forearm, arm, and dumbell of 6 participants. They were asked to perform barbell lifts correctly and incorrectly in 5 different ways."   

Projeto original: http://groupware.les.inf.puc-rio.br/har

--- .class #id

## Agenda

1. Coletando dados
2. Análise exploratória dos dados
3. Transformação de dados
4. Construção de modelo de predição

--- .class #id 

## Coletando dados

Os datasets utilizados para essa apresentação foram disponibilizados como parte do projeto do curso Practical Machine Learning da Johns Hopkins Bloomberg School of Public Health, disponibilizado no Coursera.   

Training dataset: https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv   
Testing dataset: https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv

--- .class #id 

## Coletando dados

### Baixar os datasets e carregar no R


```r
download.file(url = "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv", 
              destfile = "pml-training.csv")
download.file(url = "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv", 
              destfile = "pml-testing.csv")
```


```r
pmlTraining <- read.csv("pml-training.csv")
pmlTesting <- read.csv("pml-testing.csv")
```

--- .class #id

## Análise exploratória dos dados

A primeira coisa que precisamos fazer é entender quais dados temos para trabalhar, quais os tipos de dados e como estão distribuídos.   

Vamos começar dando uma olhada geral no que foi carregado:   


```r
dim(pmlTraining)
```

```
## [1] 19622   160
```

```r
dim(pmlTesting)
```

```
## [1]  20 160
```

A função 'dim' nos mostra a quantidade de itens em cada dimensão de um data frame ou matriz, sendo o primeiro resultado a quantidade de linhas e o segundo a quantidade de colunas.

--- .class #id

## Análise exploratória dos dados

Vimos que temos 160 variáveis (ie. colunas) que podemos utilizar na construção do modelo preditivo.   

Mas quais delas são úteis?  

Vamos dar uma olhada nesse dataset:


```r
View(pmlTraining)
```

Outras funções interessantes para explorar os dados que foram carregados:

1. summary
2. head / tail

--- .class #id

## Transformação de dados

Agora que entendemos como os dados estão estruturados, já podemos começar a trabalhar neles.

### Qualidade dos dados

* Existem muitas variáveis sem dados (vazio ou NA)
* Não faz sentido considerar as variáveis que não são números

Precisamos criar um dataset "limpo", sem essas variáveis.   
Se uma variável tiver mais de 50% de registros inválidos, excluímos ela.


--- .class #id


## Transformação de dados

### Pré-processamento dos dados

* Toda e qualquer etapa de pré-processamento que for aplicada no dataset de treinamento do modelo de predição DEVERÁ ser aplicada nos datasets de predição.

* Existem processos automáticos com objetivo de entender a correlação das variáveis e reduzir seu número automaticamente, como PCA e Bagging.

* Você pode construir sua própria função de pré-processamento.

--- .class #id .small-size

## Transformação de dados


```r
ProcessData <- function (dataframe){
  col <- vector(mode = "numeric")
  for(i in 1:ncol(dataframe))
  {
    if(colnames(dataframe)[i]=='classe'){next;}

    total <- length(dataframe[,i]);

    NAs <- length(dataframe[is.na(dataframe[,i]),i]);
    empty <- length(dataframe[dataframe[,i]=='',i])
    notNumber <- length(dataframe[!is.numeric(dataframe[,i]),i]);

    if(round(NAs/total)==1 | round(empty/total)==1 | round(notNumber/total)==1){
      col <- rbind(col, i)
    }
  }

  process <- dataframe[,as.numeric(col)*(-1)]
  for(i in 1:(ncol(process)-1)){ process[,i] <- as.numeric(process[,i]); }
  process[,c(1,2,3,4)*(-1)]
}
```

--- .class #id

## Transformação de dados

### Pré-processar o dataset de treinamento do modelo


```r
cleanedData <- ProcessData(pmlTraining)
```

Vamos ver como ele ficou:

```r
dim(cleanedData)
```

```
## [1] 19622    53
```

Reduzimos o número de variáveis de 160 para 53!   
Ou seja, 107 variáveis atrapalhariam a construção do modelo de predição.

--- .class #id

## Construção do modelo de predição

* Pacote Caret   
   
* Particionamento dos dados (train set + validation set)   
   
* Algorítimos   
   
* Cross-validation   

--- .class #id

## Construção do modelo de predição

### Pacote Caret

Pacote em R que simplifica o processo de Machine Learning.

Principais funções:
* createDataPartition
* train
* predict
* confusionMatrix

--- .class #id .small-size

## Construção do modelo de predição

### Particionamento dos dados (train set + validation set)   

Antes de montar o modelo de predição, temos que separar uma parte do dataset de treinamento, onde sabemos a resposta do que deverá ser retornado, para validar o modelo.

Para isso, utilizamos a função "createDataPartition" do pacote caret.


```r
library(caret)

inTrain <- createDataPartition(y = cleanedData$classe, p = 0.7, list = FALSE)
trainSet <- cleanedData[inTrain,]
validationSet <- cleanedData[-inTrain,]

dim(trainSet); dim(validationSet)
```

```
## [1] 13737    53
```

```
## [1] 5885   53
```




