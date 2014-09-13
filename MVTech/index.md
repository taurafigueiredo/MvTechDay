---
title       : Machine Learning com R
subtitle    : Human Activity Recognition
author      : Taurã Figueiredo
job         : MV Tech Day
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
5. Predicting

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

A variável que queremos "descobrir" é a 'classe'.   
Ela pode ser A, B, C, D, E.

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

* Treinar o modelo
   
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

--- .class #id

## Construção do modelo de predição

### Algorítimos

* Linear model (lm)
* Generalized linear model (glm)
* Random Forest (rf)
* Boosting (gbm)

--- .class #id

## Construção do modelo de predição

### Treinar o modelo

Existem basicamente duas formas de treinar o modelo:

1. Utilizando a função 'train' do pacote caret
2. Utilizando a função de treinamento do modelo específico

Por exemplo, para treinar nossa base com Random Forest, temos essas duas opções:

```r
library(caret)
fit <- train(classe ~ ., data = trainSet, method = 'rf')
```

```r
library(randomForest)
fit <- randomForest(classe ~ ., data = trainSet)
```

--- .class #id

## Construção do modelo de predição

### Cross-validation

Ok. Modelo construído. Hora de ver se ele funciona.

Para isso, temos a função 'confusionMatrix' do pacote caret.


```r
predictResult <- predict(fit, validationSet[,-53])
confusionMatrix(validationSet[,53], predictResult)
```

--- .class #id .small-size11

## Construção do modelo de predição


```
## Confusion Matrix and Statistics
## 
##           Reference
## Prediction    A    B    C    D    E
##          A 1673    1    0    0    0
##          B    8 1128    3    0    0
##          C    0    8 1014    4    0
##          D    0    0    8  956    0
##          E    0    0    0    1 1081
## 
## Overall Statistics
##                                         
##                Accuracy : 0.994         
##                  95% CI : (0.992, 0.996)
##     No Information Rate : 0.286         
##     P-Value [Acc > NIR] : <2e-16        
##                                         
##                   Kappa : 0.993         
##  Mcnemar's Test P-Value : NA            
## 
## Statistics by Class:
## 
##                      Class: A Class: B Class: C Class: D Class: E
## Sensitivity             0.995    0.992    0.989    0.995    1.000
## Specificity             1.000    0.998    0.998    0.998    1.000
## Pos Pred Value          0.999    0.990    0.988    0.992    0.999
## Neg Pred Value          0.998    0.998    0.998    0.999    1.000
## Prevalence              0.286    0.193    0.174    0.163    0.184
## Detection Rate          0.284    0.192    0.172    0.162    0.184
## Detection Prevalence    0.284    0.194    0.174    0.164    0.184
## Balanced Accuracy       0.998    0.995    0.993    0.997    1.000
```

--- .class #id

## Predicting

Logo no começo, carregamos um dataset de teste (pmlTesting) no mesmo formato que o de treinamento (pmlTraining).

Como premissa, temos que aplicar as mesmas etapas de pré-processamento no dataset de teste:




```r
cleanedTest <- ProcessData(pmlTesting)
```

Em seguida, é só usar a função 'predict' do pacote caret:

```r
predict(fit, cleanedTest)
```

```
##  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 
##  B  A  B  A  A  E  D  B  A  A  B  C  B  A  E  E  A  B  B  B 
## Levels: A B C D E
```

--- .class #id

## Considerações finais

* O que aconteceria se não tivéssemos pré-processado o dataset?

* Apesar do resultado ser praticamente o mesmo, vale avaliar a performance de se utilizar a função 'train' ou a função do algoritmo original.

* Muitas vezes podemos utilizar mais de um algoritmo para melhorar a precisão da predição.


### Dúvidas? :)
http://br.linkedin.com/in/taurafigueiredo   
taurafigueiredo@minhavida.com.br
