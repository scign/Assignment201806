# Assignment - Practical Machine Learning
## Detecting whether exercises are being performed correctly

## Background
Using devices such as Jawbone Up, Nike FuelBand, and Fitbit it is now possible to collect a large amount of data about personal activity relatively inexpensively. These type of devices are part of the quantified self movement – a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. One thing that people regularly do is quantify how much of a particular activity they do, but they rarely quantify how well they do it. In this project, your goal will be to use data from accelerometers on the belt, forearm, arm, and dumbell of 6 participants. They were asked to perform barbell lifts correctly and incorrectly in 5 different ways. More information is available from the website here: http://web.archive.org/web/20161224072740/http:/groupware.les.inf.puc-rio.br/har (see the section on the Weight Lifting Exercise Dataset).

## Reading in the data
The training data for this project are available here:
[https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv](https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv)
The test data are available here:
[https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv](https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv)
The data for this project come from this source: [http://web.archive.org/web/20161224072740/http:/groupware.les.inf.puc-rio.br/har](http://web.archive.org/web/20161224072740/http:/groupware.les.inf.puc-rio.br/har)

```{r}
library(caret)
mtrain.csv <- read.csv("pml-training.csv",stringsAsFactors=FALSE)
validation <- read.csv("pml-testing.csv",stringsAsFactors=FALSE)
dim(mtrain.csv)
dim(validation)
```
We have 19,622 observations in our training data and 20 observations to classify.

## Preprocessing
The data includes rows where summary variables have been calculated. We're just going to use the raw data so we don't need the summary calculations.

The summary rows are identified by a "yes" in the "new_window" column.

We also need to eliminate the first 7 columns which contain
- index variable
- username
- time index columns
- window index columns

We also need to make our classification into a factor variable.
```{r}
mtrain <- mtrain.csv[mtrain.csv$new_window=="no",]
mtrain <- mtrain[,8:160]
mtrain$classe <- factor(mtrain$classe)
```
It turns out we also have a bunch of missing values. Let's put them all to zero.
```{r}
mtrain <- replace(mtrain,is.na(mtrain),0)
```
Now that we have tidied our data a little bit we can see that there are several variables that don't help at all in the modelling. We can identify them easily with the nearZeroVar function and strip them out.
```{r}
nzv <- nearZeroVar(mtrain,saveMetrics=T)
mtrain.nzv <- mtrain[,!nzv$zeroVar]
```

## Modelling
The authors of the paper that this data came from found that a random forest model gave them good results. We'll use the same algorithm.

Since we don't have a fast machine we're only going to use 20% of the data to train the model, but we'll use 2-fold cross-validation.
```{r}
set.seed(1234)
inTrain <- createDataPartition(mtrain$classe,p=0.2,list=FALSE)
train <- mtrain.nzv[inTrain,]
test <- mtrain.nzv[-inTrain,]
tc <- trainControl(method="cv",number=2,verboseIter=T)
set.seed(1235)
m.rf <- train(classe~., data=train,method="rf",preProcess="pca",trainControl=tc,tuneLength=1)
```
Let's see how we did
```{r}
confusionMatrix(test$classe,predict(m.rf,test))
```
Not bad!

Before we get our final values we need to perform the same operations on our test data
```{r}
# replace NA with zero
validation <- replace(validation,is.na(validation),0)
# strip out zero variance features
v.nzv <- nearZeroVar(validation,saveMetrics=T)
validation.nzv <- validation[,!v.nzv$zeroVar]
# only keep sensor measurements
validation.predict <- validation.nzv[,7:58]
```
Finally we can run the model on our test data to classify
```{r}
predict(m.rf,validation.predict)
```
