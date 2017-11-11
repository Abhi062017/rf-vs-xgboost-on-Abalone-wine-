#Comparing results from running a random forest and xgboost on wines dataset, which features muticollinearity
getwd()
#read data : wine
wine <- read.csv('https://archive.ics.uci.edu/ml/machine-learning-databases/wine/wine.data',
                  header = F)

str(wine)
View(wine) #no real names, hence we'd get the real names from UCI:
names(wine) <- c('Class','Alcohol','Malix Acid','Ash','Alcalinity of ash','Magnesium',
                 'Total phenols','Flavanoids','Nonflavanoid phenols','Proanthocyanins',
                 'Color intensity','Hue','OD280/OD315 of diluted wines','Proline')
names(wine)
names(wine)[14] <- 'Proline2' #if a specific variable's name is to be changed
names(wine)[14] <- 'Proline'  #reverting back to it's orginal name

write.csv(wine, file = 'wine.csv', row.names = F) #saving it locally
#**************************************************************************
#Pre-Processing:
sum(is.na(wine)) #no NA's
nrow(wine)
unique(wine$Class)#'Class' has 3 distinct values, hence can be a factor
wine$Class <- as.factor(wine$Class)
str(wine)
nrow(wine[wine$Class==1,])
nrow(wine[wine$Class==2,])
nrow(wine[wine$Class==3,])
summary(wine) #this spits out almost all the above

#Finding Correlation
library(psych)
pairs.panels(wine[,-1]) #as 'Class' is a factor
cor(wine$'Total phenols', wine$Flavanoids) #0.86, max correlation of all variables
#**************************************************************************

#fitting models:
#1.fitting a linear model
wine$Class <- as.numeric(wine$Class) #factor variable can't be a response for lm
fit <- lm(Class ~ ., data = wine)
summary(fit) #6 variables play imporant role

  #fitting the linear model with important variables
  fit <- lm(Class ~ Alcohol + Flavanoids + Proline + 
            wine$`Alcalinity of ash` + wine$`Color intensity` +
            wine$`OD280/OD315 of diluted wines`,
            data = wine)
  summary(fit)
  plot(fit) #spits out 4 different types of plots
  
  #Creating gvlma object: a function for Global Validation of Linear Models assumptions
  install.packages("gvlma", dependencies = T)
  library(gvlma)
  gvlmodel <- gvlma(fit)
  summary(gvlmodel)

#2.fitting a glm
fit <- glm(Class ~ Alcohol + Flavanoids + Proline + 
            wine$`Alcalinity of ash` + wine$`Color intensity` +
            wine$`OD280/OD315 of diluted wines`,
          data = wine)
summary(fit)

#3. fitting a classification decision tree model (the C50 model)
install.packages("C50") #as this takes a 'classification response variable' unlike lm/glm
library(C50)
wine$Class <- as.factor(wine$Class)
fit_model <- C5.0(Class ~ Alcohol + Flavanoids + Proline + 
                  wine$`Alcalinity of ash` + wine$`Color intensity` +
                  wine$`OD280/OD315 of diluted wines`,
                  data = wine)
summary(fit_model)
#**************************************************************************

#plots:
plot(wine$`Total phenols`, wine$Flavanoids, col = c('blue','green')) #max corr
#notice an outlier in the above plot (one of the 'Flavanoids')
max(wine$`Total phenols`)
max(wine$Flavanoids)
library(ggplot2) #quantile-quantile plot
qqplot(wine$`Total phenols`, wine$Flavanoids,
       col = c('blue','green'),
       xlab = 'Total Phenols',
       ylab = 'Flavanoids',
       main = 'QQplot')
