#installing and loading the packages
library(car)

install.packages("gplots")
install.packages("dplyr")

library(gplots)

install.packages("moments")
library(moments)

library(dplyr)
library(ggplot2)
library(tidyr)

install.packages("gvlma")
library(gvlma)

install.packages("leaps")
library(leaps)

##taking a glance at the dataset()
head(state.x77, 5) 

#1) Convert the dataset into a dataframe
states <- as.data.frame(state.x77)
states

# correct column names
colnames(states)[4] <- "LifeExp"                   
colnames(states)[6] <- "HSGrad" 

head(states, 5)

# 2)
states_fit1 <- states[,c("Murder","Population","Income","Illiteracy","Frost")]

# 3)
###Scatter plot matrix(1.1)
scatterplotMatrix(states_fit1, 
                  spread= FALSE,
                  smoother.args = list(lty = 2),
                  main = "Scatterplot Matrix for states dataset")

## model 1(1.2)
fit1 <- lm(data=states_fit1,Murder~Population+Income+Illiteracy+ Frost)
summary(fit1)

# Create a correlation matrix(1.3)
correlation <- round(cor(states_fit1),2)
correlation

#heatmap(1.3)

column_names <- colnames(states_fit1)

heatmap.2(correlation, trace = "none", col = colorRampPalette(c("white","steelblue"))(100), scale = "none",
          main = "state.x77 dataset Correlation Heatmap",
          cexRow = 0.8, cexCol = 0.8, margins = c(6, 6),
          symm = TRUE, na.rm = TRUE,
          labRow = column_names, labCol = column_names, cellnote = correlation,
          notecol = "black", notecex = 0.8, key.title = NA)




###interaction

fit2 <- lm(data=states_fit1,Murder~Population+Income+Illiteracy+ Frost, Population:Illiteracy)
summary(fit2)

fitx <- lm(data=states_fit1,Murder~Population+Income+Illiteracy+ I(Illiteracy^2))
summary(fitx)

### multicollinearity(1.4)

vif(fit1)

#create vector of VIF values(1.4)
vif_values <- vif(fit1)
vif_values

#create bar chart to display each VIF value(1.4)
par(mfrow = c(1,1))
barplot(vif_values, main = "VIF Values", horiz = FALSE, col = "steelblue", ylim = c(0, 7))

#add horizontal line at 5(1.4)
abline(h = 5, lwd = 3, lty = 2,col = 'red')

### outliers (1.5)
outlierTest(fit1)

## removing the outliers(1.5)
states_fit1 <-  states_fit1[rownames(states_fit1) != "Nevada", ]
states_fit1

head(states_fit1,40)

fit1 <- lm(data=states_fit1,Murder~Population+Income+Illiteracy+ Frost)
summary(fit1)

fit2 <- lm(data=states_fit1,Murder~Population+Income+Illiteracy+ Frost, Population:Illiteracy)
summary(fit2)

fitx <- lm(data=states_fit1,Murder~Population+Income+Illiteracy+ I(Illiteracy^2))
summary(fitx)

fity <- lm(data=states_fit1,Murder~Population+Illiteracy , Illiteracy:Income)
summary(fity)

## 1.6)
states_fit3 <- states[,c("Murder","Population","Illiteracy")]
states_fit3 <- states_fit3[rownames(states_fit3) != "Nevada", ]
states_fit3

fit3 <- lm(data=states_fit3,Murder~Population+Illiteracy)
fit3
summary(fit3)
outlierTest((fit3))

##regression diagnostics:

##1.7)
confint(fit1)

### 1.8)
par(mfrow = c(2,2))
plot(fit1)


##Linearity (1.9)
crPlots(fit1)

##homoscedasticity: (2)
ncvTest(fit1)

par(mfrow = c(1,1))
spreadLevelPlot(fit1)

2.1) gvmodel_1 <- gvlma(fit1)
summary(gvmodel_1)


##regression diagnostics: (2.2)
confint(fit3)

## 2.3) 
par(mfrow = c(2,2))
plot(fit3)


##Linearity (2.4)
crPlots(fit3)

##homoscedasticity: (2.5)
ncvTest(fit3)

par(mfrow = c(1,1))
spreadLevelPlot(fit3)

## 2.6
gvmodel_3 <- gvlma(fit3)
summary(gvmodel_3)

### Anova (2.7)
anova(fit3,fit1)

#### AIC (2.8) 
AIC(fit3,fit1)

leaps <- regsubsets(Murder~ Population + Illiteracy + Income + Frost,
                    data = states, nbest = 4)
plot(leaps, scale ="adjr2")


subsets(leaps, statistic = "cp", 
        main = "Cp Plot for all subsets Regression")
abline(1,1,lty = 2, col= "red")







states_fit2 <- states[,c("Murder","Population","Illiteracy","Income","Frost")]
states_fit2 <- states_fit2[rownames(states_fit2) != "Nevada", ]
states_fit2

fit2 <- lm(data=states_fit2,Murder~Population+Income+Illiteracy+ Frost, Population:Illiteracy)
fit2
summary(fit2)
outlierTest((fit2))


##regression diagnostics: (2.2)
confint(fit2)

## 2.3) 
par(mfrow = c(2,2))
plot(fit2)

##Linearity (2.4)
crPlots(fit2)

##homoscedasticity: (2.5)
ncvTest(fit2)

par(mfrow = c(1,1))
spreadLevelPlot(fit2)

## 2.6
gvmodel_3 <- gvlma(fit2)
summary(gvmodel_2)



### Anova (2.7)
anova(fit2,fit1)

#### AIC (2.8) 
AIC(fit3,fit2,fit1)

leaps <- regsubsets(Murder~ Population + Illiteracy + Income + Frost,
                    data = states, nbest = 4)
plot(leaps, scale ="adjr2")

leaps <- regsubsets(Murder~ Population + Illiteracy + Income + Frost,  Population:Illiteracy
                    data = states, nbest = 4)
plot(leaps, scale ="adjr2")

subsets(leaps, statistic = "cp", 
        main = "Cp Plot for all subsets Regression")
abline(1,1,lty = 2, col= "red")