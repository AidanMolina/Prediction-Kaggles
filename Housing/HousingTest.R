library(ggplot2)
library(ggcorrplot)
library(corrplot)
library(dplyr)
library(stringr)
library(tidyr)

trainData <- read.csv("train (1).csv", header = TRUE)
summary(trainData)
summary(trainData$SalePrice)
sd(trainData$SalePrice)

ggplot(data = trainData, aes(x=SalePrice)) + 
  geom_histogram(bins = 50) +
  labs(title = "Distribution of House Sale Price",
       x = "Price in USD", y = "Frequency")

missing_cols <- which(colSums(is.na(trainData))>0)
for(i in missing_cols){
  print(colnames(trainData[i]))
  print(sum(is.na(trainData[,i])))
}

trainData <- mutate(group_by(trainData, LotShape), LotFrontage = ifelse(is.na(LotFrontage), median(LotFrontage, na.rm = TRUE), LotFrontage))
trainData$Alley <- replace_na(trainData$Alley, "No Alley")
for (i in names(trainData[, missing_cols])) {
  if (str_detect(i, "Bsmt") == TRUE) {
    trainData[, i][is.na(trainData[, i])] <- 'No Basement'
    
  } else if (str_detect(i, "Garage") == TRUE) {
    if (i == 'GarageYrBlt'){
      trainData[, i][is.na(trainData[, i])] <- 0
    } else {
      trainData[, i][is.na(trainData[, i])] <- 'No Garage'
    }
    
  }
}

trainData$MasVnrType <- replace_na(trainData$MasVnrType, "No Veneer")
trainData$MasVnrArea <- replace_na(trainData$MasVnrArea, 0)
trainData$FireplaceQu <- replace_na(trainData$FireplaceQu, "No Fireplace")
trainData$PoolQC <- replace_na(trainData$PoolQC, "No Pool")
trainData$Fence <- replace_na(trainData$Fence, "No Fence")
trainData$MiscFeature <- replace_na(trainData$MiscFeature, "No Misc")

getmode <- function(x){
  uniqx <- unique(x)
  uniqx[which.max(tabulate(match(x, uniqx)))]
}
trainData <- mutate(trainData, Electrical = if_else(is.na(Electrical),
                                                    getmode(Electrical),
                                                    Electrical))
sum(is.na(trainData))

numericVar <- names(trainData)[which(sapply(trainData, is.numeric))]
catVar <- names(trainData)[which(sapply(trainData, is.character))]
trainCategorical <- trainData[which(sapply(trainData, is.character))]
trainNumeric <- trainData[which(sapply(trainData, is.numeric))]

numericCorr <- c()
for(i in numericVar){
  numericCorr[i] <- round(cor(trainNumeric[i], trainNumeric$SalePrice), 2)
}
print(numericCorr)

#OveralQual - 0.79
#GrLivArea - 0.71
#TotalBsmtSF - 0.61
#X1stFlrSF - 0.61
#GarageCars - 0.64
#GarageArea 0.62

par(mfrow = c(1,1))
# Creating the plot
plot(trainNumeric$OverallQual, trainNumeric$SalePrice, pch = 19, col = "lightblue")
abline(lm(trainNumeric$SalePrice ~ trainNumeric$OverallQual), col = "red", lwd = 3)

plot(trainNumeric$GrLivArea, trainNumeric$SalePrice, pch = 19, col = "lightblue")
abline(lm(trainNumeric$SalePrice ~ trainNumeric$GrLivArea), col = "red", lwd = 3)

plot(trainNumeric$TotalBsmtSF, trainNumeric$SalePrice, pch = 19, col = "lightblue")
abline(lm(trainNumeric$SalePrice ~ trainNumeric$TotalBsmtSF), col = "red", lwd = 3)

plot(trainNumeric$X1stFlrSF, trainNumeric$SalePrice, pch = 19, col = "lightblue")
abline(lm(trainNumeric$SalePrice ~ trainNumeric$X1stFlrSF), col = "red", lwd = 3)

plot(trainNumeric$GarageCars, trainNumeric$SalePrice, pch = 19, col = "lightblue")
abline(lm(trainNumeric$SalePrice ~ trainNumeric$GarageCars), col = "red", lwd = 3)

plot(trainNumeric$GarageArea, trainNumeric$SalePrice, pch = 19, col = "lightblue")
abline(lm(trainNumeric$SalePrice ~ trainNumeric$GarageArea), col = "red", lwd = 3)

#Neighborhood
#LotShape
#Condition1
#HouseStyle
#Exterior1st
#Exterior2nd
#MasVnrType
#ExterQual
#Foundation
#BsmtQual

trainData %>% select(Neighborhood, SalePrice) %>% ggplot(aes(factor(Neighborhood), SalePrice)) + geom_boxplot() + theme(axis.text.x = element_text(angle = 90, hjust =1)) + xlab('Neighborhoods')
trainData %>% select(LotShape, SalePrice) %>% ggplot(aes(factor(LotShape), SalePrice)) + geom_boxplot() + theme(axis.text.x = element_text(angle = 90, hjust =1)) + xlab('LotShapes')
trainData %>% select(Condition1, SalePrice) %>% ggplot(aes(factor(Condition1), SalePrice)) + geom_boxplot() + theme(axis.text.x = element_text(angle = 90, hjust =1)) + xlab('Condition1')
trainData %>% select(HouseStyle, SalePrice) %>% ggplot(aes(factor(HouseStyle), SalePrice)) + geom_boxplot() + theme(axis.text.x = element_text(angle = 90, hjust =1)) + xlab('HouseStyle')
trainData %>% select(Exterior1st, SalePrice) %>% ggplot(aes(factor(Exterior1st), SalePrice)) + geom_boxplot() + theme(axis.text.x = element_text(angle = 90, hjust =1)) + xlab('Exterior1st')
trainData %>% select(Exterior2nd, SalePrice) %>% ggplot(aes(factor(Exterior2nd), SalePrice)) + geom_boxplot() + theme(axis.text.x = element_text(angle = 90, hjust =1)) + xlab('Exterior2nd')
trainData %>% select(MasVnrType, SalePrice) %>% ggplot(aes(factor(MasVnrType), SalePrice)) + geom_boxplot() + theme(axis.text.x = element_text(angle = 90, hjust =1)) + xlab('MasVnrType')
trainData %>% select(ExterQual, SalePrice) %>% ggplot(aes(factor(ExterQual), SalePrice)) + geom_boxplot() + theme(axis.text.x = element_text(angle = 90, hjust =1)) + xlab('ExterQual')
trainData %>% select(Foundation, SalePrice) %>% ggplot(aes(factor(Foundation), SalePrice)) + geom_boxplot() + theme(axis.text.x = element_text(angle = 90, hjust =1)) + xlab('Foundation')
trainData %>% select(BsmtQual, SalePrice) %>% ggplot(aes(factor(BsmtQual), SalePrice)) + geom_boxplot() + theme(axis.text.x = element_text(angle = 90, hjust =1)) + xlab('BsmtQual')

correlations <- cor(trainNumeric)

# correlations
row_indic <- apply(correlations, 1, function(x) sum(x > 0.3 | x < -0.3) > 1)

correlations<- correlations[row_indic ,row_indic ]
corrplot(correlations, method="square")
