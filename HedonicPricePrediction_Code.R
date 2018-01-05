#Hedonic Home Sale Price Prediction MUSA507 FALL 2017 Midterm 


#************************************SETUP***************************************

# install libraries (will be imported as needed)
install.packages('ggplot2')
install.packages('MASS')
install.packages('corrplot')
install.packages('reshape2')
install.packages('spdep')
install.packages('caret')
install.packages('stargazer')
install.packages('mosaic')
install.packages('RColorBrewer')

library(dplyr)
library(ggmap)

options(scipen=999)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#Setup map theme
mapTheme <- function(base_size = 12) {
  theme(
    text = element_text( color = "black"),
    plot.title = element_text(size = 14,colour = "black"),
    plot.subtitle=element_text(face="italic"),
    plot.caption=element_text(hjust=0),
    axis.ticks = element_blank(),
    panel.background = element_blank(),axis.title = element_blank(),
    axis.text = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(colour = "black", fill=NA, size=2)
  )
}

#Setup basemap
baseMap <- get_map(location = "Roxbury, Boston", 
                   source = "stamen", 
                   zoom = 12, 
                   maptype= 'toner')

#Invert basemap colors
invert <- function(x) rgb(t(255-col2rgb(x))/255)    
baseMap_invert <- as.raster(apply(baseMap, 2, invert))
class(baseMap_invert) <- class(baseMap)
attr(baseMap_invert, "bb") <- attr(baseMap, "bb")
ggmap(baseMap_invert)+
  mapTheme()

#import data (Data collection and wrangling done outside of R. Descriptions available in markdown)
df <- read.csv("~/PENN/Fall_Semester/MUSA507/Midterm/ForPortfolio/WrangledData.csv")
df2 <- select(df, -Parcel_No, -Latitude_1, -Longitud_1)
df$SalePrice <- sapply(df$SalePrice, as.numeric)

df_Train <- df2[df2$SalePrice > 0,]
  
df_Test <- df2[df2$SalePrice == 0,]


#**********************************EXPLORATORY VARIABLE ANALYSIS*****************************

#Analysis of numeric predictors
dfNum <- select(df_Train, -UniqueSale, -SaleSeason, -STRUCTURE_, -R_BLDG_STY, -SaleMonth, -Style , -LU, -R_ROOF_TYP, -HEAT_SYS, -R_EXT_FIN, -Neighborhood)

#Correlation Matrix
CorMatrix <- cor(dfNum)

library(corrplot)
corrplot(CorMatrix, method = "color", order = "AOE", addCoef.col="grey", addCoefasPercent = FALSE, number.cex = .7)

#Removing multi-colinear variables (>.80)
df2 <- select(df2, -GROSS_AREA, -LivingArea, -YR_REMOD, -R_BDRMS, -R_KITCH, -Dist_AP)
df_Train <- df2[df2$SalePrice > 0,]
df_Test <- df2[df2$SalePrice == 0,]

#Analysis of continuous predictors
dfCont <- select(dfNum, -GROSS_AREA, -LivingArea, -YR_REMOD, -NewlyRemodeled, -R_BDRMS, -R_KITCH, -Dist_AP, -NearCommonwealth, -NearImpBldg, -NearUni, -C_AC, -OWN_OCC, -PTYPE, -ZIPCODE, 
                 -R_FPLACE, -R_HALF_BTH, -R_FULL_BTH)

#Distribution analysis
library(reshape2)
dfContMelted <- melt(dfCont)

library(ggplot2)
ggplot(data = dfContMelted, mapping = aes(x = value)) + 
  geom_histogram(bins = 30) + facet_wrap(~variable, scales = 'free_x')

#Log-Transforming to normalize selected predictors 
hist(df2$LAND_SF, breaks = 30)
hist(log(df2$LAND_SF), breaks = 30)

df2$LogLAND_SF <- log(df2$LAND_SF)
df2$LAND_SF <- NULL

hist(df2$LIVING_ARE, breaks = 30)
hist(log(df2$LIVING_ARE), breaks = 30)

df2$LogLIVING_ARE <- log(df2$LIVING_ARE)
df2$LIVING_ARE <- NULL

hist(df2$PCTVACANT, breaks = 10)
hist(log(df2$PCTVACANT+1), breaks = 10)

df2$LogPCTVACANT <- log(df2$PCTVACANT+1)
df2$PCTVACANT <- NULL

hist(df2$Dist_Major_Road, breaks = 20)
hist(log(df2$Dist_Major_Road+1), breaks = 20)

df2$LogDist_Major_Road <- log(df2$Dist_Major_Road+1)
df2$Dist_Road <- NULL

hist(df2$Ave_SalePr, breaks = 30)
hist(log(df2$Ave_SalePr), breaks = 30)

df2$LogAve_SalePr <- log(df2$Ave_SalePr)
df2$Ave_SalePr <- NULL

df_Train <- df2[df2$SalePrice > 0,]
df_Test <- df2[df2$SalePrice == 0,]


#**********************************MODEL BUILDING*****************************

#Linear regression 1
reg1 <- lm(log(SalePrice) ~ ., data =  df_Train %>% 
             as.data.frame %>% dplyr::select(-UniqueSale))
  
summary(reg1)

#Stepwise Regression Analysis
library(MASS)
step <- stepAIC(reg1, direction="both")

step$anova

#linear regression 2: Removing very insignificant predictors
reg2 <- lm(log(SalePrice) ~ ., data =  df_Train %>% 
             as.data.frame %>% dplyr::select(-UniqueSale, -PCTOWNEROC, -DistToPoor,
                                             -DistToCBD, -SchoolGrade, -MEDHHINC, -WalkScore, -TransitSco, 
                                             -BikeScore, -FeetToParks, -HEAT_SYS, -R_EXT_FIN, -R_BLDG_STY, -R_ROOF_TYP, -STRUCTURE_,
                                             -OWN_OCC, -ZIPCODE, -Style, -SaleMonth))

summary(reg2)

reg3 <- lm(log(SalePrice) ~ ., data =  df_Train %>% 
             as.data.frame %>% dplyr::select(-UniqueSale, -LogDist_Major_Road, -PCTOWNEROC, -DistToPoor,
                                             -DistToCBD, -SchoolGrade, -MEDHHINC, -WalkScore, -TransitSco, 
                                             -BikeScore, -FeetToParks, -HEAT_SYS, -R_EXT_FIN, -R_BLDG_STY, -R_ROOF_TYP, -STRUCTURE_,
                                             -OWN_OCC, -ZIPCODE, -Style, -SaleMonth, -YR_BUILT, -R_TOTAL_RM, -NearImpBldg, -NearCommonwealth,
                                             -NearCommonwealth, -LogPCTVACANT, -Dist_Major_Road))


summary(reg3)

#Stepwise Anova 
step <- stepAIC(reg3, direction="both")
step$anova


#**********************************MODEL ANALYSIS*****************************

#testing residual distribution
Reg_Dataframe <- cbind(reg3$residuals,reg3$fitted.values)
Reg_Dataframe <- as.data.frame(Reg_Dataframe)

colnames(Reg_Dataframe) <- c("residuals", "predictedValues")


ggplot(reg3, aes(Reg_Dataframe$residuals)) + geom_histogram(bins=25) +
  labs(x="Residuals",
       y="Count")

#Testing for heteroscedasticity
ggplot(data = Reg_Dataframe, aes(x = residuals , y = predictedValues)) +
  geom_point(size = 0.1) + xlab("Residuals") + ylab("Predicted Values") + ggtitle("Residual Values vs. Predicted Values") +  
  theme(plot.title = element_text(hjust = 0.5))

#Mapping Residuals
reg_residuals <- data.frame(reg3$residuals)
LonLat <- data.frame(df[df$SalePrice>0,]$Longitud_1, df[df$SalePrice>0,]$Latitude_1)
residualsToMap <- cbind(LonLat, reg_residuals )
colnames(residualsToMap) <- c("longitude", "latitude", "residual")

ggmap(baseMap_invert) + 
  geom_point(data = residualsToMap, 
             aes(x=longitude, y=latitude, color = residual), 
             size = 1) + scale_colour_gradient(low = "blue", high = "yellow") + mapTheme()


#RasterTile
Raster <-
  ggmap(baseMap_invert) +
  stat_summary_2d(geom = "tile",
                  bins = 80,
                  data=residualsToMap,
                  aes(x = longitude, y = latitude, z = ntile(residual,5))
  ) +
  scale_fill_gradient(low = "yellow",
                      high = "blue",
                      guide = guide_legend(title = "Residuals \n (Quintiles)")) +
  labs(title="Predicted sale price residuals",
       subtitle="Raster") +
  mapTheme()
Raster


#Predicted vs Observed
regDF <- cbind(log(df_Train$SalePrice), reg3$fitted.values)
colnames(regDF) <- c("Observed", "Predicted")
regDF <- as.data.frame(regDF)
ggplot() + 
  geom_point(data=regDF, aes(Observed, Predicted)) +
  stat_smooth(data=regDF, aes(Observed, Observed), method = "lm", se = FALSE, size = 1) + 
  labs(title="Predicted Price as a function\nof Observed Price") +
  theme(plot.title = element_text(size = 18,colour = "black"))

#Moran's I Analysis
library(spdep)
coords <- cbind(df[df$SalePrice>0,]$Longitud_1, df[df$SalePrice>0,]$Latitude_1)
spatialWeights <- knn2nb(knearneigh(coords, 4))
moran.test(reg1$residuals, nb2listw(spatialWeights, style="W")) #shows no significant spatial autocorrelation present


#************************In_Sample_Training*****************************

library(caret)
inTrain <- createDataPartition(
  y = df_Train$Neighborhood, 
  p = .75, list = FALSE)

IST.training <- df_Train[ inTrain,] #the in-sample training set
IST.test <- df_Train[-inTrain,]  #the in-sample test set

reg4 <- lm(log(SalePrice) ~ ., data =  IST.training%>% #regression with no in-sample training data
             as.data.frame %>% dplyr::select(-UniqueSale, -LogDist_Major_Road, -PCTOWNEROC, -DistToPoor,
                                             -DistToCBD, -SchoolGrade, -MEDHHINC, -WalkScore, -TransitSco, 
                                             -BikeScore, -FeetToParks, -HEAT_SYS, -R_EXT_FIN, -R_BLDG_STY, -R_ROOF_TYP, -STRUCTURE_,
                                             -OWN_OCC, -ZIPCODE, -Style, -SaleMonth, -YR_BUILT, -R_TOTAL_RM, -NearImpBldg, -NearCommonwealth,
                                             -NearCommonwealth, -LogPCTVACANT, -Dist_Major_Road)) 
summary(reg4)

#predict on in-sample test set
reg4Pred <- predict(reg4, IST.test)

reg4PredValues <- 
  data.frame(observedPrice = IST.test$SalePrice,
             predictedPrice = exp(reg4Pred))

#store predictions, observed, absolute error, and percent absolute error
reg4PredValues <-
  reg4PredValues %>%
  mutate(error = predictedPrice - observedPrice) %>%
  mutate(absError = abs(predictedPrice - observedPrice)) %>%
  mutate(percentAbsError = abs(predictedPrice - observedPrice) / observedPrice) 


mean(reg4PredValues$absError) #MAE

mean(reg4PredValues$percentAbsError) #MAPE


#Cross-Validation

fitControl <- trainControl(method = "cv", number = 10)

set.seed(825) #set seed for random number generator

lmFit <- train(log(SalePrice) ~ ., data = df_Train, 
               method = "lm", 
               trControl = fitControl)

lmFit$resample

library(stargazer)
stargazer(lmFit$resample, type = "text")

#Evaluating Generalizeability: Fold MAE Frequency Plot
ggplot(as.data.frame(lmFit$resample), aes(MAE)) +
  geom_histogram(bins=5) +
  labs(x="Mean Absolute Error",
       y="Count")

#Per-Fold R-Squared
CVFolds <- cbind(lmFit$resample$Resample, lmFit$resample$Rsquared)
colnames(CVFolds) <- c("Fold", "RSquared")
CVFolds <- as.data.frame(CVFolds)
CVFolds$RSquared <- as.numeric(as.character(CVFolds$RSquared))
CVFolds$RSquared <- formatC(CVFolds$RSquared,digits=2,format="f")
CVFolds$RSquared <- as.numeric(as.character(CVFolds$RSquared))

ggplot(CVFolds, aes(x=Fold, y=RSquared)) + 
  geom_bar(stat="identity", fill = "#4682b4") + scale_y_continuous(limits = c(0, 1)) + geom_text(aes(label=RSquared), vjust=-1) +
  theme(axis.text.x=element_text(angle=45, hjust=1))



#************************TEST-SET PREDICTIONS*****************************

FinalReg <- lm(log(SalePrice) ~ ., data =  df_Train%>% 
                 as.data.frame %>% dplyr::select(-UniqueSale, -LogDist_Major_Road, -PCTOWNEROC, -DistToPoor,
                                                 -DistToCBD, -SchoolGrade, -MEDHHINC, -WalkScore, -TransitSco, 
                                                 -BikeScore, -FeetToParks, -HEAT_SYS, -R_EXT_FIN, -R_BLDG_STY, -R_ROOF_TYP, -STRUCTURE_,
                                                 -OWN_OCC, -ZIPCODE, -Style, -SaleMonth, -YR_BUILT, -R_TOTAL_RM, -NearImpBldg, -NearCommonwealth,
                                                 -NearCommonwealth, -LogPCTVACANT, -Dist_Major_Road)) 

FinalPred <- predict(FinalReg, df_Test)

FinalPredValues <- 
  data.frame(UniqueSale = df_Test$UniqueSale,
             PredictedPrice = exp(FinalPred))

head(FinalPredValues)

LonLat_Test <- data.frame(df[df$SalePrice==0,]$Longitud_1, df[df$SalePrice==0,]$Latitude_1)
PredictionsToMap <- cbind(FinalPredValues, LonLat_Test)
colnames(PredictionsToMap) <- c("UniqueSale", "PredictedPrice", "longitude", "latitude")

#mapping predictions
library(mosaic)
library(RColorBrewer)
cols <- colorRampPalette(brewer.pal(5,"RdYlGn"))(5)

ggmap(baseMap_invert) + 
  geom_point(data = PredictionsToMap, aes(x=longitude, y=latitude, color= ntiles(PredictedPrice, n = 5)), size = 3) +
  mapTheme() + theme(legend.position="bottom") + 
  scale_color_manual(name ="Predicted Prices (Quintiles)", values = c(cols))

write.csv(FinalPredValues, "predictionsFINAL.csv") #exporting predictions
