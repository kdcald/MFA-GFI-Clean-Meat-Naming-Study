# ---------------------------------------------	#
# 				PRELIMINARIES  					#
# ---------------------------------------------	#

library(sandwich)
library(lmtest)
library(stringr)
library(ggplot2)

# sets working directory.
kcPath <- "/Users/Krystal/Google Drive/surveys/gfi/cultured_survey_beef/analysis"
# gbPath <- ""

globalPath <- kcPath
rm(kcPath)

setwd(globalPath)

# reads in the cleaned data.
load("data/beefDataForAnalysis.csv.Rdata")
#df <- dfConj
class(df)

# loads functions.
source("r_code/ggplotAMCEs-2.R")



# ---------------------------------------------	#
# 		RE-ORDERS TASK FACTOR VARIABLES  		#
# ---------------------------------------------	#

df$productionMethod <- (factor(df$productionMethod, levels=c("Conventional", "Clean", "Cultured", "Humane", "Meat 2.0", "Pure", "Safe")))
df$productType <- (factor(df$productType, levels=c("Burger", "Ground beef", "Hotdog", "Meatball")))
df$price <- (factor(df$price, levels=c("$2.75/pound", "$4.50/pound","$6.75/pound", "$9.00/pound")))
df$product1 <- as.numeric(df$product1)
df$product2 <- as.numeric(df$product2)
df$productChoice <- as.numeric(df$productChoice)
df$productChoiceBinary <- as.numeric(df$productChoiceBinary)
df$productRate <- as.numeric(df$productRate)
df$currentDiet <- (factor(df$currentDiet, levels=c("No specific diet", "Atkins", "Meat Reduction", "Mediterranean", "Other","Paleolithic","Pescatarian","Vegan","Vegetarian")))
#df$meatReductionDiet <- as.numeric(df$meatReductionDiet)
df$currentMeatConsumption <- (factor(df$currentMeatConsumption, levels=c("Never","0-3 times/week", "4-6 times/week","once/day","2 times/day","3 times/day","More than 3 times/day")))
df$age <- as.numeric(levels(df$age))[df$age]
df$educ <- (factor(df$educ, levels=c("Less than grade 12, no diploma", "High school diploma (or equivalent)","Some education past high school","Associate's degree or other non-Bachelor degree","Bachelor's degree", "Graduate or professional degree")))   
df$income <- (factor(df$income, levels=c("Less than $24,999", "$25,000 - $49,999", "$50,000 - $74,999", "$75,000 - $99,999","$100,000 or more"))) 
df$race <- (factor(df$race, levels=c("European/Caucasian","African American or Black","Asian","Biracial","Indian","Latino, Hispanic, or Spanish","Middle Eastern","Native American","Other")))
df$female = as.numeric(levels(df$female))[df$female]

# -------------------------------------------------	#
# SETS FORMULA AND LIST OF ATTRIBUTES FOR ANALYSES
# FOR BINARY CHOICE
# -------------------------------------------------	#

attributes <- c("productionMethod", "productType","price")

# stores the outcome and explanatory variables in a formula. 
outcomeChoice <- "productChoiceBinary"
effectName <- "Mean Difference in Pr(would choose product)"

formula <- formula(paste(outcomeChoice,"~",paste(attributes,collapse="+"), sep=""))

# ----------------------------------------------------------------- #
# ----------------------------------------------------------------- #
# 							MAIN AMCEs  	FOR BINARY CHOICE						#
# ----------------------------------------------------------------- #
# ----------------------------------------------------------------- #

# computes the main AMCE
amce <- lm(formula, data=df)
amceCRSE <- coeftest(amce, cluster = df$respID)
amceCRSE
#write.csv(amceCRSE, file="beefAMCE.binary.csv")


# prepares the results to be plotted.
amceCRSE <- prepForPlot(df=df, coeftestResult=amceCRSE, sampleName="cultured", attributes=attributes)
# NOTE: I've double-checked that every element produced in this data-frame matches the original coding. Hence, this function works correctly. 

# plots the results.
ggplotAMCEs(amceCRSE, df2=NULL, coefNames='var', effect='estimate', lowerCI='lowerCI', upperCI='upperCI', colorFactor='attribute', effectName=effectName, title=NULL, ylab=NULL, xlimits=c(-0.4,0.4))


# -------------------------------------------------	#
# SETS FORMULA AND LIST OF ATTRIBUTES FOR ANALYSES  
# FOR PRODUCT RATING
# -------------------------------------------------	#

attributes <- c("productionMethod", "productType","price")

# stores the outcome and explanatory variables in a formula. 
outcomeRate <- "productRate"
effectName <- "Mean difference in product rating (1-7 scale)"

formula <- formula(paste(outcomeRate,"~",paste(attributes,collapse="+"), sep=""))

# ----------------------------------------------------------------- #
# ----------------------------------------------------------------- #
# 							MAIN AMCEs  FOR PRODUCT RATING							#
# ----------------------------------------------------------------- #
# ----------------------------------------------------------------- #

# computes the main AMCE
amce <- lm(formula, data=df)
amceCRSE <- coeftest(amce, cluster = df$respID)
amceCRSE
#write.csv(amceCRSE, file="beefAMCE.rating.csv")


# prepares the results to be plotted.
amceCRSE <- prepForPlot(df=df, coeftestResult=amceCRSE, sampleName="cultured", attributes=attributes)
# NOTE: I've double-checked that every element produced in this data-frame matches the original coding. Hence, this function works correctly. 

# plots the results.
ggplotAMCEs(amceCRSE, df2=NULL, coefNames='var', effect='estimate', lowerCI='lowerCI', upperCI='upperCI', colorFactor='attribute', effectName=effectName, title=NULL, ylab=NULL, xlimits=c(-1.7,1))


# 
# # ---------------------------------------------	#
# # 			EFFECTS BY DIET TYPE				#
# # ---------------------------------------------	#
# 
# # computes the main AMCE for meat reducers
# dfMeatReducer <- df[df$meatReductionDiet==1,]
# amce <- lm(formula, data=dfMeatReducer)
# amceCRSE_meatReducer <- coeftest(amce, cluster = dfMeatReducer$respID)
# amceCRSE_meatReducer
# 
# # computes the main AMCE for non-meat reduction diet
# dfNoMeatReducer <- df[df$meatReductionDiet==0,]
# amce <- lm(formula, data=dfNoMeatReducer)
# amceCRSE_NoMeatReducer <- coeftest(amce, cluster = dfNoMeatReducer$respID)
# amceCRSE_NoMeatReducer
# 
# # prepares the results to be plotted.
# amceCRSE_meatReducer <- prepForPlot(df=df, coeftestResult=amceCRSE_meatReducer, sampleName="Meat Reducers", attributes=attributes)
# amceCRSE_NoMeatReducer <- prepForPlot(df=df, coeftestResult=amceCRSE_NoMeatReducer, sampleName="Not Meat Reducers", attributes=attributes)
# 
# # ---------------------------------------------	#
# # 				EFFECTS BY GENDER				#
# # ---------------------------------------------	#
# 
# # computes the AMCEs separately for profiles with female vs. male respondent.
# amce_female <- lm(formula, data=df[df$female == 1, ])
# amceCRSE_female <- coeftest(amce_female, cluster = df$respID)
# amceCRSE_female
# 
# amce_male <- lm(formula, data=df[df$female == 0, ])
# amceCRSE_male <- coeftest(amce_male, cluster = df$respID)
# amceCRSE_male
# 
# 
# table = table(df$wouldBuyCultured, exclude = NULL)
# prop.table(table)



# # ---------------------------------------------	#
# # 				ADDITIONAL EXPLORATORY ANALYSIS
# # ---------------------------------------------	#


# BINARY PRODUCT CHOICE MODELS
model1 = lm(productChoiceBinary~productionMethod, data=df)
summary(model1)
write.csv(coeftest(model1), file="beefModel1.binary.csv")

model2 = lm(productChoiceBinary~productionMethod+productType, data=df)
summary(model2)
write.csv(coeftest(model2), file="beefModel2.binary.csv")

model3 = lm(productChoiceBinary~productionMethod+productType+price, data=df)
summary(model3)


# MEANS BINARY CHOICE
mean(df$productChoiceBinary[df$productionMethod=="Clean"], na.rm=TRUE)
mean(df$productChoiceBinary[df$productionMethod=="Cultured"], na.rm=TRUE)
mean(df$productChoiceBinary[df$productionMethod=="Pure"], na.rm=TRUE)
mean(df$productChoiceBinary[df$productionMethod=="Safe"], na.rm=TRUE)
mean(df$productChoiceBinary[df$productionMethod=="Meat 2.0"], na.rm=TRUE)
mean(df$productChoiceBinary[df$productionMethod=="Humane"], na.rm=TRUE)
mean(df$productChoiceBinary[df$productionMethod=="Conventional"], na.rm=TRUE)

# PRODUCT RATING MODELS
model1 = lm(productRate~productionMethod, data=df)
summary(model1)
write.csv(coeftest(model1), file="beefModel1.rating.csv")

model2 = lm(productRate~productionMethod+productType, data=df)
summary(model2)
write.csv(coeftest(model2), file="beefModel2.rating.csv")


model3 = lm(productRate~productionMethod+productType+price, data=df)
summary(model3)


# MEANS PRODUCT RATING
mean(df$productRate[df$productionMethod=="Clean"], na.rm=TRUE)
mean(df$productRate[df$productionMethod=="Cultured"], na.rm=TRUE)
mean(df$productRate[df$productionMethod=="Pure"], na.rm=TRUE)
mean(df$productRate[df$productionMethod=="Safe"], na.rm=TRUE)
mean(df$productRate[df$productionMethod=="Meat 2.0"], na.rm=TRUE)
mean(df$productRate[df$productionMethod=="Humane"], na.rm=TRUE)
mean(df$productRate[df$productionMethod=="Conventional"], na.rm=TRUE)


# MODELS WITH A DIFFERENT BASELINE - BINARY CHOICE BEEF

# cultured as baseline
df$productionMethod <- (factor(df$productionMethod, levels=c("Cultured", "Clean", "Conventional", "Humane", "Meat 2.0", "Pure", "Safe")))
cultured.base = lm(productChoiceBinary~productionMethod, data=df)
summary(cultured.base)

# clean as baseline
df$productionMethod <- (factor(df$productionMethod, levels=c("Clean", "Cultured", "Conventional", "Humane", "Meat 2.0", "Pure", "Safe")))
clean.base = lm(productChoiceBinary~productionMethod, data=df)
summary(clean.base)

# df$productionMethod <- (factor(df$productionMethod, levels=c("Clean", "Cultured", "Conventional", "Humane", "Meat 2.0", "Pure", "Safe")))
# clean.base = lm(productChoiceBinary~productionMethod+productType+price, data=df)
# summary(clean.base)

# safe as baseline
df$productionMethod <- (factor(df$productionMethod, levels=c("Safe", "Cultured", "Conventional", "Humane", "Meat 2.0", "Pure", "Clean")))
safe.base = lm(productChoiceBinary~productionMethod, data=df)
summary(safe.base)

# pure as baseline
df$productionMethod <- (factor(df$productionMethod, levels=c("Pure", "Cultured", "Conventional", "Humane", "Meat 2.0", "Safe", "Clean")))
pure.base = lm(productChoiceBinary~productionMethod, data=df)
summary(pure.base)

# Meat 2.0 as baseline
df$productionMethod <- (factor(df$productionMethod, levels=c("Meat 2.0", "Cultured", "Conventional", "Humane", "Pure", "Safe", "Clean")))
meat20.base = lm(productChoiceBinary~productionMethod, data=df)
summary(meat20.base)

# humane as baseline
df$productionMethod <- (factor(df$productionMethod, levels=c("Humane", "Cultured", "Conventional", "Meat 2.0", "Pure", "Safe", "Clean")))
humane.base = lm(productChoiceBinary~productionMethod, data=df)
summary(humane.base)



# MODELS WITH A DIFFERENT BASELINE - PRODUCT RATING -BEEF

# cultured as baseline
df$productionMethod <- (factor(df$productionMethod, levels=c("Cultured", "Clean", "Conventional", "Humane", "Meat 2.0", "Pure", "Safe")))
cultured.base = lm(productRate~productionMethod, data=df)
summary(cultured.base)

# clean as baseline
df$productionMethod <- (factor(df$productionMethod, levels=c("Clean", "Cultured", "Conventional", "Humane", "Meat 2.0", "Pure", "Safe")))
clean.base = lm(productRate~productionMethod, data=df)
summary(clean.base)

# safe as baseline
df$productionMethod <- (factor(df$productionMethod, levels=c("Safe", "Cultured", "Conventional", "Humane", "Meat 2.0", "Pure", "Clean")))
safe.base = lm(productRate~productionMethod, data=df)
summary(safe.base)

# pure as baseline
df$productionMethod <- (factor(df$productionMethod, levels=c("Pure", "Cultured", "Conventional", "Humane", "Meat 2.0", "Safe", "Clean")))
pure.base = lm(productRate~productionMethod, data=df)
summary(pure.base)

# Meat 2.0 as baseline
df$productionMethod <- (factor(df$productionMethod, levels=c("Meat 2.0", "Cultured", "Conventional", "Humane", "Pure", "Safe", "Clean")))
meat20.base = lm(productRate~productionMethod, data=df)
summary(meat20.base)

# humane as baseline
df$productionMethod <- (factor(df$productionMethod, levels=c("Humane", "Cultured", "Conventional", "Meat 2.0", "Pure", "Safe", "Clean")))
humane.base = lm(productRate~productionMethod, data=df)
summary(humane.base)




