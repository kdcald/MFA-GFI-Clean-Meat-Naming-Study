
library(sandwich)
library(lmtest)
library(stringr)
library(ggplot2)

# loads functions.
source("/Users/Krystal/Google Drive/research/gfi/cultured_survey_chicken/analysis/r_code/ggplotAMCEs-2.R")

load("/Users/Krystal/Google Drive/research/gfi/cultured_survey_chicken/analysis/data/chickenDataForAnalysis.csv.Rdata")
chicken = df
chicken$data = "chicken"

chicken$aggregate = NA
chicken$aggregate[chicken$productionMethod=="Pure"] = "te"
chicken$aggregate[chicken$productionMethod=="Safe"] = "te"
chicken$aggregate[chicken$productionMethod=="Cultured"] = "te"
chicken$aggregate[chicken$productionMethod=="Clean"] = "te"
chicken$aggregate[chicken$productionMethod=="Meat 2.0"] = "te"
chicken$aggregate[chicken$productionMethod=="Humane"] = "humane"
chicken$aggregate[chicken$productionMethod=="Conventional"] = "conventional"
table(chicken$aggregate, exclude=NULL)

mean(chicken$productChoiceBinary[chicken$aggregate=="te"], na.rm = TRUE)
mean(chicken$productChoiceBinary[chicken$aggregate=="humane"], na.rm = TRUE)
mean(chicken$productChoiceBinary[chicken$aggregate=="conventional"], na.rm = TRUE)

load("/Users/Krystal/Google Drive/research/gfi/cultured_survey_beef/analysis/data/beefDataForAnalysis.csv.Rdata")
beef = df
beef$data = "beef"

beef$aggregate = NA
beef$aggregate[beef$productionMethod=="Pure"] = "te"
beef$aggregate[beef$productionMethod=="Safe"] = "te"
beef$aggregate[beef$productionMethod=="Cultured"] = "te"
beef$aggregate[beef$productionMethod=="Clean"] = "te"
beef$aggregate[beef$productionMethod=="Meat 2.0"] = "te"
beef$aggregate[beef$productionMethod=="Humane"] = "humane"
beef$aggregate[beef$productionMethod=="Conventional"] = "conventional"
table(beef$aggregate, exclude=NULL)

mean(beef$productChoiceBinary[beef$aggregate=="te"], na.rm = TRUE)
mean(beef$productChoiceBinary[beef$aggregate=="humane"], na.rm = TRUE)
mean(beef$productChoiceBinary[beef$aggregate=="conventional"], na.rm = TRUE)

# merges chicken and beef 
total <- rbind(chicken, beef)

# # DATA FOR EXPORTING
# totalForExport = total
# 
# # converts MTurk ID to anonymous factor number 
# totalForExport$respID = as.factor(totalForExport$respID)
# totalForExport$respID = as.numeric(totalForExport$respID)
# 
# write.csv(totalForExport, "data/completeConjointData_forAnalysis.csv", row.names=FALSE)
# 

mean(total$productChoiceBinary[total$aggregate=="te"], na.rm = TRUE)
mean(total$productChoiceBinary[total$aggregate=="humane"], na.rm = TRUE)
mean(total$productChoiceBinary[total$aggregate=="conventional"], na.rm = TRUE)


# sets working directory.
kcPath <- "/Users/Krystal/Google Drive/surveys/gfi/cultured_survey_beef/analysis"

globalPath <- kcPath

# ---------------------------------------------	#
# 		RE-ORDERS TASK FACTOR VARIABLES  		#
# ---------------------------------------------	#

total$productionMethod <- (factor(total$productionMethod, levels=c("Conventional", "Clean", "Cultured", "Humane", "Meat 2.0", "Pure", "Safe")))
total$product1 <- as.numeric(total$product1)
total$product2 <- as.numeric(total$product2)
total$productChoice <- as.numeric(total$productChoice)
total$productChoiceBinary <- as.numeric(total$productChoiceBinary)
total$productRate <- as.numeric(total$productRate)


# ----------------------------------------------------------------- #
# ----------------------------------------------------------------- #
# 							MAIN AMCEs  	FOR BINARY CHOICE						#
# ----------------------------------------------------------------- #
# ----------------------------------------------------------------- #

# -------------------------------------------------	#
# SETS FORMULA AND LIST OF ATTRIBUTES FOR ANALYSES
# FOR BINARY CHOICE
# -------------------------------------------------	#

attributes <- c("productionMethod")

# stores the outcome and explanatory variables in a formula. 
outcomeChoice <- "productChoiceBinary"
effectName <- "Mean Difference in Pr(would choose product)"

formula <- formula(paste(outcomeChoice,"~",paste(attributes), sep=""))

# computes the main AMCE
amce <- lm(formula, data=total)
amceCRSE <- coeftest(amce, cluster = total$respID)
amceCRSE
write.csv(amceCRSE, file="combinedModel.binary.csv")

# prepares the results to be plotted.
amceCRSE <- prepForPlot(df=total, coeftestResult=amceCRSE, sampleName="cultured", attributes=attributes)
# NOTE: I've double-checked that every element produced in this data-frame matches the original coding. Hence, this function works correctly. 

# plots the results.
ggplotAMCEs(amceCRSE, df2=NULL, coefNames='var', effect='estimate', lowerCI='lowerCI', upperCI='upperCI', colorFactor='attribute', effectName=effectName, title=NULL, ylab=NULL, xlimits=c(-0.4,0.4))


# ----------------------------------------------------------------- #
# ----------------------------------------------------------------- #
# 							MAIN AMCEs  	FOR PRODUCT RATING					#
# ----------------------------------------------------------------- #
# ----------------------------------------------------------------- #

# -------------------------------------------------	#
# SETS FORMULA AND LIST OF ATTRIBUTES FOR ANALYSES  
# FOR PRODUCT RATING
# -------------------------------------------------	#

attributes <- c("productionMethod")

# stores the outcome and explanatory variables in a formula. 
outcomeRate <- "productRate"
effectName <- "Mean difference in product rating (1-7 scale)"

formula <- formula(paste(outcomeRate,"~",paste(attributes), sep=""))


# computes the main AMCE
amce <- lm(formula, data=total)
amceCRSE <- coeftest(amce, cluster = total$respID)
amceCRSE
write.csv(amceCRSE, file="combinedModel.rating.csv")

# prepares the results to be plotted.
amceCRSE <- prepForPlot(df=total, coeftestResult=amceCRSE, sampleName="cultured", attributes=attributes)
# NOTE: I've double-checked that every element produced in this data-frame matches the original coding. Hence, this function works correctly. 

# plots the results.
ggplotAMCEs(amceCRSE, df2=NULL, coefNames='var', effect='estimate', lowerCI='lowerCI', upperCI='upperCI', colorFactor='attribute', effectName=effectName, title=NULL, ylab=NULL, xlimits=c(-1.7,1))



# ----------------------------------------------------------------- #
# ----------------------------------------------------------------- #
# 							ADDITIONAL ANALYSIS					#
# ----------------------------------------------------------------- #
# ----------------------------------------------------------------- #


# MODELS WITH A DIFFERENT BASELINE -BINARY CHOICE

# cultured as baseline
total$productionMethod <- (factor(total$productionMethod, levels=c("Cultured", "Clean", "Conventional", "Humane", "Meat 2.0", "Pure", "Safe")))
cultured.base = lm(productChoiceBinary~productionMethod, data=total)
summary(cultured.base)

# clean as baseline
total$productionMethod <- (factor(total$productionMethod, levels=c("Clean", "Cultured", "Conventional", "Humane", "Meat 2.0", "Pure", "Safe")))
clean.base = lm(productChoiceBinary~productionMethod, data=total)
summary(clean.base)

# safe as baseline
total$productionMethod <- (factor(total$productionMethod, levels=c("Safe", "Cultured", "Conventional", "Humane", "Meat 2.0", "Pure", "Clean")))
safe.base = lm(productChoiceBinary~productionMethod, data=total)
summary(safe.base)

# pure as baseline
total$productionMethod <- (factor(total$productionMethod, levels=c("Pure", "Cultured", "Conventional", "Humane", "Meat 2.0", "Safe", "Clean")))
pure.base = lm(productChoiceBinary~productionMethod, data=total)
summary(pure.base)

# Meat 2.0 as baseline
total$productionMethod <- (factor(total$productionMethod, levels=c("Meat 2.0", "Cultured", "Conventional", "Humane", "Pure", "Safe", "Clean")))
meat20.base = lm(productChoiceBinary~productionMethod, data=total)
summary(meat20.base)

# humane as baseline
total$productionMethod <- (factor(total$productionMethod, levels=c("Humane", "Cultured", "Conventional", "Meat 2.0", "Pure", "Safe", "Clean")))
humane.base = lm(productChoiceBinary~productionMethod, data=total)
summary(humane.base)


# MODELS WITH A DIFFERENT BASELINE -PRODUCT RATING 

# cultured as baseline
total$productionMethod <- (factor(total$productionMethod, levels=c("Cultured", "Clean", "Conventional", "Humane", "Meat 2.0", "Pure", "Safe")))
cultured.base = lm(productRate~productionMethod, data=total)
summary(cultured.base)

# clean as baseline
total$productionMethod <- (factor(total$productionMethod, levels=c("Clean", "Cultured", "Conventional", "Humane", "Meat 2.0", "Pure", "Safe")))
clean.base = lm(productRate~productionMethod, data=total)
summary(clean.base)

# safe as baseline
total$productionMethod <- (factor(total$productionMethod, levels=c("Safe", "Cultured", "Conventional", "Humane", "Meat 2.0", "Pure", "Clean")))
safe.base = lm(productRate~productionMethod, data=total)
summary(safe.base)

# pure as baseline
total$productionMethod <- (factor(total$productionMethod, levels=c("Pure", "Cultured", "Conventional", "Humane", "Meat 2.0", "Safe", "Clean")))
pure.base = lm(productRate~productionMethod, data=total)
summary(pure.base)

# Meat 2.0 as baseline
total$productionMethod <- (factor(total$productionMethod, levels=c("Meat 2.0", "Cultured", "Conventional", "Humane", "Pure", "Safe", "Clean")))
meat20.base = lm(productRate~productionMethod, data=total)
summary(meat20.base)

# humane as baseline
total$productionMethod <- (factor(total$productionMethod, levels=c("Humane", "Cultured", "Conventional", "Meat 2.0", "Pure", "Safe", "Clean")))
humane.base = lm(productRate~productionMethod, data=total)
summary(humane.base)


# MEANS BINARY CHOICE
mean(total$productChoiceBinary[total$productionMethod=="Clean"], na.rm=TRUE)
mean(total$productChoiceBinary[total$productionMethod=="Cultured"], na.rm=TRUE)
mean(total$productChoiceBinary[total$productionMethod=="Pure"], na.rm=TRUE)
mean(total$productChoiceBinary[total$productionMethod=="Safe"], na.rm=TRUE)
mean(total$productChoiceBinary[total$productionMethod=="Meat 2.0"], na.rm=TRUE)
mean(total$productChoiceBinary[total$productionMethod=="Humane"], na.rm=TRUE)
mean(total$productChoiceBinary[total$productionMethod=="Conventional"], na.rm=TRUE)

# MEANS PRODUCT RATING
mean(total$productRate[total$productionMethod=="Clean"], na.rm=TRUE)
mean(total$productRate[total$productionMethod=="Cultured"], na.rm=TRUE)
mean(total$productRate[total$productionMethod=="Pure"], na.rm=TRUE)
mean(total$productRate[total$productionMethod=="Safe"], na.rm=TRUE)
mean(total$productRate[total$productionMethod=="Meat 2.0"], na.rm=TRUE)
mean(total$productRate[total$productionMethod=="Humane"], na.rm=TRUE)
mean(total$productRate[total$productionMethod=="Conventional"], na.rm=TRUE)


#PRICE 

table(chicken$price)
price.chick.model1 = lm(productChoiceBinary~price, data=chicken)
summary(price.chick.model1)

price.chick.model2 = lm(productRate~price, data=chicken)
summary(price.chick.model2)


table(beef$price)
price.beef.model1 = lm(productChoiceBinary~price, data=beef)
summary(price.beef.model1)

price.beef.model2 = lm(productRate~price, data=beef)
summary(price.beef.model2)


#PRODUCT

#chicken finger as baseline
table(chicken$productType)
product.chick.model1 = lm(productChoiceBinary~productType, data=chicken)
summary(product.chick.model1)
#chicken nugget as baseline
chicken$productType <- (factor(chicken$productType, levels=c("Chicken nugget","Chicken finger","Chicken patty","Ground chicken")))
nugget.base =  lm(productChoiceBinary~productType, data=chicken)
summary(nugget.base)
#chicken patty as baseline
chicken$productType <- (factor(chicken$productType, levels=c("Chicken patty","Chicken finger","Chicken nugget","Ground chicken")))
nugget.base =  lm(productChoiceBinary~productType, data=chicken)
summary(nugget.base)


product.chick.model2 = lm(productRate~productType, data=chicken)
summary(product.chick.model2)



#burger baseline
table(beef$productType)
beef$productType <- (factor(beef$productType, levels=c("Burger","Ground beef","Hotdog","Meatball")))
product.beef.model1 = lm(productChoiceBinary~productType, data=beef)
summary(product.beef.model1)

#Ground beef baseline
beef$productType <- (factor(beef$productType, levels=c("Ground beef","Burger","Hotdog","Meatball")))
groundBeef.base = lm(productChoiceBinary~productType, data=beef)
summary(groundBeef.base)

#hot dog baseline
beef$productType <- (factor(beef$productType, levels=c("Hotdog","Burger","Ground beef","Meatball")))
hotdog.base = lm(productChoiceBinary~productType, data=beef)
summary(hotdog.base)

#meatball baseline
beef$productType <- (factor(beef$productType, levels=c("Meatball","Burger","Ground beef","Hotdog")))
meatball.base = lm(productChoiceBinary~productType, data=beef)
summary(meatball.base)


product.beef.model2 = lm(productRate~productType, data=beef)
summary(product.beef.model2)


# ----------------------------------------------------------------- #
# ----------------------------------------------------------------- #
# 							PRICE ANALYSIS					#
# ----------------------------------------------------------------- #
# ----------------------------------------------------------------- #


table(total$data)
test = lm(productRate~aggregate, data=total)
summary(test)

table(total$productRate, total$aggregate, na.rm = TRUE)


# CHICKEN PRICES BINARY
mean(chicken$productChoiceBinary[chicken$aggregate=="Conventional" & chicken$price=="$5.00/pound"], na.rm = TRUE)
mean(chicken$productChoiceBinary[chicken$aggregate=="Conventional" & chicken$price=="$3.75/pound"], na.rm = TRUE)
mean(chicken$productChoiceBinary[chicken$aggregate=="Conventional" & chicken$price=="$2.50/pound"], na.rm = TRUE)
mean(chicken$productChoiceBinary[chicken$aggregate=="Conventional" & chicken$price=="$1.50/pound"], na.rm = TRUE)

mean(chicken$productChoiceBinary[chicken$aggregate=="Combined Cultured" & chicken$price=="$5.00/pound"], na.rm = TRUE)
mean(chicken$productChoiceBinary[chicken$aggregate=="Combined Cultured" & chicken$price=="$3.75/pound"], na.rm = TRUE)
mean(chicken$productChoiceBinary[chicken$aggregate=="Combined Cultured" & chicken$price=="$2.50/pound"], na.rm = TRUE)
mean(chicken$productChoiceBinary[chicken$aggregate=="Combined Cultured" & chicken$price=="$1.50/pound"], na.rm = TRUE)

mean(chicken$productChoiceBinary[chicken$aggregate=="Humane" & chicken$price=="$5.00/pound"], na.rm = TRUE)
mean(chicken$productChoiceBinary[chicken$aggregate=="Humane" & chicken$price=="$3.75/pound"], na.rm = TRUE)
mean(chicken$productChoiceBinary[chicken$aggregate=="Humane" & chicken$price=="$2.50/pound"], na.rm = TRUE)
mean(chicken$productChoiceBinary[chicken$aggregate=="Humane" & chicken$price=="$1.50/pound"], na.rm = TRUE)

chicken$aggregate <- (factor(chicken$aggregate, levels=c("Conventional", "Combined Cultured", "Humane")))

chicken.interaction.binary = lm(productChoiceBinary~price*aggregate, data=chicken)
summary(chicken.interaction.binary)

price.chicken.binary = lm(productChoiceBinary~price+aggregate, data=chicken)
summary(price.chicken.binary)

chicken$productionMethod <- (factor(chicken$productionMethod, levels=c("Conventional", "Clean", "Cultured", "Humane", "Meat 2.0", "Pure", "Safe")))
price.chicken.binary2 = lm(productChoiceBinary~price+productionMethod, data=chicken)
summary(price.chicken.binary2)


# BEEF PRICES BINARY
mean(beef$productChoiceBinary[beef$aggregate=="Conventional" & beef$price=="$9.00/pound"], na.rm = TRUE)
mean(beef$productChoiceBinary[beef$aggregate=="Conventional" & beef$price=="$6.75/pound"], na.rm = TRUE)
mean(beef$productChoiceBinary[beef$aggregate=="Conventional" & beef$price=="$4.50/pound"], na.rm = TRUE)
mean(beef$productChoiceBinary[beef$aggregate=="Conventional" & beef$price=="$2.75/pound"], na.rm = TRUE)

mean(beef$productChoiceBinary[beef$aggregate=="Combined Cultured" & beef$price=="$9.00/pound"], na.rm = TRUE)
mean(beef$productChoiceBinary[beef$aggregate=="Combined Cultured" & beef$price=="$6.75/pound"], na.rm = TRUE)
mean(beef$productChoiceBinary[beef$aggregate=="Combined Cultured" & beef$price=="$4.50/pound"], na.rm = TRUE)
mean(beef$productChoiceBinary[beef$aggregate=="Combined Cultured" & beef$price=="$2.75/pound"], na.rm = TRUE)

mean(beef$productChoiceBinary[beef$aggregate=="Humane" & beef$price=="$9.00/pound"], na.rm = TRUE)
mean(beef$productChoiceBinary[beef$aggregate=="Humane" & beef$price=="$6.75/pound"], na.rm = TRUE)
mean(beef$productChoiceBinary[beef$aggregate=="Humane" & beef$price=="$4.50/pound"], na.rm = TRUE)
mean(beef$productChoiceBinary[beef$aggregate=="Humane" & beef$price=="$2.75/pound"], na.rm = TRUE)

beef$aggregate <- (factor(beef$aggregate, levels=c("Conventional", "Combined Cultured", "Humane")))
price.beef.binary = lm(productChoiceBinary~price+aggregate, data=beef)
summary(price.beef.binary)


beef$productionMethod <- (factor(beef$productionMethod, levels=c("Conventional", "Clean", "Cultured", "Humane", "Meat 2.0", "Pure", "Safe")))
price.beef.binary2 = lm(productChoiceBinary~price+productionMethod, data=beef)
summary(price.beef.binary2)

beef.interaction.binary = lm(productChoiceBinary~price*aggregate, data=beef)
summary(beef.interaction.binary)

# CHICKEN PRICES RATING
chicken$productRate <- as.numeric(chicken$productRate)

mean(chicken$productRate[chicken$aggregate=="Conventional" & chicken$price=="$5.00/pound"], na.rm = TRUE)
mean(chicken$productRate[chicken$aggregate=="Conventional" & chicken$price=="$3.75/pound"], na.rm = TRUE)
mean(chicken$productRate[chicken$aggregate=="Conventional" & chicken$price=="$2.50/pound"], na.rm = TRUE)
mean(chicken$productRate[chicken$aggregate=="Conventional" & chicken$price=="$1.50/pound"], na.rm = TRUE)

mean(chicken$productRate[chicken$aggregate=="Combined Cultured" & chicken$price=="$5.00/pound"], na.rm = TRUE)
mean(chicken$productRate[chicken$aggregate=="Combined Cultured" & chicken$price=="$3.75/pound"], na.rm = TRUE)
mean(chicken$productRate[chicken$aggregate=="Combined Cultured" & chicken$price=="$2.50/pound"], na.rm = TRUE)
mean(chicken$productRate[chicken$aggregate=="Combined Cultured" & chicken$price=="$1.50/pound"], na.rm = TRUE)

mean(chicken$productRate[chicken$aggregate=="Humane" & chicken$price=="$5.00/pound"], na.rm = TRUE)
mean(chicken$productRate[chicken$aggregate=="Humane" & chicken$price=="$3.75/pound"], na.rm = TRUE)
mean(chicken$productRate[chicken$aggregate=="Humane" & chicken$price=="$2.50/pound"], na.rm = TRUE)
mean(chicken$productRate[chicken$aggregate=="Humane" & chicken$price=="$1.50/pound"], na.rm = TRUE)

chicken.interaction.rating = lm(productRate~price*aggregate, data=chicken)
summary(chicken.interaction.rating)

price.chicken.rating = lm(productRate~price+aggregate, data=chicken)
summary(price.chicken.rating)

# BEEF PRICES RATING
beef$productRate <- as.numeric(beef$productRate)

mean(beef$productRate[beef$aggregate=="Conventional" & beef$price=="$9.00/pound"], na.rm = TRUE)
mean(beef$productRate[beef$aggregate=="Conventional" & beef$price=="$6.75/pound"], na.rm = TRUE)
mean(beef$productRate[beef$aggregate=="Conventional" & beef$price=="$4.50/pound"], na.rm = TRUE)
mean(beef$productRate[beef$aggregate=="Conventional" & beef$price=="$2.75/pound"], na.rm = TRUE)

mean(beef$productRate[beef$aggregate=="Combined Cultured" & beef$price=="$9.00/pound"], na.rm = TRUE)
mean(beef$productRate[beef$aggregate=="Combined Cultured" & beef$price=="$6.75/pound"], na.rm = TRUE)
mean(beef$productRate[beef$aggregate=="Combined Cultured" & beef$price=="$4.50/pound"], na.rm = TRUE)
mean(beef$productRate[beef$aggregate=="Combined Cultured" & beef$price=="$2.75/pound"], na.rm = TRUE)

mean(beef$productRate[beef$aggregate=="Humane" & beef$price=="$9.00/pound"], na.rm = TRUE)
mean(beef$productRate[beef$aggregate=="Humane" & beef$price=="$6.75/pound"], na.rm = TRUE)
mean(beef$productRate[beef$aggregate=="Humane" & beef$price=="$4.50/pound"], na.rm = TRUE)
mean(beef$productRate[beef$aggregate=="Humane" & beef$price=="$2.75/pound"], na.rm = TRUE)

beef$aggregate <- (factor(beef$aggregate, levels=c("Conventional", "Combined Cultured", "Humane")))


price.beef = lm(productRate~price+aggregate, data=beef)
summary(price.beef)

beef.interaction.rating = lm(productRate~price*aggregate, data=beef)
summary(beef.interaction.rating)




