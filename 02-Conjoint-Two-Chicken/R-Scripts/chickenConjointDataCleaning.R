library(foreign)
library(plyr)

# sets working directory.
kcPath <- "/Users/Krystal/Google Drive/research/gfi/cultured_survey_chicken/analysis"

globalPath <- kcPath
rm(kcPath)

setwd(globalPath)

source("r_code/chickenConjointReshape.R")


# ---------------------------------------------	#
# 			READS IN CULTURED DATA 					#
# ---------------------------------------------	#

# Reads in the raw data - NUMERIC VALUES
dC <- read.csv("chickenConjointDataUncleaned.csv", as.is=T)
dim(dC)

# Creates a data dictionary for data, linking each question number with a question name. 
dict <- data.frame(t(dC[1,]), stringsAsFactors = FALSE)
# dict <- data.frame(cbind(colnames(dP),qNames), stringsAsFactors = FALSE)
dim(dict)
# rownames(dict) <- NULL
colnames(dict) <- "description"
# rm(qNames)

# -------------------------	#
# Removes row 1 from each dataframe.
# NOTE: row 1 of each dataframe is currently a vector of strings containing the question names. 
dC <- dC[-1,]
dim(dC)

# -------------------------	#
# renames column 2 to respID.
colnames(dC)[colnames(dC)=="V1"] <- rownames(dict)[rownames(dict)=="V1"] <- "respID"

# -------------------------	#
# cross-check: check that colnames of dC are equal to those in dict
setequal(colnames(dC), rownames(dict))  # should return TRUE.



# ---------------------------------------------	#
# 		IMPORTS DATAFRAME WITH OTHER COVARIATES		#
# ---------------------------------------------	#
chickenOtherCovar <- read.csv("~/Google Drive/research/gfi/cultured_survey_chicken/analysis/data/chickenConjointCovars.csv",stringsAsFactors = FALSE, na.strings = "")
dC = merge(dC, chickenOtherCovar, by="respID")


# ---------------------------------------------	#
# 			CREATES VARIABLE FOR TREATMENT GORUP		#
# ---------------------------------------------	#

dC$treatment_group <- apply(dC[,11:83], MARGIN=1, FUN=function(thisRow) { ifelse('Cultured' %in% thisRow, 'cultured', 
                                                                                 ifelse('Pure' %in% thisRow, 'pure', 
                                                                                  ifelse('Safe' %in% thisRow, 'safe', 
                                                                                  ifelse('Meat 2.0' %in% thisRow, 'meat', 
                                                                                  ifelse('Clean' %in% thisRow, 'clean',
                                                                                        NA)))))})
table(dC$treatment_group, exclude = NULL)

dC[1389,"treatment_group"] = "meat"
dC[633,"treatment_group"] = "meat"
dC[303,"treatment_group"] = "pure"
dC[1758,"treatment_group"] = "pure"


dC$treatment_number[dC$treatment_group=="cultured"] = 1
dC$treatment_number[dC$treatment_group=="clean"] = 2
dC$treatment_number[dC$treatment_group=="meat"] = 3
dC$treatment_number[dC$treatment_group=="safe"] = 4
dC$treatment_number[dC$treatment_group=="pure"] = 5



# -----------------------------------------------------------------	#
# 			OMITS OBSERVATIONS THAT DO NOT PASS COMPREHENSION TEST 					#
# -----------------------------------------------------------------	#
# observations are considered a "pass" if they get a minumum of 2/3 of the comprehension questions correct
# fills in comprehensinoCheck based on 3 conditions: 1) getting Q1 and Q2 correct, 2) geting Q1 and Q3 correct, and 3) getting Q2 and Q3 right.
# Q1 (Q395, Q237, Q236, Q234, Q235) correct response: 1, Amit Gefen, Tel Aviv University
# Q2 (Q393, Q240, Q239, Q238, Q241) correct response: 2, Chicken that involves <em>taking tissue cells from a live chicken</em> and growing them independent from the animal until they&#39;re combined to make a product
# Q3 (Q243, Q244, Q246, Q396, Q247) correct response: 1, The realization that chicken production has some serious issues, such as environmental damage and animal welfare concerns

# creates comprehension test variable
dC$comprehensionCheck = NA

# cultured chicken
dC$comprehensionCheck[dC$Q395==1 & dC$Q393==2] = "pass"
dC$comprehensionCheck[dC$Q395==1 & dC$Q243==1] = "pass"
dC$comprehensionCheck[dC$Q393==2 & dC$Q243==1] = "pass"
head(dC, 40)

# clean chicken
dC$comprehensionCheck[dC$Q237==1 & dC$Q240==2] = "pass"
dC$comprehensionCheck[dC$Q237==1 & dC$Q244==1] = "pass"
dC$comprehensionCheck[dC$Q240==2 & dC$Q244==1] = "pass"

# meat 2.0
dC$comprehensionCheck[dC$Q236==1 & dC$Q239==2] = "pass"
dC$comprehensionCheck[dC$Q236==1 & dC$Q246==1] = "pass"
dC$comprehensionCheck[dC$Q239==2 & dC$Q246==1] = "pass"

# pure chicken
dC$comprehensionCheck[dC$Q234==1 & dC$Q238==2] = "pass"
dC$comprehensionCheck[dC$Q234==1 & dC$Q396==1] = "pass"
dC$comprehensionCheck[dC$Q238==2 & dC$Q396==1] = "pass"

# safe chicken
dC$comprehensionCheck[dC$Q235==1 & dC$Q241==2] = "pass"
dC$comprehensionCheck[dC$Q235==1 & dC$Q247==1] = "pass"
dC$comprehensionCheck[dC$Q241==2 & dC$Q247==1] = "pass"

table(dC$comprehensionCheck, exclude=NULL)


# replaces NA's with "fail"
# "fail" includes any observations that did not get filled in with a "pass" and any observation that was NA (i.e., did not fill out the survey)
dC[,"comprehensionCheck"][is.na(dC[, "comprehensionCheck"])] <- "fail"

table(dC$comprehensionCheck, exclude = NULL)

#creates dataframe with fails and NA's
dC.fails = dC[!(dC$comprehensionCheck=="pass"),]

#creates dataframe with only those that have "passed" the comprehension questions (min 2/3 correct) and no NA's
dC = dC[!(dC$comprehensionCheck=="fail"),]
dim(dC)




# ------------------------------------------------------------------ #
# ------------------------------------------------------------------ #
# 			RESHAPES THE DATA AND CREATES OUTCOMES/COVARIATES	     #
# ------------------------------------------------------------------ #
# ------------------------------------------------------------------ #

# --------------------------------------------------------- #
# 	CREATES DATAFRAMES FOR TASKS, OUTCOMES, AND COVARIATES 	#
# --------------------------------------------------------- #

# creates a dataframe containing just the conjoint tasks (not including actual responses).
lastCol <- "F.8.2.3"
dfConj <- dC[,1:which(colnames(dC)==lastCol)]
colnames(dfConj)

# ---------------------------------------------	#
# 			RESHAPES CONJOINT TASKS 			#
# ---------------------------------------------	#

# stores the desired order of attributes.
attributes = c("Production method", "Product type", "Price")

# number of decision tasks.
tasks <- 8 

# number of profiles in each decision task.
prof <- 2

# creates a dataframe of the conjoint tasks. 
dList <- apply(dfConj, MARGIN=1, FUN=function(x) convertTask(x, tasks, prof))
dfConj <- do.call("rbind", dList)
dim(dfConj)
table(dfConj[,3])
head(dfConj, 20)



# ---------------------------------------------	#
# 				RENAMES COLNAMES				#
# ---------------------------------------------	#

colnames(dfConj)[which(colnames(dfConj) %in% attributes)] <- c("productionMethod", "productType", "price")

# ---------------------------------------------	#
# 	RESHAPES AND CREATES OUTCOME VARIABLES 		#
# ---------------------------------------------	#

outcomeNames <- c("product1", "product2", "productChoice")

# creates a dataframe of responses. 
yList <- apply(dC, MARGIN=1, FUN=function(x) convertResponses(x, tasks, prof, outcomeNames, dict))
dfOutcomes <- do.call("rbind", yList)
dim(dfOutcomes)
head(dfOutcomes, 40)


# ---------------------------------------------	#
# creates "productChoice" binary variable.
# ------------ # 

dfOutcomes$productChoiceBinary <- NA 
head(dfOutcomes, 20)
dfOutcomes$productChoiceBinary[dfOutcomes$productChoice==1 & dfOutcomes$profileNum==1] <- 1
dfOutcomes$productChoiceBinary[dfOutcomes$productChoice==2 & dfOutcomes$profileNum==2] <- 1
dfOutcomes$productChoiceBinary[dfOutcomes$productChoice==1 & dfOutcomes$profileNum==2] <- 0
dfOutcomes$productChoiceBinary[dfOutcomes$productChoice==2 & dfOutcomes$profileNum==1] <- 0
head(dfOutcomes, 20)
table(dfOutcomes$productChoiceBinary, exclude=NULL)


dfOutcomes$productRate <- NA
dfOutcomes$productRate[dfOutcomes$profileNum==1] <- dfOutcomes$product1[dfOutcomes$profileNum==1]
dfOutcomes$productRate[dfOutcomes$profileNum==2] <- dfOutcomes$product2[dfOutcomes$profileNum==2]
dfOutcomes$productRate[dfOutcomes$productRate==""] <- NA
table(dfOutcomes$productRate, exclude=NULL)

# cross check
table(dfOutcomes$product1, dfOutcomes$productRate,dfOutcomes$profileNum, exclude=NULL)
table(dfOutcomes$product2, dfOutcomes$productRate,dfOutcomes$profileNum, exclude=NULL)


# ---------------------------------------------	#
# 	CREATES DATAFRAME OF OPEN ENDED RESPONSE TO TASKNUM 1		#
# ---------------------------------------------	#

# creates datafram for openended responses
dfOpenEnd = as.data.frame(dC$respID)
colnames(dfOpenEnd) = c("respID")

# creates variable for open ended responses
dfOpenEnd$whyChoice = NA

# pastes responses to new var
dfOpenEnd$whyChoice = paste(dC$Q49, dC$Q428, dC$Q470, dC$Q554, dC$Q512)

# creates taskNum variable to merge on
dfOpenEnd$taskNum = rep(1, nrow(dfOpenEnd))

# merges dfOutcomes with dfOpenEnd
dfOutcomes = join(dfOutcomes, dfOpenEnd, by=c("respID", "taskNum"), type="left")



# ---------------------------------------------	#
# 				CREATES COVARIATES 				#
# ---------------------------------------------	#

# DO YOU HAVE CONCERNS ABOUT EATING TE PRODUCTS
# yes or no
haveConcerns = rep(NA, nrow(dC))
haveConcerns = paste(dC$Q1, dC$Q444, dC$Q486, dC$Q570, dC$Q528)
haveConcerns = as.numeric(haveConcerns)

# WHAT ARE YOUR CONCERNS WITH EATING TE CHICKEN PRODUCTS
# open-ended
whatConcerns = rep(NA, nrow(dC))
whatConcerns = paste(dC$Q2, dC$Q445, dC$Q487, dC$Q571, dC$Q529)
whatConcerns[whatConcerns=="    "] <- NA

# WHY DON'T YOU HAVE ANY CONCERNS ABOUT TE PRODUCTS
# open-ended
whyNoConcerns = rep(NA, nrow(dC))
whyNoConcerns = paste(dC$Q64, dC$Q446, dC$Q488, dC$Q572, dC$Q530)
whyNoConcerns[whyNoConcerns=="    "] <- NA

# Do you see any advantages to eating TE products?
# yes or no
seeAdvantages = rep(NA, nrow(dC))
seeAdvantages = paste(dC$Q3, dC$Q447, dC$Q489, dC$Q573, dC$Q531)
seeAdvantages = as.numeric(seeAdvantages)

# What advantages do you see in eating TE products?
# open-ended
whatAdvantages = rep(NA, nrow(dC))
whatAdvantages = paste(dC$Q5, dC$Q448, dC$Q490, dC$Q574, dC$Q532)
whatAdvantages[whatAdvantages=="    "] <- NA

# Why don't you see any advantages to eating TE products?
# open-ended
whyNoAdvantages = rep(NA, nrow(dC))
whyNoAdvantages = paste(dC$Q65, dC$Q449, dC$Q491, dC$Q575, dC$Q533)
whyNoAdvantages[whyNoAdvantages=="    "] <- NA

# How much do you trust conventional products from recognizable brands, such as Tyson?
# factor
trustConventional = rep(NA, nrow(dC))
trustConventional = paste(dC$Q7, dC$Q450, dC$Q492, dC$Q576, dC$Q534)
trustConventional = as.numeric(trustConventional)

# If you can, name up to 5 words that come to mind when you think of conventional products-Word 1
# open-ended
word1convent = rep(NA, nrow(dC))
word1convent = paste(dC$Q11_1_TEXT, dC$Q451_1_TEXT, dC$Q493_1_TEXT, dC$Q577_1_TEXT, dC$Q535_1_TEXT)
word1convent[word1convent=="    "] <- NA

# If you can, name up to 5 words that come to mind when you think of conventional products-Word 2
# open-ended
word2convent = rep(NA, nrow(dC))
word2convent = paste(dC$Q11_2_TEXT, dC$Q451_2_TEXT, dC$Q493_2_TEXT, dC$Q577_2_TEXT, dC$Q535_2_TEXT)
word2convent[word2convent=="    "] <- NA

# If you can, name up to 5 words that come to mind when you think of conventional products-Word 3
# open-ended
word3convent = rep(NA, nrow(dC))
word3convent = paste(dC$Q11_3_TEXT, dC$Q451_3_TEXT, dC$Q493_3_TEXT, dC$Q577_3_TEXT, dC$Q535_3_TEXT)
word3convent[word3convent=="    "] <- NA

# If you can, name up to 5 words that come to mind when you think of conventional products-Word 4
# open-ended
word4convent = rep(NA, nrow(dC))
word4convent = paste(dC$Q11_4_TEXT, dC$Q451_4_TEXT, dC$Q493_4_TEXT, dC$Q577_4_TEXT, dC$Q535_4_TEXT)
word4convent[word4convent=="    "] <- NA


# If you can, name up to 5 words that come to mind when you think of conventional products-Word 5
# open-ended
word5convent = rep(NA, nrow(dC))
word5convent = paste(dC$Q11_5_TEXT, dC$Q451_5_TEXT, dC$Q493_5_TEXT, dC$Q577_5_TEXT, dC$Q535_5_TEXT)
word5convent[word5convent=="    "] <- NA

# How much do you trust TE products from recognizable brands, such as Tyson?
# factor
trustTE = rep(NA, nrow(dC))
trustTE = paste(dC$Q9, dC$Q452, dC$Q494, dC$Q578, dC$Q536)
trustTE = as.numeric(trustTE)

# If you can, name up to 5 words that come to mind when you think of TE products-Word 1
# open-ended
word1TE = rep(NA, nrow(dC))
word1TE = paste(dC$Q13_1_TEXT, dC$Q453_1_TEXT, dC$Q495_1_TEXT, dC$Q579_1_TEXT, dC$Q537_1_TEXT)
word1TE[word1TE=="    "] <- NA

# If you can, name up to 5 words that come to mind when you think of TE products-Word 2
# open-ended
word2TE = rep(NA, nrow(dC))
word2TE = paste(dC$Q13_2_TEXT, dC$Q453_2_TEXT, dC$Q495_2_TEXT, dC$Q579_2_TEXT, dC$Q537_2_TEXT)
word2TE[word2TE=="    "] <- NA

# If you can, name up to 5 words that come to mind when you think of TE products-Word 3
# open-ended
word3TE = rep(NA, nrow(dC))
word3TE = paste(dC$Q13_3_TEXT, dC$Q453_3_TEXT, dC$Q495_3_TEXT, dC$Q579_3_TEXT, dC$Q537_3_TEXT)
word3TE[word3TE=="    "] <- NA

# If you can, name up to 5 words that come to mind when you think of TE products-Word 4
# open-ended
word4TE = rep(NA, nrow(dC))
word4TE = paste(dC$Q13_4_TEXT, dC$Q453_4_TEXT, dC$Q495_4_TEXT, dC$Q579_4_TEXT, dC$Q537_4_TEXT)
word4TE[word4TE=="    "] <- NA

# If you can, name up to 5 words that come to mind when you think of TE products-Word 5
# open-ended
word5TE = rep(NA, nrow(dC))
word5TE = paste(dC$Q13_5_TEXT, dC$Q453_5_TEXT, dC$Q495_5_TEXT, dC$Q579_5_TEXT, dC$Q537_5_TEXT)
word5TE[word5TE=="    "] <- NA


# How much would you trust TE products if they came from a new brand that you hadn't heard of?
# factor
trustTEnewBrand = rep(NA, nrow(dC))
trustTEnewBrand = paste(dC$Q10, dC$Q454, dC$Q496, dC$Q580, dC$Q538)
trustTEnewBrand = as.numeric(trustTEnewBrand)
table(trustTEnewBrand, exclude=NULL)

# Here's a fun question - If you could rename TE product to anything else, what would it be...
# PURE 
# factor
renameTEpure = rep(NA, nrow(dC))
renameTEpure = paste(dC$Q14_1, dC$Q455_1, dC$Q497_1, dC$Q581_1)
renameTEpure = as.numeric(renameTEpure)
table(renameTEpure, exclude=NULL)

# Here's a fun question - If you could rename TE product to anything else, what would it be...
# CLEAN 
# factor
renameTEclean = rep(NA, nrow(dC))
renameTEclean = paste(dC$Q14_2, dC$Q497_2, dC$Q581_2, dC$Q539_2)
renameTEclean = as.numeric(renameTEclean)
table(renameTEclean, exclude=NULL)

# Here's a fun question - If you could rename TE product to anything else, what would it be...
# SAFE 
# factor
renameTEsafe = rep(NA, nrow(dC))
renameTEsafe = paste(dC$Q14_3, dC$Q455_3, dC$Q497_3, dC$Q539_3)
renameTEsafe = as.numeric(renameTEsafe)
table(renameTEsafe, exclude=NULL)

# Here's a fun question - If you could rename TE product to anything else, what would it be...
# CULTURED 
# factor
renameTEcultured = rep(NA, nrow(dC))
renameTEcultured = paste(dC$Q455_2, dC$Q497_4, dC$Q581_3, dC$Q539_1)
renameTEcultured = as.numeric(renameTEcultured)
table(renameTEcultured, exclude=NULL)

# Here's a fun question - If you could rename TE product to anything else, what would it be...
# MEAT 2.0 
# factor
renameTEmeat20 = rep(NA, nrow(dC))
renameTEmeat20 = paste(dC$Q14_4, dC$Q455_4, dC$Q581_4, dC$Q539_4)
renameTEmeat20 = as.numeric(renameTEmeat20)
table(renameTEmeat20, exclude=NULL)

# If you have any ideas for names for TE products, please tell us below!-Idea 1
# open-ended
renameIdea1 = rep(NA, nrow(dC))
renameIdea1 = paste(dC$Q15_1_TEXT, dC$Q456_1_TEXT, dC$Q498_1_TEXT, dC$Q582_1_TEXT, dC$Q540_1_TEXT)
renameIdea1[renameIdea1=="    "] <- NA

# If you have any ideas for names for TE products, please tell us below!-Idea 2
# open-ended
renameIdea2 = rep(NA, nrow(dC))
renameIdea2 = paste(dC$Q15_2_TEXT, dC$Q456_2_TEXT, dC$Q498_2_TEXT, dC$Q582_2_TEXT, dC$Q540_2_TEXT)
renameIdea2[renameIdea2=="    "] <- NA

# If you have any ideas for names for TE products, please tell us below!-Idea 3
# open-ended
renameIdea3 = rep(NA, nrow(dC))
renameIdea3 = paste(dC$Q15_3_TEXT, dC$Q456_3_TEXT, dC$Q498_3_TEXT, dC$Q582_3_TEXT, dC$Q540_3_TEXT)
renameIdea3[renameIdea3=="    "] <- NA



# CURRENT DIET
# Which of these best describes what you currently eat?
# Q20

currentDiet <-  rep(NA, nrow(dC))

currentDiet[dC$Q20==1] <- "Atkins"
currentDiet[dC$Q20==2] <- "Paleolithic"
currentDiet[dC$Q20==3] <- "Pescatarian"
currentDiet[dC$Q20==4] <- "Vegetarian"
currentDiet[dC$Q20==5] <- "Vegan"
currentDiet[dC$Q20==6] <- "Meat Reduction"
currentDiet[dC$Q20==7] <- "Mediterranean"
currentDiet[dC$Q20==8] <- "No specific diet"
currentDiet[dC$Q20==9] <- "Other"
currentDiet[dC$Q20==""] <- NA
table(currentDiet, exclude=NULL)
# 
# currentDietLevels <- c("Atkins", "Paleolithic", "Pescatarian", "Vegetarian", "Vegan", "Meat Reduction", "Mediterranean", "No specific diet", "Other") 
# 
# currentDiet <- factor(dC$Q20, levels=currentDietLevels)

# create binary variable of meat reduction diets
currentMeatReductionDiet <- rep(NA, nrow(dC))
currentMeatReductionDiet[currentDiet=="Pescatarian"] <- 1
currentMeatReductionDiet[currentDiet=="Vegetarian"] <- 1
currentMeatReductionDiet[currentDiet=="Vegan"] <- 1
currentMeatReductionDiet[currentDiet=="Meat Reduction"] <- 1
currentMeatReductionDiet[currentDiet=="Atkins" | currentDiet=="Paleolithic" | currentDiet=="Mediterranean" | currentDiet=="No specific diet" | currentDiet=="Other"] <- 0

table(currentMeatReductionDiet, exclude=NULL)

# CURRENT VEG
# Krystal created this variable
# 1 = currently identifies with a  vegetarian or vegan diet
# 0 = idenfities with another diet or didn't indicate anything 

currentVegDiet <- rep(NA, nrow(dC))
currentVegDiet[currentDiet=="Vegetarian"] <- 1
currentVegDiet[currentDiet=="Vegan"] <- 1
currentVegDiet[currentDiet=="Atkins" | currentDiet=="Paleolithic" | currentDiet=="Mediterranean" | currentDiet=="No specific diet" | currentDiet=="Other" | currentDiet=="Pescatarian" | currentDiet=="Meat Reduction"] <- 0

table(currentVegDiet, exclude=NULL)



# LENGTH OF CURRENT DIET
# When did you start your current diet? A rough date is fine.
# Q22_1_TEXT
lengthCurrentDietYears = dC$Q22_1_TEXT
acceptable_range = as.character(seq(1916, 2016, by=1))
lengthCurrentDietYears[!lengthCurrentDietYears %in% acceptable_range | is.na(lengthCurrentDietYears)] <- NA

# cross checks new column with original
cbind(dC$Q22_1_TEXT, lengthCurrentDietYears)

# changes to numeric
lengthCurrentDietYears = as.numeric(lengthCurrentDietYears)
lengthCurrentDietYears = 2016-lengthCurrentDietYears


# CURRENT MEAT CONSUMPTION
# Q233: How frequently do you currently eat meat (including chicken, pork, beef, and fish)?
currentMeatConsumption <- rep(NA, nrow(dC))

currentMeatConsumption[dC$Q233==1] <- "Never"
currentMeatConsumption[dC$Q233==2] <- "0-3 times/week"
currentMeatConsumption[dC$Q233==6] <- "3 times/day"
currentMeatConsumption[dC$Q233==7] <- "More than 3 times/day"
currentMeatConsumption[dC$Q233==8] <- "4-6 times/week"
currentMeatConsumption[dC$Q233==9] <- "once/day"
currentMeatConsumption[dC$Q233==10] <- "2 times/day"
currentMeatConsumption[dC$Q233==""] <- NA
table(currentMeatConsumption, exclude=NULL)

# currentMeatConsumptionLevels <- c("Never","0-3 times/week", "3 times/week", "More than 3 times/day", "4-6 times/week", "once/day", "2 times/day")
# currentMeatConsumption <- factor(dC$Q233, levels=currentMeatConsumptionLevels)



# PAST DIET
# In the past, have you ever identified with any of the following diets?
# For approximately how long did you identify with an x diet?


# pastDiet <-  rep(NA, nrow(dC))
# 
# pastDiet[dC$Q24_1==1] <- "Atkins"
# pastDiet[dC$Q24_2==1] <- "Paleolithic"
# pastDiet[dC$Q24_3==1] <- "Pescatarian"
# pastDiet[dC$Q24_4==1] <- "Vegetarian"
# pastDiet[dC$Q24_5==1] <- "Vegan"
# pastDiet[dC$Q24_6==1] <- "Meat Reduction"
# pastDiet[dC$Q24_7==1] <- "Mediterranean"
# pastDiet[dC$Q24_8==1] <- "No specific diet"
# pastDiet[dC$Q24_9==1] <- "Other"
# pastDiet[dC$Q20==""] <- NA
# table(currentDiet, exclude=NULL)


# IDENTIFIED WITH MEAT REDUCTION DIET IN THE PAST
# Krystal created this variable
# 1 = identified with pescatarian, vegetarian, vegan, or meat reduction
# 0 = idenfitied with another diet or didn't indicate anything 

pastMeatReductionDiet <- rep(0, nrow(dC))
pastMeatReductionDiet[dC$Q24_3==1] <- 1
pastMeatReductionDiet[dC$Q24_4==1]<- 1
pastMeatReductionDiet[dC$Q24_5==1] <- 1
pastMeatReductionDiet[dC$Q24_6==1] <- 1

table(pastMeatReductionDiet, exclude=NULL)

# IDENTIFIED WITH A VEGAN OR VEG DIET IN PAST
# Krystal created this variable
# 1 = identified with vegetarian, vegan in past

# 0 = idenfitied with another diet or didn't indicate anything
pastVegDiet <- rep(0, nrow(dC))
pastVegDiet[dC$Q24_4==1]<- 1
pastVegDiet[dC$Q24_5==1] <- 1

table(pastVegDiet, exclude=NULL)




# AGE
# What is your age?
# Q16
age = dC$Q16
age[age==""] <- NA

# checks that there are no unusual values:
table(age, exclude=NULL)




# GENDER
# What is your gender?
# (binary)
# Variable: "female"
# Description: female=1, male=0, other=NA.
# Q32

female <- rep(NA, nrow(dC))

# associates each response with either f, m, other, or missing.
unique(dC$Q32)

f = c("female", "Female", "FEMALE", "femaile", "female ", "F", "f", "Female ", "fem", "f emale", "Lady", "girl", "Females", "femail", "femalw")
m = c("Male", "male",  "m", "M", "MALE", "male ", "Male ", "Make", "Dude",  "Mail", "mle", "woman", "masculine (or if you mean \"sex,\" male)", "make", "Male (sex also = male)", "man") 
other = c("Other", "nonbinary", "trans male", "Agender", "Genderqueer", "Cis Woman ", "Cis-Female", "mLW", "cis female", "Agender, masculine presenting", "Fluid")
missing = c(41, 43, 57, 48, 27,  "", 23, 1987, 35, 24,29, 30, 58, 34)


female[which(dC$Q32 %in% f)] <- 1
female[which(dC$Q32 %in% m)] <- 0

#female <- factor(female)
# table(dC$Q32, female, exclude=NULL)
# table(female, exclude=NULL)

# EUDCATION
# Q17: What is the highest grade of school or the highest degree you have completed?
# (unordered factor).
# Variable: "educ".
# Description: education level of respondent.

# changes from numeric to string
educ <- rep(NA, nrow(dC))
educ[dC$Q17==1] <- "Less than grade 12, no diploma"
educ[dC$Q17==2] <- "High school diploma (or equivalent)"
educ[dC$Q17==3] <- "Some education past high school"
educ[dC$Q17==4] <- "Associate's degree or other non-Bachelor degree"
educ[dC$Q17==5] <- "Bachelor's degree"
educ[dC$Q17==6] <- "Graduate or professional degree"
educ[dC$Q17==""] <- NA

table(educ, exclude=NULL)

# class(dC$Q17)
# educLevels <- c("Less than grade 12, no diploma", "High school diploma (or equivalent)", "Some education past high school", "Associate's degree or other non-Bachelor degree", "Bachelor's degree", "Graduate or professional degree") 
# educ <- factor(dC$Q17, levels=educLevels)


# INCOME
# Q18: What is your household income?
# Variable: "income"
income <- rep(NA, nrow(dC))
income[dC$Q18==1] <- "Less than $24,999"
income[dC$Q18==2] <- "$25,000 - $49,999"
income[dC$Q18==3] <- "$50,000 - $74,999"
income[dC$Q18==4] <- "$75,000 - $99,999"
income[dC$Q18==5] <- "$100,000 or more"
income[dC$Q18==""] <- NA

table(income, exclude=NULL)



# ADDS TREATMENT GROUP

treatmentGroup = dC$treatment_group



# RACE/ETHNICITY
# What race/ethnicity do you consider yourself
# (factor)
# Variable: "race"
# Description: race/ethnicity of respondent.
# Q26: What is your race/ethnicity? (e.g. White/caucasian, Hispanic, / Arabic, ...)
# associates each response with either f, m, other, or missing.

raceAfricanAmOrBlack = rep(0, nrow(dC))
raceAfricanAmOrBlack[dC$raceAfricanAmOrBlack=="African American or Black"] = 1

raceAsian = rep(0, nrow(dC))
raceAsian[dC$raceAsian=="Asian"] = 1
raceAsian[dC$raceOther=="FILIPINO" | dC$raceOther=="Filipino"] <- 1

raceWhite = rep(0, nrow(dC))
raceWhite[dC$raceWhite=="European/Causasian"] = 1
raceWhite[dC$raceOther=="white"| dC$raceOther=="White"| dC$raceOther=="White/Caucasian" | dC$raceOther=="Caucasian" | dC$raceOther=="caucasian (is spelled wrong)" | dC$raceOther=="caucasion"] <- 1

raceIndian = rep(0, nrow(dC))
raceIndian[dC$raceIndian=="Indian"] = 1

raceMiddleEast = rep(0, nrow(dC))
raceMiddleEast[dC$raceMiddleEast=="Middle Eastern"] = 1

raceLatino = rep(0, nrow(dC))
raceLatino[dC$raceLatino=="Latino, Hispanic, or Spanish"] = 1

raceNativeAm = rep(0, nrow(dC))
raceNativeAm[dC$raceNativeAm=="Native American"] = 1

racePacificIsl = rep(0, nrow(dC))
racePacificIsl[dC$racePacificIsl=="Pacific Islander"] = 1

raceBiracial = rep(0, nrow(dC))
raceBiracial[dC$raceOther=="mixed" | dC$raceOther=="white/odawal" | dC$raceOther=="biracial" | dC$raceOther=="Mixed" | dC$raceOther=="Caucasian / Hispanic "| dC$raceOther=="multiracial" | dC$raceOther=="Mixed Race"| dC$raceOther=="Italian & Mexican American"| dC$raceOther=="mixed race" | dC$raceOther=="Multiracial"] <- 1

raceOther = rep(0, nrow(dC))
raceOther[dC$raceOther=="I do not identify with any race/ethnicity" | dC$raceOther=="Human" | dC$raceOther=="Italian American" | dC$raceOther=="Portuguese" | dC$raceOther=="Jewish" | dC$raceOther=="Cape Verdean"] <- 1


# STATE VARIABLE
state = dC$stateChar

# WHERE LIVE VARIABLE (rural, urban, city)
whereLive = dC$whereLive

# RELIGION VARIABLES
religion = dC$religion
religionOther = dC$religionOther


# -------------------------	#
# creates a subsetted dataframe containing all covariates of interest.

dfCovar <- cbind(dC[,c("respID")], treatmentGroup, haveConcerns, whatConcerns, whyNoConcerns, seeAdvantages, whatAdvantages, 
                  whyNoAdvantages, trustConventional, word1convent, word2convent, word3convent, 
                  word4convent, word5convent, trustTE, word1TE, word2TE, word3TE, word4TE, 
                  word5TE, trustTEnewBrand, renameTEpure, renameTEclean, renameTEsafe, 
                  renameTEcultured, renameTEmeat20, renameIdea1, renameIdea2, renameIdea3, 
                  currentDiet, currentMeatReductionDiet, currentVegDiet, lengthCurrentDietYears,
                  currentMeatConsumption, pastMeatReductionDiet, pastVegDiet, age, female, 
                  educ, income, raceAfricanAmOrBlack, raceAsian, raceWhite, raceIndian,
                  raceMiddleEast, raceLatino, raceNativeAm, racePacificIsl, raceOther,
                  state, whereLive, religion, religionOther)
dfCovar <- as.data.frame(dfCovar)
colnames(dfCovar)[1] <- "respID"
head(dfCovar)


# -----------------------------------------------------------------	#
# 	MERGES TOGETHER THE CONJOINT TASKS, OUTCOMES, and COVARIATES 	#
# -----------------------------------------------------------------	#
# NOTE: each row is a respondent-task-profile.

# merges dfConj with dfOutcomes.
df <- merge(dfConj, dfOutcomes, by=c("respID", "taskNum", "profileNum", "productNum"))
# NOTE: it is only strictly necessary to merge on respID and charityNum, but merging on all 4 prevents these columns from being duplicated in the merge. 
colnames(df)
dim(df)
head(df)


# merges df with dfCovar. 
df <- join(df, dfCovar, by="respID", type="left", match="all")
dim(df)
head(df, 20)
colnames(df)



# ---------------------------------------------	#
# 				CROSS-CHECKS 					#
# ---------------------------------------------	#

# -------------------------	#
# checks tasks.

# cross-check: number of rows in df should be equal to number of rows in dGWWC * tasks * prof
nrow(df)
nrow(df) == nrow(dC)*tasks*prof # should return TRUE.


# ------------------------------------------------------------------ #
# ------------------------------------------------------------------ #
# 			  	EXPORTS THE FINAL DATAFRAME FOR ANALYSIS  			 #
# ------------------------------------------------------------------ #
# ------------------------------------------------------------------ #

# write.csv(df, "data/culturedDataForanalysis.csv", row.names=FALSE)
save(df, file="data/chickenDataForAnalysis.csv.Rdata")
write.csv(df, "data/chickenDataForAnalysis.csv", row.names=FALSE)


# ------------------------------------------------------------------ #
# ------------------------------------------------------------------ #
# 						END OF DATA CLEANING 					     #
# ------------------------------------------------------------------ #
# ------------------------------------------------------------------ #




