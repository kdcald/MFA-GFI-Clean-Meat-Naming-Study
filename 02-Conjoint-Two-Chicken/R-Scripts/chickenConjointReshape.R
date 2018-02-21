


# ---------------------------------------------	#
# Function: convertTask
# Usage: d <- apply(dConj, MARGIN=1, FUN=function(x) convertTask(x))
# ------------- #
# This function takes in a vector (x) of n conjoint tasks (where each task has m profiles and each profile has k attributes) and returns a matrix with one row for
# each profile (i.e. n*m rows) and one column for each attribute (i.e. k columns).
# 
# This matrix contains all of the conjoint tasks that a single respondent was shown.
# 
# x: vector of conjoint tasks.
# tasks: number of tasks.
# prof: number of profiles per task.
# ---------------------------------------------	#

convertTask <- function(x, tasks, prof) {
  # creates an empty df to store responses for this respondent.
  thisResp <- data.frame(matrix(NA, ncol=length(attributes)+4, nrow=tasks*prof))
  colnames(thisResp) <- c(attributes, "respID", "taskNum", "profileNum", "productNum")
  thisResp["respID"] <- x[names(x)=="respID"]
  thisResp["taskNum"] <- rep(1:tasks, each=prof)
  thisResp["profileNum"] <- rep(1:prof, tasks)
  thisResp["productNum"] <- seq(1:(tasks*prof))
  
  # for each of the tasks...
  for (task in 1:tasks) {
    # stores the colname for the attribute.
    colname <- paste("^F.",task, sep="")
    
    # creates an empty df to store the responses for this task.
    thisTask <- data.frame(matrix(NA, ncol=prof, nrow=length(attributes)))
    rownames(thisTask) <- attributes
    
    # stores the actual responses of the respondent for this task.
    thisChoice <- x[grep(colname, names(x))]
    
    # for each of the attributes in this task...
    for (attr in 1:length(attributes)) {
      # stores the colname of the attribute.
      loc <- names(thisChoice)[grep(attributes[attr], thisChoice)]
      
      # stores the position of the attribute.
      pos <- substr(loc, nchar(loc), nchar(loc))
      
      # stores the column indices of the value for this attribute on this task for this respondent. 
      values <- grep(paste("^.+", pos, "$", sep=""), names(thisChoice))[-1] # note the [-1] kludge here.
      
      # stores these values in row attr of thisTask.
      thisTask[attr,] <- thisChoice[values]
    }
    
    thisTask <- t(thisTask)
    thisResp[(task*prof-1):(task*prof),1:length(attributes)] <- thisTask
  }
  return(thisResp)
}


# ---------------------------------------------	#
# Function: convertResponses
# Usage: apply(dGWWC[1:30,], MARGIN=1, FUN=function(x) convertResponses(x, tasks, nOutcomes))
# ------------- #
# This function takes in a vector (x) containing a single individual's survey responses 
# and returns a dataframe with one row for each conjoint task. 
# 
# 
# x: vector containing an individual's survey responses.
# tasks: number of tasks.
# prof: number of profiles.
# outcomeNames: names of outcome variables.
# dict: vector of column descriptions with rownames containing question numbers.
# ---------------------------------------------	#

convertResponses <- function(x, tasks, prof, outcomeNames, dict) {
  # creates an empty df to store the responses for this individual for each of the n tasks.
  thisIndiv <- data.frame(matrix(NA, nrow=tasks*prof, ncol=length(outcomeNames)+4))
  colnames(thisIndiv) <- c(outcomeNames, "respID", "taskNum", "profileNum", "productNum")  # note outcomeNames kludge here.
  thisIndiv["respID"] <- x[names(x)=="respID"]
  thisIndiv["taskNum"] <- rep(seq(1:tasks), 2)
  thisIndiv["profileNum"] <- c(rep(1, tasks), rep(2, tasks))
  thisIndiv["productNum"] <- c(seq(1,(tasks*prof-1), by=2), seq(2,(tasks*prof), by=2))
  
  
  if (x["treatment_number"]==1) {  #cultured
    outcomeColumns = c("Q35", "Q36", "Q37",
                       "Q40", "Q41", "Q42", 
                       "Q43", "Q44", "Q45",
                       "Q48", "Q47", "Q46", 
                       "Q53", "Q52", "Q51", 
                       "Q56", "Q55", "Q54", 
                       "Q57", "Q58", "Q59", 
                       "Q62", "Q61", "Q60")
  }
  
  if (x["treatment_number"]==2) {  #clean
    outcomeColumns = c("Q419", "Q420", "Q421",
                       "Q422", "Q423", "Q424", 
                       "Q425", "Q426", "Q427",
                       "Q429", "Q430", "Q431", 
                       "Q432", "Q433", "Q434", 
                       "Q435", "Q436", "Q437", 
                       "Q438", "Q439", "Q440", 
                       "Q441", "Q442", "Q443")
  }
  
  if (x["treatment_number"]==3) {  #meat 2.0
    outcomeColumns = c("Q461", "Q462", "Q463",
                       "Q464", "Q465", "Q466", 
                       "Q467", "Q468", "Q469",
                       "Q471", "Q472", "Q473", 
                       "Q474", "Q475", "Q476", 
                       "Q477", "Q478", "Q479", 
                       "Q480", "Q481", "Q482", 
                       "Q483", "Q484", "Q485")
  }
  
  if (x["treatment_number"]==4) {  #safe
    outcomeColumns = c("Q545", "Q546", "Q547",
                       "Q548", "Q549", "Q550", 
                       "Q551", "Q552", "Q553",
                       "Q555", "Q556", "Q557", 
                       "Q558", "Q559", "Q560", 
                       "Q561", "Q562", "Q563", 
                       "Q564", "Q565", "Q566", 
                       "Q567", "Q568", "Q569")
  }
  
  if (x["treatment_number"]==5) {  #pure
    outcomeColumns = c("Q503", "Q504", "Q505",
                       "Q506", "Q507", "Q508", 
                       "Q509", "Q510", "Q511",
                       "Q513", "Q514", "Q515", 
                       "Q516", "Q517", "Q518", 
                       "Q519", "Q520", "Q521", 
                       "Q522", "Q523", "Q524", 
                       "Q525", "Q526", "Q527")
  }
  
  
  theseOutcomes <- x[outcomeColumns] 
  mat <- matrix(theseOutcomes, ncol=3, byrow=TRUE)
  mat
  mat2 <- matrix(theseOutcomes, ncol=3, byrow=TRUE)
  mat2
  
  thisIndiv[,1:length(outcomeNames)] <- rbind(mat, mat2)
  
  return(thisIndiv)
  
}

# initDF <- function(names, nrows, ncols) {

# }

# ---------------------------------------------	#
# Function: reorderVector
# Usage: 
# ------------- #
# Description: 
# ---------------------------------------------	#
orderRankings <- function(x, split, colIndices) {
  # assigns short cause names (e.g. "HIV/AIDS") to the six causes.
  longNames <- dict[colIndices,]
  names(x) <- returnStringEnd(longNames, split)
  
  # re-arranges x
  x <- x[order(x)]
  
  # returns the names of the rows.
  return(names(x))
}

# ---------------------------------------------	#
# Function: returnStringEnd.
# Usage: substrings <- returnStringEnd(stringVect, split)
# ------------- #
# Description: 
# ---------------------------------------------	#
returnStringEnd <- function(stringVect, split){
  splitList <- strsplit(stringVect, split)
  result <- sapply(splitList, FUN=function(x) return(x[2]))
  return(result)
}






