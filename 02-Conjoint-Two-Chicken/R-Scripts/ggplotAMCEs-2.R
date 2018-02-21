# ---------------------------------------------	#
# FILE: ggplotAMCEs.R
# DESCRIPTION: the two functions in this file create a color-coded 
# dotchart of the AMCEs from a conjoint survey experiment. 
# 
# A working example is provided at the end of the file using a randomly generated dataset. 
# ---------------------------------------------	#
library(stringr)

# ---------------------------------------------	#
# Function: prepForPlot.
# Usage: amceCRSE <- prepForPlot(df, coeftestResult, attributes)
# ------------ #
# DESCRIPTION:
# This function takes in a coeftest object (as returned from a coeftest() result) and prepares it to be plotted for the function ggplotAMCEs by converting it to a dataframe, dropping the intercept, adding the baseline variable names to the dataframe, re-ordering the rows, and creating 95% confidence intervals.  
# 
# This function returns a dataframe that is ready to be plotted by ggplotAMCEs.
# 
# Parameters:
# 	df (dataframe): dataframe from which regression results were produced.
# 	coeftestResult (coeftest): coeftest object containing regression results. 
# 	attributes (string vector): vector of strings containing the names of the grouping variables (i.e. attributes in a conjoint study).
# 	createCI (boolean): boolean for whether to add 95% confidence intervals to the output. TRUE by default. 
# ---------------------------------------------	#
prepForPlot <- function(df, coeftestResult, sampleName, attributes, createCI=TRUE) {
	# preps the results to be plotted.
	resultsToPlot <- data.frame(rownames(coeftestResult),coeftestResult[,1:4], sample=sampleName)  # adds coefficient names and converts to dataframe.
	resultsToPlot <- resultsToPlot[-1,]  # drops the intercept
	colnames(resultsToPlot) <- c("var", "estimate", "se", "t.statistic", "p.value", "sample")

	# adds the baseline categories to the dataframe so that they are included in the plotted results.
	baselines <- vector()
	rowOrder <- vector()
	attrCol <- vector()
	for (i in 1:length(attributes)) {
		# creates baseline variable name for attribute i.
		baselines[i] <- paste(attributes[i], levels(df[,attributes[i]])[1], sep="")
		# stores the row order for attribute i.
		rowOrder <- c(rowOrder,paste(attributes[i],levels(df[,attributes[i]]), sep=""))
		# stores the attribute name for use in assigning colors to the plotted results.
		attrCol <- c(attrCol,rep(attributes[i], length(levels(df[,attributes[i]]))))
	}

	# converts baselines into a dataframe with estimate=0 and other columns  equal to NA.
	baselines <- data.frame("var"=baselines, "estimate"=0, "se"=NA, "t.statistic"=NA, "p.value"=NA, row.names=baselines, sample=sampleName)

	# adds baselines to results dataframe and orders according to the reverse of rowOrder.
	resultsToPlot <- rbind(resultsToPlot, baselines)
	resultsToPlot <- resultsToPlot[rev(rowOrder),]

	# rename row names
	for (i in 1:length(attributes)) {
		resultsToPlot$var <- str_replace_all(resultsToPlot$var, attributes[i], "")	
	}

	# replace html pounds character with GBP
	# resultsToPlot$var <- str_replace_all(resultsToPlot$var, "&#163", "GBP")	

	# adds the attribute name to each row (for use in plot colors) and converts the coefficient names to a factor variable (for use in colors).
	resultsToPlot$attribute <- rev(attrCol)
	resultsToPlot$var <- factor(resultsToPlot$var, levels=as.character(resultsToPlot$var))

	# creates upper and lower 95% confidence intervals.
	if (createCI==TRUE) {
		resultsToPlot$lowerCI <- resultsToPlot$estimate - 1.96*resultsToPlot$se
		resultsToPlot$upperCI <- resultsToPlot$estimate + 1.96*resultsToPlot$se
	}
	
	return(resultsToPlot)
}


# ---------------------------------------------	#
# FUNCTION: ggplotAMCEs
# USAGE: p1 <- ggplotAMCEs(df, coefNames, effect, lowerCI, upperCI, colorFactor, effectName, title, ylab)
# ------------ #
# DESCRIPTION:
# This function takes in a dataframe containing regression results and creates a color-coded dotchart including errorbars. 
# 
# Parameters:
# 	df1 (dataframe): dataframe containing the regression results.
# 	df2 (dataframe): dataframe containing the regression results. NULL by default.
# 	coefNames (string): column name in df in which the variable names are stored.
# 	effect (string): column name in df in which the coefficients are stored.
# 	lowerCI (string): column name in df in which the lower confidence interval is stored.
# 	upperCI (string): column name in df in which the upper confidence interval is stored.
# 	colorFactor (string): column name in df which groups the coefficients (to be used for coloring the plotted lines by group). 
# 	effectName (string): name of the outcome variable (to be used as label for x-axis). 
# 	title (string): plot title. NULL by default.
# 	ylab (string): y-axis label. NULL by default.
# 	xlimits: limits for x-axis
# 
# ---------------------------------------------	#
ggplotAMCEs <- function(df1, df2=NULL, coefNames, effect, lowerCI, upperCI, colorFactor, effectName, title=NULL, ylab=NULL, xlimits) {
	if (is.null(df2)) {
		df <- df1
		plot <- ggplot(df, aes_string(x=effect, y=coefNames, colour=colorFactor)) +
			facet_grid(attribute~., scales="free_y", space="free_y") +
			geom_vline(xintercept=0, color="gray70", size=0.3, linetype="solid") +
			geom_point() +
			geom_errorbarh(data=df, aes_string(x=effect, y=coefNames, xmin=lowerCI, xmax=upperCI, colour=colorFactor),height=0, size=0.4) +
			scale_x_continuous(name=effectName, limits=xlimits) +  # breaks=round(seq(-0.6, 0.6, by=0.2),1), limits=c(-0.65, 0.25)
			# scale_y_discrete(expand=c(0.1, 0.1)) +
			# theme(strip.text.y = element_text(size = 10, angle = 0)) +
			theme(text = element_text(size=10)) +  
			labs(y=ylab, title=title) +
			guides(colour=FALSE) + 
			theme_bw()
			# coord_flip() 		
	} else {
		df <- rbind(df1, df2)
		plot <- ggplot(df, aes_string(x=effect, y=coefNames, colour=colorFactor)) +
			facet_grid(attribute~sample, scales="free_y", space="free_y") +
			geom_vline(xintercept=0, color="gray70", size=0.3, linetype="solid") +
			geom_point() +
			geom_errorbarh(data=df, aes_string(x=effect, y=coefNames, xmin=lowerCI, xmax=upperCI, colour=colorFactor),height=0, size=0.4) +
			scale_x_continuous(name=effectName, limits=xlimits) +  # breaks=round(seq(-0.6, 0.6, by=0.2),1), limits=c(-0.65, 0.25)
			# scale_y_discrete(expand=c(0.1, 0.1)) +
			# theme(strip.text.y = element_text(size = 10, angle = 0)) + 
			theme(text = element_text(size=10)) + 
			labs(y=ylab, title=title) +
			guides(colour=FALSE) + 
			theme_bw()
			# coord_flip() 		
	}
	return(plot)	
}
	
# ------------ #
# EXAMPLE: 

# library(foreign)
# library(sandwich)
# library(lmtest)
# library(ggplot2)

# # Creates a random dataset:
# set.seed(444)
# n <- 2000
# x <- as.factor(round(runif(n, min=0,max=4), 0))
# z <- as.factor(round(runif(n, min=0,max=5), 0))
# e <- rnorm(n, 0, 50)
# coef <- c(500, rnorm(length(levels(x))-1, 0, 10), rnorm(length(levels(z))-1, 30, 15))
# y <- round(model.matrix(~ x + z, data=data.frame(x,z))%*%coef + e, 0)
# df <- data.frame(y, x, z)

# # stores the regression output:
# lmOut <- lm(y ~ x + z, data=df)
# lmOutRobust <- coeftest(lmOut, vcov=vcovHC(lmOut, type="HC0"))
# lmOutRobust

# # prepares the output for plotting:
# lmOutRobust <- prepForPlot(df, lmOutRobust, c("x", "z"))

# # plots the results:
# ggplotAMCEs(lmOutRobust, "var", "estimate", "lowerCI", "upperCI", "attribute", "y", "TITLE", "")

 
# ------------ #
# EXAMPLE FROM GWWC STUDY: 
# outcome <- "wouldDonate"
# effectName <- "Mean Difference in Pr(would donate to charity)"
# attributes <- c("Main.charitable.cause", "Are.financial.records.available.online.", "Number.of.other.survey.respondents.who.selected.this.charity", "Recipient.location", "Size.of.donation.required.to.save.one.life.year.", "Administrative.costs")
# formula <- formula(paste(outcome,"~",paste(attributes,collapse="+"), sep=""))
# 
# # computes the main AMCE.
# amce <- lm(formula, data=dgwwc)
# amceCRSE <- coeftest(amce, cluster = dgwwc$respID)
# amceCRSE
# 
# amceCRSE <- prepForPlot(dgwwc, amceCRSE, attributes)
# 
# plots the result.
# png("plots/gwwcPrelimResult.png", width=3000, height=2000, res=200)
# p1 <- ggplotAMCEs(amceCRSE, "var", "estimate", "lowerCI", "upperCI", "attribute", effectName, "GWWC AMCE Results", ylab="")
# p1
# dev.off()
# ------------ #