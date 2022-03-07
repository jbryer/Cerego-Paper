# Load the data. See DataPrep.R for how this data was prepared.
# There are three data frames, math, biology1 (for students in the first
# half of the year), and biology2 (for students in the second half of the year).
# The biology course was revised and the new version started in July.
load('Data/CeregoData-Final.Rda')

set.seed(2112) # For reproducibility

# Load R packages
library(ggplot2)
library(mice)
library(MatchIt)
library(Matching)
library(party)
library(PSAgraphics)
library(psa)
library(cowplot)
library(reshape2)
library(psych)

source('R/cv.bal.psa.R')
source('R/psa.cerego.R')
source('R/missing.plot.R')
source('R/match.results.R')
source('R/plotting.R')

# Data structure
str(math)
str(biology1)
str(biology2)

# Minimum number of minutes a student would have to use Cerego to be considered
# in the treatment.
treat.minutes <- 5

##### Descriptive Stats ########################################################

nrow(math)
nrow(biology1)
nrow(biology2)

table(math$Treat, useNA = 'ifany')
table(biology1$Treat, useNA = 'ifany')
table(biology2$Treat, useNA = 'ifany')

mean(c(math$Age, biology1$Age, biology2$Age), na.rm = TRUE)
table(math$Ethnicity, useNA = 'ifany')
table(biology1$Ethnicity, useNA = 'ifany')
table(biology2$Ethnicity, useNA = 'ifany')

prop.table(table(c(math$Ethnicity, biology1$Ethnicity, biology2$Ethnicity), useNA = 'ifany'))


# Model formula for phase I to estimate propensity scores using logistic regression
psa.formula <- Treat ~ Gender + Ethnicity + Military + DegreeLevel + GPA + OverallGPA +
	TransferCredits + CreditRatio + EarnedCredits + Income + Age + DaysEnrolled +
	MothersEducation + FathersEducation + Employment + ESL + Pell + Repeat

##### Recoding #################################################################
# Recode missing values to zero for minutes using Cerego
for(i in paste0('Week', 1:8)) {
	math[is.na(math[,i]),i] <- 0
}

# Convert NA to 0 for minutes using Cerego
for(i in paste0('Week', 1:7)) {
	biology2[is.na(biology2[,i]),i] <- 0
}

biology1[is.na(biology1$DegreeLevel),]$DegreeLevel <- 'Non-Matriculate'
biology1[is.na(biology1$First),]$First <- 0
biology1[is.na(biology1$Second),]$Second <- 0

str(math[,all.vars(psa.formula)])

# Recode covariates
math$Income <- as.integer(math$Income)
math$MothersEducation <- as.integer(math$MothersEducation)
math$FathersEducation <- as.integer(math$FathersEducation)
math$Employment <- as.integer(math$Employment)
math$DegreeLevel <- as.character(math$DegreeLevel)
math[math$DegreeLevel %in% c('Non-Matriculate', 'Certificate'),]$DegreeLevel <- 'None'
math$DegreeLevel <- as.factor(math$DegreeLevel)
levels(math$Ethnicity)[2] <- 'Black'
math[!(math$Ethnicity %in% c('Black','Hispanic','White')),]$Ethnicity <- 'Other'
math$Ethnicity <- as.factor(as.character(math$Ethnicity))

biology1$Income <- as.integer(biology1$Income)
biology1$MothersEducation <- as.integer(biology1$MothersEducation)
biology1$FathersEducation <- as.integer(biology1$FathersEducation)
biology1$Employment <- as.integer(biology1$Employment)
biology1$DegreeLevel <- as.character(biology1$DegreeLevel)
biology1[biology1$DegreeLevel %in% c('Non-Matriculate', 'Certificate'),]$DegreeLevel <- 'None'
biology1$DegreeLevel <- as.factor(biology1$DegreeLevel)
levels(biology1$Ethnicity)[2] <- 'Black'
biology1[!(biology1$Ethnicity %in% c('Black','Hispanic','White')),]$Ethnicity <- 'Other'
biology1$Ethnicity <- as.factor(as.character(biology1$Ethnicity))

biology2$Income <- as.integer(biology2$Income)
biology2$MothersEducation <- as.integer(biology2$MothersEducation)
biology2$FathersEducation <- as.integer(biology2$FathersEducation)
biology2$Employment <- as.integer(biology2$Employment)
biology2$DegreeLevel <- as.character(biology2$DegreeLevel)
biology2[biology2$DegreeLevel %in% c('Non-Matriculate', 'Certificate'),]$DegreeLevel <- 'None'
biology2[is.na(biology2$DegreeLevel),]$DegreeLevel <- 'None'
biology2$DegreeLevel <- as.factor(biology2$DegreeLevel)
levels(biology2$Ethnicity)[2] <- 'Black'
biology2[!(biology2$Ethnicity %in% c('Black','Hispanic','White')),]$Ethnicity <- 'Other'
biology2$Ethnicity <- as.factor(as.character(biology2$Ethnicity))


##### Impute missing values ####################################################
missing.plot(math[,all.vars(psa.formula)[2:length(all.vars(psa.formula))]]) +
	ggtitle('Percent Missing Values: Mathematics')
ggsave('Figures/Missing-Math.pdf')
missing.plot(biology1[,all.vars(psa.formula)[2:length(all.vars(psa.formula))]]) +
	ggtitle('Percent Missing Values: Biology A')
ggsave('Figures/Missing-Bio1.pdf')
missing.plot(biology2[,all.vars(psa.formula)[2:length(all.vars(psa.formula))]]) +
	ggtitle('Percent Missing Values: Biology B')
ggsave('Figures/Missing-Bio2.pdf')

# Create shadow matrix to use later with PS estimates
math.shadow.matrix <- as.data.frame(
	is.na(math[,all.vars(psa.formula)[2:length(all.vars(psa.formula))]]))
names(math.shadow.matrix) <- paste0(names(math.shadow.matrix), '_miss')
bio1.shadow.matrix <- as.data.frame(
	is.na(biology1[,all.vars(psa.formula)[2:length(all.vars(psa.formula))]]))
names(bio1.shadow.matrix) <- paste0(names(bio1.shadow.matrix), '_miss')
bio2.shadow.matrix <- as.data.frame(
	is.na(biology2[,all.vars(psa.formula)[2:length(all.vars(psa.formula))]]))
names(bio2.shadow.matrix) <- paste0(names(bio2.shadow.matrix), '_miss')

math.mice <- mice(math[,all.vars(psa.formula)[2:length(all.vars(psa.formula))]], 
				  m=1, seed = 2112)
bio1.mice <- mice(biology1[,all.vars(psa.formula)[2:length(all.vars(psa.formula))]],
				  m=1, MaxNWts=2000, seed = 2112)
bio2.mice <- mice(biology2[,all.vars(psa.formula)[2:length(all.vars(psa.formula))]],
				  m=1, MaxNWts=2000, seed = 2112)

math.complete <- complete(math.mice)
math.complete$Treat <- math$Treat
math.complete$FinalAverage <- math$FinalAverage
math.complete$Section <- math$Section

bio1.complete <- complete(bio1.mice)
bio1.complete$Treat <- biology1$Treat
bio1.complete$FinalAverage <- biology1$FinalAverage
bio1.complete$Section <- biology1$Section

bio2.complete <- complete(bio2.mice)
bio2.complete$Treat <- biology2$Treat
bio2.complete$FinalAverage <- biology2$FinalAverage
bio2.complete$Section <- biology2$Section

# Remove the contrasts from the factors. This will keep the labels when dummy coding
# Also covert DegreeLevel to an integer such that None < Associate < Bacc < Master's
attributes(math.complete$Gender)$contrasts <- NULL
attributes(math.complete$Ethnicity)$contrasts <- NULL
attributes(math.complete$DegreeLevel)$contrasts <- NULL
math.complete$DegreeLevel <- as.integer(math.complete$DegreeLevel)
math.complete[math.complete$DegreeLevel == 4,]$DegreeLevel <- 0

attributes(bio2.complete$Gender)$contrasts <- NULL
attributes(bio2.complete$Ethnicity)$contrasts <- NULL
attributes(bio2.complete$DegreeLevel)$contrasts <- NULL
bio2.complete$DegreeLevel <- as.integer(bio2.complete$DegreeLevel)
bio2.complete[bio2.complete$DegreeLevel == 4,]$DegreeLevel <- 0

attributes(bio1.complete$Gender)$contrasts <- NULL
attributes(bio1.complete$Ethnicity)$contrasts <- NULL
attributes(bio1.complete$DegreeLevel)$contrasts <- NULL
bio1.complete$DegreeLevel <- as.integer(bio1.complete$DegreeLevel)
bio1.complete[bio1.complete$DegreeLevel == 4,]$DegreeLevel <- 0


##### Math #####################################################################
math.complete$Ethnicity <- relevel(math.complete$Ethnicity, ref='White')

math.lr.miss <- glm(Treat ~ ., 
					data=cbind(math.complete[,all.vars(psa.formula)], math.shadow.matrix),
					family=binomial)
# We are looking for any "_miss" variables that are statistically significant predictors
summary(math.lr.miss) # Good. Moving on

math.lr <- glm(psa.formula, data=math.complete, family=binomial)
math.complete$ps <- fitted(math.lr, math.complete)

# library(randomForest)
# math.rf <- randomForest(update.formula(psa.formula, factor(Treat) ~ .), data=math.complete)
# print(math.rf)
# math.tree <- rpart(update.formula(psa.formula, factor(Treat) ~ .), data=math.complete)
# plot(math.tree); text(math.tree)

summary(math.lr)
summary(math.complete$ps)

# Trying stepAIC. Looks to make balance worse.
# math.lr.aic <- stepAIC(math.lr)
# summary(math.lr.aic)
# math.complete$ps <- fitted(math.lr.aic)

ggplot(math.complete, aes(x=ps, fill=Treat)) + geom_density() + facet_wrap(~ Treat, ncol=1)
ggplot(math.complete, aes(x=Treat, y=ps)) + geom_boxplot()

ggplot(math.complete, aes(x=ps, color=Treat)) + geom_density() + facet_wrap(~ Gender, ncol=1)
ggplot(math.complete, aes(y=ps, x=Treat, color=Treat)) + geom_boxplot() + 
	coord_flip() + facet_wrap(~ Ethnicity, ncol=1)

# Loess plot
ggplot(math.complete, aes(x=ps, y=FinalAverage, color=Treat)) + 
	geom_point(alpha=.3) + geom_smooth(method='loess', se=FALSE) #+ facet_wrap(~ Gender, ncol=1)


# Check balance
tmp <- math.complete[,all.vars(psa.formula[2:length(all.vars(psa.formula))])]
tmp <- cv.trans.psa(tmp)[[1]]
pdf('Figures/Balance-Math.pdf')
par.orig <- par(mar=c(5.1,10,4.1,2.1))
math.bal <- cv.bal.psa(tmp, math.complete$Treat, math.complete$ps, strata=5, plot.strata=FALSE)
par(par.orig)
dev.off()

mb.math <- psa::MatchBalance(df = math.complete, 
						formu = psa.formula,
						exact=c('GPA','Gender','Ethnicity'),
						tolerance=0.5)
plot(mb.math)
summary(mb.math)

# Phase II of PSA
math.results <- psa.cerego(df = math, 
						   df.complete = math.complete,
						   out.cols = c(paste0('Quiz', 1:8), 'FinalAverage'),
						   treat.cols = c(paste0('Week', 1:8), 'Minutes') )
						   #by.cols=c('Ethnicity', 'Gender', 'Military', 'Employment'))

math.results.summary <- data.frame()
for(i in names(math.results)) {
	math.results.summary <- rbind(math.results.summary,
								  math.results[[i]]$summary)
}

# View(math.results.summary)

p.math.match <- cerego_matching_plot(math.results.summary) + 
	ggtitle('Average Treatment Effect using Matching: Mathematics')
p.math.match
ggplot2::ggsave('Figures/PSA-Math-Matching.pdf', width=10, height=4.5)

p.math.strata <- cerego_strata_plot(math.results.summary) +
	ggtitle('Average Treatment Effect using Stratification: Mathematics') 
p.math.strata
ggplot2::ggsave('Figures/PSA-Math-Strata.pdf', width=10, height=4.5)


# How often do students use Cerego
math.usage <- math[,paste0('Week', 1:8)]
math.usage$nweeks <- apply(math.usage, 1, function(x) { sum(x > 10) })
math.usage$FinalAverage <- math$FinalAverage

ggplot(math.usage, aes(x=factor(nweeks), y=..count..)) + geom_bar() + 
	xlab('Number of Modules Student used Cerego') + ylab('Count')

ggplot(math.usage[math.usage$nweeks > 0 & !is.na(math.usage$FinalAverage) & 
				  	math.usage$FinalAverage > 10,], 
	   aes(x=factor(nweeks), y=FinalAverage)) + 
	geom_boxplot() + xlab('Weeks Used Cerego') + ylab('Final Average')
ggsave('Figures/Math-FinalAverageByWeeks.png', width=10, height=4.5)
describeBy(math.usage$FinalAverage, group=math.usage$nweeks, mat=TRUE)

##### Biology 2 ################################################################
bio2.complete$Ethnicity <- relevel(bio2.complete$Ethnicity, ref='White')

bio2.lr.miss <- glm(Treat ~ ., 
					data=cbind(bio2.complete[,all.vars(psa.formula)], bio2.shadow.matrix),
					family=binomial)
# We are looking for any "_miss" variables that are statistically significant predictors
summary(bio2.lr.miss) 

bio2.lr <- glm(psa.formula, data=bio2.complete, family=binomial)
bio2.complete$ps <- fitted(bio2.lr, bio2.complete)

bio2.ps <- data.frame(ps=fitted(bio2.lr), ps.miss=fitted(bio2.lr.miss))
ggplot(bio2.ps, aes(x=ps, y=ps.miss)) + geom_point() + geom_smooth()

summary(bio2.lr)
summary(bio2.complete$ps)

# Check balance
tmp <- bio2.complete[,all.vars(psa.formula[2:length(all.vars(psa.formula))])]
tmp <- cv.trans.psa(tmp)[[1]]
pdf('Figures/Balance-Biology2.pdf')
par.orig <- par(mar=c(5.1,10,4.1,2.1))
bio2.bal <- cv.bal.psa(tmp, bio2.complete$Treat, bio2.complete$ps, strata=5, plot.strata=FALSE)
par(par.orig)
dev.off()

mb.bio2 <- psa::MatchBalance(df = bio2.complete, 
							formu = psa.formula,
							exact=c('Gender'),
							#exact=c('GPA','Gender','Ethnicity'),
							tolerance=0.5)
plot(mb.bio2)
summary(mb.bio2)

# Phase II of PSA
bio2.results <- psa.cerego(biology2, bio2.complete,
						   out.cols = c(paste0('Quiz', 1:7), 'FinalAverage'),
						   treat.cols = c(paste0('Week', 1:7), 'Minutes'),
						   nStrata=4, caliper=0.25, replace=FALSE, ties=TRUE, M=2,
						   by.cols=c('Gender'))

bio2.results.summary <- data.frame()
for(i in names(bio2.results)) {
	bio2.results.summary <- rbind(bio2.results.summary,
								  bio2.results[[i]]$summary)
}

# View(bio2.results.summary)

p.bio2.match <- cerego_matching_plot(bio2.results.summary) +
	ggtitle('Average Treatment Effect using Matching: Biology B')
p.bio2.match
ggplot2::ggsave('Figures/PSA-Bio2-Matching.pdf', width=10, height=4.5)

p.bio2.strat <- cerego_strata_plot(bio2.results.summary) +
	ggtitle('Average Treatment Effect using Stratification: Biology B')
p.bio2.strat
ggplot2::ggsave('Figures/PSA-Bio2-Strata.pdf', width=10, height=4.5)


##### Biology 1 ################################################################
bio1.complete$Ethnicity <- relevel(bio1.complete$Ethnicity, ref='White')

bio1.lr.miss <- glm(Treat ~ ., 
					data=cbind(bio1.complete[,all.vars(psa.formula)], bio1.shadow.matrix),
					family=binomial)
# We are looking for any "_miss" variables that are statistically significant predictors
summary(bio1.lr.miss) 

bio1.lr <- glm(psa.formula, data=bio1.complete, family=binomial)
bio1.complete$ps <- fitted(bio1.lr, bio1.complete)

summary(bio1.lr)
summary(bio1.complete$ps)

# Check balance
tmp <- bio1.complete[,all.vars(psa.formula[2:length(all.vars(psa.formula))])]
tmp <- cv.trans.psa(tmp)[[1]]
pdf('Figures/Balance-Biology1.pdf')
par.orig <- par(mar=c(5.1,10,4.1,2.1))
bio1.bal <- cv.bal.psa(tmp, bio1.complete$Treat, bio1.complete$ps, strata=5, plot.strata=FALSE)
par(par.orig)
dev.off()

# Phase II of PSA
bio1.results <- psa.cerego(biology1, bio1.complete,
						   out.cols = c('MidtermMC','FinalMC','FinalAverage'),
						   treat.cols = c('First','Second','Minutes'),
						   by.cols = c('Gender'),
						   # Since the treatment group is smaller for the second 
						   # half, the smaller strata size will ensure each 
						   # stratum has some students
						   nStrata = 3)

bio1.results.summary <- data.frame()
for(i in names(bio1.results)) {
	bio1.results.summary <- rbind(bio1.results.summary,
								  bio1.results[[i]]$summary)
}

# View(bio1.results.summary)

p.bio1.match <- cerego_matching_plot(bio1.results.summary) +
	ggtitle('Average Treatment Effect using Matching: Biology A')
p.bio1.match
ggplot2::ggsave('Figures/PSA-Bio1-Matching.pdf', width=6, height=4.5)


p.bio1.strat <- cerego_strata_plot(bio1.results.summary) +
	ggtitle('Average Treatment Effect using Stratification: Biology A')
p.bio1.strat
ggplot2::ggsave('Figures/PSA-Bio1-Strata.pdf', width=6, height=4.5)


##### Summary Table ############################################################

# Save logistic regression table
math.lr.sum <- summary(math.lr)
bio1.lr.sum <- summary(bio1.lr)
bio2.lr.sum <- summary(bio2.lr)

rn <- row.names(math.lr.sum$coefficients)
lr.sum <- cbind(
	math.lr.sum$coefficients[rn,3:4],
	bio1.lr.sum$coefficients[rn,3:4],
	bio2.lr.sum$coefficients[rn,3:4]
)
colnames(lr.sum) <- paste(c('Math','Math','Bio1','Bio1','Bio2','Bio2'),colnames(lr.sum))

write.csv(lr.sum, 'Tables/LRSummary.csv', row.names=TRUE)

# Save the summary table
summary.cols <- c('Outcome', 'Estimate', 'se', 't', 'p', 
				  'Estimate.Strata', 'se.Strata', 't.Strata', 'p.Strata', 
				  'meanTime', 'medianTime')

results.summary <- rbind(cbind('Subject'=rep('Mathematics', nrow(math.results.summary)),
							   math.results.summary[,summary.cols]),
						 cbind('Subject'=rep('Biology A', nrow(bio1.results.summary)),
						 	  bio1.results.summary[,summary.cols]),
						 cbind('Subject'=rep('Biology B', nrow(bio2.results.summary)),
						 	  bio2.results.summary[,summary.cols]))
results.summary

write.csv(results.summary, 'Tables/SummaryResults.csv', row.names=FALSE)

# Save combined figures
plot_grid(p.math.match + theme(legend.position='bottom'), 
		  p.math.strata + theme(legend.position='bottom'),
		  ncol=2, nrow=1)
cowplot::ggsave('Figures/PSA-Combined-Math.pdf', width=18, height=5)

plot_grid(p.bio2.match + theme(legend.position='bottom'), 
		  p.bio2.strat + theme(legend.position='bottom'), 
		  ncol=2, nrow=1)
cowplot::ggsave('Figures/PSA-Combined-Bio2.pdf', width=18, height=5)

plot_grid(p.bio1.match + theme(legend.position='bottom'),
		  p.bio1.strat + theme(legend.position='bottom'),
		  nrow=1, ncol=2)
cowplot::ggsave('Figures/PSA-Combined-Bio1.pdf', width=12, height=5)
				