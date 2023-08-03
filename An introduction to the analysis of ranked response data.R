# Tutorial from the paper "An introduction to the analysis of ranked response
# data"
# https://scholarworks.umass.edu/pare/vol27/iss1/7

library(readxl)
library(pmr)
library(PlackettLuce)
library(prefmod)
library(qvcalc)
library(smacof)

#READ AND PREPARE THE DATA#
#Experience: 1=0-5, 2-6-10, 3=11-15, 4-16-20, 5=21+
#Degree: 1=BA/BS, 2=MA/MS, 3=Specialist, 4=PhD
faculty.survey <- read_xlsx(path = "Finch_supp_data.xlsx")

# subset data and just get rankings
# each ranking in its own column
# one row per subject (n = 41)
faculty.rankings <- data.frame(faculty.survey[,1:6])

# count the number of times each particular rank order occurred.
faculty.rankings.agg <- pmr::rankagg(faculty.rankings)
colnames(faculty.rankings.agg)[1:6] <- names(faculty.rankings)
# This is needed for the pmr::destat() function

#DESCRIPTION OF THE SAMPLE#
faculty.desc <- destat(faculty.rankings.agg)
names(faculty.desc$mean.rank) <- names(faculty.rankings)
colnames(faculty.desc$pair) <- rownames(faculty.desc$pair) <- names(faculty.rankings)
rownames(faculty.desc$mar) <- names(faculty.rankings)
faculty.desc #DESCRIPTIVES

# mean.rank	
# Mean rank of the items.
 
# pair	
# The number of observations which the first item (row) is more preferred than
# the second item (column).

# mar 
# The number of observations which the item i (row) is ranked j (column).

sd(faculty.survey$contract)
sd(faculty.survey$salary)
sd(faculty.survey$health_care)
sd(faculty.survey$workload)
sd(faculty.survey$chair_support)
sd(faculty.survey$travel_budget)

#TEST FOR RANDOM MEAN#
# formula 2
null_mean <- rep(3.5, 6)
A <- ((12 * 41)/(6 * (6+1)))
chi <- A * sum((faculty.desc$mean.rank - null_mean)^2)
chi
pchisq(chi, 5, lower.tail = F)

# UMDS
#SMACOFF#
faculty.smacof <- smacof::smacofRect(faculty.rankings, itmax=1000)
# Fig 1
plot(faculty.smacof, plot.type="confplot", what="both")
plot(faculty.smacof, plot.type = "Shepard")

# Table 4
d.smacof$conf.row[2:4,]
d.smacof$conf.col
# The coefficients for the items also show that Salary and Health care
# were most closely associated with one another, as were Chair support and
# Travel budget. Contract and Workload had coefficients that differed from the
# other items and from one another. 

#MUTLIDIMENSIONAL PREFERENCE#

# The plot was created using the mdpref function from the pmr R library. This
# model explained approximately 55% of the variance in the rankings.
# CF: plot not actually in paper
mds2 <- pmr::mdpref(faculty.rankings.agg, rank.vector=TRUE) #2 dimensions
mds2$explain

pmr::mdpref(faculty.rankings.agg, rank.vector=T, ndim=3) #3 dimensions


#PlackettLuce analysis#
faculty.rankings2 <- PlackettLuce::as.rankings(faculty.rankings)
faculty.mod_mle <- PlackettLuce(faculty.rankings2, npseudo=0)
coef(faculty.mod_mle)
coef(faculty.mod_mle, log=FALSE)
# Table 6
summary(faculty.mod_mle) #CATEOGRY 1 WORTH IS THE REFERENCE
# Table 7
summary(faculty.mod_mle, ref=NULL) #MEAN WORTH IS THE REFERENCE
# Table 8
faculty.mod_mle.itempars <- psychotools::itempar(faculty.mod_mle, vcov=TRUE)
faculty.mod_mle.itempars

# evaluate performance (p. 12)
# ratio of deviance to degrees of freedom 
# whem model fits well, ratio is approximately 1
deviance(faculty.mod_mle)/faculty.mod_mle$df.residual


#PLACKETTLUCE MODEL WITH COVARIATES#
# See coefficients that begin with Beta1
# Table 9
faculty.survey$grad <- ifelse(faculty.survey$degree > 1, 1, 0)
summary(pmr::rol(faculty.rankings2,faculty.survey$experience))
summary(pmr::rol(faculty.rankings2,faculty.survey$grad))


#PLACKETTLUCE TREE#
faculty.n <- nrow(faculty.survey)
faculty.g <- PlackettLuce::group(faculty.rankings2, 
                                 index = rep(seq_len(faculty.n), 1))
faculty.tree <- pltree(faculty.g ~ grad + experience, 
                       data = faculty.survey, 
                       minsize = 2, maxdepth = 3)
faculty.tree
plot(faculty.tree)



# Extra code not covered in tutorial --------------------------------------


#COMPARE RANKINGS ACROSS GROUPS#
bachelors.rankings <- faculty.survey[ which(faculty.survey$degree==1),1:6]
graduates.rankings <- faculty.survey[ which(faculty.survey$degree>1),1:6]

bachelors.rankings.agg <- rankagg(bachelors.rankings)
graduates.rankings.agg <- rankagg(graduates.rankings)
bach.ranks <- destat(bachelors.rankings.agg)
grad.ranks <- destat(graduates.rankings.agg)
chisq.test(cbind(as.vector(bach.ranks$mar), as.vector(grad.ranks$mar)))
fisher.test(cbind(as.vector(bach.ranks$mar), as.vector(grad.ranks$mar)))
t.test(bachelors.rankings[,1],graduates.rankings[,1])
t.test(bachelors.rankings[,2],graduates.rankings[,2])

#PHI COMPONENT AND WEIGHTED DISTANCE BASED MODEL#
faculty.phicom <- pmr::phicom(faculty.rankings.agg)
faculty.wdbm <- pmr::wdbm(faculty.rankings.agg, dtype="foot")
faculty.phicom@min
faculty.wdbm@min
faculty.phicom@coef
faculty.wdbm@coef


#QUASI STANDARD ERRORS#
faculty.qv <- qvcalc(faculty.mod_mle) #QUASI STANDARD ERRORS
summary(faculty.qv)
plot(faculty.qv, xlab="Job qualities", ylab="log of worth", main="Log worth of job qualities for contract faculty")

#ITEM PROBABILITIES FOR TOP RANK#
faculty.itempars <- itempar(faculty.mod_mle, ref= 1, log = TRUE, vcov=TRUE)
attributes(faculty.itempars)
itempar(faculty.mod_mle, ref=1:6)
faculty.itempars.probabilities<-itempar(faculty.mod_mle, ref= 1:6)
attributes(faculty.itempars.probabilities)
faculty.itempars.probabilities
