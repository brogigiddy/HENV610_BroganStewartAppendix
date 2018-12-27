#Nesting Green Sea Turtle Stats Analysis
NestingData <- read.csv(file.choose())  #Bring the Raw data into R
View(NestingData)   #See the data and to make sure that it was read correctly
class(NestingData)  #It is a dataframe


##CLEANING UP THE DATA
#Removing rows 116 to 279 since they are full of NA values.
#Also removing columns 38 and 39 because they are nothing. 
NestingData <- NestingData[-c(96:259), -c(39:40)]
str(NestingData)       #Taking a look at the strucute. 115 observation of 37 variables
attach(NestingData)

#Hatchery number and Row are integer and I want them to be factor
levels(NestingData$HatcheryNumber)
NestingData$HatcheryNumber <- factor(NestingData$HatcheryNumber)

levels(NestingData$Row)
NestingData$Row <- factor(NestingData$Row)


#Installing the package I need to do a linear mixed model
library(lme4)
library(nlme)



###DATA ANALYSIS
#Making boxplots to better visualize the data per Hatchery 
boxplot(CrabPredation ~ HatcheryNumber, ylab = "Eggs Predated by Crabs", xlab = "Hatchery Number", 
        col = c("#00FFCC", "#006666", "#3333FF", "#660099"), 
        main = "Green Sea Turtle Eggs Predated by Crabs in each Hatchery")

boxplot(Fungus ~ HatcheryNumber, ylab = "Eggs Contaminated by Fungus", xlab = "Hatchery Number", 
        col = c("#00FFCC", "#006666", "#3333FF", "#660099"), 
        main = "Green Sea Turtle Eggs Contaminated by Fungus in each Hatchery")

#Making boxplots to better visualize the data by Hatchery Row 
boxplot(CrabPredation ~ HatcheryNumber*Row, ylab = "Eggs Predated by Crabs", xlab = "Hatchery Number.Row", 
        col = c("#00FFCC", "#006666", "#3333FF", "#660099"), las = 2, 
        main = "Green Sea Turtle Eggs Predated by Crabs in each Hatchery Row")

boxplot(Fungus ~ HatcheryNumber*Row, ylab = "Eggs Contaminated by Fungus", xlab = "Hatchery Number.Row", 
        col = c("#00FFCC", "#006666", "#3333FF", "#660099"), las = 2,
        main = "Green Sea Turtle Eggs Contaminated by Fungus in each Hatchery Row")


#Getting rid of Hatchery number 4 since it is on a different beach
#This will be for the emergence data only
NestingHatcheries <- NestingData[!(HatcheryNumber=="4"),]
View(NestingHatcheries)
detach(NestingData)
attach(NestingHatcheries)

#Double check to make sure there are no NA values
NestingHatcheries[!complete.cases(NestingHatcheries),] 
#They are all complete rows with no NA


#Looking at the plots to see if there is any obvious linear relationship 
NewDF <- NestingHatcheries[,c(6, 8, 9, 15, 22, 26, 28, 29, 33, 35, 36, 37)]
View(NewDF)
plot(NewDF)



#HISTOGRAMS
#Checking the distribution of the data for Crab Pred and Hatchery Number
hist(NestingData$CrabPredation, main="Histogram of Crab Predation", 
     xlab="Number of Eggs Predated by Crabs")

#Checking the fungus
hist(NestingData$Fungus, main="Histogram of Fungus", 
     xlab="Number of Eggs Contaminated by Fungus")

#checking for Emergence days and Crab Predation
hist(NestingHatcheries$NumberDays, main="Histogram of the Number of Days Before Nest Emergence",
     xlab="Number of Days")



##EMERGENCE CRAB - NO CRAB BOXPLOT
##Checking crab data - with NO predation 
require(ggplot2)
CrabNest <- NestingData[!(CrabPredation=="0"),]
NoCrab <- NestingData[(CrabPredation=="0"),]
View(NoCrab)

AvgCrab<- mean(CrabNest$NumberDays)
AvgNoCrab <- mean(NoCrab$NumberDays)
sdCrab <- sd(CrabNest$NumberDays)
sdNoCrab <- sd(NoCrab$NumberDays)
AvgCrab
AvgNoCrab
sdCrab
sdNoCrab

DF1 <- rbind(AvgCrab, AvgNoCrab)
DF2 <- rbind(sdCrab, sdNoCrab)
DF3 <- c("Crab Predation", "No Crab Predation")
DF <- data.frame(DF3, DF1, DF2)
colnames(DF) <- c("x", "Mean", "StdDev")
View(DF)

NoCrabPlot <- ggplot(DF, aes (x=x, y=Mean)) +
  geom_errorbar(aes(ymin=Mean-StdDev, ymax=Mean+StdDev), width=.2) +
  geom_line() +
  geom_point()

print(NoCrabPlot + ggtitle("The Average Days Before Emergence of Sea Turtle Nests With and Without Crab Predation"))





##DID NOT END UP USING IN PAPER
#CRAB ANOVA
#Making the one-way crab ANOVA
aov1 <- aov(CrabPredation ~ HatcheryNumber, data = NestingData)
summary(aov1)

#Making the two-way crab ANOVA
aov2 <- aov(CrabPredation ~ HatcheryNumber + Row, data = NestingData)
summary(aov2)

aov3 <- aov(CrabPredation ~ HatcheryNumber*Row, data = NestingData)
summary(aov3)


#Fungus ANOVA 
#The one-way ANOVA for Fungus and hatchery number
aov4 <- aov(Fungus ~ HatcheryNumber, data = NestingData)
summary(aov4)

#The two-way ANOVA for Fungus, hatchery number and row
aov5 <- aov(Fungus ~ HatcheryNumber + Row, data = NestingData)
summary(aov5)

aov6 <- aov(Fungus ~ HatcheryNumber*Row, data = NestingData)
summary(aov6)

#Checking for normality assumptions
par(mfrow=c(2,2))
plot(aov1)
plot(aov2)
plot(aov3)
plot(aov4)
plot(aov5)
plot(aov6)

#Testing normal distribution
shapiro.test(aov1$res)
shapiro.test(aov2$res)
shapiro.test(aov3$res)
shapiro.test(aov4$res)
shapiro.test(aov5$res)
shapiro.test(aov6$res)
#All of them are not normal since p-value < 0.05

as.factor(HatcheryNumber)
leveneTest(CrabPredation ~ as.factor(HatcheryNumber))
leveneTest(Fungus ~ as.factor(HatcheryNumber))



##Checking using percent crab predation 
#Making the one-way crab ANOVA
aov7 <- aov(PercentCrabPred ~ HatcheryNumber, data = NestingHatcheries)
summary(aov7)

#Making the two-way crab ANOVA
aov8 <- aov(PercentCrabPred ~ HatcheryNumber + Row, data = NestingHatcheries)
summary(aov8)

#?
aov9 <- aov(PercentCrabPred ~ HatcheryNumber*Row, data = NestingHatcheries)
summary(aov9)





##EXPLORING DISTRIBUTIONS 
#What is the probability distribution of my data
require(car)
require(MASS)
require(vcd)


#Figuring out the underlying distribution of the datasets
CrabPredationDist <- NestingData$CrabPredation + 1

#Testing to see if normally distributed
qqp(NestingData$CrabPredation, "norm")
qqp(CrabPredationDist, "norm")
#IT IS NOT

#lognormal
qqp(CrabPredationDist, "lnorm")
#Wouldn't be lognormal since not continuous

#Checking the negative binomial. 
nbinom1 <- fitdistr(CrabPredationDist, "Negative Binomial")
qqp(CrabPredationDist, "nbinom", size = nbinom1$estimate[[1]], 
    mu = nbinom1$estimate[[2]])

#Checking Poisson
poisson1 <- fitdistr(CrabPredationDist, "Poisson")
qqp(CrabPredationDist, "pois", lambda=mean(CrabPredationDist), poisson1$estimate)
#Doesn't look like poisson

#Checking gamma distributions
gamma1 <- fitdistr(CrabPredationDist, "gamma")
qqp(CrabPredationDist, "gamma", shape = gamma1$estimate[[1]], 
    rate = gamma1$estimate[[2]])
#Again, not continuous data so not prefereable. 

#Won't be binomial cause the y vairable doesn't have binary errors. 


#Testing to see if normally distributed
NumberDaysDist <- NumberDays + 1
qqp(NumberDaysDist, "norm")
#lognormal
qqp(NumberDaysDist, "lnorm")
#Checking the negative binomial, Poisson and gamma distributions. 
nbinom <- fitdistr(NumberDaysDist, "Negative Binomial")
qqp(NumberDaysDist, "nbinom", size = nbinom$estimate[[1]], mu = nbinom$estimate[[2]])
poisson <- fitdistr(NumberDaysDist, "Poisson")
qqp(NumberDaysDist, "pois", poisson$estimate, lambda=mean(NumberDaysDist))
gamma <- fitdistr(NumberDaysDist, "gamma")
qqp(NumberDaysDist, "gamma", shape = gamma$estimate[[1]], rate = gamma$estimate[[2]])



Avg1 <- mean(NestingData$CrabPredation)
Avg1
Var1 <- var(NestingData$CrabPredation)
Var1
#Testing for the mean=variance assumption for poisson. 
#However, this does not necessarily mean overdispersion
#since the random effect will add to the error term.
Var1/Avg1







#THE GENERALIZED LINEAR MIXED MODEL WITH POISSON
##THIS IS IN PAPER
#Fitting of a simple GLMM model
#(the default fit is maximum likelihood (Laplace Approximation))


###CrabPredation ~ Hatchery and Row - HYPOTHESIS 1
glmer1 <- glmer(CrabPredation ~ HatcheryNumber + (1|NestNumber), data=NestingData,
                family="poisson")
summary(glmer1)
#AIC 580.0, nestnumber shows variance and hatch4 significant. 

glmer2 <- glmer(CrabPredation ~ HatcheryNumber + (1|HatcheryNumber/NestNumber), data=NestingData,
                family="poisson")
summary(glmer2)

glmer3 <- glmer(CrabPredation ~ HatcheryNumber + (1|NestNumber) + (1|HatcheryNumber), data=NestingData,
                family="poisson")
summary(glmer3)
#So since std dev is zero does that mean that hatcherynumber is not significant

glmer4 <- glmer(CrabPredation ~ HatcheryNumber + (1|NestNumber/EggNumber), data=NestingData,
                family="poisson")
summary(glmer4)

glmer5 <- glmer(CrabPredation ~ HatcheryNumber + (1|EggNumber) + (1|NestNumber), data=NestingData,
                family="poisson")
summary(glmer5)
#Egg number not necesssary (tried in likelihood test below)
 
glmer6 <- glmer(CrabPredation ~ HatcheryNumber+ Row + (1|EggNumber) + (1|NestNumber), data=NestingData,
                family="poisson")
summary(glmer6)

glmer7 <- glmer(CrabPredation ~ HatcheryNumber*Row + (1|EggNumber) + (1|NestNumber), data=NestingData,
                family="poisson")
summary(glmer7)

glmer8 <- glmer(CrabPredation ~ HatcheryNumber + (1|TenderHolder) + (1|NestNumber) + (1|EggNumber), data=NestingData,
                family="poisson")
summary(glmer8)

glmer9 <- glmer(CrabPredation ~ HatcheryNumber + (1|TenderHolder/NestNumber/EggNumber), data=NestingData,
                family="poisson")
summary(glmer9)
#Tender holder not necessary 

glmer10 <- glmer(CrabPredation ~ HatcheryNumber+ Row + (1|NestNumber), data=NestingData,
                family="poisson")
summary(glmer10)

glmer11 <- glmer(CrabPredation ~ HatcheryNumber*Row + (1|NestNumber), data=NestingData,
                family="poisson")
summary(glmer11)
#Row not significant. not a good model (testing using likelihood ratio below)

#WIHTOUT random effects 
glm1 <- glm(CrabPredation ~ HatcheryNumber, data=NestingData,
                family="poisson")
summary(glm1)


#Testing to see significance of random effect
pchisq(2*(logLik(glmer1)-logLik(glm1)),
       df=1,lower.tail=FALSE)/2      
#it is significant. So need the random effect. 

pchisq(2*(logLik(glmer1)-logLik(glmer5)),
       df=1,lower.tail=FALSE)/2 
#Not significant, don't need EggNumber as random effect

#testing row as fixed effect 
pchisq(2*(logLik(glmer1)-logLik(glmer10)),
       df=1,lower.tail=FALSE)/2 
#Not significant, don't need row as fixed effect
anova(glmer1, glmer10)

#testing row as fixed effect 
pchisq(2*(logLik(glmer1)-logLik(glmer11)),
       df=1,lower.tail=FALSE)/2 
#Not significant, don't need row as fixed effect
anova(glmer1, glmer11)


#Likelihood ratio test
anova(glmer1, glm1)
#Again shows that random effect is a better model. 


#Checking for overdispersion in poisson
#We can examine a goodness of fit test, which will not be significant 
#if the residual difference is small enough to indicate a good fit model.
#This is an approximate estimate of an overdispersion parameter.
overdisp_fun <- function(model) {
  rdf <- df.residual(model)
  rp <- residuals(model,type="pearson")
  Pearson.chisq <- sum(rp^2)
  prat <- Pearson.chisq/rdf
  pval <- pchisq(Pearson.chisq, df=rdf, lower.tail=FALSE)
  c(chisq=Pearson.chisq,ratio=prat,rdf=rdf,p=pval)
}

overdisp_fun(glmer1)
#According to this function test of overdispersion
#The ratio (residual deviance/residual df) < 1 and the 
#p-value is > 0.05. Therefore there is no overdispersion. 


#Double checking the overdispersion with another test
#QUASSI POISSON (used for overdispersion*) 
#Not necessary but exploring the data for comparison
glmm1 <- glmmPQL(CrabPredation~HatcheryNumber,
                 random=~+1|NestNumber, data=NestingData, 
                 family='quasipoisson')
summary(glmm1)

#Linear mixed-effects model fit by maximum likelihood
#Data: NestingData 
#AIC BIC logLik
#NA  NA     NA

#Random effects:
 # Formula: ~+1 | NestNumber
#(Intercept)     Residual
#StdDev:   0.7388658 0.0002086945

#Variance function:
#  Structure: fixed weights
#Formula: ~invwt 
#Fixed effects: CrabPredation ~ HatcheryNumber 
#Value Std.Error DF   t-value p-value
#(Intercept)      2.3154797 0.1647392 91 14.055423  0.0000
#HatcheryNumber2 -0.2189234 0.2552129 91 -0.857807  0.3933
#HatcheryNumber3 -0.3520366 0.2505174 91 -1.405238  0.1634
#HatcheryNumber4 -1.0366324 0.2009800 91 -5.157889  0.0000
#Correlation: 
#  (Intr) HtchN2 HtchN3
#HatcheryNumber2 -0.645              
#HatcheryNumber3 -0.658  0.424       
#HatcheryNumber4 -0.820  0.529  0.539

#Standardized Within-Group Residuals:
#  Min            Q1           Med            Q3           Max 
#-2.320832e-04 -8.369831e-05  1.271089e-05  6.714534e-05  1.776221e-04 

#Number of Observations: 95
#Number of Groups: 95  

glmm2 <- glmmPQL(CrabPredation~HatcheryNumber,
                 random=~+1|NestNumber/HatcheryNumber, data=NestingData, 
                 family='quasipoisson')
summary(glmm2)

#likelihood ratio tests (doesn't work for quasi)
anova(glmer1, glmm1, test="F")
anova(glmer1, glmm1)


#NEGATIVE BINOMIAL (used for overdispersion*)
#Not necessary but exploring the data for comparison
glmerA <- glmer.nb(CrabPredation ~ HatcheryNumber + (1|NestNumber), data=NestingData)
summary(glmerA)

#Generalized linear mixed model fit by maximum likelihood (Laplace Approximation) [
#glmerMod]
#Family: Negative Binomial(1340.959)  ( log )
#Formula: CrabPredation ~ HatcheryNumber + (1 | NestNumber)
#Data: NestingData

#AIC      BIC   logLik deviance df.resid 
#582.0    597.3   -285.0    570.0       89 

#Scaled residuals: 
#  Min       1Q   Median       3Q      Max 
#-1.47708 -0.33193  0.07933  0.28049  0.66508 

#Random effects:
#  Groups     Name        Variance Std.Dev.
#NestNumber (Intercept) 0.5749   0.7582  
#Number of obs: 95, groups:  NestNumber, 95

#Fixed effects:
#  Estimate Std. Error z value Pr(>|z|)    
#(Intercept)       2.2702     0.1826  12.433  < 2e-16 ***
#  HatcheryNumber2  -0.2341     0.2849  -0.822    0.411    
#HatcheryNumber3  -0.3678     0.2818  -1.305    0.192    
#HatcheryNumber4  -1.0868     0.2321  -4.683 2.83e-06 ***
  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Correlation of Fixed Effects:
#  (Intr) HtchN2 HtchN3
#HtchryNmbr2 -0.633              
#HtchryNmbr3 -0.640  0.412       
#HtchryNmbr4 -0.773  0.501  0.506

#which model is better
anova(glmer1, glmerA)
#poisson still better


#Testing the interaction
library(multcomp)
summary(glht(glmer1, linfct=mcp(HatcheryNumber="Tukey")))


#Attempting plots
#Seems promising
plot(fitted(glmer1)~HatcheryNumber, data=NestingData, ylab = "Eggs Predated by Crabs (Fitted Values from GLMER)", xlab = "Hatchery Number", 
     col = c("#00FFCC", "#006666", "#3333FF", "#660099"), 
     main = "Turtle Eggs Predated by Crabs in each Hatchery - Fitted Values from GLMER")

#didn't use
#residual vs fitted plot
plot(glmer1)

#didn't use
library(ggplot2)
ggplot(data.frame(eta=predict(glmer1,type="link"),pearson=residuals(glmer1,type="pearson")),
       aes(x=eta,y=pearson)) +
  geom_point() +
  theme_bw()


#plotting the residuals - didn't use
plot(glmer1,HatcheryNumber~resid(.,type="pearson"), xlim=c(-1.5, 1.5), 
     main = "The Pearson Residual Distribution")

#Plotting the random effect, should be along line
plot(ranef(glmer1,condVar=TRUE), 
     main = "Nest Number as a Random Effect in the GLMM")

#Checking residuals
library(DHARMa)
citation("DHARMa")

#Plotting the residuals
simulationOutput <- simulateResiduals(fittedModel = glmer1, n = 1000)
simulationOutput$scaledResiduals
plot(simulationOutput)

#TRY NEXT TIME
plotResiduals(HatcheryNumber, simulationOutput$scaledResiduals)

#support visual inspection
testUniformity(simulationOutput = simulationOutput)

#More tests
testOverdispersion(simulationOutput = simulationOutput)
testZeroInflation(simulationOutput = simulationOutput)
testTemporalAutocorrelation(simulationOutput = simulationOutput)
testSpatialAutocorrelation(simulationOutput = simulationOutput)






###Fungus ~ Hatchery and Row - HYPOTHESIS 2
glmer1F <- glmer(Fungus ~ HatcheryNumber + (1|NestNumber), data=NestingData,
                family="poisson")
summary(glmer1F)
#AIC 217.7, DF 90, hatchery 4 sig. and random effect needed 

glmer2F <- glmer(Fungus ~ HatcheryNumber + (1|HatcheryNumber/NestNumber), data=NestingData,
                family="poisson")
summary(glmer2F)
#Hatchery number not needed as random effect

glmer3F <- glmer(Fungus ~ HatcheryNumber + (1|NestNumber) + (1|HatcheryNumber), data=NestingData,
                family="poisson")
summary(glmer3F)
#So since std dev is zero does that mean that hatcherynumber is not significant

glmer4F <- glmer(Fungus ~ HatcheryNumber + (1|NestNumber/EggNumber), data=NestingData,
                family="poisson")
summary(glmer4F)
#AIC 219.7, DF 89
#Egg number has variance

glmer5F <- glmer(Fungus ~ HatcheryNumber + (1|EggNumber) + (1|NestNumber), data=NestingData,
                family="poisson")
summary(glmer5F)
#AIC 219.7, DF 89
#Egg number has a variance
#Hatchery 4 sig

glmer6F <- glmer(Fungus ~ HatcheryNumber+ Row + (1|EggNumber) + (1|NestNumber), data=NestingData,
                family="poisson")
summary(glmer6F)
#Nothing is significant
#AIC 222.3

glmer7F <- glmer(Fungus ~ HatcheryNumber*Row + (1|EggNumber) + (1|NestNumber), data=NestingData,
                family="poisson")
summary(glmer7F)
#AIC 230.4
#Nothing significant

glmer8F <- glmer(Fungus ~ HatcheryNumber + (1|TenderHolder) + (1|NestNumber) + (1|EggNumber), data=NestingData,
                family="poisson")
summary(glmer8F)

glmer9F <- glmer(Fungus ~ HatcheryNumber + (1|TenderHolder/NestNumber/EggNumber), data=NestingData,
                family="poisson")
summary(glmer9F)
#Tender holder has no variance, not necessary

glmer10F <- glmer(Fungus ~ HatcheryNumber+ Row + (1|NestNumber), data=NestingData,
                 family="poisson")
summary(glmer10F)
#AIC 220.4
#Nothing significant

glmer11F <- glmer(Fungus ~ HatcheryNumber*Row + (1|NestNumber), data=NestingData,
                 family="poisson")
summary(glmer11F)
#nothing significat
#AIC 228.4, DF 79

glmer12F <- glmer(Fungus ~ Row + (1|NestNumber), data=NestingData,
                  family="poisson")
summary(glmer12F)
#AIC 218.1
#still not significant. Row 2 would be at a lower confidence interval. 


#WIHTOUT random effects 
glm1F <- glm(Fungus ~ HatcheryNumber, data=NestingData,
            family="poisson")
summary(glm1F)
#AIC 261.51


#Testing to see significance of random effect
pchisq(2*(logLik(glmer1F)-logLik(glm1F)),
       df=1,lower.tail=FALSE)/2      
#it is significant. So need the random effect. 

#Likelihood ratio test
anova(glmer1F, glm1F)
#Again shows that random effect is a better model. 


#IS Egg number necessary?
pchisq(2*(logLik(glmer1F)-logLik(glmer5F)),
       df=1,lower.tail=FALSE)/2 
#Not significant, don't need EggNumber as random effect
#p-value = 0.5

anova(glmer1F, glmer5F)
#Egg number not a better model. 
#p-value = 0.8239


#Checking for overdispersion in poisson
#We can examine a goodness of fit test, which will not be significant 
#if the residual difference is small enough to indicate a good fit model.
#This is an approximate estimate of an overdispersion parameter.
overdisp_fun <- function(model) {
  rdf <- df.residual(model)
  rp <- residuals(model,type="pearson")
  Pearson.chisq <- sum(rp^2)
  prat <- Pearson.chisq/rdf
  pval <- pchisq(Pearson.chisq, df=rdf, lower.tail=FALSE)
  c(chisq=Pearson.chisq,ratio=prat,rdf=rdf,p=pval)
}

overdisp_fun(glmer1F)
#According to this function test of overdispersion
#The ratio (residual deviance/residual df) < 1 and the 
#p-value is > 0.05. Therefore there is no overdispersion. 

overdisp_fun(glmer10F)
#According to this function test of overdispersion
#The ratio (residual deviance/residual df) < 1 and the 
#p-value is > 0.05. Therefore there is no overdispersion. 



#Double checking the overdispersion with another test
#QUASSI POISSON (used for overdispersion*) 
#Not necessary but exploring the data for comparison
glmm1F <- glmmPQL(Fungus~HatcheryNumber,
                 random=~+1|NestNumber, data=NestingData, 
                 family='quasipoisson')
summary(glmm1F)
#nothing significant

glmm2F <- glmmPQL(Fungus~HatcheryNumber,
                 random=~+1|HatcheryNumber/NestNumber, data=NestingData, 
                 family='quasipoisson')
summary(glmm2F)
#does not work

glmm3F <- glmmPQL(Fungus~HatcheryNumber,
                  random=~+1|NestNumber/EggNumber, data=NestingData, 
                  family='quasipoisson')
summary(glmm3F)
#Hatchery 4 is significant

glmm4F <- glmmPQL(Fungus~Row,
                  random=~+1|NestNumber, data=NestingData, 
                  family='quasipoisson')
summary(glmm4F)
#nothing significant 

#NEGATIVE BINOMIAL (used for overdispersion*)
#Not necessary but exploring the data for comparison
glmerAF <- glmer.nb(Fungus ~ HatcheryNumber + (1|NestNumber), data=NestingData)
summary(glmerAF)

anova(glmer1F, glmerAF)
#not significant. not necessary

glmerBF <- glmer.nb(Fungus ~ Row + (1|NestNumber), data=NestingData)
summary(glmerBF)
#nothing significant


#Testing the interaction
library(multcomp)
summary(glht(glmer1F, linfct=mcp(HatcheryNumber="Tukey")))
#hatchery 4 most different from hatchery 1

#Checking row, shows nothing significant
summary(glht(glmer10F, linfct=mcp(HatcheryNumber="Tukey")))



#checking if 0 inflated
library(pscl)
glm0F <- zeroinfl(Fungus ~ HatcheryNumber, data=NestingData,
                  dist=c("poisson"))
summary(glm0F)
vuong(glm1F, glm0F)
#However model does not account for random effect 

#trying multilevel zero-inf
library(glmmTMB)
glmmTMBF1 <- glmmTMB(Fungus ~ HatcheryNumber + (1|NestNumber), data=NestingData,
                     family="poisson")
summary(glmmTMBF1)

glmmTMBF2 <- glmmTMB(Fungus ~ HatcheryNumber + (1|NestNumber), data=NestingData,
                     family="poisson", ziformula=~1)
summary(glmmTMBF2)

glmmTMBF3 <- glmmTMB(Fungus ~ HatcheryNumber + (1|NestNumber), data=NestingData,
                     family="nbinom2")
summary(glmmTMBF3)

glmmTMBF4 <- glmmTMB(Fungus ~ HatcheryNumber + (1|NestNumber), data=NestingData,
                     family="nbinom2", ziformula=~1)
summary(glmmTMBF2)

anova(glmmTMBF2, glmmTMBF1)
#Zero inflated is not a better model. 




#Attempting plots
#Seems promising
plot(fitted(glmer1F)~HatcheryNumber, data=NestingData, ylab = "Eggs Contaminated by Fungus (Fitted Values from GLMER)", xlab = "Hatchery Number", 
     col = c("#00FFCC", "#006666", "#3333FF", "#660099"), 
     main = "Turtle Eggs Contaminated by Fungus in each Hatchery - Fitted Values from GLMER")

#Hatchery Number and Row
plot(fitted(glmer10F)~(HatcheryNumber + Row), data=NestingData, ylab = "Eggs Contaminated by Fungus (Fitted Values from GLMER)", xlab = "Hatchery Number + Row", 
     main = "Turtle Eggs Contaminated by Fungus in each Hatchery and Row - Fitted Values from GLMER")


#residual vs fitted plot
plot(glmer1F)


library(ggplot2)
ggplot(data.frame(eta=predict(glmer1F,type="link"),pearson=residuals(glmer1F,type="pearson")),
       aes(x=eta,y=pearson)) +
  geom_point() +
  theme_bw()


#plotting the residuals
plot(glmer1F,HatcheryNumber~resid(.,type="pearson"),
     main = "The Pearson Residual Distribution for Fungus in each Hatchery")

#Plotting the random effect, should be along line
plot(ranef(glmer1F,condVar=TRUE),
     main = "Nest Number as a Random Effect in the GLMM")
plot(ranef(glmer5F,condVar=TRUE),
     main = "Nest Number and Egg Number as a Random Effect in the GLMM")
plot(ranef(glmer10F,condVar=TRUE),
     main = "Nest Number as a Random Effect in the GLMM (with Row)")


#Checking residuals
library(DHARMa)
citation("DHARMa")

#Plotting the residuals
simulationOutput2 <- simulateResiduals(fittedModel = glmer1F, n = 1000)
simulationOutput2$scaledResiduals
plot(simulationOutput2)

#support visual inspection
testUniformity(simulationOutput = simulationOutput2)

#More tests
testOverdispersion(simulationOutput = simulationOutput2)
testZeroInflation(simulationOutput = simulationOutput2)
testTemporalAutocorrelation(simulationOutput = simulationOutput2)
testSpatialAutocorrelation(simulationOutput = simulationOutput2)







#STARTING EMERGENCE DATA - HYPOTHESIS 3 
#Checking to see what the average number of days before emergence is
AvgDays <- mean(NumberDays)
AvgDays   #mean = 50.61538 days

###NumberDays ~ CrabPredation (only 3 hatcheries)
glmer1C <- glmer(NumberDays ~ CrabPredation + (1|NestNumber), data=NestingHatcheries,
                 family="poisson")
summary(glmer1C)
#AIC 309.8 and nest number no variation in the random effects

glmer2C <- glmer(NumberDays ~ CrabPredation + (1|HatcheryNumber/NestNumber), data=NestingHatcheries,
                 family="poisson")
summary(glmer2C)
#both no variation, AIC 311.8

glmer3C <- glmer(NumberDays ~ CrabPredation + (1|NestNumber) + (1|HatcheryNumber), data=NestingHatcheries,
                 family="poisson")
summary(glmer3C)
#random effects unneccessary so far

glmer4C <- glmer(NumberDays ~ CrabPredation + (1|NestNumber/EggNumber), data=NestingHatcheries,
                 family="poisson")
summary(glmer4C)
#not egg number either

glmer5C <- glmer(NumberDays ~ CrabPredation + (1|EggNumber) + (1|NestNumber), data=NestingHatcheries,
                 family="poisson")
summary(glmer5C)
#Still 0 variance

glmer6C <- glmer(NumberDays ~ CrabPredation + (1|TenderHolder), data=NestingHatcheries,
                 family="poisson")
summary(glmer6C)
#Tender holder is 0 too

glmer7C <- glmer(NumberDays ~ CrabPredation + (1|TenderHolder/NestNumber/EggNumber), data=NestingHatcheries,
                 family="poisson")
summary(glmer7C)

glmer8C <- glmer(NumberDays ~ CrabPredation + (1|Row), data=NestingHatcheries,
                 family="poisson")
summary(glmer8C)
#Row also 0

glmer9C <- glmer(NumberDays ~ CrabPredation + (1|Undeveloped), data=NestingHatcheries,
                 family="poisson")
summary(glmer9C)

glmer10C <- glmer(NumberDays ~ CrabPredation + (1|Fungus), data=NestingHatcheries,
                  family="poisson")
summary(glmer10C)
#none of the random effects had an effect


#WIHTOUT random effects 
glm1C <- glm(NumberDays ~ CrabPredation, data=NestingHatcheries,
             family="poisson")
summary(glm1C)
#AIC = 307.82
#apparently is better model
#The null deviance is small 

glm2C <- glm(NumberDays ~ PercentCrabPred, data=NestingHatcheries,
             family="poisson")
summary(glm2C)
#AIC 307.89

#Testing to see significance of random effect
pchisq(2*(logLik(glmer1C)-logLik(glm1C)),
       df=1,lower.tail=FALSE)/2      
#it is not significant. So don't need the random effect. 

#Likelihood ratio test
anova(glmer1C, glm1C)
anova(glmer1C, glm1C, test="F")
#Again shows that random effect is not needed in the model. 


#Checking goodness of fit of the model
pchisq(glm1C$deviance, df=glm1C$df.residual, lower.tail=FALSE)
#p value not significant. so data fits model

#Checking overdispersion ratio
glm1C$deviance/glm1C$df.residual
#the ratio is 0.08125747 which is less than one and therefore
#NOT overdispersed 

#Checking for overdispersion in poisson with function 
overdisp_fun(glm1C)
#According to this function test of overdispersion
#The ratio (residual deviance/residual df) < 1 and the 
#p-value is > 0.05. Therefore there is no overdispersion. 


#Double checking the random effect with different distributions
#QUASSI POISSON 
#Not necessary but exploring the data for comparison
glmqC <- glm(NumberDays ~ CrabPredation, data=NestingHatcheries,
             family="quasipoisson")
summary(glmqC)
#Call:
#glm(formula = NumberDays ~ CrabPredation, family = "quasipoisson", 
#    data = NestingHatcheries)

#Deviance Residuals: 
#  Min       1Q   Median       3Q      Max  
#-0.6018  -0.2229   0.0230   0.1963   0.5763  

#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)    3.9325168  0.0091691 428.886   <2e-16 ***
#  CrabPredation -0.0007829  0.0006933  -1.129    0.264    
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#(Dispersion parameter for quasipoisson family taken to be 0.08108439)

#Null deviance: 4.1665  on 51  degrees of freedom
#Residual deviance: 4.0629  on 50  degrees of freedom
#AIC: NA
#Number of Fisher Scoring iterations: 3


glmm1C <- glmmPQL(NumberDays ~ CrabPredation,
                  random=~+1|NestNumber, data=NestingHatcheries, 
                  family='quasipoisson')
summary(glmm1C)

glmm2C <- glmmPQL(NumberDays ~ CrabPredation,
                  random=~+1|EggNumber, data=NestingHatcheries, 
                  family='quasipoisson')
summary(glmm2C)
#Difficult to tell if better, no AIC. 
#still insignificant but random effects have variance



#NEGATIVE BINOMIAL
#Not necessary but exploring the data for comparison
#Running a proper negative binomial with Theta = estimated k gives same results
#Testing the var(Yi)=E(Yi) and k estimate for negative binomial
var(NumberDays)
#[1] 4.12368
mean(NumberDays)
#[1] 50.61538
var(NumberDays)/mean(NumberDays)
#[1] 0.08147089

estK <- ((mean(NumberDays))^(2))/(var(NumberDays)-mean(NumberDays))
#[1] -55.10482
negBinom <- glm.nb(NumberDays ~ CrabPredation, init.theta = estK, link = log)
summary(negBinom)
#Call:
#glm.nb(formula = NumberDays ~ CrabPredation, init.theta = 11307102.8, 
#       link = log)

#Deviance Residuals: 
#  Min       1Q   Median       3Q      Max  
#-0.6018  -0.2229   0.0230   0.1963   0.5763  

#Coefficients:
#  Estimate Std. Error z value Pr(>|z|)    
#(Intercept)    3.9325168  0.0322004 122.126   <2e-16 ***
#  CrabPredation -0.0007829  0.0024348  -0.322    0.748    
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#(Dispersion parameter for Negative Binomial(11288643) family taken to be 1)

#Null deviance: 4.1665  on 51  degrees of freedom
#Residual deviance: 4.0629  on 50  degrees of freedom
#AIC: 309.82

#Number of Fisher Scoring iterations: 1


#Theta:  11307103 
#Std. Err.:  419714247 
#Warning while fitting theta: alternation limit reached 
#2 x log-likelihood:  -303.824 



glmerAC <- glmer.nb(NumberDays ~ CrabPredation + (1|NestNumber), data=NestingHatcheries)
summary(glmerAC)
#The random effect still 0, and insignificant y



#Attempting plots
plot(NumberDays ~ CrabPredation, 
     main = "The Number of Days Before Emergence and Crab Predation in Three Hatcheries",
     ylab= "Number of Days Before First Emergence", xlab = "Eggs Predated by Crabs")
abline(lm(NumberDays ~ CrabPredation, data=NestingHatcheries), col="red")

plot(NestingData$NumberDays ~ NestingData$CrabPredation, 
     main = "The Number of Days Before Emergence and Crab Predation in All Four Hatcheries",
     ylab= "Number of Days Before First Emergence", xlab = "Eggs Predated by Crabs")
abline(lm(NumberDays ~ CrabPredation, data=NestingData), col="red")


plot(glm1C)


plot(fitted(glm1C)~CrabPredation, data=NestingHatcheries)

library(ggplot2)
ggplot(data.frame(eta=predict(glm1C,type="link"),pearson=residuals(glm1C,type="pearson")),
       aes(x=eta,y=pearson)) +
  geom_point() +
  theme_bw()









#INITIAL MESSING AROUND WITH DATA
#Testing different linear mixed models
lme1 <- lme(NumberDays ~ CrabPredation, random=~1|NestNumber, 
            data = NestingHatcheries)
summary(lme1)

lme2 <- lme(NumberDays ~ CrabPredation, random=~1|EggNumber/NestNumber, 
            data = NestingHatcheries)
summary(lme2)

lme3 <- lme(NumberDays ~ CrabPredation, random=~1|NestNumber/TenderHolder, 
            data = NestingHatcheries)
summary(lme3)

lme4 <- lme(NumberDays ~ CrabPredation*HatcheryNumber*HatchingSuccess, random=~1|NestNumber, 
            data = NestingHatcheries)
summary(lme4)

lme5 <- lme(NumberDays ~ CrabPredation*HatchingSuccess, random=~1|NestNumber, 
            data = NestingHatcheries)
summary(lme5)

lme6 <- lme(NumberDays ~ CrabPredation*Row, random=~1|NestNumber, 
            data = NestingHatcheries)
summary(lme6)

lme7 <- lme(NumberDays ~ PercentCrabPred, random=~1|NestNumber, 
            data = NestingHatcheries)
summary(lme7)

lme8 <- lme(NumberDays ~ PercentPred, random=~1|NestNumber, 
            data = NestingHatcheries)
summary(lme8)

lme9 <- lme(NumberDays ~ CrabPredation, random=~1|EggNumber/NestNumber/TenderHolder, 
            data = NestingHatcheries)
summary(lme9)

lme10 <- lme(NumberDays ~ CrabPredation, random=list(~1|EggNumber, ~1|NestNumber),
             data = NestingHatcheries)
summary(lme10)

lme11 <-lme(NumberDays ~ 1, random=list(~1|CrabPredation, ~1|NestNumber),
            data = NestingHatcheries)
summary(lme11)




#Testing lme with the Crab Predation and hatchery number
lme12 <- lme(CrabPredation ~ HatcheryNumber, random=~1|NestNumber, 
            data = NestingData)
summary(lme12)

lme13 <- lme(CrabPredation ~ HatcheryNumber, random=~1|EggNumber/NestNumber, 
            data = NestingData)
summary(lme13)

lme14<- lme(CrabPredation ~ HatcheryNumber, random=~1|NestNumber/TenderHolder, 
            data = NestingData)
summary(lme14)

lme15 <- lme(CrabPredation ~ HatcheryNumber*Row, random=~1|NestNumber, 
            data = NestingData)
summary(lme15)

lme16 <- lme(CrabPredation ~ HatcheryNumber*Row, random=~1|EggNumber/NestNumber, 
            data = NestingData)
summary(lme16)



#random plots - messing around with the data
plot(as.numeric(NestingHatcheries$EmergeDate), NestingHatcheries$CrabPredation)
lines(lowess(EmergeDate, CrabPredation), col="blue")

boxplot(Fungus ~ TenderHolder)

str(TenderHolder)

boxplot(Undeveloped ~ TenderHolder)





#Won't work
lmer1 <- lmer(NumberDays ~ CrabPredation + (1 | NestNumber), data = NestingHatcheries)
summary(lmer1)

Lmer1 <- lmer(CrabPredation ~ HatcheryNumber + (1|NestNumber/HatcheryNumber), poisson, data = NestingData)
summary(Lmer1)


#Not Usable
LmerCrab <- lmer(NumberDays ~ CrabPredation + (1|NestNumber), data = NewNesting)
summary(LmerCrab)

LmeCrab <- lme(fixed = CrabPredation ~ HatcheryNumber + HatcheryNumber*Row, random = ~ (1 | NestNumber))
summary(LmeCrab)


test2 <- lme(NumberDays ~ CrabPredation, random=~1|NestNumber, data = NestingHatcheries)
summary(test2)




#Checking data to see if any correlations
library(GGally)
TestPlot <- ggpairs(NestingHatcheries, columns = c("1", 5, 6, 8(NestingHatcheries)),
                    title= "Pairwise Variables Plots")
TestPlot <- ggpairs(NestingHatcheries, columns = c("NestDate", "Row", "HatcheryNumber", "NumberDays"),
                    title= "Pairwise Variables Plots", columnLabels = colnames(NestingHatcheries[, (1,5,6,8)]))
TestPLot
