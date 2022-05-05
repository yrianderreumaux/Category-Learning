#warning! this script will take quite a long time to run due to the amount of models that need to be estimated

#Load helper functions
source('scripts/behavioral_analyses/helper_functions.R')

#Import and load libraries
packages = c("tidyverse","sjstats","ggplot2","lme4","lmerTest","Hmisc","car","lmtest","ROCR","brms", "tidybayes", "bayestestR")
ipak(packages)

#Load Data aggregated across crime conditions
Meta1DF <- read.csv("data/meta1.csv", header=T, stringsAsFactors = FALSE, na.strings=c("","NA"))
#add factor for study covariate
Meta1DF$study[Meta1DF$Condition=="steal"] <- "two"
Meta1DF$study[Meta1DF$Condition=="neg"] <- "three"

#load positively-valenced data to test individual differences
Study3Data_pos <- read.csv("data/study3_pos.csv", header=T, stringsAsFactors = FALSE, na.strings=c("","NA"))
Study3Data_pos$Participant <- as.factor(Study3Data_pos$Participant) #make participant factor

#create categorical card weight and congruency variables
#####
Meta1DF$prob[Meta1DF$Pattern == 1] <- .8947
Meta1DF$prob[Meta1DF$Pattern == 2] <- .7778
Meta1DF$prob[Meta1DF$Pattern == 3] <- .9231
Meta1DF$prob[Meta1DF$Pattern == 5] <- .8333
Meta1DF$prob[Meta1DF$Pattern == 6] <- .8947
Meta1DF$prob[Meta1DF$Pattern == 9] <- .55

Meta1DF$prob[Meta1DF$Pattern == 4 ] <- 1-.2222
Meta1DF$prob[Meta1DF$Pattern == 7] <- 1-.1053
Meta1DF$prob[Meta1DF$Pattern == 8 ] <- 1-.1667
Meta1DF$prob[Meta1DF$Pattern == 10 ] <- 1-.07
Meta1DF$prob[Meta1DF$Pattern == 11 ] <- 1-.44
Meta1DF$prob[Meta1DF$Pattern == 12] <- 1-.1053

Meta1DF$prob[Meta1DF$Pattern == 13] <- .5
Meta1DF$prob[Meta1DF$Pattern == 14] <- .5

#also code as absolute deviation from .5
Meta1DF$prob_abs[Meta1DF$Pattern == 1] <- abs(.8947-.5)
Meta1DF$prob_abs[Meta1DF$Pattern == 2] <- abs(.7778-.5)
Meta1DF$prob_abs[Meta1DF$Pattern == 3] <- abs(.9231-.5)
Meta1DF$prob_abs[Meta1DF$Pattern == 5] <- abs(.8333-.5)
Meta1DF$prob_abs[Meta1DF$Pattern == 6] <- abs(.8947-.5)
Meta1DF$prob_abs[Meta1DF$Pattern == 9] <- abs(.55-.5)

Meta1DF$prob_abs[Meta1DF$Pattern == 4 ] <- abs((1-.2222)-(.5))
Meta1DF$prob_abs[Meta1DF$Pattern == 7] <- abs((1-.1053)-(.5))
Meta1DF$prob_abs[Meta1DF$Pattern == 8 ] <- abs((1-.1667)-(.5))
Meta1DF$prob_abs[Meta1DF$Pattern == 10 ] <- abs((1-.07)-(.5))
Meta1DF$prob_abs[Meta1DF$Pattern == 11 ] <- abs((1-.44)-(.5))
Meta1DF$prob_abs[Meta1DF$Pattern == 12] <- abs((1-.1053)-(.5))

Meta1DF$prob_abs[Meta1DF$Pattern == 13] <- .5 - .5
Meta1DF$prob_abs[Meta1DF$Pattern == 14] <- .5 - .5

#create optimal choice variable
Meta1DF$opt[Meta1DF$Pattern == 1] <- 1
Meta1DF$opt[Meta1DF$Pattern == 2] <- 1
Meta1DF$opt[Meta1DF$Pattern == 3] <- 1
Meta1DF$opt[Meta1DF$Pattern == 5] <- 1
Meta1DF$opt[Meta1DF$Pattern == 6] <- 1
Meta1DF$opt[Meta1DF$Pattern == 9] <- 1

Meta1DF$opt[Meta1DF$Pattern == 4 ] <- 0
Meta1DF$opt[Meta1DF$Pattern == 7] <- 0
Meta1DF$opt[Meta1DF$Pattern == 8 ] <- 0
Meta1DF$opt[Meta1DF$Pattern == 10 ] <- 0
Meta1DF$opt[Meta1DF$Pattern == 11 ] <- 0
Meta1DF$opt[Meta1DF$Pattern == 12] <- 0

Meta1DF$opt[Meta1DF$Pattern == 13] <- .5
Meta1DF$opt[Meta1DF$Pattern == 14] <- .5

#create congruency variable
Meta1DF$congruency[Meta1DF$Stimuli == "white" & Meta1DF$Choice == "no_steal"] <- 1 #congruent
Meta1DF$congruency[Meta1DF$Stimuli == "black" & Meta1DF$Choice == "steal"] <- 1 #congruent
Meta1DF$congruency[Meta1DF$Stimuli == "white" & Meta1DF$Choice == "steal"] <- 0 #incongruent
Meta1DF$congruency[Meta1DF$Stimuli == "black" & Meta1DF$Choice == "no_steal"] <- 0 #incongruent
Meta1DF$congruency <- as.factor(Meta1DF$congruency)

Study3Data_pos$congruency[Study3Data_pos$Stimuli == "white" & Study3Data_pos$Choice == "no_touchdown"] <- 1
Study3Data_pos$congruency[Study3Data_pos$Stimuli == "black" & Study3Data_pos$Choice == "touchdown"] <- 1
Study3Data_pos$congruency[Study3Data_pos$Stimuli == "white" & Study3Data_pos$Choice == "touchdown"] <- 0
Study3Data_pos$congruency[Study3Data_pos$Stimuli == "black" & Study3Data_pos$Choice == "no_touchdown"] <- 0
Study3Data_pos$congruency <- as.factor(Study3Data_pos$congruency)
#####

#effects code card probabilities
#####
Meta1DF$prob_eff <- as.factor(Meta1DF$prob)
contrasts(Meta1DF$prob_eff) <- contr.sum(8)
colnames(contrasts(Meta1DF$prob_eff)) = c(".5", ".55", ".56",".77", "83", "89", "92")
#####

#First order models: stereotype application vs. inhibition
#####
#effect of stimuli present on each trial
MetaM1<- glmer(acc~scale(Trial)*as.factor(Stimuli)+ as.factor(study)+(scale(Trial)|Participant)+(as.factor(Stimuli)|Participant), data = Meta1DF, family = "binomial")
MetaM1_coef <- summary(MetaM1) 

#RT based on stimuli present
Meta1DF$RT <- as.numeric(Meta1DF$RT)
MetaM2<- lmer(log(RT)~scale(Trial)*as.factor(Stimuli)+ as.factor(study)+(scale(Trial)|Participant), data = Meta1DF)
MetaM2_coef <- summary(MetaM2)

#are people less accurate on congruent vs. incongruent trials?
MetaM3<- glmer(acc~scale(Trial)*as.factor(congruency)+ as.factor(study)+(scale(Trial)|Participant)+(as.factor(Stimuli)|Participant), data = Meta1DF, family = "binomial")
MetaM3_coef <- summary(MetaM3) 

#are people slower for congruent vs. incongruent trials?
MetaM4<- glmer(congruency~scale(Trial)*log(RT)+ as.factor(study)+(scale(Trial)|Participant)+(as.factor(Stimuli)|Participant), data = Meta1DF, family = "binomial")
MetaM4_coef <- summary(MetaM4) 

#does IMS moderate whether people are more likely to respond in a stereotype congruent manner?
MetaM5<- glmer(congruency~scale(Trial)*scale(IMS)+ as.factor(study)+(scale(Trial)|Participant)+(as.factor(Stimuli)|Participant), data = Meta1DF, family = "binomial")
MetaM5_coef <- summary(MetaM5) 

#does IMS moderate whether people are more likely to respond in a stereotype congruent manner differently over time?
MetaM6<- glmer(congruency~scale(Trial)*log(RT)*scale(IMS)+ as.factor(study)+(scale(Trial)|Participant)+(as.factor(Stimuli)|Participant), data = Meta1DF, family = "binomial")
MetaM6_coef <- summary(MetaM6) 

#does dominance moderate congruent responding?
MetaM7<- glmer(congruency~scale(Trial)*scale(SDO)+ as.factor(study)+(scale(Trial)|Participant)+(as.factor(Stimuli)|Participant), data = Meta1DF, family = "binomial")
MetaM7_coef <- summary(MetaM7) 

#do people rely more on stereotypes on less predictive trials?
MetaM8<- glmer(congruency~scale(Trial)+prob_eff+ as.factor(study)+(scale(Trial)|Participant)+(as.factor(Stimuli)|Participant), data = Meta1DF, family = "binomial")
MetaM8_coef <- summary(MetaM8) 

#do people rely more on stereotypes on less predictive trials (abs deviation from .5)?
MetaM9<- glmer(congruency~scale(Trial)+scale(prob_abs)+ as.factor(study)+(scale(Trial)|Participant)+(as.factor(Stimuli)|Participant), data = Meta1DF, family = "binomial")
MetaM9_coef <- summary(MetaM9) 
#####


#second order models: cumulative effects over time. 
#####
#IMS predicting accuracy
MetaM10 <- glmer(acc~scale(Trial)*scale(IMS) + as.factor(study)+(scale(Trial)|Participant)+ (1|Face_Shown), data = Meta1DF, family = "binomial",
                  control=glmerControl(optimizer="Nelder_Mead",
                                       optCtrl=list(maxfun=2e5))) #singularity is due to stimuli random effect, removing does not change estimates so keeping it in. 
MetaM10_coef<- summary(MetaM10) 
MetaM10_effects <- exp(fixef(MetaM10))
MetaM10_IMS.CI <- log(exp(confint(MetaM10,'scale(IMS)', level=0.95)))
MetaM10_IMS_Trial.CI <- log(exp(confint(MetaM10,'scale(Trial):scale(IMS)', level=0.95)))

#IMS predicting RT
MetaM11<- lmer(log(RT)~scale(Trial)*scale(IMS)+ as.factor(study)+(scale(Trial)|Participant), data = Meta1DF)
MetaM11_coef <- summary(MetaM11) 

#EMS predicting accuracy
MetaM12 <- glmer(acc~scale(Trial)*scale(EMS) + as.factor(study)+(Trial|Participant)+ (1|Face_Shown), data = Meta1DF, family = "binomial")
MetaM12_coef<- summary(MetaM12) #singularity issue due to random effect of stimuli so dropping it
MetaM12_effects <- exp(fixef(MetaM12))
MetaM12_EMS.CI <- log(exp(confint(MetaM12,'scale(Trial):scale(EMS)', level=0.95)))

#intergroup anxiety predicting accuracy
MetaM13<- glmer(acc~scale(Trial)*scale(intergroup_anx)+as.factor(study)+ (Trial|Participant)+ (1|Face_Shown), data = Meta1DF, family = "binomial")
MetaM13_coef<- summary(MetaM13)

#EMS predicting accuracy in positive stereotype condition
MetaM14<- glmer(acc~scale(Trial)*scale(EMS)+ (scale(Trial)|Participant)+ (1|Face_Shown), data = Study3Data_pos, family = "binomial")
MetaM14_coef <- summary(MetaM14) #singularity issues with random effect of stimuli, removing converges but does not change estimate

#IMS predicting accuracy in positive stereotype condition
MetaM15<- glmer(acc~scale(Trial)*scale(IMS)+ (scale(Trial)|Participant)+ (1|Face_Shown), data = Study3Data_pos, family = "binomial")
MetaM15_coef <- summary(MetaM15) #singularity issues with random effect of stimuli, removing converges but does not change estimate
#####

#Figure 5. Visualize IMS effect. 
#####
#median split onyl for figure. Not good practice for analyses. 
Meta1DF$IMSDich[Meta1DF$IMS > median(Meta1DF$IMS, na.rm = T) ] <- "high"
Meta1DF$IMSDich[Meta1DF$IMS < median(Meta1DF$IMS, na.rm = T) ] <- "low"
plotColorIMS <- wes_palette(n= 2,"GrandBudapest1")
names(plotColorIMS) <- levels(Meta1DF$IMSDich)
meta1IMSPlot <- Meta1DF[!is.na(Meta1DF$IMSDich),] #remove missing for figure. 
IMS_plot <- ggplot(meta1IMSPlot, aes(Trial, acc, color = IMSDich)) + 
  geom_smooth(method = "loess") +scale_color_manual(values = plotColorIMS)+  
  theme(legend.position = "none", axis.title = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=1))+theme(axis.ticks=element_blank())
#ggsave("IMS_plot.png", width = 4, height = 4, dpi = 700)
#####

print("FIRST ORDER MODELS")
print("no effect of stimuli on accuracy")
print(MetaM1_coef)
print("no effect of stimuli on reaction time")
print(MetaM2_coef)
print("no effect of congruency on accuracy")
print(MetaM3_coef)
print("no effect of congruency on reaction time")
print(MetaM4_coef)
print("no effect of IMS on congruent responding")
print(MetaM5_coef)
print("no effect of IMS and reaction time on congruent responding")
print(MetaM6_coef)
print("no effect of SDO on congruent responding")
print(MetaM7_coef)
print("card pattern weights (categorical) does not predict more stereotype-congruent responding")
print(MetaM8_coef)
print("card pattern weights (abs/continous) does not predict more stereotype-congruent responding")
print(MetaM9_coef)

print("SECOND ORDER MODELS")
print("significant main effect of internal motivation to respond without prejudice")
print(MetaM10_coef)
print("OR for IMS effects")
print(MetaM10_effects)
print("CI for IMS main effect")
print(MetaM10_IMS.CI)
print("CI for IMS by Trial interaction")
print(MetaM10_IMS_Trial.CI)

print("significant time by ims interaction: igher IMS, slower over time")
print(MetaM11_coef)

print("Marginal effect of EMS on accuracy")
print(MetaM12_coef)
print("OR for EMS effect")
print(MetaM12_effects)
print("CI for EMS by Trial interaction")
print(MetaM12_IMS_Trial.CI)

print("no effect of intergroup anxiety")
print(MetaM13_coef)

print("both IMS and EMS are not significant predictors of accuracy in positively-valenced condition")
print("IMS")
print(MetaM14_coef)
print("EMS")
print(MetaM15_coef)
