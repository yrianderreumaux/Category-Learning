# Load helper functions
source('scripts/behavioral_analyses/helper_functions.R')

# Import and load libraries
packages = c("tidyverse","sjstats","ggplot2","lme4","lmerTest","Hmisc","car","lmtest","ROCR","brms", "tidybayes", "emmeans")
ipak(packages)


#Load Data
Study2Data <- read.csv("data/Study2Data.csv", header=T, stringsAsFactors = FALSE, na.strings=c("","NA"))
Study2Data$Participant <- factor(Study2Data$Participant) #make participant factor

#create variable for face/cloud and prediction type crime/weather
Study2Data$stim_type <- NA
Study2Data$pred_type <- NA
Study2Data$stim_type[Study2Data$Stimuli=="cloudy" | Study2Data$Stimuli=="sunny"] <- "clouds"
Study2Data$stim_type[Study2Data$Stimuli=="white" | Study2Data$Stimuli=="black"] <- "faces"
Study2Data$pred_type[Study2Data$Choice=="rain" | Study2Data$Choice=="sun"] <- "weather"
Study2Data$pred_type[Study2Data$Choice=="no_steal" | Study2Data$Choice=="steal"] <- "crime"
#make it a factor and change ref level 
Study2Data$stim_type <- as.factor(Study2Data$stim_type)
Study2Data$pred_type <- as.factor(Study2Data$pred_type)
Study2Data$stim_type <- relevel(Study2Data$stim_type, ref = "clouds")
Study2Data$pred_type <- relevel(Study2Data$pred_type, ref = "crime")

#re-code categorical variables
#####
Study2Data$Condition_eff <- factor(Study2Data$Condition, 
                             levels = c("steal", "steal_clouds", "weather_faces", "weather"))
contrasts(Study2Data$Condition_eff) <- contr.sum(4)
colnames(contrasts(Study2Data$Condition_eff)) = c("steal", "steal_clouds", "weather_faces")

Study2Data$Condition_dum <- factor(Study2Data$Condition, 
                                   levels = c("steal", "steal_clouds", "weather_faces", "weather"))
contrasts(Study2Data$Condition_dum) <- contr.treatment(4, base = 4)
colnames(contrasts(Study2Data$Condition_dum)) = c("steal", "steal_clouds", "weather_faces")

Study2Data$ethnicity_eff <- as.factor(Study2Data$ethnicity)
contrasts(Study2Data$ethnicity_eff) <- contr.treatment(6)
#####

#run mixed models for learning by condition
######
Study2.model<- glmer(acc~scale(Trial)*Condition_dum+ (scale(Trial)|Participant)+(1|Face_Shown), data = Study2Data, family = "binomial")
#save(Study2.model, file = "study2model.rda") #function to save model to reload later for simulations
Study2.coef <- summary(Study2.model) #removing random effect for stimuli removes convergence issue, but does not change estimates and therefore we are keeping it. 
Study2.effects <- exp(fixef(Study2.model))
Study2.effects.CI <- log(exp(confint(Study2.model,'Condition_dumsteal', level=0.95)))
#does race moderate learning?
Study2.race.model<- glmer(acc~scale(Trial)+Condition_dum* ethnicity_eff+(scale(Trial)|Participant)+(1|Face_Shown), data = Study2Data, family = "binomial")
Study2.race.coef <- summary(Study2.race.model) #no effect of race
#Post-hoc simple contrasts
study2.contr <- emmeans(Study2.model, "Condition_dum")
study2.contr.coef <- pairs(study2.contr, adjust = "none")
#####

##Figure 3 in manuscript
######
my_colors <- wes_palette("GrandBudapest2")[1:4]
my_colors <- wes_palette("GrandBudapest2")[,c("#E6A0C4", "#C6CDF7", "#D8A499", "#7294D4")]
plotColor <- wes_palette(n= 4,"GrandBudapest2")

names(plotColor) <- levels(Study2Data$Condition)
my_colors <- RColorBrewer::brewer.pal(4, "Blues")[2:4]

plotcurve2 <- ggplot(Study2Data, aes(Trial, acc, fill = as.factor(Condition))) + 
  geom_smooth(method = "loess", color ="grey50")+
  scale_y_continuous(name = "Accuracy")+ scale_fill_manual(breaks = c("steal_clouds","weather", "steal", "weather_faces"),
    values = c("#E6A0C4", "#C6CDF7", "#D8A499", "#7294D4"))+
  theme(legend.position = "none",
        panel.grid = element_blank(),
        axis.title = element_blank(),
        panel.background = element_blank()) +
  theme(legend.position = "none", axis.title = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=1))+
  coord_cartesian(ylim = c(.6, .9))+theme(axis.ticks=element_blank())
#ggsave("plotcurve1", device='jpeg', width = 6, height = 5,dpi=700)
#####

#####Print the results in the order they appear in the manuscript
print('RESULTS FOR STUDY 2')
print('logistic mixed model for learning rates')
print(Study2.coef)
print(Study2.effects)
print(Study2.effects.CI)
print('no effect of race')
print(Study2.race.coef)
print('Post-hoc simple contrasts')
print(study2.contr.coef)
#plot(plotcurve2) #uncomment to plot figure. Takes a minute to run

