#R SCRIPT MASTERTHESIS:
#1. FIRST STEPS-----------------------------------------------------------------
#1.1 Load Packages:-------------------------------------------------------------
library(glue)
library(gtsummary)
library(effectsize)
library(mlogit)
library(boot)
library(ggm)
library(ggplot2)
library(Hmisc)
library(polycor)
library(car)
library(QuantPsyc)
library(lm.beta)
library(r2glmm)
library(performance)
library(see)
library(readxl)
library(psych)
library(reshape2)
library(lmtest)
library(rstatix)


#1.2 Set Working Directory:-----------------------------------------------------
setwd("~/Desktop/R & Statistics/Data/Masterthesis")


#1.3 Read in Data:--------------------------------------------------------------
#This dataframe contains the merged data from all 4 phases (sham control, 50Hz cTBS
# offline, 1Hz online, 30Hz cTBS offline, and 30Hz active control) from both
#Sosci Survey and the IAPS data.
samep_data <- read_excel("~/Desktop/R & Statistics/Data/Masterthesis/samep_data.xlsx")
head(samep_data)
View(samep_data)


#1.4 Creating Subsets:------------------------------------------------------------
#1.4.1 Creating 30Hz cTBS Offline & Sham Control Subset:--------------------------
#This dataframe contains only data from the 30Hz cTBS offline protocol & from the
#sham control group.
samep_30Hz_sc_data <- subset(samep_data, study_group == "cTBS_30Hz_exp"| study_group == "cTBS_sham")
head(samep_30Hz_sc_data)
View(samep_30Hz_sc_data)

#1.4.2 Creating 30Hz cTBS Offline Subset:---------------------------------------
#This dataframe contains only data from the 30Hz cTBS offline protocol.
samep_30Hz_data <- subset(samep_data, study_group == "cTBS_30Hz_exp")
head(samep_30Hz_data)
View(samep_30Hz_data)

#1.4.3 Creating Sham Control Subset:--------------------------------------------
#This dataframe contains only data from the sham control group.
samep_sc_data <- subset(samep_data, study_group == "cTBS_sham")
head(samep_sc_data)
View(samep_sc_data)


#2. DESCRIPTIVE STATISTICS - SEX:-----------------------------------------------
#2.1 Sex in 30Hz cTBS Offline & Sham Control combined:--------------------------
table(samep_30Hz_sc_data$sex)
prop.table(table(samep_30Hz_sc_data$sex)) * 100
#Sex: female n = 16 (53.33%); male n = 14 (46.67%); non-binary = 0 (0%) 


#2.2 Sex in 30Hz cTBS Offline Protocol:-----------------------------------------
table(samep_30Hz_data$sex)
prop.table(table(samep_30Hz_data$sex)) * 100
#Sex: female n = 8 (53.33%); male n = 7 (46.67%); non-binary = 0 (0%)


#2.3 Sex in Sham Control Group:-------------------------------------------------
table(samep_sc_data$sex)
prop.table(table(samep_sc_data$sex)) * 100
#Sex: female n = 8 (53.33%); male n = 7 (46.67%); non-binary = 0 (0%)


#3. DESCRIPTIVE STATISTICS - AGE:-----------------------------------------------
#3.1 Age in 30Hz cTBS Offline & Sham Control Combined:--------------------------
psych::describe(samep_30Hz_sc_data$age)
#Age: n = 30; mean = 23.47; SD = 3.18; median = 23; range = 18 - 30


#3.2 Age in 30Hz cTBS Offline Protocol:-----------------------------------------
psych::describe(samep_30Hz_data$age)
#Age: n = 15; mean = 23.93; SD = 3.01; median = 23; range = 20 - 28


#3.3 Age in Sham Control Group:-------------------------------------------------
psych::describe(samep_sc_data$age)
#Age: n = 15; mean = 23; SD = 3.38; median = 22; range = 18 - 30


#3.4 Age Summary by Group (30Hz offline & sham control group):------------------
by(samep_30Hz_sc_data$age,samep_30Hz_sc_data$study_group,FUN = summary)

#Creating Plot for Age by Group (only 30Hz offline & sham control group):
#Bar Chart:
ggplot(data = samep_30Hz_sc_data,aes(study_group,age,fill=study_group))+
  stat_summary(geom = "bar",fun.data = "mean_cl_normal",position = position_dodge())+
  stat_summary(geom = "errorbar",fun.data = "mean_cl_normal",
               position = position_dodge(.9),width = 0.2)+
  theme_minimal() +
  scale_fill_manual(values = c("#333333", "#AAAAAA"))

#Violin Plot:
ggplot(data = samep_30Hz_sc_data, aes(study_group, age, fill = study_group)) +
  geom_violin(trim = FALSE, alpha = 0.8) +
  geom_boxplot(width = 0.1, fill = "white", outlier.shape = NA) + 
  theme_minimal() +
  scale_fill_manual(values = c("#333333", "#AAAAAA")) +
  labs(x = "Group", y = "Age", fill = "Group")

#Creating Plot for Age by Group & Sex (30Hz offline & sham control group):
#Bar Chart:
ggplot(data = samep_30Hz_sc_data,aes(study_group,age,fill=sex))+
  stat_summary(geom = "bar",fun.data = "mean_cl_normal",position = position_dodge())+
  stat_summary(geom = "errorbar",fun.data = "mean_cl_normal",
               position = position_dodge(.9),width = 0.2)+
  theme_minimal() +
  scale_fill_manual(values = c("#333333", "#AAAAAA"))

#Box Plot
ggplot(data = samep_30Hz_sc_data, aes(x = study_group, y = age, fill = sex)) +
  geom_boxplot(width = 0.3, outlier.shape = NA, position = position_dodge(width = 0.8), color = "black", alpha = 0.8) +  # Boxplots für jede Gruppe & Geschlecht
  theme_minimal() +
  scale_fill_manual(values = c("Female" = "#333333", "Male" = "#AAAAAA")) +  # Farben für Geschlecht (Dunkel für weiblich, Hell für männlich)
  labs(x = "Group", y = "Age", fill = "Sex") +
  scale_x_discrete(labels = c("cTBS_30Hz_exp" = "Experimental Group", 
                              "cTBS_sham" = "Control Group")) +  # X-Achse nur mit Gruppenbeschriftung
  theme(legend.position = "right")



#4. DESCRIPTIVE STATISTICS - RESTING MOTOR THRESHOLD (rMT):---------------------
#4.1 rMT in 30Hz cTBS Offline & Sham Control Combined:--------------------------
psych::describe(samep_30Hz_sc_data$rMT)
#rMT: n = 30; mean = 55.5; SD = 6.85; median = 55.5; range = 40 - 63


#4.2 rMT in 30Hz cTBS Offline Protocol:-----------------------------------------
psych::describe(samep_30Hz_data$rMT)
#rMT: n = 15; mean = 55.87; SD = 6.53; median = 56; range = 45 - 63


#4.3 rMT in Sham Control Group:-------------------------------------------------
psych::describe(samep_sc_data$rMT)
#rMT: n = 15; mean = 55.13; SD = 7.36; median = 54; range = 40 - 63


#4.4 rMT Summary by Group (30Hz offline & sham control group):------------------
by(samep_30Hz_sc_data$rMT,samep_30Hz_sc_data$study_group,FUN = summary)

#Creating Plot for rMT by Group (only 30Hz offline & sham control group):
#Bar Chart:
ggplot(data = samep_30Hz_sc_data,aes(study_group,rMT,fill=study_group))+
  stat_summary(geom = "bar",fun.data = "mean_cl_normal",position = position_dodge())+
  stat_summary(geom = "errorbar",fun.data = "mean_cl_normal",
               position = position_dodge(.9),width = 0.2)+
  theme_minimal() +
  scale_fill_manual(values = c("#333333", "#AAAAAA"))

#Box Plot:

#Creating Plot for rMT by Group & Sex (30Hz offline & sham control group):
#Bar Chart:
ggplot(data = samep_30Hz_sc_data,aes(study_group,rMT,fill=sex))+
  stat_summary(geom = "bar",fun.data = "mean_cl_normal",position = position_dodge())+
  stat_summary(geom = "errorbar",fun.data = "mean_cl_normal",
               position = position_dodge(.9),width = 0.2)+
  theme_minimal() +
  scale_fill_manual(values = c("#333333", "#AAAAAA"))

#Box Plot:


#5. DESCRIPTIVE STATISTICS - MEMORY PERFORMANCE:--------------------------------
#5.1 Memory Performance Overall:------------------------------------------------
#Note: Memory Performance (IAPS_HIT) is the number of remembered pictures in total.

#5.1.1 Memory Performance Overall in 30Hz cTBS Offline & Sham Control Combined:----
psych::describe(samep_30Hz_sc_data$IAPS_HIT)
#n = 30; mean = 20.87; SD = 7.89; median = 22.5; range = 7 - 45

#5.1.2 Memory Performance Overall in 30Hz cTBS Offline Protocol:----------------
psych::describe(samep_30Hz_data$IAPS_HIT)
#n = 15; mean = 18.67; SD = 6.18; median = 17; range = 7 - 30

#5.1.3 Memory Performance Overall in Sham Control Group:------------------------
psych::describe(samep_sc_data$IAPS_HIT)
#n = 15; mean = 23.07; SD = 8.96; median = 25; range = 8 - 45

#5.1.4 Memory Performance Overall Summary by Group:-----------------------------
by(samep_30Hz_sc_data$IAPS_HIT,samep_30Hz_sc_data$study_group,FUN = summary)

#Creating Plot for Memory Performance by Group:
ggplot(data = samep_30Hz_sc_data,aes(study_group,IAPS_HIT,fill=study_group))+
  stat_summary(geom = "bar",fun.data = "mean_cl_normal",position = position_dodge())+
  stat_summary(geom = "errorbar",fun.data = "mean_cl_normal",
               position = position_dodge(.9),width = 0.2)+
  theme_minimal() +
  scale_fill_manual(values = c("#333333", "#AAAAAA"))

#Creating Plot for Memory Performance by Group & Sex:
ggplot(data = samep_30Hz_sc_data,aes(study_group,IAPS_HIT,fill=sex))+
  stat_summary(geom = "bar",fun.data = "mean_cl_normal",position = position_dodge())+
  stat_summary(geom = "errorbar",fun.data = "mean_cl_normal",
               position = position_dodge(.9),width = 0.2)+
  theme_minimal() +
  scale_fill_manual(values = c("#333333", "#AAAAAA"))


###JUST FOR FUN:
#Creating Plot for Memory Performance by Group & Sex (all groups):
ggplot(data = samep_data,aes(study_group,IAPS_HIT,fill=sex))+
  stat_summary(geom = "bar",fun.data = "mean_cl_normal",position = position_dodge())+
  stat_summary(geom = "errorbar",fun.data = "mean_cl_normal",
               position = position_dodge(.9),width = 0.2)+
  theme_minimal() +
  scale_fill_manual(values = c("#333333", "#AAAAAA"))
#Looks like subjects that identified as female where in general better at remembering
#but only in the experimental conditions, not in the two control groups!
#Maybe the protocols worked, but only on subjects that identified as male?

#5.1.5 Remembered Pictures by Valence & Group:----------------------------------
#Creating a long format data set for (standard) valence of remembered pictures (for 30Hz cTBS offline & sham control):
samep_30Hz_sc_long_MP <- melt(data = samep_30Hz_sc_data,
                              id.vars = c("VP_nr","study_group","sex","age","rMT","handedness","stimulation_time"),
                              measure.vars = c("IAPS_HIT_NEG","IAPS_HIT_POS","IAPS_HIT_NEU"),
                              variable.name = "valence",
                              value.name = "Hit")
View(samep_30Hz_sc_long_MP)

#Creating Plot for number of remembered pictures in each group & valence category:
ggplot(data = samep_30Hz_sc_long_MP, aes(x = study_group, y = Hit, fill = valence)) +
  geom_boxplot(position = position_dodge(width = 0.9), alpha = (0.85)) +
  theme_minimal() +
  scale_fill_manual(values = c("#333333", "#666666", "#FFFFFF"),
                    labels = c("Negative Pictures", "Positive Pictures", "Neutral Pictures")) +
  labs(x = "Group", y = "Number of Remembered Pictures", fill = "Valence") +
  scale_x_discrete(labels = c("cTBS_30Hz_exp" = "Experimental", 
                              "cTBS_sham" = "Control"))

#5.1.6 Remembered Pictures by Valence, Group & Sex:-----------------------------
#Creating Plot for number of remembered pictures in each group & valence category (divded by sex):
ggplot(data = samep_30Hz_sc_long_MP,aes(study_group,Hit,fill=valence))+
  stat_summary(geom = "bar",fun.data = "mean_cl_normal",position = position_dodge())+
  stat_summary(geom = "errorbar",fun.data = "mean_cl_normal",
               position = position_dodge(.9),width = 0.2)+
  theme_minimal()+facet_wrap(~ sex) +
  scale_fill_manual(values = c("#333333", "#666666", "#999999"))


#5.2 Neutral Memory Performance (MP):-------------------------------------------
#Note: Neutral Memory Performance (IAPS_HIT_NEU) is the number of neutral pictures 
#that were remembered.

#5.2.1 Neutral MP in 30Hz cTBS Offline & Sham Control Combined:-----------------
psych::describe(samep_30Hz_sc_data$IAPS_HIT_NEU)
#n = 30; mean = 4; SD = 2.3; median = 4; range = 1 - 12

#5.2.2 Neutral MP in 30Hz cTBS Offline Protocol:--------------------------------
psych::describe(samep_30Hz_data$IAPS_HIT_NEU)
#n = 15; mean = 3.2; SD = 1.61; median = 4; range = 1 - 5

#5.2.3 Neutral MP in Sham Control Group:----------------------------------------
psych::describe(samep_sc_data$IAPS_HIT_NEU)
#n = 15; mean = 4.8; SD = 2.65; median = 5; range = 1 - 12

#5.2.4 Neutral MP Summary by Group:---------------------------------------------
by(samep_30Hz_sc_data$IAPS_HIT_NEU,samep_30Hz_sc_data$study_group,FUN = summary)

#Creating Plot for Neutral MP by Group:
ggplot(data = samep_30Hz_sc_data,aes(study_group,IAPS_HIT_NEU,fill=study_group))+
  stat_summary(geom = "bar",fun.data = "mean_cl_normal",position = position_dodge())+
  stat_summary(geom = "errorbar",fun.data = "mean_cl_normal",
               position = position_dodge(.9),width = 0.2)+
  theme_minimal() +
  scale_fill_manual(values = c("#333333", "#AAAAAA"))

#Creating Plot for Neutral MP by Group & Sex:
ggplot(data = samep_30Hz_sc_data,aes(study_group,IAPS_HIT_NEU,fill=sex))+
  stat_summary(geom = "bar",fun.data = "mean_cl_normal",position = position_dodge())+
  stat_summary(geom = "errorbar",fun.data = "mean_cl_normal",
               position = position_dodge(.9),width = 0.2)+
  theme_minimal() +
  scale_fill_manual(values = c("#333333", "#AAAAAA"))


#5.3 Positive Memory Performance (MP):------------------------------------------
#Note: Positive Memory Performance (IAPS_HIT_POS) is the number of positive remembered pictures in total.
#5.3.1 Positive MP in 30Hz cTBS Offline & Sham Control Combined:----------------
psych::describe(samep_30Hz_sc_data$IAPS_HIT_POS)
#n = 30; mean = 8.87; SD = 3.42; median = 9.5; range = 2 - 15

#5.3.2 Positive MP in 30Hz cTBS Offline Protocol:-------------------------------
psych::describe(samep_30Hz_data$IAPS_HIT_POS)
#n = 15; mean = 8.53; SD = 3.68; median = 9; range = 2 - 14

#5.3.3 Positive MP in Sham Control Group:---------------------------------------
psych::describe(samep_sc_data$IAPS_HIT_POS)
#n = 15; mean = 9.2; SD = 3.23; median = 10; range = 4 - 15

#5.3.4 Positive MP Summary by Group:--------------------------------------------
by(samep_30Hz_sc_data$IAPS_HIT_POS,samep_30Hz_sc_data$study_group,FUN = summary)

#Creating Plot for Positive MP by Group:
ggplot(data = samep_30Hz_sc_data,aes(study_group,IAPS_HIT_POS,fill=study_group))+
  stat_summary(geom = "bar",fun.data = "mean_cl_normal",position = position_dodge())+
  stat_summary(geom = "errorbar",fun.data = "mean_cl_normal",
               position = position_dodge(.9),width = 0.2)+
  theme_minimal() +
  scale_fill_manual(values = c("#333333", "#AAAAAA"))

#Creating Plot for Positive MP by Group & Sex:
ggplot(data = samep_30Hz_sc_data,aes(study_group,IAPS_HIT_POS,fill=sex))+
  stat_summary(geom = "bar",fun.data = "mean_cl_normal",position = position_dodge())+
  stat_summary(geom = "errorbar",fun.data = "mean_cl_normal",
               position = position_dodge(.9),width = 0.2)+
  theme_minimal() +
  scale_fill_manual(values = c("#333333", "#AAAAAA"))


#5.4 Negative Memory Performance (MP):------------------------------------------
#Note: Negative Memory Performance (IAPS_HIT_NEG) is the number of negative remembered pictures in total.
#5.4.1 Negative MP in 30Hz cTBS Offline & Sham Control Combined:----------------
psych::describe(samep_30Hz_sc_data$IAPS_HIT_NEG)
#n = 30; mean = 8; SD = 3.83; median = 8; range = 0 - 20

#5.4.2 Negative MP in 30Hz cTBS Offline Protocol:-------------------------------
psych::describe(samep_30Hz_data$IAPS_HIT_NEG)
#n = 15; mean = 6.93; SD = 2.81; median = 7; range = 3 - 14

#5.4.3 Negative MP in Sham Control Group:---------------------------------------
psych::describe(samep_sc_data$IAPS_HIT_NEG)
#n = 15; mean = 9.07; SD = 4.48; median = 9; range = 0 - 20

#5.4.4 Negative MP Summary by Group:--------------------------------------------
by(samep_30Hz_sc_data$IAPS_HIT_NEG,samep_30Hz_sc_data$study_group,FUN = summary)

#Creating Plot for Negative MP by Group:
ggplot(data = samep_30Hz_sc_data,aes(study_group,IAPS_HIT_NEG,fill=study_group))+
  stat_summary(geom = "bar",fun.data = "mean_cl_normal",position = position_dodge())+
  stat_summary(geom = "errorbar",fun.data = "mean_cl_normal",
               position = position_dodge(.9),width = 0.2)+
  theme_minimal() +
  scale_fill_manual(values = c("#333333", "#AAAAAA"))

#Creating Plot for Negative MP by Group & Sex:
ggplot(data = samep_30Hz_sc_data,aes(study_group,IAPS_HIT_NEG,fill=sex))+
  stat_summary(geom = "bar",fun.data = "mean_cl_normal",position = position_dodge())+
  stat_summary(geom = "errorbar",fun.data = "mean_cl_normal",
               position = position_dodge(.9),width = 0.2)+
  theme_minimal() +
  scale_fill_manual(values = c("#333333", "#AAAAAA"))


#5.5 Memory Performance Difference:---------------------------------------------
#5.5.1 Number of positive remembered pictures - number of negative remembered pictures:-------
#Note: A higher memory difference means that less negative pictures were remembered
#in contrast to positive pictures. A lower memory difference indicates that more 
#negative pictures were remembered in contrast to positive pictures. 0 would mean
#that exactly the same number of positive and negative pictures were remembered.
samep_30Hz_sc_data$MP_dif <- samep_30Hz_sc_data$IAPS_HIT_POS - samep_30Hz_sc_data$IAPS_HIT_NEG

samep_30Hz_data$MP_dif <- samep_30Hz_data$IAPS_HIT_POS - samep_30Hz_data$IAPS_HIT_NEG

samep_sc_data$MP_dif <- samep_sc_data$IAPS_HIT_POS - samep_sc_data$IAPS_HIT_NEG

#5.5.2 Memory Difference in 30Hz cTBS Offline & Sham Control Combined:----------
psych::describe(samep_30Hz_sc_data$MP_dif)
#n = 30; mean = 0.87; SD = 3.38; median = 1; range = -7 - 7

#5.5.3 Memory Difference in 30Hz cTBS Offline Group:----------------------------
psych::describe(samep_30Hz_data$MP_dif)
#n = 15; mean = 1.6; SD = 3.14; median = 2; range = -6 - 7

#5.5.4 Memory Difference in Sham Control Group:---------------------------------
psych::describe(samep_sc_data$IAPS_HIT_NEG)
#n = 15; mean = 9.07; SD = 4.48; median = 9; range = 0 - 20

#5.5.5 Memory Difference Summary by Group:--------------------------------------
by(samep_30Hz_sc_data$MP_dif,samep_30Hz_sc_data$study_group,summary)

#Creating Plot for Memory Difference by Group:
ggplot(data = samep_30Hz_sc_data,aes(study_group,MP_dif,fill=study_group))+
  stat_summary(geom = "bar",fun.data = "mean_cl_normal",position = position_dodge())+
  stat_summary(geom = "errorbar",fun.data = "mean_cl_normal",
               position = position_dodge(.9),width = 0.2)+
  theme_minimal() +
  scale_fill_manual(values = c("#333333", "#AAAAAA"))

#Creating Plot for Negative MP by Group & Sex:
ggplot(data = samep_30Hz_sc_data,aes(study_group,MP_dif,fill=sex))+
  stat_summary(geom = "bar",fun.data = "mean_cl_normal",position = position_dodge())+
  stat_summary(geom = "errorbar",fun.data = "mean_cl_normal",
               position = position_dodge(.9),width = 0.2)+
  theme_minimal() +
  scale_fill_manual(values = c("#333333", "#AAAAAA"))


#6. DESCRIPTIVE STATISTICS - VALENCE:-------------------------------------------
#Note: Change Variables into numeric values:
samep_30Hz_sc_data$mean_valence_rating_1 <- as.numeric(samep_30Hz_sc_data$mean_valence_rating_1)
samep_30Hz_sc_data$mean_valence_rating_1_nrem <- as.numeric(samep_30Hz_sc_data$mean_valence_rating_1_nrem)
samep_30Hz_sc_data$mean_valence_rating_1_rem <- as.numeric(samep_30Hz_sc_data$mean_valence_rating_1_rem)
samep_30Hz_sc_data$mean_valence_rating_neu_1 <- as.numeric(samep_30Hz_sc_data$mean_valence_rating_neu_1)
samep_30Hz_sc_data$mean_valence_rating_neu_1_nrem <- as.numeric(samep_30Hz_sc_data$mean_valence_rating_neu_1_nrem)
samep_30Hz_sc_data$mean_valence_rating_neu_1_rem <- as.numeric(samep_30Hz_sc_data$mean_valence_rating_neu_1_rem)
samep_30Hz_sc_data$mean_valence_rating_pos_1 <- as.numeric(samep_30Hz_sc_data$mean_valence_rating_pos_1)
samep_30Hz_sc_data$mean_valence_rating_pos_1_nrem <- as.numeric(samep_30Hz_sc_data$mean_valence_rating_pos_1_nrem)
samep_30Hz_sc_data$mean_valence_rating_pos_1_rem <- as.numeric(samep_30Hz_sc_data$mean_valence_rating_pos_1_rem)
samep_30Hz_sc_data$mean_valence_rating_neg_1 <- as.numeric(samep_30Hz_sc_data$mean_valence_rating_neg_1)
samep_30Hz_sc_data$mean_valence_rating_neg_1_nrem <- as.numeric(samep_30Hz_sc_data$mean_valence_rating_neg_1_nrem)
samep_30Hz_sc_data$mean_valence_rating_neg_1_rem <- as.numeric(samep_30Hz_sc_data$mean_valence_rating_neg_1_rem)

samep_30Hz_data$mean_valence_rating_1 <- as.numeric(samep_30Hz_data$mean_valence_rating_1)
samep_30Hz_data$mean_valence_rating_1_nrem <- as.numeric(samep_30Hz_data$mean_valence_rating_1_nrem)
samep_30Hz_data$mean_valence_rating_1_rem <- as.numeric(samep_30Hz_data$mean_valence_rating_1_rem)
samep_30Hz_data$mean_valence_rating_neu_1 <- as.numeric(samep_30Hz_data$mean_valence_rating_neu_1)
samep_30Hz_data$mean_valence_rating_neu_1_nrem <- as.numeric(samep_30Hz_data$mean_valence_rating_neu_1_nrem)
samep_30Hz_data$mean_valence_rating_neu_1_rem <- as.numeric(samep_30Hz_data$mean_valence_rating_neu_1_rem)
samep_30Hz_data$mean_valence_rating_pos_1 <- as.numeric(samep_30Hz_data$mean_valence_rating_pos_1)
samep_30Hz_data$mean_valence_rating_pos_1_nrem <- as.numeric(samep_30Hz_data$mean_valence_rating_pos_1_nrem)
samep_30Hz_data$mean_valence_rating_pos_1_rem <- as.numeric(samep_30Hz_data$mean_valence_rating_pos_1_rem)
samep_30Hz_data$mean_valence_rating_neg_1 <- as.numeric(samep_30Hz_data$mean_valence_rating_neg_1)
samep_30Hz_data$mean_valence_rating_neg_1_nrem <- as.numeric(samep_30Hz_data$mean_valence_rating_neg_1_nrem)
samep_30Hz_data$mean_valence_rating_neg_1_rem <- as.numeric(samep_30Hz_data$mean_valence_rating_neg_1_rem)

samep_sc_data$mean_valence_rating_1 <- as.numeric(samep_sc_data$mean_valence_rating_1)
samep_sc_data$mean_valence_rating_1_nrem <- as.numeric(samep_sc_data$mean_valence_rating_1_nrem)
samep_sc_data$mean_valence_rating_1_rem <- as.numeric(samep_sc_data$mean_valence_rating_1_rem)
samep_sc_data$mean_valence_rating_neu_1 <- as.numeric(samep_sc_data$mean_valence_rating_neu_1)
samep_sc_data$mean_valence_rating_neu_1_nrem <- as.numeric(samep_sc_data$mean_valence_rating_neu_1_nrem)
samep_sc_data$mean_valence_rating_neu_1_rem <- as.numeric(samep_sc_data$mean_valence_rating_neu_1_rem)
samep_sc_data$mean_valence_rating_pos_1 <- as.numeric(samep_sc_data$mean_valence_rating_pos_1)
samep_sc_data$mean_valence_rating_pos_1_nrem <- as.numeric(samep_sc_data$mean_valence_rating_pos_1_nrem)
samep_sc_data$mean_valence_rating_pos_1_rem <- as.numeric(samep_sc_data$mean_valence_rating_pos_1_rem)
samep_sc_data$mean_valence_rating_neg_1 <- as.numeric(samep_sc_data$mean_valence_rating_neg_1)
samep_sc_data$mean_valence_rating_neg_1_nrem <- as.numeric(samep_sc_data$mean_valence_rating_neg_1_nrem)
samep_sc_data$mean_valence_rating_neg_1_rem <- as.numeric(samep_sc_data$mean_valence_rating_neg_1_rem)


#6.1 Valence Overall:-----------------------------------------------------------
#Note: Valence describes the subject's valence rating of the pictures when they first
#saw the pictures (right after each picture.)

#6.1.1 Valence Overall in 30Hz cTBS Offline & Sham Control Combined:------------
#Valence Overall:
psych::describe(samep_30Hz_sc_data$mean_valence_rating_1)
#n = 29; mean = 4.04; SD = 15.74; median = -0.14; range = -17.01 - 49.25

#Valence Overall Not Remembered:
psych::describe(samep_30Hz_sc_data$mean_valence_rating_1_nrem)
#n = 29; mean = 3.12; SD = 18.88; median = -1.28; range = -34.38 - 49.49

#Valence Overall Remembered:
psych::describe(samep_30Hz_sc_data$mean_valence_rating_1_rem)
#n = 29; mean = 10.54; SD = 27.76; median = 8.96; range = -34.56 - 75.95


#6.1.2 Valence Overall in 30Hz cTBS Offline Protocol:---------------------------
#Valence Overall:
psych::describe(samep_30Hz_data$mean_valence_rating_1)
#n = 14; mean = 10.29; SD = 18.85; median = 6.68; range = -10.74 - 49.25

#Valence Overall Not Remembered:
psych::describe(samep_30Hz_data$mean_valence_rating_1_nrem)
#n = 14; mean = 7.57; SD = 20.72; median = 5.99; range = -26.95 - 49.49

#Valence Overall Remembered:
psych::describe(samep_30Hz_data$mean_valence_rating_1_rem)
#n = 14; mean = 19.44; SD = 29.16; median = 14.41; range = -29.4 - 75.95


#6.1.3 Valence Overall in Sham Control Group:-----------------------------------
#Valence Overall:
psych::describe(samep_sc_data$mean_valence_rating_1)
#n = 15; mean = -1.81; SD = 9.49; median = -1.07; range = -17.01 - 14.62

#Valence Overall Not Remembered:
psych::describe(samep_sc_data$mean_valence_rating_1_nrem)
#n = 15; mean = -1.04; SD = 16.62; median = -3.44; range = -34.38 - 27.3

#Valence Overall Remembered:
psych::describe(samep_sc_data$mean_valence_rating_1_rem)
#n = 15; mean = 2.23; SD = 24.46; median = 4.23; range = -34.56 - 64.25


#6.1.4 Valence Overall Summary by Group:----------------------------------------
#Valence Overall:
by(samep_30Hz_sc_data$mean_valence_rating_1,samep_30Hz_sc_data$study_group,FUN = summary)

#Creating Plot for Valence Overall by Group:
ggplot(data = samep_30Hz_sc_data,aes(study_group,mean_valence_rating_1,fill=study_group))+
  stat_summary(geom = "bar",fun.data = "mean_cl_normal",position = position_dodge())+
  stat_summary(geom = "errorbar",fun.data = "mean_cl_normal",
               position = position_dodge(.9),width = 0.2)+
  theme_minimal() +
  scale_fill_manual(values = c("#333333", "#AAAAAA"))

#Creating Plot for Valence Overall by Group & Sex:
ggplot(data = samep_30Hz_sc_data,aes(study_group,mean_valence_rating_1,fill=sex))+
  stat_summary(geom = "bar",fun.data = "mean_cl_normal",position = position_dodge())+
  stat_summary(geom = "errorbar",fun.data = "mean_cl_normal",
               position = position_dodge(.9),width = 0.2)+
  theme_minimal() +
  scale_fill_manual(values = c("#333333", "#AAAAAA"))


#Valence Overall Not Remembered:
by(samep_30Hz_sc_data$mean_valence_rating_1_nrem,samep_30Hz_sc_data$study_group,FUN = summary)

#Creating Plot for Valence Overall Not Remembered by Group:
ggplot(data = samep_30Hz_sc_data,aes(study_group,mean_valence_rating_1_nrem,fill=study_group))+
  stat_summary(geom = "bar",fun.data = "mean_cl_normal",position = position_dodge())+
  stat_summary(geom = "errorbar",fun.data = "mean_cl_normal",
               position = position_dodge(.9),width = 0.2)+
  theme_minimal() +
  scale_fill_manual(values = c("#333333", "#AAAAAA"))

#Creating Plot for Valence Overall Not Remembered by Group & Sex:
ggplot(data = samep_30Hz_sc_data,aes(study_group,mean_valence_rating_1_nrem,fill=sex))+
  stat_summary(geom = "bar",fun.data = "mean_cl_normal",position = position_dodge())+
  stat_summary(geom = "errorbar",fun.data = "mean_cl_normal",
               position = position_dodge(.9),width = 0.2)+
  theme_minimal() +
  scale_fill_manual(values = c("#333333", "#AAAAAA"))


#Valence Overall Remembered:
by(samep_30Hz_sc_data$mean_valence_rating_1_rem,samep_30Hz_sc_data$study_group,FUN = summary)

#Creating Plot for Valence Overall Remembered by Group:
ggplot(data = samep_30Hz_sc_data,aes(study_group,mean_valence_rating_1_rem,fill=study_group))+
  stat_summary(geom = "bar",fun.data = "mean_cl_normal",position = position_dodge())+
  stat_summary(geom = "errorbar",fun.data = "mean_cl_normal",
               position = position_dodge(.9),width = 0.2)+
  theme_minimal() +
  scale_fill_manual(values = c("#333333", "#AAAAAA"))

#Creating Plot for Valence Overall Remembered by Group & Sex:
ggplot(data = samep_30Hz_sc_data,aes(study_group,mean_valence_rating_1_rem,fill=sex))+
  stat_summary(geom = "bar",fun.data = "mean_cl_normal",position = position_dodge())+
  stat_summary(geom = "errorbar",fun.data = "mean_cl_normal",
               position = position_dodge(.9),width = 0.2)+
  theme_minimal() +
  scale_fill_manual(values = c("#333333", "#AAAAAA"))


#6.1.5 Valence Rating by Valence of Pictures & Group:---------------------------
#Creating a long format data set for emotional valence ratings (standard) (for 30Hz cTBS offline & sham control):
samep_30Hz_sc_long_VR <- melt(data = samep_30Hz_sc_data,
                                   id.vars = c("VP_nr", "study_group", "sex", "age", "rMT", "handedness", "stimulation_time"),
                                   measure.vars = c("mean_valence_rating_neg_1", "mean_valence_rating_pos_1", "mean_valence_rating_neu_1"),
                                   variable.name = "valence1",
                                   value.name = "ValenceRating")
View(samep_30Hz_sc_long_VR)

#Creating Plot for Mean Valence Rating in each Group & Valence Category:
ggplot(data = samep_30Hz_sc_long_VR, aes(x = study_group, y = ValenceRating, fill = valence1)) +
  geom_boxplot(position = position_dodge(width = 0.9), alpha = (0.85)) +
  theme_minimal() +
  scale_fill_manual(values = c("#333333", "#666666", "#FFFFFF"),
                    labels = c("Negative Pictures", "Positive Pictures", "Neutral Pictures")) +
  labs(x = "Group", y = "Mean Valence Rating", fill = "Valence") +
  scale_x_discrete(labels = c("cTBS_30Hz_exp" = "Experimental", 
                              "cTBS_sham" = "Control")) +
  theme(legend.position = "right")


#6.1.6 Valence Rating by Valence of Pictures, Group & Sex:----------------------
#Creating Plot for Mean Valence Rating in each Group & Valence Categorie by Sex:
ggplot(data = samep_30Hz_sc_long_VR, aes(study_group, ValenceRating, fill = valence1)) +
  stat_summary(geom = "bar", fun.data = "mean_cl_normal", position = position_dodge()) +
  stat_summary(geom = "errorbar", fun.data = "mean_cl_normal", position = position_dodge(.9), width = 0.2) +
  theme_minimal() + facet_wrap(~ sex) +
  scale_fill_manual(values = c("#333333", "#666666", "#999999"),
                    labels = c("Negative Pictures", "Positive Pictures", "Neutral Pictures")) +
  labs(x = "Group", y = "Mean Valence Rating", fill = "Valence") +
  scale_x_discrete(labels = c("cTBS_30Hz_exp" = "Experimental Group", "cTBS_sham" = "Control Group")) +
  theme(legend.position = "right")


#6.2 Neutral Valence:-----------------------------------------------------------
#Note: Neutral Valence describes the subject's valence rating of the neutral (?) 
#pictures when they first saw the pictures (right after each picture.)

#6.2.1 Neutral Valence in 30Hz cTBS Offline & Sham Control Combined:-----------
#Neutral Valence:
psych::describe(samep_30Hz_sc_data$mean_valence_rating_neu_1)
#n = 29; mean = 23.41; SD = 19.73; median = 20.48; range = -11.54 - 75.75

#Neutral Valence Not Remembered:
psych::describe(samep_30Hz_sc_data$mean_valence_rating_neu_1_nrem)
#n = 29; mean = 22.97; SD = 20.02; median = 21.83; range = -10 - 79.04

#Neutral Valence Remembered:
psych::describe(samep_30Hz_sc_data$mean_valence_rating_neu_1_rem)
#n = 29; mean = 21.62; SD = 40.87; median = 6; range = -59 - 103.5


#6.2.2 Neutral Valence in 30Hz cTBS Offline Protocol:--------------------------
#Neutral Valence:
psych::describe(samep_30Hz_data$mean_valence_rating_neu_1)
#n = 14; mean = 30.22; SD = 22.02; median = 24.9; range = 1.33 - 75.75

#Neutral Valence Not Remembered:
psych::describe(samep_30Hz_data$mean_valence_rating_neu_1_nrem)
#n = 14; mean = 30.22; SD = 23.12; median = 25.65; range = 1.68 - 79.04

#Neutral Valence Remembered:
psych::describe(samep_30Hz_data$mean_valence_rating_neu_1_rem)
#n = 14; mean = 25.17; SD = 44.23; median = 4.83; range = -45 - 103.5


#6.2.3 Neutral Valence in Sham Control Group:----------------------------------
#Neutral Valence:
psych::describe(samep_sc_data$mean_valence_rating_neu_1)
#n = 15; mean = 17.05; SD = 15.45; median = 18.46; range = -11.54 - 41.62

#Neutral Valence Not Remembered:
psych::describe(samep_sc_data$mean_valence_rating_neu_1_nrem)
#n = 15; mean = 16.21; SD = 14.26; median = 15.89; range = -10 - 38.18

#Neutral Valence Remembered:
psych::describe(samep_sc_data$mean_valence_rating_neu_1_rem)
#n = 15; mean = 18.31; SD = 38.73; median = 6; range = -59 - 100


#6.2.4 Neutral Valence Summary by Group:----------------------------------------
#Neutral Valence:
by(samep_30Hz_sc_data$mean_valence_rating_neu_1,samep_30Hz_sc_data$study_group,FUN = summary)

#Creating Plot for Neutral Valence by Group:
ggplot(data = samep_30Hz_sc_data,aes(study_group,mean_valence_rating_neu_1,fill=study_group))+
  stat_summary(geom = "bar",fun.data = "mean_cl_normal",position = position_dodge())+
  stat_summary(geom = "errorbar",fun.data = "mean_cl_normal",
               position = position_dodge(.9),width = 0.2)+
  theme_minimal() +
  scale_fill_manual(values = c("#333333", "#AAAAAA"))

#Creating Plot for Neutral Valence by Group & Sex:
ggplot(data = samep_30Hz_sc_data,aes(study_group,mean_valence_rating_neu_1,fill=sex))+
  stat_summary(geom = "bar",fun.data = "mean_cl_normal",position = position_dodge())+
  stat_summary(geom = "errorbar",fun.data = "mean_cl_normal",
               position = position_dodge(.9),width = 0.2)+
  theme_minimal() +
  scale_fill_manual(values = c("#333333", "#AAAAAA"))


#Neutral Valence Not Remembered:
by(samep_30Hz_sc_data$mean_valence_rating_neu_1_nrem,samep_30Hz_sc_data$study_group,FUN = summary)

#Creating Plot for Neutral Valence Not Remembered by Group:
ggplot(data = samep_30Hz_sc_data,aes(study_group,mean_valence_rating_neu_1_nrem,fill=study_group))+
  stat_summary(geom = "bar",fun.data = "mean_cl_normal",position = position_dodge())+
  stat_summary(geom = "errorbar",fun.data = "mean_cl_normal",
               position = position_dodge(.9),width = 0.2)+
  theme_minimal() +
  scale_fill_manual(values = c("#333333", "#AAAAAA"))

#Creating Plot for Neutral Valence Not Remembered by Group & Sex:
ggplot(data = samep_30Hz_sc_data,aes(study_group,mean_valence_rating_neu_1_nrem,fill=sex))+
  stat_summary(geom = "bar",fun.data = "mean_cl_normal",position = position_dodge())+
  stat_summary(geom = "errorbar",fun.data = "mean_cl_normal",
               position = position_dodge(.9),width = 0.2)+
  theme_minimal() +
  scale_fill_manual(values = c("#333333", "#AAAAAA"))


#Neutral Valence Remembered:
by(samep_30Hz_sc_data$mean_valence_rating_neu_1_rem,samep_30Hz_sc_data$study_group,FUN = summary)

#Creating Plot for Neutral Valence Remembered by Group:
ggplot(data = samep_30Hz_sc_data,aes(study_group,mean_valence_rating_neu_1_rem,fill=study_group))+
  stat_summary(geom = "bar",fun.data = "mean_cl_normal",position = position_dodge())+
  stat_summary(geom = "errorbar",fun.data = "mean_cl_normal",
               position = position_dodge(.9),width = 0.2)+
  theme_minimal() +
  scale_fill_manual(values = c("#333333", "#AAAAAA"))

#Creating Plot for Neutral Valence Remembered by Group & Sex:
ggplot(data = samep_30Hz_sc_data,aes(study_group,mean_valence_rating_neu_1_rem,fill=sex))+
  stat_summary(geom = "bar",fun.data = "mean_cl_normal",position = position_dodge())+
  stat_summary(geom = "errorbar",fun.data = "mean_cl_normal",
               position = position_dodge(.9),width = 0.2)+
  theme_minimal() +
  scale_fill_manual(values = c("#333333", "#AAAAAA"))


#6.3 Positive Valence:----------------------------------------------------------
#Note: Positive Valence describes the subject's valence rating of the positive (?) 
#pictures when they first saw the pictures (right after each picture.)

#6.3.1 Positive Valence in 30Hz cTBS Offline & Sham Control Combined:-----------
#Positive Valence:
psych::describe(samep_30Hz_sc_data$mean_valence_rating_pos_1)
#n = 29; mean = 120.57; SD = 36.21; median = 119.62; range = 25.96 - 191.42

#Positive Valence Not Remembered:
psych::describe(samep_30Hz_sc_data$mean_valence_rating_pos_1_nrem)
#n = 29; mean = 114.36; SD = 40.67; median = 111.07; range = 20.38 - 180.08 

#Positive Valence Remembered:
psych::describe(samep_30Hz_sc_data$mean_valence_rating_pos_1_rem)
#n = 29; mean = 132.92; SD = 31.1; median = 131.88; range = 65 - 199.86


#6.3.2 Positive Valence in 30Hz cTBS Offline Protocol:--------------------------
#Positive Valence:
psych::describe(samep_30Hz_data$mean_valence_rating_pos_1)
#n = 14; mean = 124.56; SD = 40.32; median = 125.6; range = 25.96 - 191.42

#Positive Valence Not Remembered:
psych::describe(samep_30Hz_data$mean_valence_rating_pos_1_nrem)
#n = 14; mean = 119.96; SD = 42.75; median = 123.53; range = 20.38 - 179.6

#Positive Valence Remembered:
psych::describe(samep_30Hz_data$mean_valence_rating_pos_1_rem)
#n = 14; mean = 138.25; SD = 32.24; median = 133.44; range = 65 - 199.86


#6.3.3 Positive Valence in Sham Control Group:----------------------------------
#Positive Valence:
psych::describe(samep_sc_data$mean_valence_rating_pos_1)
#n = 15; mean = 116.84; SD = 32.9; median = 119.62; range = 67.62 - 181.88

#Positive Valence Not Remembered:
psych::describe(samep_sc_data$mean_valence_rating_pos_1_nrem)
#n = 15; mean = 109.13; SD = 39.39; median = 110.29; range = 51.27 - 180.08

#Positive Valence Remembered:
psych::describe(samep_sc_data$mean_valence_rating_pos_1_rem)
#n = 15; mean = 127.95; SD = 30.24; median = 124.8; range = 81.46 - 184


#6.3.4 Positive Valence Summary by Group:---------------------------------------
#Positive Valence:
by(samep_30Hz_sc_data$mean_valence_rating_pos_1,samep_30Hz_sc_data$study_group,FUN = summary)

#Creating Plot for Positive Valence by Group:
ggplot(data = samep_30Hz_sc_data,aes(study_group,mean_valence_rating_pos_1,fill=study_group))+
  stat_summary(geom = "bar",fun.data = "mean_cl_normal",position = position_dodge())+
  stat_summary(geom = "errorbar",fun.data = "mean_cl_normal",
               position = position_dodge(.9),width = 0.2)+
  theme_minimal() +
  scale_fill_manual(values = c("#333333", "#AAAAAA"))

#Creating Plot for Positive Valence by Group & Sex:
ggplot(data = samep_30Hz_sc_data,aes(study_group,mean_valence_rating_pos_1,fill=sex))+
  stat_summary(geom = "bar",fun.data = "mean_cl_normal",position = position_dodge())+
  stat_summary(geom = "errorbar",fun.data = "mean_cl_normal",
               position = position_dodge(.9),width = 0.2)+
  theme_minimal() +
  scale_fill_manual(values = c("#333333", "#AAAAAA"))


#Positive Valence Not Remembered:
by(samep_30Hz_sc_data$mean_valence_rating_pos_1_nrem,samep_30Hz_sc_data$study_group,FUN = summary)

#Creating Plot for Positive Valence Not Remembered by Group:
ggplot(data = samep_30Hz_sc_data,aes(study_group,mean_valence_rating_pos_1_nrem,fill=study_group))+
  stat_summary(geom = "bar",fun.data = "mean_cl_normal",position = position_dodge())+
  stat_summary(geom = "errorbar",fun.data = "mean_cl_normal",
               position = position_dodge(.9),width = 0.2)+
  theme_minimal() +
  scale_fill_manual(values = c("#333333", "#AAAAAA"))

#Creating Plot for Positive Valence Not Remembered by Group & Sex:
ggplot(data = samep_30Hz_sc_data,aes(study_group,mean_valence_rating_pos_1_nrem,fill=sex))+
  stat_summary(geom = "bar",fun.data = "mean_cl_normal",position = position_dodge())+
  stat_summary(geom = "errorbar",fun.data = "mean_cl_normal",
               position = position_dodge(.9),width = 0.2)+
  theme_minimal() +
  scale_fill_manual(values = c("#333333", "#AAAAAA"))


#Positive Valence Remembered:
by(samep_30Hz_sc_data$mean_valence_rating_pos_1_rem,samep_30Hz_sc_data$study_group,FUN = summary)

#Creating Plot for Positive Valence Remembered by Group:
ggplot(data = samep_30Hz_sc_data,aes(study_group,mean_valence_rating_pos_1_rem,fill=study_group))+
  stat_summary(geom = "bar",fun.data = "mean_cl_normal",position = position_dodge())+
  stat_summary(geom = "errorbar",fun.data = "mean_cl_normal",
               position = position_dodge(.9),width = 0.2)+
  theme_minimal() +
  scale_fill_manual(values = c("#333333", "#AAAAAA"))

#Creating Plot for Positive Valence Remembered by Group & Sex:
ggplot(data = samep_30Hz_sc_data,aes(study_group,mean_valence_rating_pos_1_rem,fill=sex))+
  stat_summary(geom = "bar",fun.data = "mean_cl_normal",position = position_dodge())+
  stat_summary(geom = "errorbar",fun.data = "mean_cl_normal",
               position = position_dodge(.9),width = 0.2)+
  theme_minimal() +
  scale_fill_manual(values = c("#333333", "#AAAAAA"))


#6.4 Negative Valence:----------------------------------------------------------
#Note: Negative Valence describes the subject's valence rating of the negative (?) pictures 
#when they first saw the pictures (right after each picture.)

#6.4.1 Negative Valence in 30Hz cTBS Offline & Sham Control Combined:-----------
#Negative Valence:
psych::describe(samep_30Hz_sc_data$mean_valence_rating_neg_1)
#n = 29; mean = -131.82; SD = 40.1; median = -133.88; range = -192.5 - -22.04

#Negative Valence Not Remembered:
psych::describe(samep_30Hz_sc_data$mean_valence_rating_neg_1_nrem)
#n = 29; mean = -128.32; SD = 41.8; median = -124.2; range = -199.7 - -21.88

#Negative Valence Remembered:
psych::describe(samep_30Hz_sc_data$mean_valence_rating_neg_1_rem)
#n = 28; mean = -139.54; SD = 40.76; median = -147.34; range = -200 - -22.38


#6.4.2 Negative Valence in 30Hz cTBS Offline Protocol:--------------------------
#Negative Valence:
psych::describe(samep_30Hz_data$mean_valence_rating_neg_1)
#n = 14; mean = -123.47; SD = 48.44; median = -126.56; range = -192.5 - -22.04

#Negative Valence Not Remembered:
psych::describe(samep_30Hz_data$mean_valence_rating_neg_1_nrem)
#n = 14; mean = -118.1; SD = 48.62; median = -118.53, range = -198.8 - -21.88

#Negative Valence Remembered:
psych::describe(samep_30Hz_data$mean_valence_rating_neg_1_rem)
#n = 14; mean = -137.79; SD = 49.47; median = -144.75; range = -200 - -22.38


#6.4.3 Negative Valence in Sham Control Group:----------------------------------
#Negative Valence:
psych::describe(samep_sc_data$mean_valence_rating_neg_1)
#n = 15; mean = -139.62; SD = 30.03; median = -146.12; range = -178.75 - -74.17

#Negative Valence Not Remembered:
psych::describe(samep_sc_data$mean_valence_rating_neg_1_nrem)
#n = 15; mean = -137.85; SD = 33.12; median = -124.2; range = -199.7 - -76.53 

#Negative Valence Remembered:
psych::describe(samep_sc_data$mean_valence_rating_neg_1_rem)
#n = 14; mean = -141.29; SD = 31.58; median = -147.34; range = -184.4 - -68.43


#6.4.4 Negative Valence Summary by Group:---------------------------------------
#Negative Valence:
by(samep_30Hz_sc_data$mean_valence_rating_neg_1,samep_30Hz_sc_data$study_group,FUN = summary)

#Creating Plot for Negative Valence by Group:
ggplot(data = samep_30Hz_sc_data,aes(study_group,mean_valence_rating_neg_1,fill=study_group))+
  stat_summary(geom = "bar",fun.data = "mean_cl_normal",position = position_dodge())+
  stat_summary(geom = "errorbar",fun.data = "mean_cl_normal",
               position = position_dodge(.9),width = 0.2)+
  theme_minimal() +
  scale_fill_manual(values = c("#333333", "#AAAAAA"))

#Creating Plot for Negative Valence by Group & Sex:
ggplot(data = samep_30Hz_sc_data,aes(study_group,mean_valence_rating_neg_1,fill=sex))+
  stat_summary(geom = "bar",fun.data = "mean_cl_normal",position = position_dodge())+
  stat_summary(geom = "errorbar",fun.data = "mean_cl_normal",
               position = position_dodge(.9),width = 0.2)+
  theme_minimal() +
  scale_fill_manual(values = c("#333333", "#AAAAAA"))


#Negative Valence Not Remembered:
by(samep_30Hz_sc_data$mean_valence_rating_neg_1_nrem,samep_30Hz_sc_data$study_group,FUN = summary)

#Creating Plot for Negative Valence Not Remembered by Group:
ggplot(data = samep_30Hz_sc_data,aes(study_group,mean_valence_rating_neg_1_nrem,fill=study_group))+
  stat_summary(geom = "bar",fun.data = "mean_cl_normal",position = position_dodge())+
  stat_summary(geom = "errorbar",fun.data = "mean_cl_normal",
               position = position_dodge(.9),width = 0.2)+
  theme_minimal() +
  scale_fill_manual(values = c("#333333", "#AAAAAA"))

#Creating Plot for Negative Valence Not Remembered by Group & Sex:
ggplot(data = samep_30Hz_sc_data,aes(study_group,mean_valence_rating_neg_1_nrem,fill=sex))+
  stat_summary(geom = "bar",fun.data = "mean_cl_normal",position = position_dodge())+
  stat_summary(geom = "errorbar",fun.data = "mean_cl_normal",
               position = position_dodge(.9),width = 0.2)+
  theme_minimal() +
  scale_fill_manual(values = c("#333333", "#AAAAAA"))


#Negative Valence Remembered:
by(samep_30Hz_sc_data$mean_valence_rating_neg_1_rem,samep_30Hz_sc_data$study_group,FUN = summary)

#Creating Plot for Negative Valence Remembered by Group:
ggplot(data = samep_30Hz_sc_data,aes(study_group,mean_valence_rating_neg_1_rem,fill=study_group))+
  stat_summary(geom = "bar",fun.data = "mean_cl_normal",position = position_dodge())+
  stat_summary(geom = "errorbar",fun.data = "mean_cl_normal",
               position = position_dodge(.9),width = 0.2)+
  theme_minimal() +
  scale_fill_manual(values = c("#333333", "#AAAAAA"))

#Creating Plot for Negative Valence Remembered by Group & Sex:
ggplot(data = samep_30Hz_sc_data,aes(study_group,mean_valence_rating_neg_1_rem,fill=sex))+
  stat_summary(geom = "bar",fun.data = "mean_cl_normal",position = position_dodge())+
  stat_summary(geom = "errorbar",fun.data = "mean_cl_normal",
               position = position_dodge(.9),width = 0.2)+
  theme_minimal() +
  scale_fill_manual(values = c("#333333", "#AAAAAA"))


#6.5 Valence Rating Balance:----------------------------------------------------
#6.5.1 Score of positive valence rating + score of negative valence rating:-----
#Note: VR_balance = Valence Rating Balance = Positive VR + Negative VR: 
#Valence rating balance indicates whether positive (VR_balance > 0) or negative 
#(VR_balance < 0) valence predominates.
#VR_balance < 0: more negative ratings
#VR_balance > 0: more positive ratings
#VR_balance = 0: exactly same ammout of negative & positive valence rating

samep_30Hz_sc_data$VR_balance <- samep_30Hz_sc_data$mean_valence_rating_pos_1 + samep_30Hz_sc_data$mean_valence_rating_neg_1

samep_30Hz_data$VR_balance <- samep_30Hz_data$mean_valence_rating_pos_1 + samep_30Hz_data$mean_valence_rating_neg_1

samep_sc_data$VR_balance <- samep_sc_data$mean_valence_rating_pos_1 + samep_sc_data$mean_valence_rating_neg_1

#6.5.2 Valence Rating Balance in 30Hz cTBS Offline Group:-----------------------
psych::describe(samep_30Hz_data$VR_balance)
#n = 14; mean = 1.1; SD = 35.12; median = -4.69; range = -39.96 - 72

#6.5.3 Valence Rating Balance in Sham Control Group:----------------------------
psych::describe(samep_sc_data$VR_balance)
#n = 15; mean = -22.78; SD = 22.64; median = -19.33; range = -78.5 - 3.12

#6.5.4 Valence Rating Balance Summary by Group:---------------------------------
by(samep_30Hz_sc_data$VR_balance,samep_30Hz_sc_data$study_group,summary)

#Creating Plot for Valence Rating Balance by Group:
ggplot(data = samep_30Hz_sc_data,aes(study_group,VR_balance,fill=study_group))+
  stat_summary(geom = "bar",fun.data = "mean_cl_normal",position = position_dodge())+
  stat_summary(geom = "errorbar",fun.data = "mean_cl_normal",
               position = position_dodge(.9),width = 0.2)+
  theme_minimal() +
  scale_fill_manual(values = c("#333333", "#AAAAAA"))

#Creating Plot for Negative MP by Group & Sex:
ggplot(data = samep_30Hz_sc_data,aes(study_group,VR_balance,fill=sex))+
  stat_summary(geom = "bar",fun.data = "mean_cl_normal",position = position_dodge())+
  stat_summary(geom = "errorbar",fun.data = "mean_cl_normal",
               position = position_dodge(.9),width = 0.2)+
  theme_minimal() +
  scale_fill_manual(values = c("#333333", "#AAAAAA"))


#7. DESCRIPTIVE STATISTICS - OVERALL:-------------------------------------------
#7.1 Demographic Statistics Table:----------------------------------------------
#Creating a table for the demographic statistics in APA style:
demographic_table <- samep_30Hz_sc_data %>%
  dplyr::select(study_group, age, sex, handedness) %>%
  tbl_summary(
    by = study_group, 
    statistic = all_continuous() ~ "{mean} ({sd})",
    missing = "no") %>% 
  add_overall() %>%         
  modify_header(label = "**Variable**") %>%
  bold_labels()

demographic_table


#7.2 Descriptive Statistics of Main Variables Table:----------------------------
descriptive_table <- samep_30Hz_sc_data %>%
  dplyr::select(study_group, rMT, IAPS_HIT, IAPS_HIT_POS, IAPS_HIT_NEU, IAPS_HIT_NEG, MP_dif, mean_valence_rating_1, mean_valence_rating_pos_1, mean_valence_rating_neu_1, mean_valence_rating_neg_1, VR_balance) %>%
  tbl_summary(
    by = study_group, 
    statistic = all_continuous() ~ "{mean} ({sd})",
    type = list(IAPS_HIT_NEU ~ "continuous"),
    missing = "no") %>% 
  add_overall() %>%         
  modify_header(label = "**Variable**") %>%
  bold_labels()

descriptive_table


#7.3 Variability in Response Graph:---------------------------------------------
ggplot(samep_30Hz_sc_data, aes(x = factor(VP_nr), y = MP_dif, fill = sex)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9), width = 0.9) +
  theme_minimal() +
  labs(title = "",
       x = "Subject",
       y = "MP_dif",
       fill = "Gender") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = c("#333333", "#999999")) 


#8. ANALYTICAL ANALYSIS - FIRST STEPS:------------------------------------------
#8.1: Missing Data:-------------------------------------------------------------
colSums(is.na(samep_30Hz_sc_data))

#8.2 Outliers:------------------------------------------------------------------
boxplot(samep_30Hz_sc_data$MP_dif, main = "Boxplot Memory Performance Difference")
boxplot(samep_30Hz_sc_data$VR_balance, main = "Boxplot Valence Rating Balance")


#8.3 Normality:-----------------------------------------------------------------
#8.3.1 Histogram Dependent Variables:-------------------------------------------
hist(samep_30Hz_sc_data$MP_dif, main = "Histogramm Memory Performance Difference", xlab = "Memory Performance Difference")
hist(samep_30Hz_sc_data$VR_balance, main = "Histogramm Valence Rating Balance", xlab = "Valence Rating Balance")

#8.3.2 QQ-Plot:-----------------------------------------------------------------
qqnorm(samep_30Hz_sc_data$MP_dif)
qqline(samep_30Hz_sc_data$MP_dif)
qqnorm(samep_30Hz_sc_data$VR_balance)
qqline(samep_30Hz_sc_data$VR_balance)

#8.3.3 Shapiro-Wilk-Test:-------------------------------------------------------
shapiro.test(samep_30Hz_sc_data$MP_dif) #normally distributed
shapiro.test(samep_30Hz_sc_data$VR_balance) #not normally distributed


#9. PRIMARY ANALYSIS - HYPOTHESIS 1:--------------------------------------------
#Active 30 Hz cTBS will specifically influence aversive memory encoding performance
#in the experimental group (i.e. the group receiving active stimulation) compared 
#to the control group (i.e. the group receiving sham control stimulation). 
#Meaning less neg. remembered pictures in cTBS 30Hz group compared to sham control group
#Meaning difference between pos. to neg. remembered pictures is higher (more positive 
#remembered pictures compared to neg. remembered pictures) in the cTBS 30Hz group.

#(number pos. – number neg. pictures remembered)  ~  Group + Gender + Age + rM
#9.1 Run Model:-----------------------------------------------------------------
samep_30Hz_sc_data$study_group <- factor(samep_30Hz_sc_data$study_group)
samep_30Hz_sc_data$study_group <- relevel(samep_30Hz_sc_data$study_group, ref = "cTBS_sham")

model1 <- lm(MP_dif ~ study_group + sex + age + rMT, data = samep_30Hz_sc_data)
summary(model1)

#9.2 Linearity:-----------------------------------------------------------------
plot(model1, which = 1)  #Not perfect, but acceptable

#9.3 Variance Homogeneity (Levene Test):----------------------------------------
leveneTest(residuals(model1) ~ samep_30Hz_sc_data$study_group) 
#p = 0.75 => Variance homogeneity is given

#9.4 Normality (Shapiro-Wilk Test):---------------------------------------------
qqnorm(residuals(model1))
qqline(residuals(model1))
shapiro.test(residuals(model1))  #p = 0.32 => Normality is given

#9.5 Multicollinearity:---------------------------------------------------------
vif(model1) #no problematic multicollinearity

#9.6 Independence of Residuals (Durbin-Watson-Test):----------------------------
dwtest(model1) #p = 0.4 => No significant Autocorrelation

#9.7 Effect Size:---------------------------------------------------------------
#9.7.1 Effect Size for Study Group:---------------------------------------------
model_full1_sg <- lm(MP_dif ~ study_group + sex + age + rMT, data = samep_30Hz_sc_data)
model_reduced1_sg <- lm(MP_dif ~ sex + age + rMT, data = samep_30Hz_sc_data)

R2_full1_sg <- summary(model_full1_sg)$r.squared
R2_reduced1_sg <- summary(model_reduced1_sg)$r.squared

f2_1_sg <- (R2_full1_sg - R2_reduced1_sg) / (1 - R2_full1_sg)
f2_1_sg

#9.7.2 Effect Size for Sex:-----------------------------------------------------
model_full1_sex <- lm(MP_dif ~ study_group + sex + age + rMT, data = samep_30Hz_sc_data)
model_reduced1_sex <- lm(MP_dif ~ study_group + age + rMT, data = samep_30Hz_sc_data)

R2_full1_sex <- summary(model_full1_sex)$r.squared
R2_reduced1_sex <- summary(model_reduced1_sex)$r.squared

f2_1_sex <- (R2_full1_sex - R2_reduced1_sex) / (1 - R2_full1_sex)
f2_1_sex

#9.7.3 Effect Size for Age:-----------------------------------------------------
model_full1_age <- lm(MP_dif ~ study_group + sex + age + rMT, data = samep_30Hz_sc_data)
model_reduced1_age <- lm(MP_dif ~ study_group + sex + rMT, data = samep_30Hz_sc_data)

R2_full1_age <- summary(model_full1_age)$r.squared
R2_reduced1_age <- summary(model_reduced1_age)$r.squared

f2_1_age <- (R2_full1_age - R2_reduced1_age) / (1 - R2_full1_age)
f2_1_age

#9.7.4 Effect Size for rMT:-----------------------------------------------------
model_full1_rMT <- lm(MP_dif ~ study_group + sex + age + rMT, data = samep_30Hz_sc_data)
model_reduced1_rMT <- lm(MP_dif ~ study_group + sex + age, data = samep_30Hz_sc_data)

R2_full1_rMT <- summary(model_full1_rMT)$r.squared
R2_reduced1_rMT <- summary(model_reduced1_rMT)$r.squared

f2_1_rMT <- (R2_full1_rMT - R2_reduced1_rMT) / (1 - R2_full1_rMT)
f2_1_rMT


#9.8 Plot MP Difference by Group:-----------------------------------------------
ggplot(data = samep_30Hz_sc_data, aes(x = study_group, y = MP_dif, fill = study_group)) +
  geom_boxplot(width = 0.6, alpha = (0.75)) +  
  scale_fill_manual(values = c("cTBS_30Hz_exp" = "#333333", "cTBS_sham" = "#999999")) +
  theme_minimal() +
  labs(x = "Group", y = "Memory Performance Difference", title = "", fill = "Group") +
  scale_x_discrete(labels = c("cTBS_30Hz_exp" = "Experimental", "cTBS_sham" = "Control")) +
  scale_y_continuous(limits = c(-8, 9), expand = c(0, 0)) +  
  theme(text = element_text(size = 12),
    legend.position = "none") +
  annotate("text", x = 1.5, y = 6.5,
           label = "p = 0.256\nf² = 0.054",
           size = 4,
           fontface = "italic")


#9.9 Non-Parametric Test:-------------------------------------------------------
#9.9.1 Run Model:---------------------------------------------------------------
#For Study Group:
model1_np_group <- wilcox.test(MP_dif ~ study_group, data = samep_30Hz_sc_data)
print(model1_np_group)

#For Gender:
model1_np_sex <- wilcox.test(MP_dif ~ sex, data = samep_30Hz_sc_data)
print(model1_np_sex)

#9.9.2 Distribution Similarity:-------------------------------------------------
#For Study Group:
ks.test(samep_30Hz_sc_data$MP_dif[samep_30Hz_sc_data$study_group == "cTBS_sham"],
        samep_30Hz_sc_data$MP_dif[samep_30Hz_sc_data$study_group == "cTBS_30Hz_exp"])

#For Gender:
ks.test(samep_30Hz_sc_data$MP_dif[samep_30Hz_sc_data$sex == "Female"],
        samep_30Hz_sc_data$MP_dif[samep_30Hz_sc_data$sex == "Male"])

#9.9.3 Effect Sizes:------------------------------------------------------------
#For Study Group:
effect_size1_np_group <- samep_30Hz_sc_data %>%
  wilcox_effsize(MP_dif ~ study_group)
print(effect_size1_np_group)

#For Gender:
effect_size1_np_sex <- samep_30Hz_sc_data %>%
  wilcox_effsize(MP_dif ~ sex)
print(effect_size1_np_sex)


#10. PRIMARY ANALYSIS - HYPOTHESIS 2:-------------------------------------------
#Active 30 Hz cTBS will result in the inhibition of specifically negative valence 
#rating in the experimental group compared to the control group.
#Meaning less neg. valence rating in the cTBS 30Hz group compared to the sham control group
#Meaning higher valence rating balance in the cTBS 30Hz group compared to the sham control group.

#(pos. valence rating – neg. valence rating) ~  Group + Gender + Age + rMT
#10.1 T-Test for Positive Valence between Groups:-------------------------------
#Test if the assumption is correct that the positive valence ratings do not
#differ between the groups.
#10.1.1 Run Test:---------------------------------------------------------------
t.test(mean_valence_rating_pos_1 ~ study_group, data = samep_30Hz_sc_data, var.equal = TRUE)

#10.1.2 Assumptions for T-Test:-------------------------------------------------
#Normality:
shapiro.test(samep_30Hz_sc_data$mean_valence_rating_pos_1[samep_30Hz_sc_data$study_group == "cTBS_30Hz_exp"])
shapiro.test(samep_30Hz_sc_data$mean_valence_rating_pos_1[samep_30Hz_sc_data$study_group == "cTBS_sham"])

#Valence Homogeneitiy:
leveneTest(mean_valence_rating_pos_1 ~ study_group, data = samep_30Hz_sc_data)


#10.2 Run Model:----------------------------------------------------------------
model2 <- lm(VR_balance ~ study_group + sex + age + rMT, data = samep_30Hz_sc_data)
summary(model2)

used_data_model2 <- model.frame(model2)
nrow(used_data_model2)


#10.3 Linearity:----------------------------------------------------------------
plot(model2, which = 1)  #Not perfect, but acceptable

#10.4 Variance Homogeneity (Levene Test):---------------------------------------
leveneTest(residuals(model2) ~ samep_30Hz_sc_data$study_group) #Error!

used_data <- model.frame(model2)
nrow(used_data)  #Number of Rows used in the model 
complete_cases <- complete.cases(samep_30Hz_sc_data[, c("VR_balance", "study_group", "sex", "age", "rMT")])
which(!complete_cases)  #VPN 174 is NA, should be excluded

filtered_data <- samep_30Hz_sc_data[complete_cases, ]
leveneTest(residuals(model2) ~ filtered_data$study_group)
#p = 0.31 => Variance homogeneity is given

#10.5 Normality (Shapiro-Wilk Test):--------------------------------------------
qqnorm(residuals(model2))
qqline(residuals(model2))
shapiro.test(residuals(model2))  #p = 0.087 => Normality is given

#10.6 Multicollinearity:--------------------------------------------------------
vif(model2) #no problematic multicollinearity

#10.7 Independence of Residuals (Durbin-Watson-Test):---------------------------
dwtest(model2) #p = 0.84 => No significant Autocorelation

#10.8 Effect Size:--------------------------------------------------------------
#10.8.1 Effect Size for Group:--------------------------------------------------
model_full2_g <- lm(VR_balance ~ study_group + sex + age + rMT, data = samep_30Hz_sc_data)
model_reduced2_g <- lm(VR_balance ~ sex + age + rMT, data = samep_30Hz_sc_data)

R2_full2_g <- summary(model_full2_g)$r.squared
R2_reduced2_g <- summary(model_reduced2_g)$r.squared

f2_2_g <- (R2_full2_g - R2_reduced2_g) / (1 - R2_full2_g)
f2_2_g

#10.8.2 Effect Size for Sex:----------------------------------------------------
model_full2_sex <- lm(VR_balance ~ study_group + sex + age + rMT, data = samep_30Hz_sc_data)
model_reduced2_sex <- lm(VR_balance ~ study_group + age + rMT, data = samep_30Hz_sc_data)

R2_full2_sex <- summary(model_full2_sex)$r.squared
R2_reduced2_sex <- summary(model_reduced2_sex)$r.squared

f2_2_sex <- (R2_full2_sex - R2_reduced2_sex) / (1 - R2_full2_sex)
f2_2_sex

#10.8.3 Effect Size for Age:----------------------------------------------------
model_full2_age <- lm(VR_balance ~ study_group + sex + age + rMT, data = samep_30Hz_sc_data)
model_reduced2_age <- lm(VR_balance ~ study_group + sex + rMT, data = samep_30Hz_sc_data)

R2_full2_age <- summary(model_full2_age)$r.squared
R2_reduced2_age <- summary(model_reduced2_age)$r.squared

f2_2_age <- (R2_full2_age - R2_reduced2_age) / (1 - R2_full2_age)
f2_2_age

#10.8.4 Effect Size for rMT:----------------------------------------------------
model_full2_rMT <- lm(VR_balance ~ study_group + sex + age + rMT, data = samep_30Hz_sc_data)
model_reduced2_rMT <- lm(VR_balance ~ study_group + sex + age, data = samep_30Hz_sc_data)

R2_full2_rMT <- summary(model_full2_rMT)$r.squared
R2_reduced2_rMT <- summary(model_reduced2_rMT)$r.squared

f2_2_rMT <- (R2_full2_rMT - R2_reduced2_rMT) / (1 - R2_full2_rMT)
f2_2_rMT

#10.9 Plot VR Balance by Group:-------------------------------------------------
ggplot(data = samep_30Hz_sc_data, aes(x = study_group, y = VR_balance, fill = study_group)) +
  geom_boxplot(width = 0.6, alpha = (0.75)) +  
  scale_fill_manual(values = c("cTBS_30Hz_exp" = "#333333", "cTBS_sham" = "#999999")) +
  theme_minimal() +
  labs(x = "Group", y = "VR Balance", title = "", fill = "Group") +
  scale_x_discrete(labels = c("cTBS_30Hz_exp" = "Experimental", "cTBS_sham" = "Control")) +
  scale_y_continuous(limits = c(-100, 100), expand = c(0, 0)) +  
  theme(text = element_text(size = 12),
        legend.position = "none") +
  annotate("text", x = 1.5, y = 50,
           label = "p = 0.063\nf² = 0.16",
           size = 4,
           fontface = "italic")


#10.10 Non-Parametric Test:------------------------------------------------------
#10.10.1 Run Model:---------------------------------------------------------------
model2_np <- wilcox.test(VR_balance ~ study_group, data = samep_30Hz_sc_data)
print(model2_np)

#10.10.2 Distribution Similarity:-------------------------------------------------
ks.test(samep_30Hz_sc_data$VR_balance[samep_30Hz_sc_data$study_group == "cTBS_sham"],
        samep_30Hz_sc_data$VR_balance[samep_30Hz_sc_data$study_group == "cTBS_30Hz_exp"])

#10.10.3 Effect Sizes:-----------------------------------------------------------
effect_size2_np <- samep_30Hz_sc_data %>%
  wilcox_effsize(VR_balance ~ study_group)
print(effect_size2_np)


#11. SECONDARY ANALYSIS - GENDER DIFFERENCES:-----------------------------------
#Running test for hypothesis 1 again but for female and male subjects individually.
#11.1 First Steps:--------------------------------------------------------------
#11.1.1 Creating 30Hz cTBS Offline Protocol & Sham Control only Female Subset:----
samep_30Hz_sc_data_f<- subset(samep_30Hz_sc_data, sex == "Female")
head(samep_30Hz_sc_data_f)
View(samep_30Hz_sc_data_f)

#11.1.2 Creating 30Hz cTBS Offline Protocol & Sham Control only Male Subset:-----
samep_30Hz_sc_data_m<- subset(samep_30Hz_sc_data, sex == "Male")
head(samep_30Hz_sc_data_m)
View(samep_30Hz_sc_data_m)

#11.1.3 Memory Performance Difference:------------------------------------------
samep_30Hz_sc_data_f$MP_dif <- samep_30Hz_sc_data_f$IAPS_HIT_POS - samep_30Hz_sc_data_f$IAPS_HIT_NEG
samep_30Hz_sc_data_m$MP_dif <- samep_30Hz_sc_data_m$IAPS_HIT_POS - samep_30Hz_sc_data_m$IAPS_HIT_NEG


#11.2 Run Model:----------------------------------------------------------------
#For Female Subjects:
samep_30Hz_sc_data_f$study_group <- factor(samep_30Hz_sc_data_f$study_group)
samep_30Hz_sc_data_f$study_group <- relevel(samep_30Hz_sc_data_f$study_group, ref = "cTBS_sham")

model3 <- lm(MP_dif ~ study_group + age + rMT, data = samep_30Hz_sc_data_f)
summary(model3)

#For Male Subjects:
samep_30Hz_sc_data_m$study_group <- factor(samep_30Hz_sc_data_m$study_group)
samep_30Hz_sc_data_m$study_group <- relevel(samep_30Hz_sc_data_m$study_group, ref = "cTBS_sham")

model4 <- lm(MP_dif ~ study_group + age + rMT, data = samep_30Hz_sc_data_m)
summary(model4)


#11.3 Linearity:----------------------------------------------------------------
#For Female Subjects:
plot(model3, which = 1)  #Problematic

#For Male Subjects:
plot(model4, which = 1) #Problematic


#11.4 Variance Homogeneity (Levene Test):----------------------------------------
#For Female Subjects:
leveneTest(residuals(model3) ~ samep_30Hz_sc_data_f$study_group) 
#p = 0.71 => Variance homogeneity is given

#For Male Subjects:
leveneTest(residuals(model4) ~ samep_30Hz_sc_data_m$study_group) 
#p = 0.59 => Variance homogeneity is given


#11.5 Normality (Shapiro-Wilk Test):---------------------------------------------
#For Female Subjects:
qqnorm(residuals(model3))
qqline(residuals(model3))
shapiro.test(residuals(model3))  #p = 0.80 => Normality is given

#For Male Subjects:
qqnorm(residuals(model4))
qqline(residuals(model4))
shapiro.test(residuals(model4))  #p = 0.71 => Normality is given


#11.6 Multicollinearity:---------------------------------------------------------
#For Female Subjects:
vif(model3) #no problematic multicollinearity

#For Male Subjects:
vif(model4) #no problematic multicollinearity


#11.7 Independence of Residuals (Durbin-Watson-Test):-------------------------
#For Female Subjects:
dwtest(model3) #p = 0.66 => No significant Autocorrelation

#For Male Subjects:
dwtest(model4) #p = 0.46 => No significant Autocorrelation


#11.8 Effect Size:--------------------------------------------------------------
#11.8.1 Effect Size For Study Group:--------------------------------------------
#For Female:
model_full3_g <- lm(MP_dif ~ study_group + age + rMT, data = samep_30Hz_sc_data_f)
model_reduced3_g <- lm(MP_dif ~ age + rMT, data = samep_30Hz_sc_data_f)

R2_full3_g <- summary(model_full3_g)$r.squared
R2_reduced3_g <- summary(model_reduced3_g)$r.squared

f2_3_g <- (R2_full3_g - R2_reduced3_g) / (1 - R2_full3_g)
f2_3_g

#For Male:
model_full4_g <- lm(MP_dif ~ study_group + age + rMT, data = samep_30Hz_sc_data_m)
model_reduced4_g <- lm(MP_dif ~ age + rMT, data = samep_30Hz_sc_data_m)

R2_full4_g <- summary(model_full4_g)$r.squared
R2_reduced4_g <- summary(model_reduced4_g)$r.squared

f2_4_g <- (R2_full4_g - R2_reduced4_g) / (1 - R2_full4_g)
f2_4_g

#11.8.2 Effect Size For Age:----------------------------------------------------
#For Female:
model_full3_age <- lm(MP_dif ~ study_group + age + rMT, data = samep_30Hz_sc_data_f)
model_reduced3_age <- lm(MP_dif ~ study_group + rMT, data = samep_30Hz_sc_data_f)

R2_full3_age <- summary(model_full3_age)$r.squared
R2_reduced3_age <- summary(model_reduced3_age)$r.squared

f2_3_age <- (R2_full3_age - R2_reduced3_age) / (1 - R2_full3_age)
f2_3_age

#For Male:
model_full4_age <- lm(MP_dif ~ study_group + age + rMT, data = samep_30Hz_sc_data_m)
model_reduced4_age <- lm(MP_dif ~ study_group + rMT, data = samep_30Hz_sc_data_m)

R2_full4_age <- summary(model_full4_age)$r.squared
R2_reduced4_age <- summary(model_reduced4_age)$r.squared

f2_4_age <- (R2_full4_age - R2_reduced4_age) / (1 - R2_full4_age)
f2_4_age

#11.8.3 Effect Size For rMT:----------------------------------------------------
#For Female:
model_full3_rMT <- lm(MP_dif ~ study_group + age + rMT, data = samep_30Hz_sc_data_f)
model_reduced3_rMT <- lm(MP_dif ~ study_group + age, data = samep_30Hz_sc_data_f)

R2_full3_rMT <- summary(model_full3_rMT)$r.squared
R2_reduced3_rMT <- summary(model_reduced3_rMT)$r.squared

f2_3_rMT <- (R2_full3_rMT - R2_reduced3_rMT) / (1 - R2_full3_rMT)
f2_3_rMT

#For Male:
model_full4_rMT <- lm(MP_dif ~ study_group + age + rMT, data = samep_30Hz_sc_data_m)
model_reduced4_rMT <- lm(MP_dif ~ study_group + age, data = samep_30Hz_sc_data_m)

R2_full4_rMT <- summary(model_full4_rMT)$r.squared
R2_reduced4_rMT <- summary(model_reduced4_rMT)$r.squared

f2_4_rMT <- (R2_full4_rMT - R2_reduced4_rMT) / (1 - R2_full4_rMT)
f2_4_rMT


#11.9 Non Parametric Test:------------------------------------------------------
#11.9.1 Run Model:--------------------------------------------------------------
#For Female Subjects:
model3_np <- wilcox.test(MP_dif ~ study_group, data = samep_30Hz_sc_data_f)
print(model3_np)

#For Male Subjects:
model4_np <- wilcox.test(MP_dif ~ study_group, data = samep_30Hz_sc_data_m)
print(model4_np)

#11.9.2 Distribution Similarity:------------------------------------------------
#For Female Subjects:
ks.test(samep_30Hz_sc_data_f$MP_dif[samep_30Hz_sc_data_f$study_group == "cTBS_sham"],
        samep_30Hz_sc_data_f$MP_dif[samep_30Hz_sc_data_f$study_group == "cTBS_30Hz_exp"])

#For Male Subjects:
ks.test(samep_30Hz_sc_data_m$MP_dif[samep_30Hz_sc_data_m$study_group == "cTBS_sham"],
        samep_30Hz_sc_data_m$MP_dif[samep_30Hz_sc_data_m$study_group == "cTBS_30Hz_exp"])

#11.9.3 Effect Sizes:-----------------------------------------------------------
#For Female Subjects:
effect_size3_np <- samep_30Hz_sc_data_f %>%
  wilcox_effsize(MP_dif ~ study_group)
print(effect_size3_np)

#For Male Subjects.
effect_size4_np <- samep_30Hz_sc_data_m %>%
  wilcox_effsize(MP_dif ~ study_group)
print(effect_size4_np)

#11.9.4 Plot for Difference between Groups Regarding MP Difference:-------------
#For Female Subjects:
ggplot(data = samep_30Hz_sc_data_f, aes(x = study_group, y = MP_dif, fill = study_group)) +
  geom_boxplot(width = 0.6, alpha = (0.75)) +  
  scale_fill_manual(values = c("cTBS_30Hz_exp" = "#333333", "cTBS_sham" = "#999999")) +
  theme_minimal() +
  labs(x = "Group", y = "Female Memory Performance Difference", title = "", fill = "Group") +
  scale_x_discrete(labels = c("cTBS_30Hz_exp" = "Experimental", "cTBS_sham" = "Control")) +
  scale_y_continuous(limits = c(-10, 10), expand = c(0, 0)) +  
  theme(text = element_text(size = 12),
        legend.position = "none") +
  annotate("text", x = 1.5, y = 7,
           label = "p = 0.79\nr = 0.08",
           size = 4,
           fontface = "italic")

#For Male Subjects:
ggplot(data = samep_30Hz_sc_data_m, aes(x = study_group, y = MP_dif, fill = study_group)) +
  geom_boxplot(width = 0.6, alpha = (0.75)) +  
  scale_fill_manual(values = c("cTBS_30Hz_exp" = "#333333", "cTBS_sham" = "#999999")) +
  theme_minimal() +
  labs(x = "Group", y = "Male Memory Performance Difference", title = "", fill = "Group") +
  scale_x_discrete(labels = c("cTBS_30Hz_exp" = "Experimental", "cTBS_sham" = "Control")) +
  scale_y_continuous(limits = c(-10, 10), expand = c(0, 0)) +  
  theme(text = element_text(size = 12),
        legend.position = "none") +
  annotate("text", x = 1.5, y = 7,
           label = "p = 0.07\nr = 0.50",
           size = 4,
           fontface = "italic")


#12. EXPLORATORY ANALYSIS - ACTIVE CONTROL:-------------------------------------
#Active 30 Hz cTBS will result in the inhibition of specifically negative valence 
#rating in the experimental group compared to the active control group.
#Meaning less neg. valence rating in the cTBS 30Hz group compared to the active control group
#Meaning higher valence rating balance in the cTBS 30Hz group compared to the active control group.

#(pos. valence rating – neg. valence rating) ~  Group + Gender + Age + rMT

#Note: Only Variables used in the Hypothesis 2 will be analyzed here, since 
#a moderate effect size was found. Variables used in Hypothesis 1 will not be 
#analyzed further, since no moderate to large effect size was found.

#12.1 First Steps:--------------------------------------------------------------
#12.1.1 Creating 30Hz cTBS Offline & Active Control Subset:---------------------
samep_30Hz_ac_data <- subset(samep_data, study_group == "cTBS_30Hz_exp"| study_group == "cTBS_30Hz_control")
head(samep_30Hz_ac_data)
View(samep_30Hz_ac_data)

samep_30Hz_ac_data$study_group <- factor(samep_30Hz_ac_data$study_group)
samep_30Hz_ac_data$study_group <- relevel(samep_30Hz_ac_data$study_group, ref = "cTBS_30Hz_control")

#12.1.2 Prepare Valence Variables:
samep_30Hz_ac_data$mean_valence_rating_1 <- as.numeric(samep_30Hz_ac_data$mean_valence_rating_1)
samep_30Hz_ac_data$mean_valence_rating_pos_1 <- as.numeric(samep_30Hz_ac_data$mean_valence_rating_pos_1)
samep_30Hz_ac_data$mean_valence_rating_neu_1 <- as.numeric(samep_30Hz_ac_data$mean_valence_rating_neu_1)
samep_30Hz_ac_data$mean_valence_rating_neg_1 <- as.numeric(samep_30Hz_ac_data$mean_valence_rating_neg_1)

#12.1.3 Creating VR Balance Variable:-------------------------------------------
samep_30Hz_ac_data$VR_balance <- samep_30Hz_ac_data$mean_valence_rating_pos_1 + samep_30Hz_ac_data$mean_valence_rating_neg_1

#12.2 Descriptive Statistics in Active Control:---------------------------------
#12.2.1 Demographic Statistics Table:-------------------------------------------
#Creating a table for the demographic statistics in APA style:
demographic_table_ac <- samep_30Hz_ac_data %>%
  dplyr::select(study_group, age, sex, handedness) %>%
  tbl_summary(
    by = study_group, 
    statistic = all_continuous() ~ "{mean} ({sd})",
    missing = "no") %>% 
  add_overall() %>%         
  modify_header(label = "**Variable**") %>%
  bold_labels()

demographic_table_ac

#12.2.2 Descriptive Statistics of Main Variables Table:-------------------------
descriptive_table_ac <- samep_30Hz_ac_data %>%
  dplyr::select(study_group, rMT, mean_valence_rating_1, mean_valence_rating_pos_1, mean_valence_rating_neu_1, mean_valence_rating_neg_1, VR_balance) %>%
  tbl_summary(
    by = study_group, 
    statistic = all_continuous() ~ "{mean} ({sd})",
    missing = "no") %>% 
  add_overall() %>%         
  modify_header(label = "**Variable**") %>%
  bold_labels()

descriptive_table_ac

#12.3 T-Test for Positive Valence between Groups:-------------------------------
#Test if the assumption is correct that the positive valence ratings do not
#differ between the groups.
#12.3.1 Run Test:---------------------------------------------------------------
t.test(mean_valence_rating_pos_1 ~ study_group, data = samep_30Hz_ac_data, var.equal = TRUE)

#12.2.2 Assumptions for T-Test:-------------------------------------------------
#Normality:
shapiro.test(samep_30Hz_ac_data$mean_valence_rating_pos_1[samep_30Hz_ac_data$study_group == "cTBS_30Hz_exp"])
shapiro.test(samep_30Hz_ac_data$mean_valence_rating_pos_1[samep_30Hz_ac_data$study_group == "cTBS_30Hz_control"])

#Variance Homogeneity:
leveneTest(mean_valence_rating_pos_1 ~ study_group, data = samep_30Hz_ac_data)
#p = 0.041 => Homogeneity of Variance not given 

#12.3.2 Non-Parametric Test:----------------------------------------------------
wilcox.test(mean_valence_rating_pos_1 ~ study_group, data = samep_30Hz_ac_data)


#12.4 Run Model:----------------------------------------------------------------
model5 <- lm(VR_balance ~ study_group + sex + age + rMT, data = samep_30Hz_ac_data)
summary(model5)

#12.5 Linearity:----------------------------------------------------------------
plot(model5, which = 1) #Problematic

#12.6 Variance Homogeneity (Levene Test):---------------------------------------
leveneTest(residuals(model5) ~ samep_30Hz_ac_data$study_group) #Error!

used_data_ac <- model.frame(model5)
nrow(used_data_ac)  #Number of Rows used in the model 
complete_cases_ac <- complete.cases(samep_30Hz_ac_data[, c("VR_balance", "study_group", "sex", "age", "rMT")])
which(!complete_cases_ac)  #VPN 174 & 193 are NA, should be excluded

filtered_data_ac <- samep_30Hz_ac_data[complete_cases_ac, ]
leveneTest(residuals(model5) ~ filtered_data_ac$study_group)
#p = 0.36 => Variance homogeneity is given

#12.7 Normality (Shapiro-Wilk Test):--------------------------------------------
qqnorm(residuals(model5))
qqline(residuals(model5))
shapiro.test(residuals(model5))  #p = 0.068 => Normality is given

#12.8 Multicollinearity:--------------------------------------------------------
vif(model5) #no problematic multicollinearity

#12.9 Independence of Residuals (Durbin-Watson-Test):---------------------------
dwtest(model5) #p = 0.98 => No significant Autocorelation

#12.10 Effect Size:--------------------------------------------------------------
#12.10.1 Effect Size for Group:--------------------------------------------------
model_full5_g <- lm(VR_balance ~ study_group + sex + age + rMT, data = samep_30Hz_ac_data)
model_reduced5_g <- lm(VR_balance ~ sex + age + rMT, data = samep_30Hz_ac_data)

R2_full5_g <- summary(model_full5_g)$r.squared
R2_reduced5_g <- summary(model_reduced5_g)$r.squared

f2_5_g <- (R2_full5_g - R2_reduced5_g) / (1 - R2_full5_g)
f2_5_g

#12.10.2 Effect Size for Sex:----------------------------------------------------
model_full5_sex <- lm(VR_balance ~ study_group + sex + age + rMT, data = samep_30Hz_ac_data)
model_reduced5_sex <- lm(VR_balance ~ study_group + age + rMT, data = samep_30Hz_ac_data)

R2_full5_sex <- summary(model_full5_sex)$r.squared
R2_reduced5_sex <- summary(model_reduced5_sex)$r.squared

f2_5_sex <- (R2_full5_sex - R2_reduced5_sex) / (1 - R2_full5_sex)
f2_5_sex

#12.10.3 Effect Size for Age:----------------------------------------------------
model_full5_age <- lm(VR_balance ~ study_group + sex + age + rMT, data = samep_30Hz_ac_data)
model_reduced5_age <- lm(VR_balance ~ study_group + sex + rMT, data = samep_30Hz_ac_data)

R2_full5_age <- summary(model_full5_age)$r.squared
R2_reduced5_age <- summary(model_reduced5_age)$r.squared

f2_5_age <- (R2_full5_age - R2_reduced5_age) / (1 - R2_full5_age)
f2_5_age

#12.10.4 Effect Size for rMT:----------------------------------------------------
model_full5_rMT <- lm(VR_balance ~ study_group + sex + age + rMT, data = samep_30Hz_ac_data)
model_reduced5_rMT <- lm(VR_balance ~ study_group + sex + age, data = samep_30Hz_ac_data)

R2_full5_rMT <- summary(model_full5_rMT)$r.squared
R2_reduced5_rMT <- summary(model_reduced5_rMT)$r.squared

f2_5_rMT <- (R2_full5_rMT - R2_reduced5_rMT) / (1 - R2_full5_rMT)
f2_5_rMT

#12.11 Non-Parametric Test:------------------------------------------------------
#12.11.1 Run Model:---------------------------------------------------------------
model5_np <- wilcox.test(VR_balance ~ study_group, data = samep_30Hz_ac_data)
print(model5_np)

#12.11.2 Distribution Similarity:-------------------------------------------------
ks.test(samep_30Hz_ac_data$VR_balance[samep_30Hz_ac_data$study_group == "cTBS_30Hz_control"],
        samep_30Hz_ac_data$VR_balance[samep_30Hz_ac_data$study_group == "cTBS_30Hz_exp"])

#12.11.3 Effect Sizes:-----------------------------------------------------------
effect_size5_np <- samep_30Hz_ac_data %>%
  wilcox_effsize(VR_balance ~ study_group)
print(effect_size5_np)

#12.11.4 Plot for Differences in VR Balance between Groups (active Control):-----
ggplot(data = samep_30Hz_ac_data, aes(x = study_group, y = VR_balance, fill = study_group)) +
  geom_boxplot(width = 0.6, alpha = (0.75)) +  
  scale_fill_manual(values = c("cTBS_30Hz_exp" = "#333333", "cTBS_30Hz_control" = "#999999")) +
  theme_minimal() +
  labs(x = "Group", y = "Valence Rating Balance", title = "", fill = "Group") +
  scale_x_discrete(labels = c("cTBS_30Hz_exp" = "Experimental", "cTBS_30Hz_control" = "Activ Control")) +
  scale_y_continuous(limits = c(-10, 10), expand = c(0, 0)) +  
  theme(text = element_text(size = 12),
        legend.position = "none") +
  annotate("text", x = 1.5, y = 7,
           label = "p = 0.09\nr = 0.32",
           size = 4,
           fontface = "italic")

