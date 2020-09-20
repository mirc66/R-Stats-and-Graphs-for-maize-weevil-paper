#Ref: https://rcompanion.org/rcompanion/d_06.html
#This is the analysis by locatino in room not by location in the plant
#desktop
#setwd("D:/Users/mromero/Files/files/tower/R Project Charlie  Maize Weevil 1_6_2020")

#laptop with ext HD
setwd("D:/telework transfer files/R Project Charlie  Maize Weevil 1_6_2020")

# ================ data Wrangling=============================================
# Note the library command not working
library("reshape2")  #  melt functionhad to physically click on in Packages  tried both w/wo ""
library("tidyverse")#transmutate function
library(dplyr)

#load data
my_data.df <-read.csv("SAS Allcorrected.csv")# dataframe to manipulate

#  remove the nocapture 
my_data.df<-my_data.df%>%filter(substrate != "nocapture")

# #add all females and males into one column each note name change
 my_data.df$f <- (my_data.df$lfemale+my_data.df$sfemale+my_data.df$rfemale)
 my_data.df$m <- (my_data.df$lmale+my_data.df$smale+my_data.df$rmale)
 my_data.df$tot_Insects <- (my_data.df$leaf+my_data.df$soil+my_data.df$root)

#roomposition from int to factor
my_data.df$roomposition <-as.factor(my_data.df$roomposition)

#analysis by roomposition
dataplot2_1<-my_data.df[,c(3,4,14,15,16)]#anal inc room loc agglomerating to plant only
#combine male & female columns into one sex column and  freq column
dataplot2_2  <-melt(dataplot2_1,c("rearing","substrate","roomposition"))
#rename column5 freq
names(dataplot2_2)[5]<-"freq"

#since we are interested in wheather room position affects the plant
#selected drop the rearing grain from the analysis
dataplot=subset(dataplot2_2,select=-(rearing))
#rename column1 Plant selected
names(dataplot)[1]<-"Plant"
names(dataplot)[3]<-"sex"
dataplot_all<-dataplot

#write.csv(dataplot,"D:/telework transfer files/R Project Charlie  Maize Weevil 1_6_2020/dataplot_weevils_roomloc.csv")

#=====================Plots===================================================
#Summary  Stats :

#https://www.datanovia.com/en/lessons/anova-in-r/#two-way-independent-anova
library(ggpubr)# not library is not loading packages click on them
#install.packages("rstatix")
library(rstatix)
#install.packages ("cowplot")
library(cowplot)
#install.packages ("ggrepel")

#reorder columns orig plant,loc, sex, freq
library("tibble")
dataplot_reord<-dataplot[,c(1,3,2,4)]#plant,sex,loc, freq

#combine plant and sex
dataplot_all<-dataplot_reord%>% unite("Plant_sex",Plant:sex, remove=F)

#Summary  by loc all----------------------------

 summary_mean_all<-dataplot_all %>%group_by(Plant_sex,sex,roomposition,Plant) %>%
   get_summary_stats(freq, type = "mean_sd")

#calculate the SE
summary_mean_all$SE <- (summary_mean_all$mean/sqrt(summary_mean_all$sd))

#Note: boxplot no error bar wiskers are max and min, edges are lower upper quantile and horiz line median
par(mar=c(1,1,1,1))

##boxplot all
# 

P_boxplot_all<- ggplot(dataplot_all,aes(x=Plant_sex,y=freq ,fill=sex))+
  geom_boxplot()+ ylab('Weevils')+ scale_fill_grey(start=0.8,end=0.5)+
  #theme_classic()+#greyscale color select
  #theme(axis.text.x=element_text(size=9))+#font x axis
  stat_summary(fun=mean,geom="point",shape=18,size=4)+#mean diamond
  #theme(axis.text.x = element_blank() )+
  #theme(axis.title.x = element_blank() )+
  facet_grid(~roomposition)  

  P_boxplot_all# actually displays the grap

#bar chart===============================================
# 
P_bar_loc1<-ggplot(summary_mean_loc1,aes(x=Plant_sex,y=mean ,fill=sex))+

# P_bar_Mean& SE

  P_bar_all<-ggplot(summary_mean_all,aes(x=Plant_sex,y=mean ,fill=sex))+
    ylab('Weevil mean')+
    theme(axis.text.x=element_text(size=11))+#font x axis
    geom_errorbar(aes(ymin=mean-SE,ymax=mean+SE),width=0.1)+
    scale_fill_grey(start=0.25,end=0.75)+#greyscale
    geom_bar(stat="identity",position=position_dodge()) +#creates bar
    #scale_fill_grey(start=0.25,end=0.75)+
    scale_y_continuous(breaks=seq(0,60,5))+#ticks every 5fom0 to 60
  #  theme(axis.text.x = element_blank() )+#blankx axis labes too tight
  #  theme(axis.title.x = element_blank() )+
    facet_grid(~roomposition) 
  

  P_bar_all
#=============================STATS=======================================================
#linear model with interaction
model_all=lm(freq~Plant_sex*roomposition,data=dataplot_all)
summary(model_all)

# Call:
#   lm(formula = freq ~ Plant_sex * roomposition, data = dataplot_all)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -18.350  -5.400  -0.750   4.506  37.750 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                      15.250      1.248  12.224  < 2e-16 ***
# Plant_sexMaize_m                 -1.525      1.764  -0.864 0.387553    
# Plant_sexMilo_f                   4.975      1.764   2.820 0.004881 ** 
# Plant_sexMilo_m                   4.100      1.764   2.324 0.020293 *  
# Plant_sexOats_f                   0.825      1.764   0.468 0.640147    
# Plant_sexOats_m                  -0.725      1.764  -0.411 0.681196    
# Plant_sexRice_f                   6.600      1.764   3.741 0.000192 ***
# Plant_sexRice_m                   7.000      1.764   3.968 7.67e-05 ***
# roomposition2                    -1.925      1.764  -1.091 0.275447    
# roomposition3                    -2.675      1.764  -1.516 0.129727    
# roomposition4                    -1.475      1.764  -0.836 0.403301    
# Plant_sexMaize_m:roomposition2    1.725      2.495   0.691 0.489471    
# Plant_sexMilo_f:roomposition2    -1.700      2.495  -0.681 0.495785    
# Plant_sexMilo_m:roomposition2     1.425      2.495   0.571 0.568021    
# Plant_sexOats_f:roomposition2     2.250      2.495   0.902 0.367354    
# Plant_sexOats_m:roomposition2     2.825      2.495   1.132 0.257759    
# Plant_sexRice_f:roomposition2     3.325      2.495   1.333 0.182902    
# Plant_sexRice_m:roomposition2     2.100      2.495   0.842 0.400145    
# Plant_sexMaize_m:roomposition3    2.425      2.495   0.972 0.331286    
# Plant_sexMilo_f:roomposition3     0.375      2.495   0.150 0.880556    
# Plant_sexMilo_m:roomposition3     2.150      2.495   0.862 0.389024    
# Plant_sexOats_f:roomposition3     3.325      2.495   1.333 0.182902    
# Plant_sexOats_m:roomposition3     3.800      2.495   1.523 0.128015    
# Plant_sexRice_f:roomposition3     6.375      2.495   2.555 0.010736 *  
# Plant_sexRice_m:roomposition3     2.675      2.495   1.072 0.283881    
# Plant_sexMaize_m:roomposition4    0.550      2.495   0.220 0.825571    
# Plant_sexMilo_f:roomposition4    -0.375      2.495  -0.150 0.880556    
# Plant_sexMilo_m:roomposition4    -1.675      2.495  -0.671 0.502143    
# Plant_sexOats_f:roomposition4     2.900      2.495   1.162 0.245344    
# Plant_sexOats_m:roomposition4     2.425      2.495   0.972 0.331286    
# Plant_sexRice_f:roomposition4     0.925      2.495   0.371 0.710904    
# Plant_sexRice_m:roomposition4    -1.125      2.495  -0.451 0.652151    
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 7.89 on 1248 degrees of freedom
# Multiple R-squared:  0.1624,	Adjusted R-squared:  0.1416 
# F-statistic: 7.805 on 31 and 1248 DF,  p-value: < 2.2e-16
#==================
##linear model without interaction
model_all=lm(freq~Plant_sex,data=dataplot_all)
summary(model_all)

# Call:
#   lm(formula = freq ~ Plant_sex, data = dataplot_all)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -17.306  -5.381  -0.731   4.619  38.356 
# 
# Coefficients:
#                   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)       13.7312     0.6234  22.026  < 2e-16 ***
# Plant_sexMaize_m  -0.3500     0.8817  -0.397 0.691447    
# Plant_sexMilo_f    4.5500     0.8817   5.161 2.85e-07 ***
# Plant_sexMilo_m    4.5750     0.8817   5.189 2.46e-07 ***
# Plant_sexOats_f    2.9437     0.8817   3.339 0.000865 ***
# Plant_sexOats_m    1.5375     0.8817   1.744 0.081420 .  
# Plant_sexRice_f    9.2562     0.8817  10.499  < 2e-16 ***
# Plant_sexRice_m    7.9125     0.8817   8.975  < 2e-16 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 7.886 on 1272 degrees of freedom
# Multiple R-squared:  0.1472,	Adjusted R-squared:  0.1426 
# F-statistic: 31.38 on 7 and 1272 DF,  p-value: < 2.2e-16



#============
#analsis of variance
library(car)
Anova(model_all)

# Anova Table (Type II tests)
# 
# Response: freq
#           Sum Sq   Df F value    Pr(>F)    
# Plant_sex  13658    7  31.376 < 2.2e-16 ***
#   Residuals  79099 1272                      
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1


#==========
#histogram of residuals
x_all=residuals(model_all)
library(rcompanion)
plotNormalHistogram(x_all)#passed normality
library(multcompView)
library(lsmeans)

marginal=lsmeans(model_all,~Plant_sex)
pairs(marginal,adjust='tukey')
#

# contrast          estimate    SE   df t.ratio p.value
# Maize_f - Maize_m    0.350 0.882 1272   0.397 0.9999 
# Maize_f - Milo_f    -4.550 0.882 1272  -5.161 <.0001 
# Maize_f - Milo_m    -4.575 0.882 1272  -5.189 <.0001 
# Maize_f - Oats_f    -2.944 0.882 1272  -3.339 0.0195 
# Maize_f - Oats_m    -1.538 0.882 1272  -1.744 0.6583 
# Maize_f - Rice_f    -9.256 0.882 1272 -10.499 <.0001 
# Maize_f - Rice_m    -7.912 0.882 1272  -8.975 <.0001 
# Maize_m - Milo_f    -4.900 0.882 1272  -5.558 <.0001 
# Maize_m - Milo_m    -4.925 0.882 1272  -5.586 <.0001 
# Maize_m - Oats_f    -3.294 0.882 1272  -3.736 0.0048 
# Maize_m - Oats_m    -1.887 0.882 1272  -2.141 0.3891 
# Maize_m - Rice_f    -9.606 0.882 1272 -10.896 <.0001 
# Maize_m - Rice_m    -8.262 0.882 1272  -9.372 <.0001 
# Milo_f - Milo_m     -0.025 0.882 1272  -0.028 1.0000 
# Milo_f - Oats_f      1.606 0.882 1272   1.822 0.6052 
# Milo_f - Oats_m      3.013 0.882 1272   3.417 0.0150 
# Milo_f - Rice_f     -4.706 0.882 1272  -5.338 <.0001 
# Milo_f - Rice_m     -3.362 0.882 1272  -3.814 0.0036 
# Milo_m - Oats_f      1.631 0.882 1272   1.850 0.5856 
# Milo_m - Oats_m      3.038 0.882 1272   3.445 0.0137 
# Milo_m - Rice_f     -4.681 0.882 1272  -5.310 <.0001 
# Milo_m - Rice_m     -3.337 0.882 1272  -3.786 0.0040 
# Oats_f - Oats_m      1.406 0.882 1272   1.595 0.7534 
# Oats_f - Rice_f     -6.312 0.882 1272  -7.160 <.0001 
# Oats_f - Rice_m     -4.969 0.882 1272  -5.636 <.0001 
# Oats_m - Rice_f     -7.719 0.882 1272  -8.755 <.0001 
# Oats_m - Rice_m     -6.375 0.882 1272  -7.231 <.0001 
# Rice_f - Rice_m      1.344 0.882 1272   1.524 0.7945 
# 
# P value adjustment: tukey method for comparing a family of 8 estimates 
#
library(multcompView)
library(lsmeans)
CLD=CLD(marginal, alpha=0.5, Letters=letters, adjust="tukey")

CLD
#
# Plant_sex lsmean    SE   df lower.CL upper.CL .group
# Maize_m     13.4 0.623 1272     11.7     15.1  a    
# Maize_f     13.7 0.623 1272     12.0     15.4  ab   
# Oats_m      15.3 0.623 1272     13.6     17.0   bc  
# Oats_f      16.7 0.623 1272     15.0     18.4    cd 
# Milo_f      18.3 0.623 1272     16.6     20.0     d 
# Milo_m      18.3 0.623 1272     16.6     20.0     d 
# Rice_m      21.6 0.623 1272     19.9     23.3      e
# Rice_f      23.0 0.623 1272     21.3     24.7      e
# 
# Confidence level used: 0.95 
# Conf-level adjustment: sidak method for 8 estimates 
# P value adjustment: tukey method for comparing a family of 8 estimates 
# significance level used: alpha = 0.5 