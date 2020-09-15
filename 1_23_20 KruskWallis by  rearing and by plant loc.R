#Ref: https://rcompanion.org/rcompanion/d_06.html

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

#add all females and males into one column each note name change
my_data.df$Female <- (my_data.df$lfemale+my_data.df$sfemale+my_data.df$rfemale)
my_data.df$male <- (my_data.df$lmale+my_data.df$smale+my_data.df$rmale)
my_data.df$tot_Insects <- (my_data.df$leaf+my_data.df$soil+my_data.df$root)

#roomposition from int to factor
my_data.df$roomposition <-as.factor(my_data.df$roomposition)

#create working subset by sex-----------------------------------------
databysex.df<- my_data.df[,c(3,4,14:17)]

#melt female and male columns in to sex and freq column 
databysex.df <- melt(databysex.df,c("rearing","substrate","roomposition","tot_Insects"))
#rename column 4 sex
names(databysex.df)[5]<-"sex"
#renmae column4 freq
names(databysex.df)[6]<-"freq"
str(databysex.df)

#to match plot create a substrate_sex column
databysex.df<-mutate(databysex.df,substrate_sex=paste(substrate, sex))
str(databysex.df)


#subset for plot-----note uses orig data df----------------------------------------------------------
dataplot<-my_data.df[,c(3,4,6,7,9,10,12,13)]
str(dataplot)

dataplot <- melt(dataplot,c("rearing","substrate"))

#rename column 4 sex
names(dataplot)[3]<-"location_plant"
#renmae column4 freq
names(dataplot)[4]<-"freq"
str(dataplot)

#write.csv(dataplot, "dataplot.csv")

# 
#=====================Plots===================================================
#Summary  Stats :

#https://www.datanovia.com/en/lessons/anova-in-r/#two-way-independent-anova

library(ggpubr)# not library is not loading packages click on them
#install.packages("rstatix")
library(rstatix)

#install.packages ("cowplot")
library(cowplot)

#-----Summary stats and plots--------------------------------------------------

#Summary  by plant loc all----------------------------

summary_mean<-dataplot %>%
     group_by(substrate,rearing,location_plant) %>%
     get_summary_stats(freq, type = "mean_sd")

#calculate the SE
summary_mean$SE <- (summary_mean$mean/sqrt(summary_mean$sd))

#write.csv(summary_mean, "summary_mean.csv")

# NOTE: confirmed calculations up to here with excel

#plot by plant loc all
p2<-ggplot(data=summary_mean, aes(x=substrate, y=mean, fill=location_plant)) + 
     geom_errorbar(aes(x=substrate, ymin=mean-SE, ymax=mean+SE), width=.2,position=position_dodge(.9))+
     geom_bar(stat="identity",position=position_dodge()) +
     facet_grid(~rearing) + theme_bw()+
     labs(title="Mean weevil mean count by Plant Selection based on rearing grain with no capture",
          x="Plant.Selection", y="Mean weevil count", fill="sex") + 
     theme(plot.title = element_text(size=15, margin=margin(t=20, b=20)))
#

print(p2)



#Summary by plant location ---------------------------------------------------
#suBset by plant loc

summary_meanbylocation_leaf<-summary_mean%>%filter(location_plant ==  c("lmale","lfemale"))
summary_meanbylocation_soil<-summary_mean%>%filter(location_plant ==  c("smale","sfemale"))
summary_meanbylocation_root<-summary_mean%>%filter(location_plant ==  c("rmale","rfemale"))



#Note up to here confirmed with excel
#PLOT by plant loc----------------------------------------------------------------


# offset for letters over error bars
# offset.v=0 # : 0= places later on top of bar
# offset.h=0 #0.5

pleaf<-ggplot(data=summary_meanbylocation_leaf, aes(x=substrate, y=mean, fill=location_plant)) + 
       geom_errorbar(aes(x=substrate, ymin=mean-SE, ymax=mean+SE), width=.2,
                     position=position_dodge(.9))+
       geom_bar(stat="identity",position=position_dodge()) +
               facet_grid(~rearing) + theme_bw()+
       labs( x="Plant Selection", y="Mean weevil leaves", fill="sex") + 
       ylim(0,30)+
              theme(
            strip.text=element_text(size=13), #SizeTextinBox header
            axis.title.y= element_text(size=14,face="bold"),#y axis
            axis.title.x= element_text(size=14,face="bold"),# x axis
            legend.text=element_text(size=12), #legend text size
            legend.title=element_text(size=12))+
      scale_fill_manual(values=c("grey", "black"),
                         breaks=c("lmale", "lfemale"),
                         labels=c("male", "female"))
       
# #stats letters display order
# pleaf <-pleaf + geom_text(aes(label=c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25","26","27","28","29","30","31","32"),hjust=offset.h, vjust=offset.v),position=position_dodge(.9)) 

#wilcoxon paired comparison
#pleaf <-pleaf + geom_text(aes(label=c("c","a","c","c","c","b","c","c","b","b","ab","ab","b","a","b","b","b","b","b","bc","a","b","bc","b","a","a","a","a","b","a","a","a")),position=position_dodge(.9))

# Dunn (1964) Kruskal-Wallis multiple comparison p-values adjusted with the Benjamini-Hochberg method 
 pleaf <-pleaf + geom_text(aes(x =substrate,y= mean+(SE+2),
     label=c("c","c","c","c","c","c","c","c","b","b","b","b","a","a","b","b","b","b","b","b","b","b","b","bc","a","a","a","a","a","a","a","a")),position=position_dodge(.9)) 

#note geom_text is the letter above
 
print(pleaf)

#3---------------------------------------------------------
 
psoil<-ggplot(data=summary_meanbylocation_soil, aes(x=substrate, y=mean, fill=location_plant)) + 
     geom_errorbar(aes(x=substrate, ymin=mean, ymax=mean+SE), width=.2,position=position_dodge(.9))+
     geom_bar(stat="identity",position=position_dodge()) +
          facet_grid(~rearing) + theme_bw()+
     labs(x="Plant Selection", y="Mean weevil soil", fill="sex") +
     ylim(0,30)+
     theme(
          strip.text=element_text(size=13), #SizeTextinBox header
          axis.title.y= element_text(size=14,face="bold"),#y axis
          axis.title.x= element_text(size=14,face="bold"),# x axis
          legend.text=element_text(size=12), #legend text size
          legend.title=element_text(size=12))+ #legend title text size
     scale_fill_manual(values=c("grey", "black"), 
                       breaks=c("smale", "sfemale"),
                       labels=c("male", "female"))
#Wilcoson test
#psoil<-psoil+geom_text(aes(label=c("a","a","b","b","b","b","ab","a","a","a","b","ab","b","b","ab","a","a","a","a","a","a","a","b","a","a","a","b","b","ab","ab","a","a"),hjust=offset.h, vjust=offset.v),position=position_dodge(.9))  
#
#Dunn Test  note SE+2 spaces the stat letter above  bar
psoil <-psoil + geom_text(aes(x =substrate, y= mean+(SE+2),label=c("a","a","a","a","a","a","a","ab","a","a","a","a","a","ab","a","ab","a","a","a","a","b","b","a","b","a","a","a","a","ab","b","a","a")),position=position_dodge(.9)) 
#
print(psoil)
#-------------------------------------------------------------
proot <-ggplot(data=summary_meanbylocation_root, aes(x=substrate, y=mean, fill=location_plant)) + 
     geom_errorbar(aes(x=substrate, ymin=mean, ymax=mean+SE),width=.2,position=position_dodge(.9))+
     geom_bar(stat="identity",position=position_dodge()) +
           facet_grid(~rearing) + theme_bw()+
  
     labs( x="Plant Selection", y="Mean weevil roots", fill="sex") + 
     ylim(0,30)+
     theme(
          strip.text=element_text(size=13), #SizeTextinBox header
          axis.title.y= element_text(size=14,face="bold"),#y axis
          axis.title.x= element_text(size=14,face="bold"),# x axis
          legend.text=element_text(size=12), #legend text size
          legend.title=element_text(size=12))+
     scale_fill_manual(values=c("grey", "black"), 
                      breaks=c("rmale", "rfemale"),
                      labels=c("male", "female"))
  
# #Wilcoxon
#proot<-proot+geom_text(aes(label=c("a","a","c","b","a","a","a","a","a","a","ab","a","a","a","ab","a","a","a","bc","b","a","a","b","a","a","a","a","ab","a","a","a","a"),hjust=offset.h, vjust=offset.v),position=position_dodge(.9)) 
#
##Dunn
#
proot <-proot + geom_text(aes(x =substrate,y= mean+(SE+2),label=c("a","a","a","a","a","a","a","a","a","a","a","a","a","a","a","a","a","a","a","a","a","a","a","a","a","a","a","a","a","a","a","a")),position=position_dodge(.9)) 

print(proot)
#----------------------------------------------------------
#combine into single graph ggarrange from gg pubr Pk
#library(ggpubr)
#ggarrange(pleaf,psoil, proot, ncol=1, nrow=3,common.legend=TRUE)     

#error was cause by # in the #--------------line dissapearing
#above put legend on top prefer on right

#worked but need to zoom in to see

# #combining using cowplot pk columns 1 3 ros

plot_grid(pleaf, psoil, proot, nrow=3, labels=c('a','b','c'),rel_widths= c(1,1,1),rel_heights = c(1, 1,1)) 

#then load into windows drawing program and erase extra legends
#to export the right  way need to double the size : width=1600, height=885
#error caused by ggplot not loading when you call library
# library("cowplot")

#================    Leaves            ==================================
#run anova for each grain and combined substrate_sex so it one way
#Ref:   https://rcompanion.org/rcompanion/d_06.html


if(!require(dplyr)){install.packages("dplyr")}
if(!require(FSA)){install.packages("FSA")}
if(!require(DescTools)){install.packages("DescTools")}
if(!require(rcompanion)){install.packages("rcompanion")}
if(!require(multcompView)){install.packages("multcompView")}

#---------- mleaf----------------------------------------
#dataplot subset by lmale
dataplot_mleaf<-dataplot%>%filter(location_plant == c( "lmale"))

# summary_dataplot_mleafBarley<-dataplot_mleaf %>%
#      group_by(substrate,rearing) %>%
#      get_summary_stats(freq, type = "mean_sd")
#---------mleafBarley------
dataplot_mleafBarley<-dataplot_mleaf%>%filter(rearing=="Barley grain")

str(dataplot_mleafBarley)

library(FSA)
Summarize(freq~substrate, data=dataplot_mleafBarley)
#   substrate  n   mean       sd min    Q1 median    Q3 max percZero
# 1     Maize 40  4.750 3.881250   0  2.00      4  6.25  16     12.5
# 2      Milo 40  8.675 5.351192   1  4.00      8 13.25  20      0.0
# 3      Oats 40 10.050 7.038174   1  5.00      9 11.50  37      0.0
# 4      Rice 40 16.350 7.145197   3 11.75     16 19.25  38      0.0
# matches excel


#One way anova test
modelmleaf_barley<-lm(freq~substrate, data=dataplot_mleafBarley)

library(car) 
aovmleafBarley<-anova(modelmleaf_barley)#produces the sum of squares
aovmleafBarley

# Analysis of Variance Table
# 
# Response: freq
#            Df Sum Sq Mean Sq F value    Pr(>F)    
# substrate   3 2785.4  928.47  25.739 1.394e-13 ***
#      Residuals 156 5627.3   36.07                      
# ---
#      Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

#---------------ck Anova assumptions for model--------------------------------

#NOrmality  tests

#other way to run anova
res.aov<-aov(freq~substrate, data=dataplot_mleafBarley)

#summary of anova  Note same as aovleafBarley above
summary(res.aov)
#               Df Sum Sq Mean Sq F value   Pr(>F)    
# substrate     3   2785   928.5   25.74 1.39e-13 ***
# Residuals   156   5627    36.1                     
# ---
#      Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

#shapiro-wilk test
shapiro.test(residuals(modelmleaf_barley)) #failed P<0.05

# Shapiro-Wilk normality test
# 
# data:  residuals(modelmleaf_barley)
# W = 0.94231, p-value = 4.152e-06

#
#using other method res.aov output same as above

# Extract the residuals
aov_residuals <- residuals(object = res.aov )
# Run Shapiro-Wilk test
shapiro.test(x = aov_residuals )

# Shapiro-Wilk normality test
# 
# data:  aov_residuals
# W = 0.94231, p-value = 4.152e-06



#homogeneity of variance test
library(car)
leveneTest(freq~substrate, data=dataplot_mleafBarley) #passed P>0.05

#Levene's Test for Homogeneity of Variance (center = median)
#        Df F value  Pr(>F)  
# group   3  2.4335 0.06705 .
#       156                  
# ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1   


#since we failed homogeneity need to use Kruskal.test
kruskal.test(freq~substrate, data=dataplot_mleafBarley)
# 
# Kruskal-Wallis rank sum test
# 
# data:  freq by substrate
# Kruskal-Wallis chi-squared = 56.512, df = 3, p-value = 3.267e-12 #significant

#Pairwise Mann-Witney U test = Mann-Whitney-Wilcoxon test-------------------------------------
PT = pairwise.wilcox.test(dataplot_mleafBarley$freq,
                          dataplot_mleafBarley$substrate,
                          p.adjust.method="none")

PT
# Pairwise comparisons using Wilcoxon rank sum test 
# 
# data:  dataplot_mleafBarley$freq and dataplot_mleafBarley$substrate 
# 
# Maize   Milo    Oats   
# Milo 0.00083 -       -      
# Oats 3.9e-05 0.54031 -      
# Rice 2.6e-11 1.5e-06 3.1e-05
# 
# P value adjustment method: none

PT = PT$p.value

library(rcompanion)

PT1 = fullPTable(PT)

PT1

library(multcompView)

multcompLetters(PT1,
                compare="<",
                threshold=0.05,
                Letters=letters,
                reversed=T) #reversed=T give the largest value a

# Maize  Milo  Oats  Rice 
# "c"   "b"   "b"   "a" 

#Dun test multiple comparison
PTdunn=dunnTest(freq~substrate, data=dataplot_mleafBarley, method="bh")
PTdunn
#
# Dunn (1964) Kruskal-Wallis multiple comparison
# p-values adjusted with the Benjamini-Hochberg method.
# 
# Comparison          Z      P.unadj        P.adj
# 1 Maize - Milo -3.0170140 2.552780e-03 3.063336e-03
# 2 Maize - Oats -3.6803463 2.329175e-04 3.493762e-04
# 3  Milo - Oats -0.6633323 5.071178e-01 5.071178e-01
# 4 Maize - Rice -7.4682273 8.128238e-14 4.876943e-13
# 5  Milo - Rice -4.4512133 8.538649e-06 2.561595e-05
# 6  Oats - Rice -3.7878810 1.519376e-04 3.038752e-04
PTdunn=PTdunn$res
PTdunn
library(rcompanion)

cldList(comparison = PTdunn$Comparison,
        p.value    = PTdunn$P.adj,
        threshold  = 0.05,
        reversed=T)
# 
# Group Letter MonoLetter
# 1 Maize      c          c
# 2  Milo      b         b 
# 3  Oats      b         b 
# 4  Rice      a        a  



#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#mleafmaize---------------------------------------------------------

dataplot_mleafMaize<-dataplot_mleaf%>%filter(rearing=="Maize grain")

Summarize(freq~substrate, data=dataplot_mleafMaize)
#   substrate  n  mean       sd min Q1 median    Q3 max percZero
# 1     Maize 40  8.60 3.747136   0  6    8.0 11.25  15      2.5
# 2      Milo 40 11.75 6.412128   1  7   12.0 16.25  25      0.0
# 3      Oats 40 10.95 5.742107   2  8   10.0 15.00  27      0.0
# 4      Rice 40 13.50 4.673987   5 10   13.5 16.25  24      0.0


#One way anova test
modelmleaf_Maize<-lm(freq~substrate, data=dataplot_mleafMaize)

library(car) 
aovmleafMaize<-anova(modelmleaf_Maize)#produces the sum of squares
aovmleafMaize

# Analysis of Variance Table
# 
# Response: freq
#             Df Sum Sq Mean Sq F value    Pr(>F)    
# substrate   3  496.6 165.533  6.0208 0.0006598 ***
#      Residuals 156 4289.0  27.494                      
# ---
#      Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

#---------------ck Anova assumptions for model--------------------------------

#NOrmality  tests

#shapiro-wilk test
shapiro.test(residuals(modelmleaf_Maize)) #passed P>0.05

# Shapiro-Wilk normality test
# 
# data:  residuals(modelmleaf_Maize)
# W = 0.98725, p-value = 0.1531
# 

#homogeneity of variance test
library(car)
leveneTest(freq~substrate, data=dataplot_mleafMaize) #failed P<0.05

# Levene's Test for Homogeneity of Variance (center = median)
#        Df F value   Pr(>F)   
# group   3  4.0595 0.008237 **
# 156                    
# ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1           

#since we failed homogeneity need to use Kruskal.test
kruskal.test(freq~substrate, data=dataplot_mleafMaize)
# 
# Kruskal-Wallis rank sum test
# 
# data:  freq by substrate
# Kruskal-Wallis chi-squared = 18.437, df = 3, p-value = 0.0003574#sig


#Pairwise Mann-Witney U test = Mann-Whitney-Wilcoxon test-------------------------------------
PT = pairwise.wilcox.test(dataplot_mleafMaize$freq,
                          dataplot_mleafMaize$substrate,
                          p.adjust.method="none")

PT
# Pairwise comparisons using Wilcoxon rank sum test 
# 
# data:  dataplot_mleafMaize$freq and dataplot_mleafMaize$substrate 
# 
# Maize   Milo  Oats 
# Milo 0.021   -     -    
#      Oats 0.048   0.572 -    
#      Rice 7.5e-06 0.196 0.018
# 
# P value adjustment method: none 

PT = PT$p.value

library(rcompanion)

PT1 = fullPTable(PT)

PT1

library(multcompView)

multcompLetters(PT1,
                compare="<",
                threshold=0.05,
                Letters=letters,
                reversed=T) #reversed=T give the largest value a
# 
# Maize  Milo  Oats  Rice 
# "c"  "ab"   "b"   "a" 


#Dun test multiple comparison
PTdunnM=dunnTest(freq~substrate, data=dataplot_mleafMaize, method="bh")
PTdunnM
# Dunn (1964) Kruskal-Wallis multiple comparison
# p-values adjusted with the Benjamini-Hochberg method.
# 
# Comparison          Z      P.unadj       P.adj
# 1 Maize - Milo -2.5971782 9.399314e-03 0.028197942
# 2 Maize - Oats -1.9599748 4.999873e-02 0.074998101
# 3  Milo - Oats  0.6372034 5.239924e-01 0.523992356
# 4 Maize - Rice -4.2403650 2.231566e-05 0.000133894
# 5  Milo - Rice -1.6431868 1.003443e-01 0.120413152
# 6  Oats - Rice -2.2803902 2.258456e-02 0.045169113

PTdunnM=PTdunnM$res
PTdunnM
library(rcompanion)

cldList(comparison = PTdunnM$Comparison,
        p.value    = PTdunnM$P.adj,
        threshold  = 0.05,
        reversed=T)

# Group Letter MonoLetter
# 1 Maize      c          c
# 2  Milo     ab        ab 
# 3  Oats     bc         bc
# 4  Rice      a        a  

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#mleafMilo---------------------------------------------------------

dataplot_mleafMilo<-dataplot_mleaf%>%filter(rearing=="Milo grain")

#Summarize(freq~substrate, data=dataplot_mleafMilo)

#One way anova test
modelmleaf_Milo<-lm(freq~substrate, data=dataplot_mleafMilo)

library(car) 
aovmleafMilo<-anova(modelmleaf_Milo)#produces the sum of squares
aovmleafMilo
# Analysis of Variance Table
# 
# Response: freq
#             Df Sum Sq Mean Sq F value    Pr(>F)    
# substrate   3 1083.1  361.05  10.626 2.136e-06 ***
#      Residuals 156 5300.4   33.98                      
# ---
#      Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# > 
#---------------ck Anova assumptions for model--------------------------------

#NOrmality  tests

#shapiro-wilk test
shapiro.test(residuals(modelmleaf_Milo)) #failed P<0.05


# Shapiro-Wilk normality test
# 
# data:  residuals(modelmleaf_Milo)
# W = 0.92897, p-value = 4.105e-07

#homogeneity of variance test
library(car)
leveneTest(freq~substrate, data=dataplot_mleafMilo) #failed P<0.05

# Levene's Test for Homogeneity of Variance (center = median)
#        Df F value   Pr(>F)   
# group   3  5.3585 0.001542 **
# 156                    
# ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

#since we failed homogeneity need to use Kruskal.test
kruskal.test(freq~substrate, data=dataplot_mleafMilo)
# 
# Kruskal-Wallis rank sum test
# 
# data:  freq by substrate
# Kruskal-Wallis chi-squared = 29.196, df = 3, p-value = 2.037e-06 #sig


#Pairwise Mann-Witney U test = Mann-Whitney-Wilcoxon test-------------------------------------
PT = pairwise.wilcox.test(dataplot_mleafMilo$freq,
                          dataplot_mleafMilo$substrate,
                          p.adjust.method="none")

PT
# Pairwise comparisons using Wilcoxon rank sum test 
# 
# data:  dataplot_mleafMilo$freq and dataplot_mleafMilo$substrate 
# 
# Maize   Milo   Oats  
# Milo 2.2e-05 -      -     
#      Oats 0.0164  0.0087 -     
#      Rice 5.9e-06 0.3161 0.0101
# 
# P value adjustment method: none 

PT = PT$p.value

library(rcompanion)

PT1 = fullPTable(PT)

PT1

library(multcompView)

multcompLetters(PT1,
                compare="<",
                threshold=0.05,
                Letters=letters,
                reversed=T) #reversed=T give the largest value a
# 
# Maize  Milo  Oats  Rice 
# "c"   "b"   "a"   "b" 

#Dun test multiple comparison
PTdunnMI=dunnTest(freq~substrate, data=dataplot_mleafMilo, method="bh")
PTdunnMI
# Comparison          Z      P.unadj        P.adj
# 1 Maize - Milo -4.7799603 1.753298e-06 1.051979e-05
# 2 Maize - Oats -2.1010549 3.563615e-02 4.276338e-02
# 3  Milo - Oats  2.6789054 7.386325e-03 1.477265e-02
# 4 Maize - Rice -4.3326702 1.473117e-05 4.419352e-05
# 5  Milo - Rice  0.4472902 6.546656e-01 6.546656e-01
# 6  Oats - Rice -2.2316153 2.564040e-02 3.846060e-02
 


PTdunnMI=PTdunnMI$res
PTdunnMI
library(rcompanion)

cldList(comparison = PTdunnMI$Comparison,
        p.value    = PTdunnMI$P.adj,
        threshold  = 0.05,
        reversed=T)

# Group Letter MonoLetter
# 1 Maize      c          c
# 2  Milo      b         b 
# 3  Oats      a        a  
# 4  Rice      b         b 




#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#mleafRice---------------------------------------------------------

dataplot_mleafRice<-dataplot_mleaf%>%filter(rearing=="Rice grain")

#One way anova test
modelmleaf_Rice<-lm(freq~substrate, data=dataplot_mleafRice)

library(car) 
aovmleafRice<-anova(modelmleaf_Rice)#produces the sum of squares
aovmleafRice
# Analysis of Variance Table
# 
# Response: freq
#           Df Sum Sq Mean Sq F value    Pr(>F)    
# substrate   3 1212.6  404.21  9.8305 5.618e-06 ***
#  Residuals 156 6414.3   41.12                      
# ---
#      Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#---------------ck Anova assumptions for model--------------------------------

#NOrmality  tests

#shapiro-wilk test
shapiro.test(residuals(modelmleaf_Rice)) #failed P<0.05


# Shapiro-Wilk normality test
# 
# data:  residuals(modelmleaf_Rice)
# W = 0.89638, p-value = 3.537e-09

#homogeneity of variance test
library(car)
leveneTest(freq~substrate, data=dataplot_mleafRice) #passed P>0.05

# Levene's Test for Homogeneity of Variance (center = median)
#        Df F value Pr(>F)
# group   3   0.502 0.6814
# 156          
#since we failed homogeneity need to use Kruskal.test
kruskal.test(freq~substrate, data=dataplot_mleafRice)
# 
# Kruskal-Wallis rank sum test
# 
# data:  freq by substrate
# Kruskal-Wallis chi-squared = 30.92, df = 3, p-value = 8.838e-07


#Pairwise Mann-Witney U test = Mann-Whitney-Wilcoxon test-------------------------------------
PT = pairwise.wilcox.test(dataplot_mleafRice$freq,
                          dataplot_mleafRice$substrate,
                          p.adjust.method="none")

PT
# Pairwise comparisons using Wilcoxon rank sum test 
# 
# data:  dataplot_mleafRice$freq and dataplot_mleafRice$substrate 
# 
# Maize   Milo   Oats   
# Milo 0.0133  -      -      
# Oats 0.1059  0.3776 -      
# Rice 3.9e-07 0.0012 7.0e-05
# 
# P value adjustment method: none
PT = PT$p.value

library(rcompanion)

PT1 = fullPTable(PT)

PT1

library(multcompView)

multcompLetters(PT1,
                compare="<",
                threshold=0.05,
                Letters=letters,
                reversed=T) #reversed=T give the largest value a
# Maize  Milo  Oats  Rice 
# "c"   "b"  "bc"   "a" 
# > 

#Dun test multiple comparison
PTdunn=dunnTest(freq~substrate, data=dataplot_mleafRice, method="bh")
PTdunn
# Dunn (1964) Kruskal-Wallis multiple comparison
# p-values adjusted with the Benjamini-Hochberg method.

# Comparison          Z      P.unadj        P.adj
# 1 Maize - Milo -2.3333608 1.962922e-02 2.944383e-02
# 2 Maize - Oats -1.4862940 1.372014e-01 1.646416e-01
# 3  Milo - Oats  0.8470668 3.969579e-01 3.969579e-01
# 4 Maize - Rice -5.3832844 7.313883e-08 4.388330e-07
# 5  Milo - Rice -3.0499237 2.288995e-03 4.577991e-03
# 6  Oats - Rice -3.8969904 9.739548e-05 2.921864e-04

PTdunn=PTdunn$res
PTdunn
library(rcompanion)

cldList(comparison = PTdunn$Comparison,
        p.value    = PTdunn$P.adj,
        threshold  = 0.05,
        reversed=T)

# Group Letter MonoLetter
# 1 Maize      c          c
# 2  Milo      b         b 
# 3  Oats     bc         bc
# 4  Rice      a        a  


#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#---------- femleaf----------------------------------------
#dataplot subset by female
dataplot_femleaf<-dataplot%>%filter(location_plant == c( "lfemale"))

#---------femleafBarley------
dataplot_femleafBarley<-dataplot_femleaf%>%filter(rearing=="Barley grain")


#ck data
#Summarize(freq~substrate, data=dataplot_femleafBarley)
# substrate  n   mean       sd min Q1 median    Q3 max percZero
# 1     Maize 40  5.775 5.015297   0  2    4.0  8.25  20       10
# 2      Milo 40 10.500 6.156089   0  7   10.5 14.25  26        5
# 3      Oats 40 11.150 6.204010   1  6   11.0 15.00  23        0
# 4      Rice 40 17.575 8.283092   3 11   17.5 23.00  38        0
#One way anova test
modelfemleaf_barley<-lm(freq~substrate, data=dataplot_femleafBarley)

library(car) 
aovfemleafBarley<-anova(modelfemleaf_barley)#produces the sum of squares
aovfemleafBarley

# Analysis of Variance Table
# 
# Response: freq
#             Df Sum Sq Mean Sq F value    Pr(>F)    
# substrate   3 2822.2  940.72  22.115 5.494e-12 ***
#      Residuals 156 6635.9   42.54                      
# ---
#      Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# > 
#---------------ck Anova assumptions for model--------------------------------

#NOrmality  tests

#shapiro-wilk test
shapiro.test(residuals(modelfemleaf_barley)) #passed P>0.05

# Shapiro-Wilk normality test
# 
# data:  residuals(modelfemleaf_barley)
# W = 0.98344, p-value = 0.05282


#homogeneity of variance test
library(car)
leveneTest(freq~substrate, data=dataplot_femleafBarley) #failed P<0.05

# Levene's Test for Homogeneity of Variance (center = median)
#        Df F value  Pr(>F)  
# group   3  3.3015 0.02196 *
# 156                  
# ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# > 
#since we failed homogeneity need to use Kruskal.test
kruskal.test(freq~substrate, data=dataplot_femleafBarley)
# 
# Kruskal-Wallis rank sum test
# 
# data:  freq by substrate
# Kruskal-Wallis chi-squared = 46.305, df = 3, p-value = 4.884e-10  # sig

# #Pairwise Mann-Witney U test = Mann-Whitney-Wilcoxon test-------------------------------------
PT = pairwise.wilcox.test(dataplot_femleafBarley$freq,
                          dataplot_femleafBarley$substrate,
                          p.adjust.method="none")

PT
# Pairwise comparisons using Wilcoxon rank sum test 
# 
# data:  dataplot_femleafBarley$freq and dataplot_femleafBarley$substrate 
# 
# Maize   Milo    Oats   
# Milo 0.00034 -       -      
# Oats 6.0e-05 0.67497 -      
#Rice 1.2e-09 0.00014 0.00044
# 
# P value adjustment method: none 
# > 

PT = PT$p.value

library(rcompanion)

PT1 = fullPTable(PT)

PT1

library(multcompView)

multcompLetters(PT1,
                compare="<",
                threshold=0.05,
                Letters=letters,
                reversed=T) #reversed=T give the largest value a

# Maize  Milo  Oats  Rice 
# "c"   "b"   "b"   "a"

#Dun test multiple comparison
PTdunn=dunnTest(freq~substrate, data=dataplot_femleafBarley, method="bh")
PTdunn
#
# Dunn (1964) Kruskal-Wallis multiple comparison
# p-values adjusted with the Benjamini-Hochberg method.
# 
# Comparison          Z      P.unadj        P.adj
# 1 Maize - Milo -3.2665423 1.088695e-03 1.633043e-03
# 2 Maize - Oats -3.6591555 2.530477e-04 7.591432e-04
# 3  Milo - Oats -0.3926133 6.946051e-01 6.946051e-01
# 4 Maize - Rice -6.7928133 1.099676e-11 6.598058e-11
# 5  Milo - Rice -3.5262710 4.214554e-04 8.429108e-04
# 6  Oats - Rice -3.1336578 1.726421e-03 2.071705e-03

PTdunn=PTdunn$res
PTdunn
library(rcompanion)

cldList(comparison = PTdunn$Comparison,
        p.value    = PTdunn$P.adj,
        threshold  = 0.05,
        reversed=T)
# 
# Group Letter MonoLetter
# 1 Maize      c          c
# 2  Milo      b         b 
# 3  Oats      b         b 
# 4  Rice      a        a  
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#femleafmaize---------------------------------------------------------

dataplot_femleafMaize<-dataplot_femleaf%>%filter(rearing=="Maize grain")

write.csv(dataplot_femleafMaize, "dataplot_femleafMaize.csv")
# doing means on this output works matches summary_meanbylocation_leaf.csv but not
#the calculation belos

Summarize(freq~substrate, data=dataplot_femleafMaize)

# substrate  n   mean       sd min Q1 median    Q3 max percZero
# 1     Maize 40  9.275 3.782466   0  6   10.0 12.00  16      2.5
# 2      Milo 40 12.925 6.149369   1 10   12.5 17.25  27      0.0
# 3      Oats 40 10.800 5.292472   1  7   10.5 14.00  24      0.0
# 4      Rice 40 15.675 6.607639   5 11   15.5 18.25  31      0.0



#One way anova test
modelfemleaf_Maize<-lm(freq~substrate, data=dataplot_femleafMaize)


library(car) 
aovfemleafMaize<-anova(modelfemleaf_Maize)#produces the sum of squares
aovfemleafMaize

# Analysis of Variance Table
# 
# Response: freq
#             Df Sum Sq Mean Sq F value   Pr(>F)    
# substrate   3  924.5 308.173  9.9577 4.81e-06 ***
#      Residuals 156 4827.9  30.948                     
# ---
#      Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
 
#---------------ck Anova assumptions for model--------------------------------

#NOrmality  tests

#shapiro-wilk test
shapiro.test(residuals(modelfemleaf_Maize)) #passed P>0.05

# Shapiro-Wilk normality test
# 
# data:  residuals(modelfemleaf_Maize)
# W = 0.98358, p-value = 0.05487

#homogeneity of variance test
library(car)
leveneTest(freq~substrate, data=dataplot_femleafMaize) #failed P<0.05

# Levene's Test for Homogeneity of Variance (center = median)
#        Df F value Pr(>F)  
# group   3   2.918  0.036 *
# 156                 
# ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

#since we failed homogeneity need to use Kruskal.test
kruskal.test(freq~substrate, data=dataplot_femleafMaize)
# 
# Kruskal-Wallis rank sum test
# 
# data:  freq by substrate
# Kruskal-Wallis chi-squared = 23.506, df = 3, p-value = 3.168e-05 #sig


#Pairwise Mann-Witney U test = Mann-Whitney-Wilcoxon test-------------------------------------
PT = pairwise.wilcox.test(dataplot_femleafMaize$freq,
                          dataplot_femleafMaize$substrate,
                          p.adjust.method="none")

PT
# Pairwise comparisons using Wilcoxon rank sum test 
# 
# data:  dataplot_femleafMaize$freq and dataplot_femleafMaize$substrate 
# 
# Maize   Milo   Oats  
# Milo 0.0054  -      -     
# Oats 0.2410  0.0906 -     
# Rice 3.8e-06 0.1003 0.0011
# 
# P value adjustment method: none 
# > 

PT = PT$p.value

library(rcompanion)

PT1 = fullPTable(PT)

PT1

library(multcompView)

multcompLetters(PT1,
                compare="<",
                threshold=0.05,
                Letters=letters,
                reversed=T) #reversed=T give the largest value a
# Maize  Milo  Oats  Rice 
# "c"  "ab"  "bc"   "a" 

#Dun test multiple comparison
PTdunn=dunnTest(freq~substrate, data=dataplot_femleafMaize, method="bh")
PTdunn
# Dunn (1964) Kruskal-Wallis multiple comparison
# p-values adjusted with the Benjamini-Hochberg method.
# 
# Comparison         Z      P.unadj        P.adj
# 1 Maize - Milo -2.862373 4.204818e-03 8.409636e-03
# 2 Maize - Oats -1.205146 2.281469e-01 2.281469e-01
# 3  Milo - Oats  1.657227 9.747361e-02 1.169683e-01
# 4 Maize - Rice -4.543775 5.525556e-06 3.315334e-05
# 5  Milo - Rice -1.681402 9.268479e-02 1.390272e-01
# 6  Oats - Rice -3.338629 8.419285e-04 2.525785e-03
PTdunn=PTdunn$res
PTdunn
library(rcompanion)

cldList(comparison = PTdunn$Comparison,
        p.value    = PTdunn$P.adj,
        threshold  = 0.05,
        reversed=T)

# Group Letter MonoLetter
# 1 Maize      c          c
# 2  Milo     ab        ab 
# 3  Oats     bc         bc
# 4  Rice      a        a 
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#femleafMilo---------------------------------------------------------

dataplot_femleafMilo<-dataplot_femleaf%>%filter(rearing=="Milo grain")

Summarize(freq~substrate, data=dataplot_femleafMilo)
# substrate  n   mean       sd min    Q1 median    Q3 max percZero
# 1     Maize 40  8.575 3.941154   2  5.75    8.5 12.00  17      0.0
# 2      Milo 40 14.225 5.540006   3 10.00   13.5 18.25  26      0.0
# 3      Oats 40 11.625 7.621453   0  6.00   10.5 15.00  36      2.5
# 4      Rice 40 16.425 5.486055   6 12.75   16.0 20.00  28      0.0

#One way anova test
modelfemleaf_Milo<-lm(freq~substrate, data=dataplot_femleafMilo)

library(car) 
aovfemleafMilo<-anova(modelfemleaf_Milo)#produces the sum of squares
aovfemleafMilo
# Analysis of Variance Table
# 
# Response: freq
# Df Sum Sq Mean Sq F value    Pr(>F)    
# substrate   3 1374.9  458.29  13.639 6.015e-08 ***
#      Residuals 156 5241.9   33.60                      
# ---
#      Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# > 
# #---------------ck Anova assumptions for model--------------------------------

#NOrmality  tests

#shapiro-wilk test
shapiro.test(residuals(modelfemleaf_Milo)) #failed P<0.05

# 
# Shapiro-Wilk normality test
# 
# data:  residuals(modelfemleaf_Milo)
# W = 0.96718, p-value = 0.0007524

#homogeneity of variance test
library(car)
leveneTest(freq~substrate, data=dataplot_femleafMilo) #failed P<0.05

# Levene's Test for Homogeneity of Variance (center = median)
#        Df F value Pr(>F)  
# group   3  3.3623 0.0203 *
# 156                 
# ---
#since we failed homogeneity need to use Kruskal.test
kruskal.test(freq~substrate, data=dataplot_femleafMilo)
# 
# Kruskal-Wallis rank sum test
# 
# data:  freq by substrate
# Kruskal-Wallis chi-squared = 37.951, df = 3, p-value = 2.894e-08 #sig


#Pairwise Mann-Witney U test = Mann-Whitney-Wilcoxon test-------------------------------------
PT = pairwise.wilcox.test(dataplot_femleafMilo$freq,
                          dataplot_femleafMilo$substrate,
                          p.adjust.method="none")

PT
# Pairwise comparisons using Wilcoxon rank sum test 
# 
# data:  dataplot_femleafMilo$freq and dataplot_femleafMilo$substrate 
# 
# Maize   Milo    Oats   
# Milo 1.0e-05 -       -      
# Oats 0.09337 0.03521 -      
# rice 5.6e-09 0.07846 0.00057

PT = PT$p.value

library(rcompanion)

PT1 = fullPTable(PT)

PT1

library(multcompView)

multcompLetters(PT1,
                compare="<",
                threshold=0.05,
                Letters=letters,
                reversed=T) #reversed=T give the largest value a
# Maize  Milo  Oats  Rice 
# "b"   "a"   "b"   "a" 
# > 

#Dun test multiple comparison
PTdunn=dunnTest(freq~substrate, data=dataplot_femleafMilo, method="bh")
PTdunn
# Dunn (1964) Kruskal-Wallis multiple comparison
# p-values adjusted with the Benjamini-Hochberg method.
# 
# Comparison         Z      P.unadj        P.adj
# 1 Maize - Milo -4.182083 2.888510e-05 8.665531e-05
# 2 Maize - Oats -2.016728 4.372390e-02 5.246868e-02
# 3  Milo - Oats  2.165355 3.036055e-02 4.554082e-02
# 4 Maize - Rice -5.758973 8.462724e-09 5.077635e-08
# 5  Milo - Rice -1.576890 1.148207e-01 1.148207e-01
# 6  Oats - Rice -3.742245 1.823836e-04 3.647672e-04

PTdunn=PTdunn$res
PTdunn
library(rcompanion)

cldList(comparison = PTdunn$Comparison,
        p.value    = PTdunn$P.adj,
        threshold  = 0.05,
        reversed=T)
# 
# Group Letter MonoLetter
# 1 Maize      b          b
# 2  Milo      a         a 
# 3  Oats      b          b
# 4  Rice      a         a 
# 
# #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#femleafRice---------------------------------------------------------

dataplot_femleafRice<-dataplot_femleaf%>%filter(rearing=="Rice grain")

#One way anova test
modelfemleaf_Rice<-lm(freq~substrate, data=dataplot_femleafRice)

library(car) 
aovfemleafRice<-anova(modelfemleaf_Rice)#produces the sum of squares
aovfemleafRice
# Analysis of Variance Table
# 
# Response: freq
#           Df Sum Sq Mean Sq F value    Pr(>F)    
# substrate   3 1878.1  626.02  16.593 2.086e-09 ***
#      Residuals 156 5885.7   37.73                      
# ---
#      Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#---------------ck Anova assumptions for model--------------------------------

#NOrmality  tests

#shapiro-wilk test
shapiro.test(residuals(modelfemleaf_Rice)) #failed P<0.05

# Shapiro-Wilk normality test
# 
# data:  residuals(modelfemleaf_Rice)
# W = 0.94657, p-value = 9.237e-06

#homogeneity of variance test
library(car)
leveneTest(freq~substrate, data=dataplot_femleafRice) #failed P<0.05

# Levene's Test for Homogeneity of Variance (center = median)
#        Df F value   Pr(>F)   
# group   3  4.8603 0.002928 **
# 156                    
# ---      
#since we failed homogeneity need to use Kruskal.test
kruskal.test(freq~substrate, data=dataplot_femleafRice)
# Kruskal-Wallis rank sum test
# 
# data:  freq by substrate
# Kruskal-Wallis chi-squared = 42.501, df = 3, p-value = 3.141e-09  #sig

#Pairwise Mann-Witney U test = Mann-Whitney-Wilcoxon test-------------------------------------
PT = pairwise.wilcox.test(dataplot_femleafRice$freq,
                          dataplot_femleafRice$substrate,
                          p.adjust.method="none")

PT

# 
# Pairwise comparisons using Wilcoxon rank sum test 
# 
# data:  dataplot_femleafRice$freq and dataplot_femleafRice$substrate 
# 
# Maize  Milo   Oats  
# Milo 0.0030 -      -     
# Oats 0.0019 0.4179 -     
# Rice 7e-10  1e-05  0.0037
# 

     
PT = PT$p.value

library(rcompanion)

PT1 = fullPTable(PT)

PT1

library(multcompView)

multcompLetters(PT1,
                compare="<",
                threshold=0.05,
                Letters=letters,
                reversed=T) #reversed=T give the largest value a
# Maize  Milo  Oats  Rice 
# "c"   "b"   "b"   "a" 
# # > 
#Dun test multiple comparison
PTdunn=dunnTest(freq~substrate, data=dataplot_femleafRice, method="bh")
PTdunn
# Dunn (1964) Kruskal-Wallis multiple comparison
# p-values adjusted with the Benjamini-Hochberg method.
# 
# Comparison          Z      P.unadj        P.adj
# 1 Maize - Milo -2.4995906 1.243369e-02 1.492043e-02
# 2 Maize - Oats -3.3190888 9.031170e-04 1.806234e-03
# 3  Milo - Oats -0.8194983 4.125022e-01 4.125022e-01
# 4 Maize - Rice -6.4520379 1.103560e-10 6.621357e-10
# 5  Milo - Rice -3.9524474 7.735595e-05 2.320679e-04
# 6  Oats - Rice -3.1329491 1.730595e-03 2.595892e-03
# > 

PTdunn=PTdunn$res
PTdunn
library(rcompanion)

cldList(comparison = PTdunn$Comparison,
        p.value    = PTdunn$P.adj,
        threshold  = 0.05,
        reversed=T)
# 
# Group Letter MonoLetter
# 1 Maize      c          c
# 2  Milo      b         b 
# 3  Oats      b         b 
# 4  Rice      a        a  

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
######################## Roots   ################################################
#---------- mroot----------------------------------------
#dataplot subset by rmale
dataplot_mroot<-dataplot%>%filter(location_plant == c( "rmale"))

#---------mrootBarley------
dataplot_mrootBarley<-dataplot_mroot%>%filter(rearing=="Barley grain")
Summarize(freq~substrate, data=dataplot_mrootBarley)
# matched

#One way anova test
modelmroot_Barley<-lm(freq~substrate, data=dataplot_mrootBarley)

library(car) 
aovmrootBarley<-anova(modelmroot_Barley)#produces the sum of squares
aovmrootBarley

# Analysis of Variance Table
# 
# Response: freq
#              Df  Sum Sq Mean Sq F value Pr(>F)
# substrate   3    5.92   1.975  0.1916  0.902
# Residuals 156 1608.05  10.308               

#---------------ck Anova assumptions for model--------------------------------

#NOrmality  tests

#shapiro-wilk test
shapiro.test(residuals(modelmroot_Barley)) #failed P<0.05

# 
# Shapiro-Wilk normality test
# 
# data:  residuals(modelmroot_Barley)
# W = 0.85626, p-value = 3.21e-11


#homogeneity of variance test
library(car)
leveneTest(freq~substrate, data=dataplot_mrootBarley) #passed P>0.05
# Levene's Test for Homogeneity of Variance (center = median)
#        Df F value Pr(>F)
# group   3  0.4379 0.7262

# since we failed homogeneity need to use Kruskal.test
kruskal.test(freq~substrate, data=dataplot_mrootBarley)
# 
# Kruskal-Wallis rank sum test
# 
# data:  freq by substrate
# Kruskal-Wallis chi-squared = 0.75092, df = 3, p-value = 0.8612# P>0.5not sig

#Pairwise Mann-Witney U test = Mann-Whitney-Wilcoxon test
PT = pairwise.wilcox.test(dataplot_mrootBarley$freq,
                          dataplot_mrootBarley$substrate,
                          p.adjust.method="none")

PT
# Pairwise comparisons using Wilcoxon rank sum test 
# 
# data:  dataplot_mrootBarley$freq and dataplot_mrootBarley$substrate 
# 
# Maize Milo Oats
# Milo 0.99  -    -   
# Oats 0.63  0.67 -   
# Rice 0.69  0.72 0.38

PT = PT$p.value

library(rcompanion)

PT1 = fullPTable(PT)

PT1

library(multcompView)

multcompLetters(PT1,
                compare="<",
                threshold=0.05,
                Letters=letters,
                reversed=T) #reversed=T give the largest value a
# Maize  Milo  Oats  Rice 
# "a"   "a"   "a"   "a" 


#Dun test multiple comparison
PTdunn=dunnTest(freq~substrate, data=dataplot_mrootBarley, method="bh")
PTdunn
# Dunn (1964) Kruskal-Wallis multiple comparison
# p-values adjusted with the Benjamini-Hochberg method.
# 
# Comparison           Z   P.unadj     P.adj
# 1 Maize - Milo  0.01097818 0.9912409 0.9912409
# 2 Maize - Oats  0.47572101 0.6342732 1.0000000
# 3  Milo - Oats  0.46474283 0.6421156 1.0000000
# 4 Maize - Rice -0.38911539 0.6971908 0.8366289
# 5  Milo - Rice -0.40009357 0.6890876 1.0000000
# 6  Oats - Rice -0.86483640 0.3871286 1.0000000
PTdunn=PTdunn$res
PTdunn
library(rcompanion)

cldList(comparison = PTdunn$Comparison,
        p.value    = PTdunn$P.adj,
        threshold  = 0.05,
        reversed=T)
# 
#Error: No significant differences.



#mrootmaize---------------------------------------------------------

dataplot_mrootMaize<-dataplot_mroot%>%filter(rearing=="Maize grain")

#Summarize(freq~substrate, data=dataplot_mrootMaize)
# matched exel
#One way anova test
modelmroot_Maize<-lm(freq~substrate, data=dataplot_mrootMaize)

library(car) 
aovmrootMaize<-anova(modelmroot_Maize)#produces the sum of squares
aovmrootMaize

# Analysis of Variance Table
# 
# Response: freq
#           Df  Sum Sq Mean Sq F value   Pr(>F)   
# substrate   3  164.72  54.908  4.5392 0.004431 **
#      Residuals 156 1887.05  12.096                    
# ---
#      Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#---------------ck Anova assumptions for model--------------------------------

#NOrmality  tests

#shapiro-wilk test
shapiro.test(residuals(modelmroot_Maize)) #failed P<0.05

# Shapiro-Wilk normality test
# 
# data:  residuals(modelmroot_Maize)
# W = 0.93627, p-value = 1.409e-06

#homogeneity of variance test
library(car)
leveneTest(freq~substrate, data=dataplot_mrootMaize) #failed P<0.05

# Levene's Test for Homogeneity of Variance (center = median)
#        Df F value   Pr(>F)   
# group   3  4.9077 0.002754 **
# 156                    


#since we failed homogeneity need to use Kruskal.test
kruskal.test(freq~substrate, data=dataplot_mrootMaize)
# Kruskal-Wallis rank sum test
# 
# data:  freq by substrate
# Kruskal-Wallis chi-squared = 12.789, df = 3, p-value = 0.005117 #signif
# 

#Pairwise Mann-Witney U test = Mann-Whitney-Wilcoxon test-------------------------------------
PT = pairwise.wilcox.test(dataplot_mrootMaize$freq,
                          dataplot_mrootMaize$substrate,
                          p.adjust.method="none")

PT
# Pairwise comparisons using Wilcoxon rank sum test 
# 
# data:  dataplot_mrootMaize$freq and dataplot_mrootMaize$substrate 
# 
# Maize  Milo   Oats  
# Milo 0.0141 -      -     
# Oats 0.3144 0.0725 -     
# Rice 0.0022 0.9153 0.0136

PT = PT$p.value

library(rcompanion)

PT1 = fullPTable(PT)

PT1

library(multcompView)

multcompLetters(PT1,
                compare="<",
                threshold=0.05,
                Letters=letters,
                reversed=T) #reversed=T give the largest value a
# 
# Maize  Milo  Oats  Rice 
# "c"  "ab"  "bc"   "a" 

#Dun test multiple comparison
PTdunn=dunnTest(freq~substrate, data=dataplot_mrootMaize, method="bh")
PTdunn
# Dunn (1964) Kruskal-Wallis multiple comparison
# p-values adjusted with the Benjamini-Hochberg method.
# 
# Comparison          Z     P.unadj      P.adj
# 1 Maize - Milo -2.7326293 0.006283100 0.01884930
# 2 Maize - Oats -0.8182106 0.413236918 0.49588430
# 3  Milo - Oats  1.9144187 0.055566681 0.08335002
# 4 Maize - Rice -2.9948452 0.002745842 0.01647505
# 5  Milo - Rice -0.2622159 0.793155016 0.79315502
# 6  Oats - Rice -2.1766345 0.029507845 0.05901569
PTdunn=PTdunn$res
PTdunn
library(rcompanion)

cldList(comparison = PTdunn$Comparison,
        p.value    = PTdunn$P.adj,
        threshold  = 0.05,
        reversed=T)
# Group Letter MonoLetter
# 1 Maize      b          b
# 2  Milo      a         a 
# 3  Oats     ab         ab
# 4  Rice      a         a 
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#mrootMilo---------------------------------------------------------

dataplot_mrootMilo<-dataplot_mroot%>%filter(rearing=="Milo grain")

#One way anova test
modelmroot_Milo<-lm(freq~substrate, data=dataplot_mrootMilo)

library(car) 
aovmrootMilo<-anova(modelmroot_Milo)#produces the sum of squares
aovmrootMilo
# Analysis of Variance Table
# 
# Response: freq
#             Df Sum Sq Mean Sq F value Pr(>F)
# substrate   3   3.92  1.3062   0.214 0.8866
# Residuals 156 952.17  6.1037      
#---------------ck Anova assumptions for model--------------------------------

#NOrmality  tests

#shapiro-wilk test
shapiro.test(residuals(modelmroot_Milo)) #failed P<0.05

# 
# Shapiro-Wilk normality test
# 
# data:  residuals(modelmroot_Milo)
# W = 0.83458, p-value = 3.613e-12

#homogeneity of variance test
library(car)
leveneTest(freq~substrate, data=dataplot_mrootMilo) #passed P>0.05
# 
# Levene's Test for Homogeneity of Variance (center = median)
#        Df F value Pr(>F)
# group   3  0.9845 0.4017
#       156    
# # ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

#since we failed homogeneity need to use Kruskal.test
kruskal.test(freq~substrate, data=dataplot_mrootMilo)
# Kruskal-Wallis rank sum test
# 
# data:  freq by substrate
# Kruskal-Wallis chi-squared = 1.8262, df = 3, p-value = 0.6093 #not sig


#Pairwise Mann-Witney U test = Mann-Whitney-Wilcoxon test-------------------------------------
PT = pairwise.wilcox.test(dataplot_mrootMilo$freq,
                          dataplot_mrootMilo$substrate,
                          p.adjust.method="none")

PT
# Pairwise comparisons using Wilcoxon rank sum test 
# 
# data:  dataplot_mrootMilo$freq and dataplot_mrootMilo$substrate 
# 
# Maize Milo Oats
# Milo 0.86  -    -   
# Oats 0.97  0.84 -   
# Rice 0.29  0.27 0.27

PT = PT$p.value

library(rcompanion)

PT1 = fullPTable(PT)

PT1

library(multcompView)

multcompLetters(PT1,
                compare="<",
                threshold=0.05,
                Letters=letters,
                reversed=T) #reversed=T give the largest value a
# 
# Maize  Milo  Oats  Rice 
# "a"   "a"   "a"   "a" 

#Dun test multiple comparison
PTdunn=dunnTest(freq~substrate, data=dataplot_mrootMilo, method="bh")
PTdunn
# Dunn (1964) Kruskal-Wallis multiple comparison
# p-values adjusted with the Benjamini-Hochberg method.
# 
# Comparison           Z   P.unadj     P.adj
# 1 Maize - Milo  0.14030975 0.8884153 1.0000000
# 2 Maize - Oats -0.01353866 0.9891980 0.9891980
# 3  Milo - Oats -0.15384841 0.8777293 1.0000000
# 4 Maize - Rice -1.05232315 0.2926513 0.8779539
# 5  Milo - Rice -1.19263290 0.2330132 1.0000000
# 6  Oats - Rice -1.03878449 0.2989050 0.5978100

PTdunn=PTdunn$res
PTdunn
library(rcompanion)

cldList(comparison = PTdunn$Comparison,
        p.value    = PTdunn$P.adj,
        threshold  = 0.05,
        reversed=T)

#No significant differences.
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#mleafRice---------------------------------------------------------

dataplot_mrootRice<-dataplot_mroot%>%filter(rearing=="Rice grain")

#One way anova test
modelmroot_Rice<-lm(freq~substrate, data=dataplot_mrootRice)

library(car) 
aovmrootRice<-anova(modelmroot_Rice)#produces the sum of squares
aovmrootRice
# Analysis of Variance Table
# 
# Response: freq
#            Df  Sum Sq Mean Sq F value Pr(>F)
# substrate   3   57.07  19.023  1.6923 0.1709
# Residuals 156 1753.62  11.241          

#---------------ck Anova assumptions for model--------------------------------

#NOrmality  tests

#shapiro-wilk test
shapiro.test(residuals(modelmroot_Rice)) #failed P<0.05
# 
# Shapiro-Wilk normality test
# 
# data:  residuals(modelmroot_Rice)
# W = 0.89808, p-value = 4.422e-09
# 
#homogeneity of variance test
library(car)
leveneTest(freq~substrate, data=dataplot_mrootRice) #passed P>0.05

# Levene's Test for Homogeneity of Variance (center = median)
#        Df F value Pr(>F)
# group   3  0.4371 0.7267
# 156               
#since we failed homogeneity need to use Kruskal.test
kruskal.test(freq~substrate, data=dataplot_mrootRice)
# 
# 
# Kruskal-Wallis rank sum test
# 
# data:  freq by substrate
# Kruskal-Wallis chi-squared = 7.3045, df = 3, p-value = 0.0628 # not sig
# 

#Pairwise Mann-Witney U test = Mann-Whitney-Wilcoxon test-------------------------------------
PT = pairwise.wilcox.test(dataplot_mrootRice$freq,
                          dataplot_mrootRice$substrate,
                          p.adjust.method="none")

PT
# Pairwise comparisons using Wilcoxon rank sum test 
# 
# data:  dataplot_mrootRice$freq and dataplot_mrootRice$substrate 
# 
# Maize Milo  Oats 
# Milo 0.210 -     -    
# Oats 0.857 0.294 -    
# Rice 0.021 0.295 0.021
# 
# P value adjustment method: none 

 PT = PT$p.value

library(rcompanion)

PT1 = fullPTable(PT)

PT1

library(multcompView)

multcompLetters(PT1,
                compare="<",
                threshold=0.05,
                Letters=letters,
                reversed=T) #reversed=T give the largest value a
# Maize  Milo  Oats  Rice 
# "b"  "ab"   "b"   "a" 
# > 

#Dun test multiple comparison
PTdunn=dunnTest(freq~substrate, data=dataplot_mrootRice, method="bh")
PTdunn
#
# Dunn (1964) Kruskal-Wallis multiple comparison
# p-values adjusted with the Benjamini-Hochberg method.
# 
# Comparison          Z    P.unadj      P.adj
# 1 Maize - Milo -1.2578579 0.20844316 0.41688632
# 2 Maize - Oats -0.1459796 0.88393746 0.88393746
# 3  Milo - Oats  1.1118782 0.26619051 0.39928576
# 4 Maize - Rice -2.3673031 0.01791825 0.10750950
# 5  Milo - Rice -1.1094452 0.26723815 0.32068578
# 6  Oats - Rice -2.2213235 0.02632906 0.07898718
PTdunn=PTdunn$res
PTdunn
library(rcompanion)

cldList(comparison = PTdunn$Comparison,
        p.value    = PTdunn$P.adj,
        threshold  = 0.05,
        reversed=T)

# No significant differences.
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



#---------- femroot----------------------------------------
#dataplot subset by female
dataplot_femroot<-dataplot%>%filter(location_plant == c( "rfemale"))

#---------femrootBarley------
dataplot_femrootBarley<-dataplot_femroot%>%filter(rearing=="Barley grain")

#Summarize(freq~substrate, data=dataplot_femrootBarley)
# same as Excel

#One way anova test
modelfemroot_barley<-lm(freq~substrate, data=dataplot_femrootBarley)

library(car) 
aovfemrootBarley<-anova(modelfemroot_barley)#produces the sum of squares
aovfemrootBarley

# Analysis of Variance Table
# 
# Response: freq
#            Df  Sum Sq Mean Sq F value Pr(>F)
# substrate   3   11.92  3.9729  0.3079 0.8196
# Residuals 156 2012.77 12.9024#---------------ck Anova assumptions for model--------------------------------

#NOrmality  tests

#shapiro-wilk test
shapiro.test(residuals(modelfemroot_barley)) #failed P<0.05

# Shapiro-Wilk normality test
# 
# data:  residuals(modelfemroot_barley)
# W = 0.84987, p-value = 1.65e-11

#homogeneity of variance test
library(car)
leveneTest(freq~substrate, data=dataplot_femrootBarley) #passed P>0.05
# Levene's Test for Homogeneity of Variance (center = median)
#        Df F value Pr(>F)
# group   3  0.6706 0.5713

#since we failed homogeneity need to use Kruskal.test
kruskal.test(freq~substrate, data=dataplot_femrootBarley)

# Kruskal-Wallis rank sum test
# 
# data:  freq by substrate
# Kruskal-Wallis chi-squared = 0.58279, df = 3, p-value = 0.9004  # not sig

# #Pairwise Mann-Witney U test = Mann-Whitney-Wilcoxon test-------------------------------------
PT = pairwise.wilcox.test(dataplot_femrootBarley$freq,
                          dataplot_femrootBarley$substrate,
                          p.adjust.method="none")

PT

# Pairwise comparisons using Wilcoxon rank sum test 
# 
# data:  dataplot_femrootBarley$freq and dataplot_femrootBarley$substrate 
# 
# Maize Milo Oats
# Milo 0.73  -    -   
# Oats 0.90  0.66 -   
# Rice 0.67  0.45 0.80
# 
# P value adjustment method: none 

PT = PT$p.value

library(rcompanion)

PT1 = fullPTable(PT)

PT1

library(multcompView)

multcompLetters(PT1,
                compare="<",
                threshold=0.05,
                Letters=letters,
                reversed=T) #reversed=T give the largest value a

# Maize  Milo  Oats  Rice 
# "a"   "a"   "a"   "a"

#Dun test multiple comparison
PTdunn=dunnTest(freq~substrate, data=dataplot_femrootBarley, method="bh")
PTdunn
# Dunn (1964) Kruskal-Wallis multiple comparison
# p-values adjusted with the Benjamini-Hochberg method.
# 
# Comparison          Z   P.unadj     P.adj
# 1 Maize - Milo  0.3364906 0.7365010 1.0000000
# 2 Maize - Oats -0.1316702 0.8952451 0.8952451
# 3  Milo - Oats -0.4681608 0.6396696 1.0000000
# 4 Maize - Rice -0.4145174 0.6784952 1.0000000
# 5  Milo - Rice -0.7510080 0.4526479 1.0000000
# 6  Oats - Rice -0.2828472 0.7772940 0.9327528
PTdunn=PTdunn$res
PTdunn
library(rcompanion)

cldList(comparison = PTdunn$Comparison,
        p.value    = PTdunn$P.adj,
        threshold  = 0.05,
        reversed=T)
#No significant differences. 
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#femrootmaize---------------------------------------------------------

dataplot_femrootMaize<-dataplot_femroot%>%filter(rearing=="Maize grain")

#Summarize(freq~substrate, data=dataplot_femrootMaize)
#matched
#One way anova test
modelfemroot_Maize<-lm(freq~substrate, data=dataplot_femrootMaize)



library(car) 
aovfemrootMaize<-anova(modelfemroot_Maize)#produces the sum of squares
aovfemrootMaize

# Analysis of Variance Table
# 
# Response: freq
# Df  Sum Sq Mean Sq F value   Pr(>F)   
# substrate   3  188.15  62.717  4.1678 0.007161 **
#      Residuals 156 2347.45  15.048                    
# ---
#      Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# > 
#---------------ck Anova assumptions for model--------------------------------

#NOrmality  tests

#shapiro-wilk test
shapiro.test(residuals(modelfemroot_Maize)) #failed P<0.05

# Shapiro-Wilk normality test
# 
# data:  residuals(modelfemroot_Maize)
# W = 0.89398, p-value = 2.592e-09


#homogeneity of variance test
library(car)
leveneTest(freq~substrate, data=dataplot_femrootMaize) #failed P<0.05

# Levene's Test for Homogeneity of Variance (center = median)
#        Df F value   Pr(>F)   
# group   3  4.6889 0.003652 **
# 156                    
# ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

#since we failed homogeneity need to use Kruskal.test
kruskal.test(freq~substrate, data=dataplot_femrootMaize)
# 
# Kruskal-Wallis rank sum test
# 
# data:  freq by substrate
# Kruskal-Wallis chi-squared = 9.8941, df = 3, p-value = 0.01949 #sig P<.05


#Pairwise Mann-Witney U test = Mann-Whitney-Wilcoxon test-------------------------------------
PT = pairwise.wilcox.test(dataplot_femrootMaize$freq,
                          dataplot_femrootMaize$substrate,
                          p.adjust.method="none")

PT

# Pairwise comparisons using Wilcoxon rank sum test 
# 
# data:  dataplot_femrootMaize$freq and dataplot_femrootMaize$substrate 
# 
# Maize Milo  Oats 
# Milo 0.016 -     -    
# Oats 0.641 0.017 -    
# Rice 0.067 0.321 0.052
# 
# P value adjustment method: none 
# > 

PT = PT$p.value

library(rcompanion)

PT1 = fullPTable(PT)

PT1

library(multcompView)

multcompLetters(PT1,
                compare="<",
                threshold=0.05,
                Letters=letters,
                reversed=T) #reversed=T give the largest value a

# Maize  Milo  Oats  Rice 
# "b"   "a"   "b"  "ab" 

#Dun test multiple comparison
PTdunn=dunnTest(freq~substrate, data=dataplot_femrootMaize, method="bh")
PTdunn
# Dunn (1964) Kruskal-Wallis multiple comparison
# p-values adjusted with the Benjamini-Hochberg method.
# 
# Comparison          Z     P.unadj      P.adj
# 1 Maize - Milo -2.6390128 0.008314784 0.04988871
# 2 Maize - Oats -0.2125298 0.831693738 0.83169374
# 3  Milo - Oats  2.4264830 0.015245965 0.04573789
# 4 Maize - Rice -1.8799778 0.060111101 0.12022220
# 5  Milo - Rice  0.7590350 0.447831637 0.53739796
# 6  Oats - Rice -1.6674480 0.095425351 0.14313803

PTdunn=PTdunn$res
PTdunn
library(rcompanion)

cldList(comparison = PTdunn$Comparison,
        p.value    = PTdunn$P.adj,
        threshold  = 0.05,
        reversed=T)
# Group Letter MonoLetter
# 1 Maize      b          b
# 2  Milo      a         a 
# 3  Oats      b          b
# 4  Rice     ab         ab
# > 
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#femrootMilo---------------------------------------------------------

dataplot_femrootMilo<-dataplot_femroot%>%filter(rearing=="Milo grain")

#One way anova test
modelfemroot_Milo<-lm(freq~substrate, data=dataplot_femrootMilo)

library(car) 
aovfemrootMilo<-anova(modelfemroot_Milo)#produces the sum of squares
aovfemrootMilo

# Analysis of Variance Table
# 
# Response: freq
# Df Sum Sq Mean Sq F value Pr(>F)
# substrate   3   7.12  2.3729  0.5381 0.6568
# Residuals 156 687.87  4.4095 
# > 
# #---------------ck Anova assumptions for model--------------------------------

#NOrmality  tests

#shapiro-wilk test
shapiro.test(residuals(modelfemroot_Milo)) #failed P<0.05

# 
# Shapiro-Wilk normality test
# 
# data:  residuals(modelfemroot_Milo)
# W = 0.85848, p-value = 4.062e-11

#homogeneity of variance test
library(car)
leveneTest(freq~substrate, data=dataplot_femrootMilo) #passed P>0.05

# Levene's Test for Homogeneity of Variance (center = median)
#        Df F value Pr(>F)
# group   3  0.1942 0.9003
# ---

#since we failed homogeneity need to use Kruskal.test
kruskal.test(freq~substrate, data=dataplot_femrootMilo)
# 
# Kruskal-Wallis rank sum test
# 
# data:  freq by substrate
# Kruskal-Wallis chi-squared = 3.445, df = 3, p-value = 0.328 #not sig


#Pairwise Mann-Witney U test = Mann-Whitney-Wilcoxon test-------------------------------------
PT = pairwise.wilcox.test(dataplot_femrootMilo$freq,
                          dataplot_femrootMilo$substrate,
                          p.adjust.method="none")

PT

# Pairwise comparisons using Wilcoxon rank sum test 
# 
# data:  dataplot_femrootMilo$freq and dataplot_femrootMilo$substrate 
# 
# Maize Milo  Oats 
# Milo 0.204 -     -    
# Oats 0.441 0.723 -    
# Rice 0.084 0.502 0.301
# 
# P value adjustment method: none 

PT = PT$p.value

library(rcompanion)

PT1 = fullPTable(PT)

PT1

library(multcompView)

multcompLetters(PT1,
                compare="<",
                threshold=0.05,
                Letters=letters,
                reversed=T) #reversed=T give the largest value a
# Maize  Milo  Oats  Rice 
# "a"   "a"   "a"   "a"  
# > 

#Dun test multiple comparison
PTdunn=dunnTest(freq~substrate, data=dataplot_femrootMilo, method="bh")
PTdunn
#
# Dunn (1964) Kruskal-Wallis multiple comparison
# p-values adjusted with the Benjamini-Hochberg method.
# 
# Comparison          Z    P.unadj     P.adj
# 1 Maize - Milo -1.1833754 0.23666040 0.7099812
# 2 Maize - Oats -0.7868582 0.43136489 0.6470473
# 3  Milo - Oats  0.3965172 0.69172350 0.6917235
# 4 Maize - Rice -1.8096503 0.07035003 0.4221002
# 5  Milo - Rice -0.6262749 0.53113466 0.6373616
# 6  Oats - Rice -1.0227921 0.30640615 0.6128123
PTdunn=PTdunn$res
PTdunn
library(rcompanion)

cldList(comparison = PTdunn$Comparison,
        p.value    = PTdunn$P.adj,
        threshold  = 0.05,
        reversed=T)
#  No significant differences
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#femrootRice---------------------------------------------------------

dataplot_femrootRice<-dataplot_femroot%>%filter(rearing=="Rice grain")

#One way anova test
modelfemroot_Rice<-lm(freq~substrate, data=dataplot_femrootRice)

library(car) 
aovfemrootRice<-anova(modelfemroot_Rice)#produces the sum of squares
aovfemrootRice

# Analysis of Variance Table
# 
# Response: freq
# Df  Sum Sq Mean Sq F value Pr(>F)
# substrate   3    3.25  1.0833  0.1011 0.9593
# Residuals 156 1672.35 10.7202  

#---------------ck Anova assumptions for model--------------------------------

#NOrmality  tests

#shapiro-wilk test
shapiro.test(residuals(modelfemroot_Rice)) #failed P<0.05

# Shapiro-Wilk normality test
# 
# data:  residuals(modelfemroot_Rice)
# W = 0.86113, p-value = 5.402e-11

#homogeneity of variance test
library(car)
leveneTest(freq~substrate, data=dataplot_femrootRice) #passed P>0.05

# Levene's Test for Homogeneity of Variance (center = median)
#        Df F value Pr(>F)
# group   3  0.5898 0.6226
# ---  

#since we failed homogeneity need to use Kruskal.test
kruskal.test(freq~substrate, data=dataplot_femrootRice)

# Kruskal-Wallis rank sum test
# 
# data:  freq by substrate
# Kruskal-Wallis chi-squared = 2.7043, df = 3, p-value = 0.4395 #not sig


#Pairwise Mann-Witney U test = Mann-Whitney-Wilcoxon test-------------------------------------
PT = pairwise.wilcox.test(dataplot_femrootRice$freq,
                          dataplot_femrootRice$substrate,
                          p.adjust.method="none")

PT

# Pairwise comparisons using Wilcoxon rank sum test 
# 
# data:  dataplot_femrootRice$freq and dataplot_femrootRice$substrate 
# 
# Maize Milo Oats
# Milo 0.62  -    -   
# Oats 0.21  0.39 -   
# Rice 0.17  0.33 0.91



PT = PT$p.value

library(rcompanion)

PT1 = fullPTable(PT)

PT1

library(multcompView)

multcompLetters(PT1,
                compare="<",
                threshold=0.05,
                Letters=letters,
                reversed=T) #reversed=T give the largest value a
# $Letters
# Maize  Milo  Oats  Rice 
# "a"   "a"   "a"   "a" 

# > #Dun test multiple comparison
PTdunn=dunnTest(freq~substrate, data=dataplot_femrootRice, method="bh")
PTdunn
#
# Dunn (1964) Kruskal-Wallis multiple comparison
# p-values adjusted with the Benjamini-Hochberg method.
# 
# Comparison          Z      P.unadj        P.adj
# 1 Maize - Milo -3.0170140 2.552780e-03 3.063336e-03
# 2 Maize - Oats -3.6803463 2.329175e-04 3.493762e-04
# 3  Milo - Oats -0.6633323 5.071178e-01 5.071178e-01
# 4 Maize - Rice -7.4682273 8.128238e-14 4.876943e-13
# 5  Milo - Rice -4.4512133 8.538649e-06 2.561595e-05
# 6  Oats - Rice -3.7878810 1.519376e-04 3.038752e-04
PTdunn=PTdunn$res
PTdunn
library(rcompanion)

cldList(comparison = PTdunn$Comparison,
        p.value    = PTdunn$P.adj,
        threshold  = 0.05,
        reversed=T)
#   No significant differences
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
######################## Soil   ################################################
#---------- msoil----------------------------------------
#dataplot subset by rmale
dataplot_msoil<-dataplot%>%filter(location_plant == c( "smale"))

#---------msoilBarley------
dataplot_msoilBarley<-dataplot_msoil%>%filter(rearing=="Barley grain")

Summarize(freq~substrate, data=dataplot_msoilBarley)
#matched excel

#One way anova test
modelmsoil_Barley<-lm(freq~substrate, data=dataplot_msoilBarley)

library(car) 
aovmsoilBarley<-anova(modelmsoil_Barley)#produces the sum of squares
aovmsoilBarley

# Analysis of Variance Table
# 
# Response: freq
#            Df  Sum Sq Mean Sq F value Pr(>F)
# substrate   3   85.32  28.442  1.9716 0.1205
# Residuals 156 2250.45  14.426            

#---------------ck Anova assumptions for model--------------------------------

#NOrmality  tests

#shapiro-wilk test
shapiro.test(residuals(modelmsoil_Barley)) #failed P<0.05

# 
# Shapiro-Wilk normality test
# 
# data:  residuals(modelmsoil_Barley)
# W = 0.86575, p-value = 8.951e-11
# 

#homogeneity of variance test
library(car)
leveneTest(freq~substrate, data=dataplot_msoilBarley) #passed P>0.05

# Levene's Test for Homogeneity of Variance (center = median)
#        Df F value Pr(>F)
# group   3  1.1134 0.3455

# since we failed homogeneity need to use Kruskal.test
kruskal.test(freq~substrate, data=dataplot_msoilBarley)

# 
# Kruskal-Wallis rank sum test
# 
# data:  freq by substrate
# Kruskal-Wallis chi-squared = 4.5068, df = 3, p-value = 0.2117  #not sig


#Pairwise Mann-Witney U test = Mann-Whitney-Wilcoxon test
PT = pairwise.wilcox.test(dataplot_msoilBarley$freq,
                          dataplot_msoilBarley$substrate,
                          p.adjust.method="none")

PT

# Pairwise comparisons using Wilcoxon rank sum test 
# 
# data:  dataplot_msoilBarley$freq and dataplot_msoilBarley$substrate 
# 
# Maize Milo Oats
# Milo 0.33  -    -   
# Oats 0.51  0.78 -   
# Rice 0.24  0.06 0.11

PT = PT$p.value

library(rcompanion)

PT1 = fullPTable(PT)

PT1

library(multcompView)

multcompLetters(PT1,
                compare="<",
                threshold=0.05,
                Letters=letters,
                reversed=T) #reversed=T give the largest value a

# Maize  Milo  Oats  Rice 
# "a"   "a"   "a"   "a" 

#Dun test multiple comparison
PTdunn=dunnTest(freq~substrate, data=dataplot_msoilBarley, method="bh")
PTdunn
# Dunn (1964) Kruskal-Wallis multiple comparison
# p-values adjusted with the Benjamini-Hochberg method.
# 
# Comparison          Z    P.unadj     P.adj
# 1 Maize - Milo  0.9056045 0.36514522 0.5477178
# 2 Maize - Oats  0.6142572 0.53904537 0.6468544
# 3  Milo - Oats -0.2913473 0.77078572 0.7707857
# 4 Maize - Rice -1.0537061 0.29201751 0.5840350
# 5  Milo - Rice -1.9593106 0.05007643 0.3004586
# 6  Oats - Rice -1.6679633 0.09532302 0.2859690

PTdunn=PTdunn$res
PTdunn
library(rcompanion)

cldList(comparison = PTdunn$Comparison,
        p.value    = PTdunn$P.adj,
        threshold  = 0.05,
        reversed=T)

# No significant differences.


#msoilmaize---------------------------------------------------------

dataplot_msoilMaize<-dataplot_msoil%>%filter(rearing == "Maize grain")
Summarize(freq~substrate, data=dataplot_msoil)
Summarize(freq~substrate, data=dataplot_msoilMaize)

#One way anova test 1
modelmsoil_Maize<-lm(freq~substrate, data=dataplot_msoilMaize)

library(car) 
aovmsoilMaize<-anova(modelmsoil_Maize)#produces the sum of squares
aovmsoilMaize

# Analysis of Variance Table
# 
# Response: freq
#         Df Sum Sq Mean Sq F value   Pr(>F)   
# substrate   3  40.85 13.6167  5.1527 0.002009 **
# Residuals 156 412.25  2.6426                    
-----
#      Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#---------------ck Anova assumptions for model--------------------------------

#NOrmality  tests

#shapiro-wilk test 1
shapiro.test(residuals(modelmsoil_Maize)) 

# Error in -shapiro.test(residuals(modelmsoil_Maize)) : 
#      invalid argument to unary operator

#other way to run anova 2
res.aov<-aov(freq~substrate, data=dataplot_msoilMaize)

#summary of anova  Note same as aovleafBarley above
summary(res.aov)
# Df Sum Sq Mean Sq F value  Pr(>F)   
# substrate     3   40.8  13.617   5.153 0.00201 **
# Residuals   156  412.2   2.643                   
# ---
#      Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

# shapiro wilks 2
#using other method res.aov output same as above

# Extract the residuals
aov_residuals <- residuals(object = res.aov )
# Run Shapiro-Wilk test
shapiro.test(x = aov_residuals )

# Shapiro-Wilk normality test
# 
# data:  aov_residuals
# W = 0.85313, p-value = 2.311e-11  #failes p<0.05



#homogeneity of variance test
library(car)
leveneTest(freq~substrate, data=dataplot_msoilMaize) #failed P<0.05

# Levene's Test for Homogeneity of Variance (center = median)
#        Df F value Pr(>F)  
# group   3  3.0779 0.0293 *
# 156                 
---
#since we failed homogeneity need to use Kruskal.test
kruskal.test(freq~substrate, data=dataplot_msoilMaize)


# Error in -kruskal.test(freq ~ substrate, data = dataplot_msoilMaize) : 
#      invalid argument to unary operator
# will asume sig based on anova



#Pairwise Mann-Witney U test = Mann-Whitney-Wilcoxon test-------------------------------------
PT = pairwise.wilcox.test(dataplot_msoilMaize$freq,
                          dataplot_msoilMaize$substrate,
                          p.adjust.method="none")

PT

# Pairwise comparisons using Wilcoxon rank sum test 
# 
# data:  dataplot_msoilMaize$freq and dataplot_msoilMaize$substrate 
# 
# Maize  Milo    Oats  
# Milo 0.5505 -       -     
#Oats 0.0032 6.5e-05 -     
#Rice 0.7732 0.2958  0.0013

PT = PT$p.value

library(rcompanion)

PT1 = fullPTable(PT)

PT1

library(multcompView)

multcompLetters(PT1,
                compare="<",
                threshold=0.05,
                Letters=letters,
                reversed=T) #reversed=T give the largest value a
# 
# Maize  Milo  Oats  Rice 
# "b"   "b"   "a"   "b" 

#Dun test multiple comparison
PTdunn=dunnTest(freq~substrate, data=dataplot_msoilMaize, method="bh")
PTdunn
# Dunn (1964) Kruskal-Wallis multiple comparison
# p-values adjusted with the Benjamini-Hochberg method.
# 
# Comparison          Z      P.unadj        P.adj
# 1 Maize - Milo -0.7627790 4.455952e-01 0.5347142321
# 2 Maize - Oats  3.1516471 1.623524e-03 0.0048705709
# 3  Milo - Oats  3.9144261 9.061948e-05 0.0005437169
# 4 Maize - Rice  0.1947788 8.455661e-01 0.8455660977
# 5  Milo - Rice  0.9575579 3.382858e-01 0.5074286424
# 6  Oats - Rice -2.9568683 3.107809e-03 0.0062156171
PTdunn=PTdunn$res
PTdunn
library(rcompanion)

cldList(comparison = PTdunn$Comparison,
        p.value    = PTdunn$P.adj,
        threshold  = 0.05,
        reversed=T)
# 
# Group Letter MonoLetter
# 1 Maize      b          b
# 2  Milo      b          b
# 3  Oats      a         a 
# 4  Rice      b          b
# > 
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#msoilMilo---------------------------------------------------------

dataplot_msoilMilo<-dataplot_msoil%>%filter(rearing=="Milo grain")

#One way anova test
modelmsoil_Milo<-lm(freq~substrate, data=dataplot_msoilMilo)

library(car) 
aovmsoilMilo<-anova(modelmsoil_Milo)#produces the sum of squares
aovmsoilMilo

# Analysis of Variance Table
# 
# Response: freq
#           Df Sum Sq Mean Sq F value  Pr(>F)  
# substrate   3  44.37 14.7896  2.5365 0.05877 .
# Residuals 156 909.57  5.8306  


#---------------ck Anova assumptions for model--------------------------------

#NOrmality  tests

#shapiro-wilk test
shapiro.test(residuals(modelmsoil_Milo)) #failed P<0.05
# 
# Shapiro-Wilk normality test
# 
# data:  residuals(modelmsoil_Milo)
# W = 0.88644, p-value = 1.001e-09

#homogeneity of variance test
library(car)
leveneTest(freq~substrate, data=dataplot_msoilMilo) #passed P>0.05
# 
# Levene's Test for Homogeneity of Variance (center = median)
#        Df F value Pr(>F)
# group   3  1.8419 0.1418
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

#since we failed homogeneity need to use Kruskal.test
kruskal.test(freq~substrate, data=dataplot_msoilMilo)

# Kruskal-Wallis rank sum test
# 
# data:  freq by substrate
# Kruskal-Wallis chi-squared = 9.0347, df = 3, p-value = 0.02883 # P<.05 sig


#Pairwise Mann-Witney U test = Mann-Whitney-Wilcoxon test-------------------------------------
PT = pairwise.wilcox.test(dataplot_msoilMilo$freq,
                          dataplot_msoilMilo$substrate,
                          p.adjust.method="none")

PT

# Pairwise comparisons using Wilcoxon rank sum test 
# 
# data:  dataplot_msoilMilo$freq and dataplot_msoilMilo$substrate 
# 
# Maize  Milo   Oats  
# Milo 0.4084 -      -     
# Oats 0.0032 0.0297 -     
# Rice 0.1610 0.4347 0.2495



PT = PT$p.value

library(rcompanion)

PT1 = fullPTable(PT)

PT1

library(multcompView)

multcompLetters(PT1,
                compare="<",
                threshold=0.05,
                Letters=letters,
                reversed=T) #reversed=T give the largest value a
# 
# Maize  Milo  Oats  Rice 
# "b"   "b"   "a"  "ab" 

#Dun test multiple comparison
PTdunn=dunnTest(freq~substrate, data=dataplot_msoilMilo, method="bh")
PTdunn
#
# Dunn (1964) Kruskal-Wallis multiple comparison
# p-values adjusted with the Benjamini-Hochberg method.
# 
# Comparison          Z     P.unadj      P.adj
# 1 Maize - Milo  0.7714356 0.440448793 0.44044879
# 2 Maize - Oats  2.8739037 0.004054326 0.02432596
# 3  Milo - Oats  2.1024681 0.035512293 0.10653688
# 4 Maize - Rice  1.5661367 0.117316623 0.23463325
# 5  Milo - Rice  0.7947011 0.426787394 0.51214487
# 6  Oats - Rice -1.3077670 0.190952356 0.28642853

PTdunn=PTdunn$res
PTdunn
library(rcompanion)

cldList(comparison = PTdunn$Comparison,
        p.value    = PTdunn$P.adj,
        threshold  = 0.05,
        reversed=T)
#  No significant differences.
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#msoilRice---------------------------------------------------------

dataplot_msoilRice<-dataplot_msoil%>%filter(rearing=="Rice grain")

#One way anova test
modelmsoil_Rice<-lm(freq~substrate, data=dataplot_msoilRice)

library(car) 
aovmsoilRice<-anova(modelmsoil_Rice)#produces the sum of squares
aovmsoilRice

# Analysis of Variance Table
# 
# Response: freq
#            Df  Sum Sq Mean Sq F value  Pr(>F)  
# substrate   3   57.27 19.0896  2.2054 0.08965 .
# Residuals 156 1350.32  8.6559                  
# ---
#      Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1        

#---------------ck Anova assumptions for model--------------------------------

#NOrmality  tests

#shapiro-wilk test
shapiro.test(residuals(modelmsoil_Rice)) #failed P<0.05
# 
# Shapiro-Wilk normality test
# 
# data:  residuals(modelmsoil_Rice)
# W = 0.79352, p-value = 9.371e-14

#homogeneity of variance test
library(car)
leveneTest(freq~substrate, data=dataplot_msoilRice) #passed P>0.05

# Levene's Test for Homogeneity of Variance (center = median)
#        Df F value Pr(>F)
# group   3  1.9209 0.1284
# 156               

#since we failed homogeneity need to use Kruskal.test
kruskal.test(freq~substrate, data=dataplot_msoilRice)
# 
# 
# Kruskal-Wallis rank sum test
# 
# data:  freq by substrate
# Kruskal-Wallis chi-squared = 5.6974, df = 3, p-value = 0.1273 #not sig
# 

#Pairwise Mann-Witney U test = Mann-Whitney-Wilcoxon test-------------------------------------
PT = pairwise.wilcox.test(dataplot_msoilRice$freq,
                          dataplot_msoilRice$substrate,
                          p.adjust.method="none")

PT

# Pairwise comparisons using Wilcoxon rank sum test 
# 
# data:  dataplot_msoilRice$freq and dataplot_msoilRice$substrate 
# 
# Maize Milo  Oats 
# Milo 0.507 -     -    
# Oats 0.173 0.557 -    
# Rice 0.348 0.113 0.022

PT = PT$p.value

library(rcompanion)

PT1 = fullPTable(PT)

PT1

library(multcompView)

multcompLetters(PT1,
                compare="<",
                threshold=0.05,
                Letters=letters,
                reversed=T) #reversed=T give the largest value a

# Maize  Milo  Oats  Rice 
# "ab"  "ab"   "b"   "a" 
# > 
#Dun test multiple comparison
PTdunn=dunnTest(freq~substrate, data=dataplot_msoilRice, method="bh")
PTdunn
#
# Dunn (1964) Kruskal-Wallis multiple comparison
# p-values adjusted with the Benjamini-Hochberg method.
# 
# Comparison          Z    P.unadj     P.adj
# 1 Maize - Milo  0.6915728 0.48920565 0.5870468
# 2 Maize - Oats  1.3367149 0.18131571 0.3626314
# 3  Milo - Oats  0.6451421 0.51883509 0.5188351
# 4 Maize - Rice -0.9383885 0.34804477 0.5220672
# 5  Milo - Rice -1.6299613 0.10310967 0.3093290
# 6  Oats - Rice -2.2751035 0.02289972 0.1373983
PTdunn=PTdunn$res
PTdunn
library(rcompanion)

cldList(comparison = PTdunn$Comparison,
        p.value    = PTdunn$P.adj,
        threshold  = 0.05,
        reversed=T)

#  No significant differences.
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#---------- femsoil----------------------------------------
#dataplot subset by female
dataplot_femsoil<-dataplot%>%filter(location_plant == c( "sfemale"))

#---------femsoilBarley------
dataplot_femsoilBarley<-dataplot_femsoil%>%filter(rearing=="Barley grain")

#Summarize(freq~substrate, data=dataplot_femsoilBarley)
#matched excel
#One way anova test
modelfemsoil_barley<-lm(freq~substrate, data=dataplot_femsoilBarley)

library(car) 
aovfemsoilBarley<-anova(modelfemsoil_barley)#produces the sum of squares
aovfemsoilBarley

# Analysis of Variance Table
# 
# Response: freq
#           Df  Sum Sq Mean Sq F value Pr(>F)
# substrate   3   47.77  15.923  1.0064 0.3916
# Residuals 156 2468.17  15.822   

#---------------ck Anova assumptions for model--------------------------------

#NOrmality  tests

#shapiro-wilk test
shapiro.test(residuals(modelfemsoil_barley)) #failed P<0.05

# Shapiro-Wilk normality test
# 
# data:  residuals(modelfemsoil_barley)
# W = 0.87404, p-value = 2.283e-10

#homogeneity of variance test
library(car)
leveneTest(freq~substrate, data=dataplot_femsoilBarley) #passed P>0.05

# Levene's Test for Homogeneity of Variance (center = median)
#        Df F value Pr(>F)
# group   3  1.6562 0.1788

#since we failed homogeneity need to use Kruskal.test
kruskal.test(freq~substrate, data=dataplot_femsoilBarley)

# Kruskal-Wallis rank sum test
# 
# data:  freq by substrate
# Kruskal-Wallis chi-squared = 0.66025, df = 3, p-value = 0.8825 #not sig

# #Pairwise Mann-Witney U test = Mann-Whitney-Wilcoxon test-------------------------------------
PT = pairwise.wilcox.test(dataplot_femsoilBarley$freq,
                          dataplot_femsoilBarley$substrate,
                          p.adjust.method="none")

PT

# Pairwise comparisons using Wilcoxon rank sum test 
# 
# data:  dataplot_femsoilBarley$freq and dataplot_femsoilBarley$substrate 
# 
# Maize Milo Oats
# Milo 0.82  -    -   
# Oats 0.78  0.57 -   
# Rice 0.61  0.48 0.77

PT = PT$p.value

library(rcompanion)

PT1 = fullPTable(PT)

PT1

library(multcompView)

multcompLetters(PT1,
                compare="<",
                threshold=0.05,
                Letters=letters,
                reversed=T) #reversed=T give the largest value a

# Maize  Milo  Oats  Rice 
# "a"   "a"   "a"   "a"

#Dun test multiple comparison
PTdunn=dunnTest(freq~substrate, data=dataplot_femsoilBarley, method="bh")
PTdunn
# #
# Dunn (1964) Kruskal-Wallis multiple comparison
# p-values adjusted with the Benjamini-Hochberg method.
# 
# Comparison          Z   P.unadj     P.adj
# 1 Maize - Milo  0.2399843 0.8103424 0.9724109
# 2 Maize - Oats -0.2838393 0.7765335 1.0000000
# 3  Milo - Oats -0.5238236 0.6004013 1.0000000
# 4 Maize - Rice -0.5213872 0.6020971 1.0000000
# 5  Milo - Rice -0.7613715 0.4464352 1.0000000
# 6  Oats - Rice -0.2375479 0.8122318 0.8122318

PTdunn=PTdunn$res
PTdunn
library(rcompanion)

cldList(comparison = PTdunn$Comparison,
        p.value    = PTdunn$P.adj,
        threshold  = 0.05,
        reversed=T)
# No significant differences
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#femsoilmaize---------------------------------------------------------

dataplot_femsoilMaize<-dataplot_femsoil%>%filter(rearing=="Maize grain")
#Summarize(freq~substrate, data=dataplot_femsoilMaize)
#matches excel

#One way anova test
modelfemsoil_Maize<-lm(freq~substrate, data=dataplot_femsoilMaize)

library(car) 
aovfemsoilMaize<-anova(modelfemsoil_Maize)#produces the sum of squares
aovfemsoilMaize

# Analysis of Variance Table
# 
# Response: freq
# Df Sum Sq Mean Sq F value  Pr(>F)  
# substrate   3  27.87  9.2896  2.2275 0.08716 .
# Residuals 156 650.57  4.1704                  
# ---
#      Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

#---------------ck Anova assumptions for model--------------------------------

#NOrmality  tests

#shapiro-wilk test
shapiro.test(residuals(modelfemsoil_Maize)) #failed P<0.05

# Shapiro-Wilk normality test
# 
# data:  residuals(modelfemsoil_Maize)
# W = 0.84297, p-value = 8.21e-12


#homogeneity of variance test
library(car)
leveneTest(freq~substrate, data=dataplot_femsoilMaize) #passed P>0.05

# Levene's Test for Homogeneity of Variance (center = median)
#        Df F value Pr(>F)
# group   3    1.61 0.1893          
# ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

#since we failed homogeneity need to use Kruskal.test
kruskal.test(freq~substrate, data=dataplot_femsoilMaize)

# 
# Kruskal-Wallis rank sum test
# 
# data:  freq by substrate
# Kruskal-Wallis chi-squared = 6.9081, df = 3, p-value = 0.07489 #P>.05 not sig

#Pairwise Mann-Witney U test = Mann-Whitney-Wilcoxon test-------------------------------------
PT = pairwise.wilcox.test(dataplot_femsoilMaize$freq,
                          dataplot_femsoilMaize$substrate,
                          p.adjust.method="none")

PT

# Pairwise comparisons using Wilcoxon rank sum test 
# 
# data:  dataplot_femsoilMaize$freq and dataplot_femsoilMaize$substrate 
# 
# Maize Milo  Oats 
# Milo 0.790 -     -    
# Oats 0.028 0.077 -    
# Rice 0.976 0.683 0.017

# > 

PT = PT$p.value

library(rcompanion)

PT1 = fullPTable(PT)

PT1

library(multcompView)

multcompLetters(PT1,
                compare="<",
                threshold=0.05,
                Letters=letters,
                reversed=T) #reversed=T give the largest value a

# Maize  Milo  Oats  Rice 
# "b"  "ab"   "a"   "b"


#Dun test multiple comparison
PTdunn=dunnTest(freq~substrate, data=dataplot_femsoilMaize, method="bh")
PTdunn
#
# Dunn (1964) Kruskal-Wallis multiple comparison
# p-values adjusted with the Benjamini-Hochberg method.
# 
# Comparison          Z    P.unadj     P.adj
# 1 Maize - Milo  0.3406682 0.73335338 0.8800241
# 2 Maize - Oats  2.1926644 0.02833157 0.0849947
# 3  Milo - Oats  1.8519962 0.06402637 0.1280527
# 4 Maize - Rice -0.1003423 0.92007260 0.9200726
# 5  Milo - Rice -0.4410105 0.65920542 0.9888081
# 6  Oats - Rice -2.2930067 0.02184762 0.1310857
PTdunn=PTdunn$res
PTdunn
library(rcompanion)

cldList(comparison = PTdunn$Comparison,
        p.value    = PTdunn$P.adj,
        threshold  = 0.05,
        reversed=T)
# No significant differences
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#femsoilMilo---------------------------------------------------------

dataplot_femsoilMilo<-dataplot_femsoil%>%filter(rearing=="Milo grain")

#One way anova test
modelfemsoil_Milo<-lm(freq~substrate, data=dataplot_femsoilMilo)

library(car) 
aovfemsoilMilo<-anova(modelfemsoil_Milo)#produces the sum of squares
aovfemsoilMilo

# Analysis of Variance Table
# 
# Response: freq
#             Df Sum Sq Mean Sq F value  Pr(>F)  
# substrate   3  35.92 11.9729  3.0909 0.02881 *
#      Residuals 156 604.27  3.8736                  
# ---
#      Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# > 
# #---------------ck Anova assumptions for model--------------------------------

#NOrmality  tests

#shapiro-wilk test
shapiro.test(residuals(modelfemsoil_Milo)) #failed P<0.05

# 
# Shapiro-Wilk normality test
# 
# data:  residuals(modelfemsoil_Milo)
# W = 0.92769, p-value = 3.335e-07

#homogeneity of variance test
library(car)
leveneTest(freq~substrate, data=dataplot_femsoilMilo) #passed P>0.05

# Levene's Test for Homogeneity of Variance (center = median)
#        Df F value  Pr(>F)  
# group   3  2.5025 0.06138 .
# 156                  
# ---

#since we failed homogeneity need to use Kruskal.test
kruskal.test(freq~substrate, data=dataplot_femsoilMilo)

# 
# Kruskal-Wallis rank sum test
# 
# data:  freq by substrate
# Kruskal-Wallis chi-squared = 7.0941, df = 3, p-value = 0.06896 # not sig



#Pairwise Mann-Witney U test = Mann-Whitney-Wilcoxon test-------------------------------------
PT = pairwise.wilcox.test(dataplot_femsoilMilo$freq,
                          dataplot_femsoilMilo$substrate,
                          p.adjust.method="none")

PT

# Pairwise comparisons using Wilcoxon rank sum test 
# 
# data:  dataplot_femsoilMilo$freq and dataplot_femsoilMilo$substrate 
# 
# Maize Milo  Oats 
# Milo 0.914 -     -    
# Oats 0.018 0.032 -    
# Rice 0.250 0.336 0.201

PT = PT$p.value

library(rcompanion)

PT1 = fullPTable(PT)

PT1

library(multcompView)

multcompLetters(PT1,
                compare="<",
                threshold=0.05,
                Letters=letters,
                reversed=T) #reversed=T give the largest value a
# Maize  Milo  Oats  Rice 
# "b"   "b"   "a"  "ab" 

#Dun test multiple comparison
PTdunn=dunnTest(freq~substrate, data=dataplot_femsoilMilo, method="bh")
PTdunn
# Dunn (1964) Kruskal-Wallis multiple comparison
# p-values adjusted with the Benjamini-Hochberg method.
# 
# Comparison          Z    P.unadj      P.adj
# 1 Maize - Milo  0.1584387 0.87411109 0.87411109
# 2 Maize - Oats  2.3618426 0.01818436 0.10910617
# 3  Milo - Oats  2.2034038 0.02756630 0.08269889
# 4 Maize - Rice  1.1250379 0.26057299 0.39085949
# 5  Milo - Rice  0.9665991 0.33374447 0.40049337
# 6  Oats - Rice -1.2368047 0.21615959 0.43231918

PTdunn=PTdunn$res
PTdunn
library(rcompanion)

cldList(comparison = PTdunn$Comparison,
        p.value    = PTdunn$P.adj,
        threshold  = 0.05,
        reversed=T)
# No significant differences.

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#femsoilRice---------------------------------------------------------

dataplot_femsoilRice<-dataplot_femsoil%>%filter(rearing=="Rice grain")

#One way anova test
modelfemsoil_Rice<-lm(freq~substrate, data=dataplot_femsoilRice)

library(car) 
aovfemsoilRice<-anova(modelfemsoil_Rice)#produces the sum of squares
aovfemsoilRice

# Analysis of Variance Table
# 
# Response: freq
#           Df  Sum Sq Mean Sq F value Pr(>F)
# substrate   3   25.32  8.4417   1.217 0.3055
# Residuals 156 1082.05  6.9362  

#---------------ck Anova assumptions for model--------------------------------

#NOrmality  tests

#shapiro-wilk test
shapiro.test(residuals(modelfemsoil_Rice)) #failed P<0.05

# Shapiro-Wilk normality test
# 
# data:  residuals(modelfemsoil_Rice)
# W = 0.82035, p-value = 9.549e-13


#homogeneity of variance test
library(car)
leveneTest(freq~substrate, data=dataplot_femsoilRice) #passed P>0.05

# Levene's Test for Homogeneity of Variance (center = median)
#        Df F value Pr(>F)
# group   3  1.7393 0.1612
# ---  

#since we failed homogeneity need to use Kruskal.test
kruskal.test(freq~substrate, data=dataplot_femsoilRice)

# Kruskal-Wallis rank sum test
# 
# data:  freq by substrate
# Kruskal-Wallis chi-squared = 1.2217, df = 3, p-value = 0.7478 # not sig



#Pairwise Mann-Witney U test = Mann-Whitney-Wilcoxon test-------------------------------------
PT = pairwise.wilcox.test(dataplot_femsoilRice$freq,
                          dataplot_femsoilRice$substrate,
                          p.adjust.method="none")

PT

# Pairwise comparisons using Wilcoxon rank sum test 
# 
# data:  dataplot_femsoilRice$freq and dataplot_femsoilRice$substrate 
# 
# Maize Milo Oats
# Milo 0.68  -    -   
# Oats 0.32  0.47 -   
# Rice 0.91  0.95 0.39


PT = PT$p.value

library(rcompanion)

PT1 = fullPTable(PT)

PT1

library(multcompView)

multcompLetters(PT1,
                compare="<",
                threshold=0.05,
                Letters=letters,
                reversed=T) #reversed=T give the largest value a
# $Letters
# Maize  Milo  Oats  Rice 
# "a"   "a"   "a"   "a" 

#Dun test multiple comparison
PTdunn=dunnTest(freq~substrate, data=dataplot_femsoilRice, method="bh")
PTdunn
#
# Dunn (1964) Kruskal-Wallis multiple comparison
# p-values adjusted with the Benjamini-Hochberg method.
# 
# Comparison          Z   P.unadj     P.adj
# 1 Maize - Milo  0.3285996 0.7424583 1.0000000
# 2 Maize - Oats  1.0311652 0.3024634 1.0000000
# 3  Milo - Oats  0.7025656 0.4823265 0.9646530
# 4 Maize - Rice  0.1802393 0.8569647 1.0000000
# 5  Milo - Rice -0.1483603 0.8820584 0.8820584
# 6  Oats - Rice -0.8509259 0.3948105 1.0000000
PTdunn=PTdunn$res
PTdunn
library(rcompanion)

cldList(comparison = PTdunn$Comparison,
        p.value    = PTdunn$P.adj,
        threshold  = 0.05,
        reversed=T)
# 
# > No significant differences.
###################################bivariate visualiz###########################
# Ref: https://community.rstudio.com/t/bivariate-data-visualization-advice/10829

# Improved ?? Maybe??  NO
ggplot(dataplot_femleaf, aes(x=substrate,y=freq, colour = substrate)) +
     geom_jitter() +
     stat_ellipse(size = 1, alpha = 0.6) +
     facet_wrap(.~rearing, nrow = 4, scales = "free")

ggplot(databysex.df, aes(x=substrate,y=freq, colour = sex)) +
     geom_jitter() +
     stat_ellipse(size = 1, alpha = 0.6) +
     facet_wrap(.~rearing, nrow = 4, scales = "free")
