#### Anova analysis by R ####
####
####


#### define working directory ####
setwd("/Users/jumaoyuan/Downloads/Guam_Project2")
getwd()
workdir = getwd()
print ('your working directory is: ')
print (workdir)

#######################################
######## 6-20-13 data analysis ########
#######################################
### we don't care about "pair" and "reps", we only focus on blocks, geography and origin (split-plot mixed model)
### two dependent variables y = height or diameter
### 

require(gdata)
sec1 = read.xls('LSU ironwood provenance trial data 2.xlsx', sheet="6-20-13", header=TRUE, skip=2)
names(sec1) = c("treeNumber", "blocks", "geo_char", "geo_num", "pair", "reps", "origin", "treeCondition", "height", "diameter")
names(sec1)
head(sec1)
sec1=sec1[,1:10]
sec1 <- na.omit(sec1) #remove NaN 
write.csv(sec1, file = "data1.csv") #use for SAS
dim(sec1)
sec1$blocks = factor(sec1$blocks)

### anova analysis ####
attach(sec1)
a1 <- aov(height ~ geo_char + factor(blocks) )
summary(a1)
TukeyHSD(a1, 'geo_char', conf.level = .95)
library(agricolae)
out <- HSD.test(a1, 'geo_char')
out$means
out$groups
detach(sec1)

attach(sec2)
a2 <- aov(height ~ geo_char + factor(blocks) )
summary(a2)
TukeyHSD(a2, 'geo_char', conf.level = .95)
library(agricolae)
out <- HSD.test(a2, 'geo_char')
out$means
out$groups

detach(sec2)
attach(sec3)
a3 <- aov(height ~ geo_char + factor(blocks) )
summary(a3)
TukeyHSD(a3, 'geo_char', conf.level = .95)
library(agricolae)
out <- HSD.test(a3, 'geo_char')
out$means
out$groups

detach(sec3)
attach(sec4)
a4 <- aov(height ~ geo_char + factor(blocks) )
summary(a4)
TukeyHSD(a4, 'geo_char', conf.level = .95)
library(agricolae)
out <- HSD.test(a4, 'geo_char')
out$means
out$groups
detach(sec4)


