### Description ####
# data analysis for Guam project in four sections
#
#


#### define working directory ####
setwd("/Users/jumaoyuan/Downloads/Guam_Project2")
getwd()
workdir = getwd()
print ('your working directory is: ')
print (workdir)


#### read excel.xlsx data file  #####
## sec1: 6-20-13, original data without any damages
## sec2: 9-9-13, first thinning
## sec3: 12-2014, second thinning
## sec4: Typhoon2015, after typhoon damage
## sec0: full data set (raw data)

require(gdata)
sec1 = read.xls('LSU ironwood provenance trial data.xlsx', sheet="6-20-13", header=TRUE)
sec2 = read.xls('LSU ironwood provenance trial data.xlsx', sheet="9-9-13", header=TRUE)
sec3 = read.xls('LSU ironwood provenance trial data.xlsx', sheet="12-2014", header=TRUE)
sec4 = read.xls('LSU ironwood provenance trial data.xlsx', sheet="Typhoon2015", header=TRUE)
sec0 = read.xls('LSU ironwood provenance trial data.xlsx', sheet="full_data", header=TRUE)


#######################################
######## 6-20-13 data analysis ########
#######################################
sec1 = read.xls('LSU ironwood provenance trial data.xlsx', sheet="6-20-13", header=TRUE, skip=2)
names(sec1) = c("treeNumber", "blocks", "geo", "pair", "reps", "origin", "treeCondition", "height", "diameter")
names(sec1)
head(sec1)
sec1=sec1[,1:9]
dim(sec1)

## [1]: relationship between height and diameter for all trees without classification ##
plot(na.omit(sec1$height), na.omit(sec1$diameter), col="blue", lwd=1, main = "Height(m) versus diameter(mm)", 
     xlab = "Height(m)", ylab = "Diameter(mm)")

### [2]: look at block#1 ####
block1dat = sec1[which(sec1$blocks==1),]
dim(block1dat)

#### [2.1] classification: geography ####
block1dat$geo[which(block1dat$geo=="1(Malaysia)")] = 1
block1dat$geo[which(block1dat$geo=="2(Kenya)")] = 2
block1dat$geo[which(block1dat$geo=="3(India)")] = 3
block1dat$geo[which(block1dat$geo=="4(Thailand)")] = 4
block1dat$geo[which(block1dat$geo=="5(Guam)")] = 5
block1dat$geo[which(block1dat$geo=="6(Papua New Guinea)")] = 6
block1dat$geo[which(block1dat$geo=="7(China)")] = 7
block1dat$geo[which(block1dat$geo=="7(Vietnam)")] = 7
block1dat$geo[which(block1dat$geo=="8(Australia)")] = 8
block1dat$geo[which(block1dat$geo=="9(Solomon Islands)")] = 9
block1dat$geo[which(block1dat$geo=="9(Vanuatu)")] = 9
block1dat$geo[which(block1dat$geo=="10(China)")] = 10

block1dat$geo = factor(block1dat$geo)
nlevels(block1dat$geo)

library(lattice)
df <- data.frame(block1dat)
xyplot(diameter~height, groups = geo, data=df, auto.key = list(corner=c(0,.98)), cex=1.5, 
       main = "Classification: geography")

### [2.2]: classification: origin ###
xyplot(diameter~height, groups = origin, data=df, auto.key = list(corner=c(0,.98)), cex=1.5, 
       main="Classification: Origin")


### [3]: look at block#2 ###
block2dat = sec1[which(sec1$blocks==2),]
dim(block2dat)

block2dat$geo[which(block2dat$geo=="1(Solomon Islands)")] = 9
block2dat$geo[which(block2dat$geo=="1(Vanuatu)")] = 9
block2dat$geo[which(block2dat$geo=="2(China)")] = 10
block2dat$geo[which(block2dat$geo=="3(India)")] = 3
block2dat$geo[which(block2dat$geo=="4(Thailand)")] = 4
block2dat$geo[which(block2dat$geo=="5(Papua New Guinea)")] = 6
block2dat$geo[which(block2dat$geo=="6(Kenya)")] = 2
block2dat$geo[which(block2dat$geo=="7(China)")] = 7
block2dat$geo[which(block2dat$geo=="8(Malaysia)")] = 1
block2dat$geo[which(block2dat$geo=="7(Vietnam)")] = 7
block2dat$geo[which(block2dat$geo=="9(Australia)")] = 8
block2dat$geo[which(block2dat$geo=="10(Guam)")] = 5

block2dat$geo = factor(block2dat$geo)
nlevels(block2dat$geo)

library(lattice)
df <- data.frame(block2dat)
xyplot(diameter~height, groups = geo, data=df, auto.key = list(corner=c(0,.98)), cex=1.5, 
       main = "Classification: geography")
xyplot(diameter~height, groups = origin, data=df, auto.key = list(corner=c(0,.98)), cex=1.5, 
       main="Classification: Origin")


### [4]: look at block#3 ###
block3dat = sec1[which(sec1$blocks==3),]
dim(block3dat)

block3dat$geo[which(block3dat$geo=="1(Malaysia)")] = 1
block3dat$geo[which(block3dat$geo=="2(Solomon Islands)")] = 2
block3dat$geo[which(block3dat$geo=="2(Vanuatu)")] = 2
block3dat$geo[which(block3dat$geo=="3(Guam)")] = 3
block3dat$geo[which(block3dat$geo=="4(Australia)")] = 4
block3dat$geo[which(block3dat$geo=="10(Papua New Guinea)")] = 10
block3dat$geo[which(block3dat$geo=="7(Kenya)")] = 7
block3dat$geo[which(block3dat$geo=="8(China)")] = 8
block3dat$geo[which(block3dat$geo=="9(Thailand)")] = 9
block3dat$geo[which(block3dat$geo=="8(Vietnam)")] = 8
block3dat$geo[which(block3dat$geo=="5(India)")] = 5
block3dat$geo[which(block3dat$geo=="6(China)")] = 6

block3dat$geo = factor(block3dat$geo)
nlevels(block3dat$geo)

library(lattice)
df <- data.frame(block3dat)
xyplot(diameter~height, groups = geo, data=df, auto.key = list(corner=c(0,.98)), cex=1.5, 
       main = "Classification: geography")
xyplot(diameter~height, groups = origin, data=df, auto.key = list(corner=c(0,.98)), cex=1.5, 
       main="Classification: Origin")


#######################################
######## 9-9-13 data analysis ########
#######################################
sec2 = read.xls('LSU ironwood provenance trial data.xlsx', sheet="9-9-13", header=TRUE, skip=1)
sec2 = sec2[,1:8]
height = na.omit(sec2$Height.m.)
diameter = na.omit(sec2$Diameter.at.breast.height.mm.)
plot(height, diameter, col="red", lwd=1, main = "Height(m) versus diameter(mm)", 
     xlab = "Height(m)", ylab = "Diameter(mm)")
par(new=TRUE)
plot(na.omit(sec1$height), axes=FALSE, na.omit(sec1$diameter), col="blue", lwd=1, main = "Height(m) versus diameter(mm)", 
     xlab = "Height(m)", ylab = "Diameter(mm)")
legend(1,150,c("6-20-13","9-9-13"),lty=c(1,1),lwd=c(2.5,2.5),col=c("blue","red"))

windDamage = sec2$wind.damage.severity
rootDamage = sec2$rood.damage.severity.
treeVolume = sec2$Tree.volume

library(scatterplot3d)
library(rgl)
scatterplot3d(x = sec2$Height.m., y = sec2$Diameter.at.breast.height.mm., z = sec2$Tree.volume, 
              color = "blue", main = "3D Scatter Plot", xlab = "height(m)", ylab = "diameter(mm)", zlab = "volume")

plot3d(x = sec2$Height.m., y = sec2$Diameter.at.breast.height.mm., z = sec2$Tree.volume, 
       col = "red", main = "3D Scatter Plot", xlab = "height(m)", ylab = "diameter(mm)", zlab = "volume")

#### classification #####
a <- xyplot(sec2$Diameter.at.breast.height.mm.~sec2$Height.m., groups = sec2$wind.damage.severity, data=df, auto.key = list(corner=c(0,.98)), cex=1.5, 
       main = "Classification: wind damage", xlab = "height(m)", ylab = "diameter(mm)")
b <- xyplot(sec2$Diameter.at.breast.height.mm.~sec2$Height.m., groups = sec2$rood.damage.severity., data=df, auto.key = list(corner=c(0,.98)), cex=1.5, 
       main = "Classification: root damage", xlab = "height(m)", ylab = "diameter(mm)")
a
b

#######################################
######## 12-2014 data analysis ########
#######################################
sec3 = read.xls('LSU ironwood provenance trial data.xlsx', sheet="12-2014", header=TRUE, skip=2)
names(sec3) = c("treeNumber", "standThinned", "height.m", "diameter.mm")
sec3 = sec3[,1:4]
dim(sec3)

plot(na.omit(sec3$height.m), na.omit(sec3$diameter.mm), col="green", lwd=1, main = "Height(m) versus diameter(mm)", 
     xlab = "Height(m)", ylab = "Diameter(mm)")
par(new=TRUE)
height = na.omit(sec2$Height.m.)
diameter = na.omit(sec2$Diameter.at.breast.height.mm.)
plot(height, diameter, axes=FALSE, col="red", lwd=1, main = "Height(m) versus diameter(mm)", 
     xlab = "Height(m)", ylab = "Diameter(mm)")
par(new=TRUE)
plot(na.omit(sec1$height), axes=FALSE, na.omit(sec1$diameter), col="blue", lwd=1, main = "Height(m) versus diameter(mm)", 
     xlab = "Height(m)", ylab = "Diameter(mm)")
legend(1,150,c("6-20-13","9-9-13","12-2014"),lty=c(1,1),lwd=c(2.5,2.5),col=c("blue","red","green"))


#######################################
######## Typhoon2015 data analysis ####
#######################################
sec4 = read.xls('LSU ironwood provenance trial data.xlsx', sheet=4, header=TRUE, skip=1)
names(sec4)
sec4 = sec4[,1:22]