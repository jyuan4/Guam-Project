#### Split-plot mixed model by R ####
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
sec1$origin = factor(sec1$origin)

lmeFit1 <- lme(height ~ geo + origin + geo:origin, data=sec1, random = ~1 | geo/blocks)
anova(lmeFit1)
summary(lmeFit1)

lmeFit1 <- lme(height ~ geo*origin, data=height_dat, random = ~1 | blocks/geo)
anova(lmeFit1)
summary(lmeFit1)

## interaction plot
interaction.plot(sec1$geo, sec1$origin, sec1$diameter, type = "b", col = 'blue', 
                 xlab = "geography", ylab = "height", main = "interaction plot", legend=TRUE)

## interaction plot [2] for height
matplot(tapply(sec1$height,list(sec1$geo_char,sec1$blocks),mean,na.rm=TRUE),type='b',xaxt='n',
        main='Interaction Plot',ylab="Average height",xlab="Location")
nn <- ncol(sec1)
legend("topright", c("Block1","Block2","Block3"),col=seq_len(nn),cex=0.8,fill=seq_len(nn))
axis(1, at=1:10, labels=row.names(tapply(sec1$height,list(sec1$geo_char,sec1$blocks),mean,na.rm=TRUE)))

## interaction plot [2] for diameter
matplot(tapply(sec1$diameter,list(sec1$geo_char,sec1$blocks),mean,na.rm=TRUE),type='b',xaxt='n',
        main='Interaction Plot',ylab="Average diameter",xlab="Location")
nn <- ncol(sec1)
legend("topright", c("Block1","Block2","Block3"),col=seq_len(nn),cex=0.8,fill=seq_len(nn))
axis(1, at=1:10, labels=row.names(tapply(sec1$diameter,list(sec1$geo_char,sec1$blocks),mean,na.rm=TRUE)))


#######################################
######## 9-9-13 data analysis ########
#######################################
sec2 = read.xls('LSU ironwood provenance trial data 2.xlsx', sheet="9-9-13", header=TRUE, skip=1)
sec2 = sec2[,1:length(names(sec2))-1]
names(sec2) = c("treeNumber", "blocks", "geo_char", "geo_num", "pair", "rep", "origin", "stand thinned", 
                "storm damage", "height", "diameter", "tree_volume", "wind_damage_severity", "root_damage_severity")
write.csv(sec2, file = "data2.csv")


height_dat <- cbind(sec2[1:6], sec2$Height.m.)
dim(height_dat)
names(height_dat) <- c("treeNumber", "blocks", "geo_char", "geo_num", "pair", "rep", "origin", "height")
head(height_dat)
height_dat <- na.omit(height_dat)
height_dat$blocks = factor(height_dat$blocks)
height_dat$geo = factor(height_dat$geo)

library(lme4)
fit.lmer1 = lmer(height ~ geo * origin + (1 | blocks/geo), data=height_dat)
anova(fit.lmer1)

lmeFit1 <- lme(height ~ geo*origin, data=height_dat, random = ~1 | blocks/geo)
anova(lmeFit1)
summary(lmeFit1)


interaction.plot(factor(height_dat$geo), height_dat$origin, height_dat$height, type = "b", col = 'blue', 
                 xlab = "geography", ylab = "height", main = "interaction plot", legend=TRUE)

## interaction plot [2] for height
matplot(tapply(sec2$height,list(sec2$geo_char,sec2$blocks),mean,na.rm=TRUE),type='b',xaxt='n',
        main='Interaction Plot',ylab="Average height",xlab="Location")
nn <- ncol(sec2)
legend("topright", c("Block1","Block2","Block3"),col=seq_len(nn),cex=0.8,fill=seq_len(nn))
axis(1, at=1:10, labels=row.names(tapply(sec2$height,list(sec2$geo_char,sec2$blocks),mean,na.rm=TRUE)))

## interaction plot [2] for diameter
matplot(tapply(sec2$diameter,list(sec2$geo_char,sec2$blocks),mean,na.rm=TRUE),type='b',xaxt='n',
        main='Interaction Plot',ylab="Average diameter",xlab="Location")
nn <- ncol(sec2)
legend("topright", c("Block1","Block2","Block3"),col=seq_len(nn),cex=0.8,fill=seq_len(nn))
axis(1, at=1:10, labels=row.names(tapply(sec2$diameter,list(sec2$geo_char,sec2$blocks),mean,na.rm=TRUE)))

## interaction plot [2] for tree volume
matplot(tapply(sec2$tree_volume,list(sec2$geo_char,sec2$blocks),mean,na.rm=TRUE),type='b',xaxt='n',
        main='Interaction Plot',ylab="Average tree volume",xlab="Location")
nn <- ncol(sec2)
legend("topright", c("Block1","Block2","Block3"),col=seq_len(nn),cex=0.8,fill=seq_len(nn))
axis(1, at=1:10, labels=row.names(tapply(sec2$tree_volume,list(sec2$geo_char,sec2$blocks),mean,na.rm=TRUE)))

## interaction plot [2] for wind damage severity
matplot(tapply(sec2$wind_damage_severity,list(sec2$geo_char,sec2$blocks),mean,na.rm=TRUE),type='b',xaxt='n',
        main='Interaction Plot',ylab="Average wind damage severity",xlab="Location")
nn <- ncol(sec2)
legend("topright", c("Block1","Block2","Block3"),col=seq_len(nn),cex=0.8,fill=seq_len(nn))
axis(1, at=1:10, labels=row.names(tapply(sec2$wind_damage_severity,list(sec2$geo_char,sec2$blocks),mean,na.rm=TRUE)))

## interaction plot [2] for root damage severity
matplot(tapply(sec2$root_damage_severity,list(sec2$geo_char,sec2$blocks),mean,na.rm=TRUE),type='b',xaxt='n',
        main='Interaction Plot',ylab="Average root damage severity",xlab="Location")
nn <- ncol(sec2)
legend("topright", c("Block1","Block2","Block3"),col=seq_len(nn),cex=0.8,fill=seq_len(nn))
axis(1, at=1:10, labels=row.names(tapply(sec2$root_damage_severity,list(sec2$geo_char,sec2$blocks),mean,na.rm=TRUE)))


#######################################
######## 12-2014 data analysis ########
#######################################
sec3 = read.xls('LSU ironwood provenance trial data 2.xlsx', sheet="12-2014", header=TRUE, skip=2)
sec3 = sec3[,1:length(names(sec3))-1]
names(sec3) = c("treeNumber", "blocks", "geo_char", "geo_num", "pair", "rep", "origin", 
                "stand thinned",  "height.m", "diameter.mm")
write.csv(sec3, file = "data3.csv")


## interaction plot [2] for height
matplot(tapply(sec3$height.m,list(sec3$geo_char,sec3$blocks),mean,na.rm=TRUE),type='b',xaxt='n',
        main='Interaction Plot',ylab="Average height",xlab="Location")
nn <- ncol(sec3)
legend("topright", c("Block1","Block2","Block3"),col=seq_len(nn),cex=0.8,fill=seq_len(nn))
axis(1, at=1:10, labels=row.names(tapply(sec3$height.m,list(sec3$geo_char,sec3$blocks),mean,na.rm=TRUE)))

## interaction plot [2] for diameter
matplot(tapply(sec3$diameter.mm,list(sec3$geo_char,sec3$blocks),mean,na.rm=TRUE),type='b',xaxt='n',
        main='Interaction Plot',ylab="Average diameter",xlab="Location")
nn <- ncol(sec3)
legend("topright", c("Block1","Block2","Block3"),col=seq_len(nn),cex=0.8,fill=seq_len(nn))
axis(1, at=1:10, labels=row.names(tapply(sec3$diameter.mm,list(sec3$geo_char,sec3$blocks),mean,na.rm=TRUE)))


#######################################
######## Typhoon2015 data analysis ####
#######################################
sec4 = read.xls('LSU ironwood provenance trial data 2.xlsx', sheet=4, header=TRUE, skip=1)
sec4 = sec4[,1:length(names(sec4))-1]
names(sec4) <- c("treeNumber", "blocks", "geo_char", "geo_num", "pair", "rep", "origin", 
                 "typhoon",  "stem_axis", "stem_staightness", "tree_volume", "internodes_lower", 
                 "internodes_middle", "internodes_top", "branch_max", "branch_length", "branch_density", "branch_max2",
                 "branch_thickness", "branch_angle", "branch_habit", "branchlet_habit", "branchlet_length",
                 "cones", "flowers", "root_damage", "stem_damage", "branch_damage")
write.csv(sec4, file = "data4.csv")

## interaction plot [2] for tree volume
matplot(tapply(sec4$tree_volume,list(sec4$geo_char,sec4$blocks),mean,na.rm=TRUE),type='b',xaxt='n',
        main='Interaction Plot',ylab="Average tree volume",xlab="Location")
nn <- ncol(sec4)
legend("topright", c("Block1","Block2","Block3"),col=seq_len(nn),cex=0.8,fill=seq_len(nn))
axis(1, at=1:11, labels=row.names(tapply(sec4$tree_volume,list(sec4$geo_char,sec4$blocks),mean,na.rm=TRUE)))

## interaction plot [2] for stem straightness
matplot(tapply(sec4$stem_staightness,list(sec4$geo_char,sec4$blocks),mean,na.rm=TRUE),type='b',xaxt='n',
        main='Interaction Plot',ylab="Average stem straightness",xlab="Location")
nn <- ncol(sec4)
legend("topright", c("Block1","Block2","Block3"),col=seq_len(nn),cex=0.8,fill=seq_len(nn))
axis(1, at=1:11, labels=row.names(tapply(sec4$stem_staightness,list(sec4$geo_char,sec4$blocks),mean,na.rm=TRUE)))


## interaction plot [2] for root damage
matplot(tapply(sec4$root_damage,list(sec4$geo_char,sec4$blocks),mean,na.rm=TRUE),type='b',xaxt='n',
        main='Interaction Plot',ylab="Average root damage",xlab="Location")
nn <- ncol(sec4)
legend("topright", c("Block1","Block2","Block3"),col=seq_len(nn),cex=0.8,fill=seq_len(nn))
axis(1, at=1:11, labels=row.names(tapply(sec4$root_damage,list(sec4$geo_char,sec4$blocks),mean,na.rm=TRUE)))

## interaction plot [2] for branch density 
matplot(tapply(sec4$branch_density,list(sec4$geo_char,sec4$blocks),mean,na.rm=TRUE),type='b',xaxt='n',
        main='Interaction Plot',ylab="Average branch density",xlab="Location")
nn <- ncol(sec4)
legend("topright", c("Block1","Block2","Block3"),col=seq_len(nn),cex=0.8,fill=seq_len(nn))
axis(1, at=1:11, labels=row.names(tapply(sec4$branch_density,list(sec4$geo_char,sec4$blocks),mean,na.rm=TRUE)))


