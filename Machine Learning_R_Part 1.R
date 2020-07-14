#make sure you setwd("...") to the proper directory 
require(foreign)

#iris
data(iris) #this one is built in
pairs(iris[,-5])
x1<-tapply(iris[,2],iris[,5]=="setosa",mean)
y1<-tapply(iris[,3],iris[,5]=="setosa",mean)
plot(iris[,2:3])
points(x1,y1,pch=16,cex=2,col=2)
pairs(iris[,-5],col=(2:4)[iris[,5]])
iris.pc <- princomp(iris[,-5])
print(iris.pc$loadings)
summary(iris.pc)
pairs(iris.pc$scores)

#crabs
require(mclust)
crabs <- read.dta("crabs.dta")
pairs(princomp(crabs[,-(1:3)])$scores)
pc.crabs <- princomp(crabs[,-(1:3)])$scores
plot(pc.crabs[,2:3],col=1)
#plot(pc.crabs[,2:3],col=1+(crabs[,1]=="Blue")*2+1*(crabs[,2]=="Male"))
mcl <- Mclust(pc.crabs)
plot(mcl)

#try stdzd:
pairs(princomp(crabs[,-(1:3)],cor=T)$scores)
pc.crabs <- princomp(crabs[,-(1:3)],cor=T)$scores
plot(pc.crabs[,2:3],col=1)
mcl <- Mclust(pc.crabs)
plot(mcl,dimens=c(2,3))

#simNagin
require(mclust)
mm <- read.dta("simNagin.dta")
matplot(t(mm[,13:22]),type='l',col=1,pch=1,xlab='Time',ylab='Outcome')
mcl <- Mclust(mm[,13:22])
plot(mcl,dimens=1:5)
matplot(t(mm[,13:22]),type='l',col=(c(4,2,3))[mcl$class],pch=1,xlab='Time',ylab='Outcome')

#################################################
#in class exercise: visualize PCA loadings for simNagin;
require(lattice);require(reshape2)
ldgs.nagin <- t(princomp(mm[,13:22])$loadings[])
ldgs.nagin[1,] <- -ldgs.nagin[1,] #flip the sign
pcaNagin <- as.data.frame(ldgs.nagin)
nids <- dim(pcaNagin)[[1]]
pcaNagin$component <- factor(1:nids,levels=nids:1,labels=paste("component",nids:1,sep=":"))
longPCANagin <- melt(pcaNagin,id.vars="component")
#SLIGHTLY COMPLICATED:
barchart(value~variable|component,panel=function(x,...) {panel.barchart(x,...);panel.abline(h=0,col=8)},data=longPCANagin,layout = c(1,10))
#SIMPLER:
#barchart(value~variable|component,data=longPCANagin,layout = c(1,10))
#ALTERNATIVE:
dotplot(value~variable|component,panel=function(x,...) {panel.dotplot(x,...);panel.abline(h=0,col=8)},data=longPCANagin,layout = c(1,10))
#ALTERNATIVE-2:
dotplot(value~variable|component,panel=function(x,...) {panel.dotplot(x,...);panel.abline(h=0,col="darkgrey")},data=longPCANagin,layout = c(1,10))

#visualize  - pairs plot - raw data
pairs(mm[,13:22],cex=.2)
#load the 'scores' version of the file
scoresNagin <- pcaNagin <- as.data.frame(princomp(mm[,13:22])$scores)
pairs(scoresNagin,cex=.2)
plot(scoresNagin[,1:2],cex=1.4)
#################################################


#mds:
#eurodist
eurodist <- read.dta("eurodist.dta")
mds <- cmdscale(eurodist)
plot(mds,type='n',xlim=c(-2100,2300),xlab='Dimension 1',ylab='Dimension 2')
text(mds,dimnames(eurodist)[[2]],cex=1)

#crabs
crabs <- read.csv("crabs.csv",header=T)
pc.crabs <- princomp(crabs[,-(1:3)])$scores
plot(pc.crabs[,1],pc.crabs[,2],col=1,pch=c(3,1)[crabs$sex],cex=1.2,lwd=1.5)
mds <- cmdscale(dist(crabs[,-(1:3)]),eig=F)
plot(-mds[,1],-mds[,2],col=1,pch=c(3,1)[crabs$sex],cex=1.2,lwd=1.5)

#repub:
require(cluster)
data(votes.repub)

#NJ,NM,NY,NC comparisons
round(votes.repub[30:33,29:31],1)
round(dist(votes.repub[30:33,29:31],method='manhattan'),1)
round(dist(votes.repub[30:33,],method='manhattan'),1)

#prep for MDS:
lbls0 <- dimnames(votes.repub)[[1]]
lbls <- c("AL","AK","AZ","AR","CA","CO","CT","DE","FL","GA","HI","ID","IL","IN","IA","KS","KY","LA","ME","MD","MA","MI","MN","MS","MO","MT","NE","NV","NH","NJ","NM","NY","NC","ND","OH","OK","OR","PA","RI","SC","SD","TN","TX","UT","VT","VA","WA","WV","WI","WY")
#First MDS:
mdist.votes <- dist(votes.repub,method='manhattan')
mds1 <- cmdscale(mdist.votes)
len <- length(lbls)
plot(mds1,type='n',xlab='MDS Dimension 1',ylab='MDS Dimension 2')
for (i in 1:len) text(mds1[i,1],mds1[i,2],lbls[i],cex=1,col=rainbow(len)[i]) #colors are pretty basic

#red state blue state improvement:
blueStates <- c("MA","RI","NY","HI","VT","MD","IL","CT","CA","DE","ME","NJ","WA","MI","MN","OR","PA","NM","WI","IA","NH")
purpleStates <- c("NV","WV","AR","CO","FL","MO","OH")
b.blue <- !is.na(match(lbls,blueStates))
b.purp <- !is.na(match(lbls,purpleStates))
col.state <- 1+1*b.blue+2*b.purp

#full dataset
plot(mds1,type='n',xlab='MDS Dimension 1',ylab='MDS Dimension 2')
for (i in 1:len) text(mds1[i,1],mds1[i,2],lbls[i],cex=1,col=c("red","blue","purple")[col.state[i]])

######################################################################
#repeat code:
require(cluster)
data(votes.repub)
lbls <- c("AL","AK","AZ","AR","CA","CO","CT","DE","FL","GA","HI","ID","IL","IN","IA","KS","KY","LA","ME","MD","MA","MI","MN","MS","MO","MT","NE","NV","NH","NJ","NM","NY","NC","ND","OH","OK","OR","PA","RI","SC","SD","TN","TX","UT","VT","VA","WA","WV","WI","WY")
len <- length(lbls)
#red state blue state improvement:
blueStates <- c("MA","RI","NY","HI","VT","MD","IL","CT","CA","DE","ME","NJ","WA","MI","MN","OR","PA","NM","WI","IA","NH")
purpleStates <- c("NV","WV","AR","CO","FL","MO","OH")
b.blue <- !is.na(match(lbls,blueStates))
b.purp <- !is.na(match(lbls,purpleStates))
col.state <- 1+1*b.blue+2*b.purp
####done repeat
par(mfrow=c(1,2)) # could look at two diff dimens comparisons on same page
#just recent years
mds2 <- cmdscale(dist(votes.repub[,27:31],method='manhattan'))
plot(mds2,type='n',xlab='MDS Dimension 1',ylab='MDS Dimension 2')
for (i in 1:len) text(mds2[i,1],mds2[i,2],lbls[i],cex=1,col=c("red","blue","purple")[col.state[i]])
#dimensions 2 & 3 (not in notes)
mds3 <- cmdscale(dist(votes.repub[,27:31],method='manhattan'),k=3)
plot(mds3[,c(2,3)],type='n',xlab='MDS Dimension 2',ylab='MDS Dimension 3')
for (i in 1:len) text(mds3[i,2],mds3[i,3],lbls[i],cex=1,col=c("red","blue","purple")[col.state[i]])
#try higher dimension
mds4 <- cmdscale(dist(votes.repub[,27:31],method='manhattan'),k=4)
pairs(mds4)
#######################################################################

par(mfrow=c(1,1))
#compare to PC solns (cols 1 & 2)
pc2 <- princomp(votes.repub[,27:31])$scores
plot(pc2[,1:2],type='n',xlab='PC 1',ylab='PC 2')
for (i in 1:len) text(pc2[i,1],pc2[i,2],lbls[i],cex=1,col=c("red","blue","purple")[col.state[i]])
#not so  good.  look at pairs plots.
pairs(pc2,col=c("red","blue","purple")[col.state])
#PCs 2&3 look better:
plot(pc2[,2:3],type='n',xlab='PC 2',ylab='PC 3')
for (i in 1:len) text(pc2[i,2],pc2[i,3],lbls[i],cex=1,col=c("red","blue","purple")[col.state[i]])

##################################################
###########DISTANCE METRIC, under hood:
head(votes.repub)
votes.repub[2,]-votes.repub[1,]
sum(abs(votes.repub[2,]-votes.repub[1,]),na.rm=T)
as.matrix(mdist.votes)[1:2,1:2]
sum(is.na(votes.repub[2,]))
sum(!is.na(votes.repub[2,]))
sum(abs(votes.repub[2,]-votes.repub[1,]),na.rm=T)*31/5
#############################################
##MDS:
mds.iris <- cmdscale(dist(iris[,-5]),k=3)
pairs(mds.iris,col=iris$Species)
############################################

#classification:
data(iris)
iris$virginica <- iris$Species=='virginica'
iris$setosa <- iris$Species=='setosa'
fit <- glm(setosa~Petal.Length+Petal.Width,data=iris,family=binomial)
summary(fit)
require(arm)
fit <- bayesglm(setosa~Petal.Length+Petal.Width,data=iris,family=binomial)
summary(fit)

#maleLifeExp
require(foreign)
require(lattice)

maleLifeExp <- read.dta("maleLifeExp.dta")
maleLifeExp <- read.dta("malelifeexp.dta")  #if case sensitive

par(mfrow=c(2,2))
plot(density(maleLifeExp$expage0),main='Life expectancy at birth')
plot(density(maleLifeExp$expage25),main='Life expectancy at age 25')
plot(density(maleLifeExp$expage50),main='Life expectancy at age 50')
plot(density(maleLifeExp$expage75),main='Life expectancy at age 75')
par(mfrow=c(1,1))

#naive parallel coord plot.
matplot(1:4,t(maleLifeExp[,2:5]),type='b',ylab='value',xlab='feature')
#better par. coor. plot:
parallelplot(maleLifeExp[,2:5])
#bivariate relationships:
pairs(maleLifeExp[,2:5],pch=16)

#######################################################################
#see if we can get anywhere with additional exploration of:
pairs(maleLifeExp[,2:5])
pc <- princomp(maleLifeExp[,2:5])$scores
pairs(pc) #pointless.
#3D?
require(rgl)
plot3d(maleLifeExp[,2:4], type = "s", size=2)
plot3d(pc.crabs[,1:3], type = "s",col=c("cyan","orange")[crabs$species],size=c(1.1,1.6)[crabs$sex])
plot3d(iris[,2:4],type='s',col=(1:3)[iris$Species])
#######################################################################
### for fun.. why is this different?  ##########################################
##MDS:
mds.iris <- cmdscale(dist(iris[,-5]),k=3)
pairs(mds.iris,col=iris$Species)
plot3d(mds.iris,type='s',size=1,col=(1:3)[iris$Species])

############################################



