require(foreign)
library(nnet)
library(MASS)
library(klaR)
library(biotools)
library(class)
###NN
plot(iris[,c(3,4)],col=iris$Species,cex=1.1,pch=16,ylim=c(1,2.5),xlim=c(3,7))
points(iris[78,c(3,4)],pch=16,col=4,cex=1.1)
points(iris[78,c(3,4)],pch=1,col=4,cex=6.6)

#do same thing for whole dataset:
newLabel <- knn.cv(iris[,-5],cl=iris$Species,k=1)
plot(iris[,c(3,4)],col=iris$Species,pch=16,cex=1,ylim=c(1,2.5),xlim=c(3,7))
points(iris[,c(3,4)],pch=1,col=as.numeric(newLabel),cex=1.5)
table(iris$Species,newLabel)

par(mfrow=c(1,2))
#do same thing for k=5 nn:
newLabel <- knn.cv(iris[,-5],cl=iris$Species,k=5)
plot(iris[,c(3,4)],col=iris$Species,pch=16,cex=1,ylim=c(1,2.5),xlim=c(3,7),main='knn; k=5')
points(iris[,c(3,4)],pch=1,col=as.numeric(newLabel),cex=1.5)
table(iris$Species,newLabel)

#do same thing for k=7 nn:
newLabel <- knn.cv(iris[,-5],cl=iris$Species,k=7)
plot(iris[,c(3,4)],col=iris$Species,pch=16,cex=1,ylim=c(1,2.5),xlim=c(3,7),main='knn; k=7')
points(iris[,c(3,4)],pch=1,col=as.numeric(newLabel),cex=1.5)
table(iris$Species,newLabel)

#RESCALE, then do same thing for k=7 nn:
newLabel <- knn.cv(scale(iris[,-5]),cl=iris$Species,k=7)
plot(iris[,c(3,4)],col=iris$Species,pch=16,cex=1,ylim=c(1,2.5),xlim=c(3,7),main='knn; k=7')
points(iris[,c(3,4)],pch=1,col=as.numeric(newLabel),cex=1.5)
table(iris$Species,newLabel)

#################### IN CLASS ########################

#consider k=15,31 ???

##########################################################

crabs <- read.dta("crabs.dta")
data(iris)
inv.logit <- function(x) exp(x)/(1+exp(x))
logit.fit2 = glm(I(sex=="Male")~FL+RW+CL+CW+BD+species,data=crabs,family='binomial')
logit.fit1 = glm(I(sex=="Male")~FL+RW+CL+CW+BD,data=crabs,family='binomial')
plot(inv.logit(predict(logit.fit1)),1:200,col=c(2,4)[crabs$sex],ylab='Obs number',xlab='Prob(Male)')
plot(inv.logit(predict(logit.fit2)),1:200,col=c(2,4)[crabs$sex],ylab='Obs number',xlab='Prob(Male)')

library(nnet)
multinom.fit <- multinom(Species~.,data=iris)
preds <- predict(multinom.fit,type='probs')
plot(jitter(preds[,2],factor=2000),jitter(preds[,3],factor=2000),col=(2:4)[iris[,5]],xlab='Pr(Versicolor)',ylab='Pr(Virginica)')

#alternate using logit:
logit.iris = glm(Species~.,data=iris,family='binomial',subset=Species!="setosa")
pred3 <- predict(logit.iris)
preds <- matrix(c(1,0,0),50,3,byrow=T) # Setosa preds
preds <- rbind(preds,cbind(0,1-inv.logit(pred3),inv.logit(pred3)))
plot(jitter(preds[,2],factor=2000),jitter(preds[,3],factor=2000),col=(2:4)[iris[,5]],xlab='Pr(Versicolor)',ylab='Pr(Virginica)')

#variant in which we project into 1-dim:
z1 = -0.4*iris$Sepal.Width + 1.0*iris$Petal.Length
z2 = -0.5*iris$Sepal.Width + 1.0*iris$Petal.Length
z3 = -0.522*iris$Sepal.Width + 1.068*iris$Petal.Length

plot(density(iris$Petal.Length),type='n',xlab='Z1',xlim=c(-1,6),ylim=c(0,1.7),main='Density of Projection')
by(iris,iris$Species,function(x,a1,a2) lines(density(a1*x$Sepal.Width+a2*x$Petal.Length),col=x$Species),a1=-0.4,a2=1.0)
plot(density(iris$Petal.Length),type='n',xlab='Z2',xlim=c(-1,6),ylim=c(0,1.7),main='Density of Projection')
by(iris,iris$Species,function(x,a1,a2) lines(density(a1*x$Sepal.Width+a2*x$Petal.Length),col=x$Species),a1=-0.5,a2=1.0)
plot(density(iris$Petal.Length),type='n',xlab='Z3',xlim=c(-1,6),ylim=c(0,1.7),main='Density of Projection')
by(iris,iris$Species,function(x,a1,a2) lines(density(a1*x$Sepal.Width+a2*x$Petal.Length),col=x$Species),a1=-0.522,a2=1.068)

summary(aov(cbind(z1,z2,z3)~iris$Species))

#LDA:
iris.lda2 <-lda(Species ~Sepal.Length+Sepal.Width+Petal.Length+Petal.Width, data=iris,na.action="na.omit", CV=FALSE,subset=Species!="virginica")
iris.lda2
#look at 'classification accuracy'
xtabs(~predict(iris.lda2)$class+iris$Species[iris$Species!="virginica"])
#compare scores across Species:
#first identify which species is which, within the subset used to fit the model:
subdat <- subset(iris,Species!="virginica")
iris.scores <- predict(iris.lda2)$x
#this sets up an overall density and then the two subgroups.
plot(density(iris.scores,bw=.5),col=8,ylim=c(0,.24))
dns.s <- density(iris.scores[subdat$Species=="setosa"])
dns.c <- density(iris.scores[subdat$Species=="versicolor"])
lines(dns.s$x,dns.s$y/2,col=2) #rescaling to sum to one, once combined
lines(dns.c$x,dns.c$y/2,col=3) #rescaling to sum to one, once combined

iris.lda3 <-lda(Species ~Sepal.Length+Sepal.Width+Petal.Length+Petal.Width, data=iris,na.action="na.omit", CV=FALSE)
iris.lda3

#wilk's lambda:
manova.fit <- manova(as.matrix(iris[,-5])~iris[,5])
summary(manova.fit,test="Wilks")
#lambdas associated with each LD projection:
#hard way:
B<-summary(manova.fit)$SS[[1]]
W<-summary(manova.fit)$SS[[2]]
B.xprod <- t(iris.lda3$scaling)%*%B%*%iris.lda3$scaling
W.xprod <- t(iris.lda3$scaling)%*%W%*%iris.lda3$scaling
lambdas <- diag(B.xprod/W.xprod)
lambdas
#easy way:
summary(manova.fit)$Eigen[1:2]

#store projections:
iris.LDs <- predict(iris.lda3)$x

#show 1-D discrimination (LD1):
plot(iris.lda3,dimen=1)
#next LD2
ldahist(iris.LDs[,2],iris$Species,type='both')

#default discrim map:
plot(iris.lda3,abbrev=T,col=(2:4)[iris$Species])
#default territory map:
partimat(Species ~Sepal.Length+Sepal.Width+Petal.Length+Petal.Width, data=iris,method="lda",gs=c("s","v","c")[iris$Species])
#now in LD space:
newIris <- data.frame(cbind(iris,predict(iris.lda3)$x))
partimat(Species ~LD2+LD1, data=newIris,method="lda",gs=c("s","v","c")[iris$Species]) #order seems to flip the axes properly

#box's m:
library(biotools)
boxM(iris[,-5],iris[,5])

#crosstab:
xtabs(~predict(iris.lda3)$class+iris$Species)

######################## IN CLASS ##################################################
crabs$group <- factor(2*(crabs$sex=="Female")+(crabs$species=="Orange"))
crabs.lda <-lda(group~FL+RW+CL+CW+BD, data=crabs,na.action="na.omit", CV=FALSE)
plot(crabs.lda)
partimat(group~FL+RW+CL+CW+BD, data=crabs,method="lda")
crabs.pc <- crabs
crabs.pc[,4:8] <- princomp(crabs[,4:8])$scores #replace with PCs, at least for viz.
crabs.lda.pc <-lda(group~FL+RW+CL+CW+BD, data=crabs.pc,na.action="na.omit", CV=FALSE)
plot(crabs.lda.pc)
partimat(group~FL+RW+CL+CW+BD, data=crabs.pc,method="lda")
crabs.pc <- data.frame(cbind(crabs.pc,predict(crabs.lda.pc)$x),class=predict(crabs.lda.pc)$class)
partimat(group~LD1+LD2+LD3, data=crabs.pc,method="lda",gs=c("bm","om","bf","of")[crabs.pc$group],nplots.vert=1, nplots.hor=3)
library(rgl)
plot3d(crabs.pc[,c("LD1","LD2","LD3")], type = "s",col=c("cyan","orange")[crabs$species],size=c(1.1,1.6)[crabs$sex])
xtabs(~group+class,data=crabs.pc)
##########################################################################
