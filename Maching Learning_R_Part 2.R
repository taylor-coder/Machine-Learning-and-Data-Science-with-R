require(cluster) # for animals
require(foreign)
require(lattice)
require(gtools)

#setwd()

if (exists("maleLifeExp.dta")) maleLifeExp <- read.dta("maleLifeExp.dta") else maleLifeExp <- read.dta("malelifeexp.dta")

hcl.single <- hclust(dist(maleLifeExp[,2:5]),meth='single')
plot(hcl.single,labels=maleLifeExp[,1])
hcl.complete <- hclust(dist(maleLifeExp[,2:5]),meth='complete')
plot(hcl.complete,labels=maleLifeExp[,1]) 
hcl.ward <- hclust(dist(maleLifeExp[,2:5]),meth='ward.D2')
plot(hcl.ward,labels=maleLifeExp[,1]) 

#######################################################################   in class ######
data(animals)
animals <- animals-1 #make it 0/1
#fix NAs:
animals["fro","gro"] <- 0
animals["lio","end"] <- 1
animals["lob","gro"] <- 0
animals["sal","gro"] <- 0
animals["spi","end"] <- 0
a.nms <- c("ant","bee","cat","caterpillar","chimpanzee","cow","duck","eagle","elephant","fly","frog","herring","lion","lizard","lobster","man","rabbit","salmon","spider","whale")
v.nms <-c("warm blooded","flies","vertibrate","endangered","lives in groups","hair")
dimnames(animals) <- list(a.nms,v.nms)
cl.sing <- hclust(dist(animals,method='manhattan'),method='single')
cl.cent <- hclust(dist(animals,method='manhattan'),method='centroid')
cl.comp <- hclust(dist(animals,method='manhattan'),method='complete')
cl.ward <- hclust(dist(animals,method='manhattan'),method='ward.D2')
#which one do you 'like' best, and why?
par(mfrow=c(2,2));plot(cl.sing);plot(cl.cent);plot(cl.comp);plot(cl.ward)
par(mfrow=c(1,1));plot(cl.cent)
#look at the merge in of eagle - height is lower than prior merge - a dendrogram reversal.
cbind(cl.cent$merge,cl.cent$height)
#see the non-monotonicity:
plot(cl.cent$height,type='l',ylim=c(0,4))
lines(cl.sing$height,col=2);lines(cl.comp$height,col=3) 
#######################################################################

lbls.single <- cutree(hcl.single,k=3)
lbls.complete <- cutree(hcl.complete,k=3)
lbls.ward <- cutree(hcl.ward,k=3)

xtabs(~lbls.single+lbls.complete)
xtabs(~lbls.single+lbls.ward)

plot(hcl.ward,labels=maleLifeExp[,1])
rect.hclust(hcl.ward,k=3)


eurowork <- read.dta("eurowork.dta")
hcl.cent <- hclust(dist(scale(eurowork[,2:10]))^2,meth='centroid')
plot(hcl.cent,labels=eurowork[,1])
#explore the reversal - slightly:
#assign rownames to eurowork (to improve labeling later)
dimnames(eurowork)[[1]] <- eurowork$country
hcl.cent <- hclust(dist(scale(eurowork[,2:10]))^2,meth='centroid')
#code to clean up agglomeration schedule, from:http://r.789695.n4.nabble.com/Is-it-possible-to-obtain-an-agglomeration-schedule-with-R-cluster-analyis-td4659351.html
f <- function(hc){
    data.frame(row.names=paste0("Cluster",seq_along(hc$height)),
    height=hc$height,
    components=ifelse(hc$merge<0, hc$labels[abs(hc$merge)], paste0("Cluster",hc$merge)),
    stringsAsFactors=FALSE)
}
#compare raw output
data.frame(hcl.cent[2:1])
#to a cleaned up version:
f(hcl.cent)

lbls.cent <- cutree(hcl.cent,k=9)
names(lbls.cent) <- eurowork$country
as.matrix(lbls.cent)
plot(hcl.cent,labels=eurowork[,1])
rect.hclust(hcl.cent,k=9)

pc.euro <- princomp(eurowork[,-1],cor=T)$scores
plot(pc.euro[,2:3],col=rainbow(9)[lbls.cent],xlim=c(-4,2),cex=.75)
text(pc.euro[,2:3],labels=eurowork[,1],col=rainbow(9)[lbls.cent],pos=2)

############################# IN CLASS #########################
lbl3.sing <- cutree(cl.sing,k=3)
lbl3.cent <- cutree(cl.cent,k=3)
lbl3.comp <- cutree(cl.comp,k=3)
lbl3.ward <- cutree(cl.ward,k=3)
lbl7.ward <- cutree(cl.ward,k=7)
#manually determine the maximal agreement
xtabs(~lbl3.sing+lbl3.cent)
xtabs(~lbl3.comp+lbl3.ward)
#how to deal with different # of clusters???
xtabs(~lbl3.ward+lbl7.ward)

#write a function to get the maximal agreement
require(gtools)
optLabel <- function(src,trg) {
    #input two sets of labels, find permuation that maximizes agreement
    #to be complete search, and handle simpler diag eval, trg must have larger # of labels
    n1 <- length(unique(src))
    n2 <- length(unique(trg))
    if (n1>n2) {
        optRslt <- optLabel(trg,src)
        optRslt$best.tbl <- t(optRslt$best.tbl)
        optRslt$match.by="rows"
        return(optRslt)
    }
    tbl <- xtabs(~src+trg)
    best.match <- sum(diag(tbl)) #still works for a non-square matrix.
    best.perm <- 1:n2
    allPerms <- permutations(n2,n2)
    for (i in 1:dim(allPerms)[1]) {
        cur.match <- sum(diag(tbl[,allPerms[i,]]))
        if (cur.match>best.match) {
            best.match <- cur.match
            best.perm <- allPerms[i,]
        }
    }
    list(match.by="cols",best.match=best.match,best.perm=best.perm,best.tbl=tbl[,best.perm])
}
optLabel(lbl3.sing,lbl3.cent)
optLabel(lbl3.comp,lbl3.ward)
optLabel(lbl3.ward,lbl7.ward)
######################################################

#now k-means
require(foreign)
require(cluster)
crabs <- read.csv("crabs.csv",header=T)
if (exists("maleLifeExp.dta")) maleLifeExp <- read.dta("maleLifeExp.dta") else maleLifeExp <- read.dta("malelifeexp.dta")
data(iris)
eurowork <- read.dta("eurowork.dta")

set.seed(2011)
km.crabs.4<-kmeans(crabs[,-(1:3)],4)
km.crabs.7<-kmeans(crabs[,-(1:3)],7)
km.euro.4<-kmeans(eurowork[,-1],4)
km.euro.7<-kmeans(eurowork[,-1],7)
km.life.2<-kmeans(maleLifeExp[,2:5],2)
km.life.3<-kmeans(maleLifeExp[,2:5],3)
set.seed(2011001)
km.iris.3<-kmeans(iris[,-5],3)
km.iris.4<-kmeans(iris[,-5],4)
#need this run later:
complete.iris.4<-cutree(hclust(dist(iris[,-5]),method='complete'),4)
xtabs(~complete.iris.4+km.iris.4$clust)

par(mfrow=c(1,2))
plot(iris[,2:3],col=km.iris.3$clust,pch=1,cex=1.5,lwd=2)
plot(iris[,2:3],col=km.iris.4$clust,pch=1,cex=1.5,lwd=2)

par(mfrow=c(1,2))
plot(iris[,2:3],col=km.iris.3$clust,pch=1,cex=1.5,lwd=2)
plot(iris[,2:3],col=km.iris.4$clust,pch=1,cex=1.5,lwd=2)

plot(crabs[,5:6],col=km.crabs.4$clust,pch=1,cex=1.5,lwd=2)
plot(crabs[,5:6],col=km.crabs.7$clust,pch=1,cex=1.5,lwd=2)

plot(maleLifeExp[,c(2,4)],col=km.life.2$clust,pch=1,cex=1.5,lwd=2)
plot(maleLifeExp[,c(2,4)],col=km.life.3$clust,pch=1,cex=1.5,lwd=2)

pc.euro <- princomp(eurowork[,-1])$scores

plot(pc.euro[,2:3],col=km.euro.4$clust,pch='.',type='n')
text(pc.euro[,2:3],as.character(eurowork[,1]),col=km.euro.4$clust,cex=.8)

plot(pc.euro[,2:3],col=km.euro.7$clust,pch='.',type='n')
text(pc.euro[,2:3],as.character(eurowork[,1]),col=km.euro.7$clust,cex=.8)
par(mfrow=c(1,1))


############# IN CLASS #########################################
set.seed(10101)
km.euro.4<-kmeans(scale(eurowork[,-1]),centers=4,nstart=100)
km.euro.7<-kmeans(scale(eurowork[,-1]),centers=7,nstart=100)
pc.euro <- princomp(eurowork[,-1])$scores
par(mfrow=c(1,2))
plot(pc.euro[,2:3],col=km.euro.4$clust,pch='.',type='n')
text(pc.euro[,2:3],as.character(eurowork[,1]),col=km.euro.4$clust,cex=.8)
plot(pc.euro[,2:3],col=km.euro.7$clust,pch='.',type='n')
text(pc.euro[,2:3],as.character(eurowork[,1]),col=rainbow(7)[km.euro.7$clust],cex=.8)


#visualize using mds:
mds.animals <-cmdscale(dist(animals),k=3)
pairs(mds.animals)
plot(mds.animals[,c(2,3)]+c(-.05,0,+.05,0),type='n')
text(mds.animals[,c(2,3)]+c(-.05,0,+.05,0),labels=a.nms)

#dependency on initial seed/random start
par(mfrow=c(1,2))
set.seed(2011001)
km.animals.4a<-kmeans(animals,4) # Euclidean metric inappropriate here, but...
plot(mds.animals[,c(2,3)]+c(-.05,0,+.05,0),type='n',xlab='MDS2',ylab='MDS3')
text(mds.animals[,c(2,3)]+c(-.05,0,+.05,0),labels=a.nms,col=km.animals.4a$clust)
set.seed(2011002)
km.animals.4b<-kmeans(animals,4) # Euclidean metric inappropriate here, but...
plot(mds.animals[,c(2,3)]+c(-.05,0,+.05,0),type='n',xlab='MDS2',ylab='MDS3')
text(mds.animals[,c(2,3)]+c(-.05,0,+.05,0),labels=a.nms,col=optLabel(km.animals.4a$clust,km.animals.4b$clust)$best.perm[km.animals.4b$clust])
#try again, but with lots of random starts
set.seed(2011001)
km.animals.4a<-kmeans(animals,4,nstart=100) # Euclidean metric inappropriate here, but...
plot(mds.animals[,c(2,3)]+c(-.05,0,+.05,0),type='n',xlab='MDS2',ylab='MDS3')
text(mds.animals[,c(2,3)]+c(-.05,0,+.05,0),labels=a.nms,col=km.animals.4a$clust)
set.seed(2011002)
km.animals.4b<-kmeans(animals,4,nstart=100) # Euclidean metric inappropriate here, but...
plot(mds.animals[,c(2,3)]+c(-.05,0,+.05,0),type='n',xlab='MDS2',ylab='MDS3')
text(mds.animals[,c(2,3)]+c(-.05,0,+.05,0),labels=a.nms,col=optLabel(km.animals.4a$clust,km.animals.4b$clust)$best.perm[km.animals.4b$clust])
#look at k=2,3,4,5
set.seed(2011001)
par(mfrow=c(2,2))
for (i in 2:5) {
  km.animals.i<-kmeans(animals,i,nstart=100)
  plot(mds.animals[,c(2,3)]+c(-.05,0,+.05,0),type='n',xlab='MDS2',ylab='MDS3')
  text(mds.animals[,c(2,3)]+c(-.05,0,+.05,0),labels=a.nms,col=km.animals.i$clust)
}

######################################################



#b/w crit for # groups:
c.crit <- function(km.obj) {
    #based on k-means, for convenience due to amt of addl info in the km result object.
    #cd be generalized.
    sizes <- km.obj$size
    n <- sum(sizes)
    g <- length(sizes)
    msW<-sum(km.obj$withinss)/(n-g)
    overall.mean <- apply(km.obj$centers*km.obj$size,2,sum)/sum(km.obj$size)
    msB<-sum(km.obj$size*(t(t(km.obj$centers)-overall.mean))^2)/(g-1)
    list(msB=msB,msW=msW,C.g=msB/msW)
}


numGroupSearch <- function(features,rng=c(2,10),wilks=T,nstart=100) {

    mn <- rng[1]
    mx <- rng[2]
    m.list <- km.list <- vector("list",length=mx-mn+1)
    cFn <- p.rsq <- rep(NA,mx-mn+1)
    i <- 0
    for (k in mn:mx) {
        i <- i+1
        km.list[[i]] <- kmeans(features,k,nstart=nstart)
        U <- as.matrix(features)
        m.list[[i]] <- manova(U~factor(km.list[[i]]$cluster))
        if (wilks) { #avoids some degenerate cases
            p.rsq[i] <- 1-summary(m.list[[i]],test="Wilks")$stats[1,2]
        }
        cFn[i] <- c.crit(km.list[[i]])$C.g
    }
    return(list(km.list=km.list,m.list=m.list,p.rsq=p.rsq,cFn=cFn))
}

ngp.iris <- numGroupSearch(iris[,-5])
ngp.crabs <- numGroupSearch(crabs[,-(1:3)])
ngp.euro <- numGroupSearch(eurowork[,-1])
ngp.life <- numGroupSearch(maleLifeExp[,2:5])

#WARNING: EXPECT THESE PLOTS TO DIFFER FROM THE NOTES DUE TO DIFFERENT CLUSTERINGS
par(mfrow=c(1,2))
plot(2:10,ngp.iris$p.rsq,type='l',xlab='Number of Clusters',ylab='1-Wilks Lambda')
plot(2:10,ngp.iris$cFn,type='l',xlab='Number of Clusters (g)',ylab='C(g)')
plot(2:10,ngp.crabs$p.rsq,type='l',xlab='Number of Clusters',ylab='1-Wilks Lambda')
plot(2:10,ngp.crabs$cFn,type='l',xlab='Number of Clusters (g)',ylab='C(g)')
plot(2:10,ngp.euro$p.rsq,type='l',xlab='Number of Clusters',ylab='1-Wilks Lambda')
plot(2:10,ngp.euro$cFn,type='l',xlab='Number of Clusters (g)',ylab='C(g)')
plot(2:10,ngp.life$p.rsq,type='l',xlab='Number of Clusters',ylab='1-Wilks Lambda')
plot(2:10,ngp.life$cFn,type='l',xlab='Number of Clusters (g)',ylab='C(g)')

################# IN CLASS #####################################
par(mfrow=c(1,1))
set.seed(2011001)
ngp.animals <- numGroupSearch(animals,wilks=F,nstart=100)
plot(2:10,ngp.animals$cFn,type='l',xlab='Number of Clusters (g)',ylab='C(g)')

require(NbClust) # easier library to use:
NbC.rslt <- NbClust(data=animals, distance = NULL, min.nc=2, max.nc=9,method = "kmeans", index = "ch")
plot(2:9,NbC.rslt$All.index)

######################################################

#RAND INDEX
randI <- function(c1,c2) {
    
    xm <- xtabs(~c1+c2)
    n <- sum(xm)
    r.m <- apply(xm,1,sum)
    c.m <- apply(xm,2,sum)
    A <-  sum(choose(xm,2))+(choose(n,2) - (sum(choose(r.m,2))+sum(choose(c.m,2))-sum(choose(xm,2)))) #'a'+'d'
    A/choose(n,2)
}

#rerun kmeans:
set.seed(2011001)
#iris
km.iris.3<-kmeans(iris[,-5],3,nstart=100)
km.iris.4<-kmeans(iris[,-5],4,nstart=100)
comp.iris.4 <- cutree(hclust(dist(iris[,-5]),meth='complete'),4)

xtabs(~comp.iris.4+km.iris.4$clust)
randI(comp.iris.4,km.iris.4$clust)
randI(km.iris.3$clust,iris$Species)
require(phyclust)
RRand(comp.iris.4,km.iris.4$clust)
RRand(km.iris.3$clust,as.numeric(iris$Species))
###########################################################################################

