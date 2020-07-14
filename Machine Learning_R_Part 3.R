require(foreign)
require(cluster)
crabs <- read.dta("crabs.dta")
if (exists("maleLifeExp.dta")) maleLifeExp <- read.dta("maleLifeExp.dta") else maleLifeExp <- read.dta("malelifeexp.dta")
data(iris)
eurowork <- read.dta("eurowork.dta")

#rerun kmeans:
set.seed(2011001)
#iris
km.iris.3<-kmeans(iris[,-5],3,nstart=100)
km.iris.4<-kmeans(iris[,-5],4,nstart=100)
comp.iris.4 <- cutree(hclust(dist(iris[,-5]),meth='complete'),4)

#crosstab
xtabs(~km.iris.3$clust+km.iris.4$clust)
#crab
km.crabs.4<-kmeans(crabs[,-(1:3)],4,nstart=100)
km.crabs.7<-kmeans(crabs[,-(1:3)],7,nstart=100)
#crosstab
xtabs(~km.crabs.4$clust+km.crabs.7$clust)

#silhoutte plots
par(mfrow=c(1,2))
plot(silhouette(km.iris.3$clust,dist(iris[,-5])))
plot(silhouette(km.iris.4$clust,dist(iris[,-5])))
par(mfrow=c(1,2))
plot(silhouette(km.crabs.4$clust,dist(crabs[,-(1:3)])))
plot(silhouette(km.crabs.7$clust,dist(crabs[,-(1:3)])))
par(mfrow=c(1,1))

##################################################################################################

## MORE PAM material at end

###########################################################################################



#now Mclust:
require(foreign)
require(cluster)
require(mclust)

crabs <- read.csv("crabs.csv",header=T)
if (exists("maleLifeExp.dta")) maleLifeExp <- read.dta("maleLifeExp.dta") else maleLifeExp <- read.dta("malelifeexp.dta")
data(iris)
eurowork <- read.dta("eurowork.dta")

set.seed(2011)
y1 <- rbinom(50,100,.25)
y2 <- rbinom(50,100,.75)
y <- cbind(x=c(y1,y2),g=c(rep(1,50),rep(2,50)))

x <- jitter(rep(0,100))

simBinomMix <- function(n.m=c(50,50),bin.draws=100,theta=c(.25,.75),init.seed=2011) {
    
    set.seed(init.seed)
    r <- NULL
    for (g in 1:length(n.m)) {
        y <- rbinom(n.m[g],bin.draws,theta[g])
        r <- rbind(r,cbind(y,g))
    }
    r
}

learnBinMix <- function(dat,n.groups=2,init.seed=2012,n.iter=10,nstart=1,plot=F,pause=F) {
    #wrapper to allow multiple starts:
    set.seed(init.seed)
    #store results:
    solns <- vector("list",nstart)
    lbls <- vector("list",nstart)
    for (i in 1:nstart) {
       solns[[i]] <- learnBinMix.single(dat,n.groups,init.seed=NULL,n.iter,plot,pause)
       lbls[[i]] <- solns[[i]]$cluster
    }
    #now we have to choose one!
    d <- matrix(1,nstart,nstart)
    for (i in 1:(nstart-1)) {
        for(j in (i+1):nstart) {
            #compare i,j
            d[i,j]<-d[j,i] <- RRand(lbls[[i]],lbls[[j]])$Rand
        }
    }
    #compute avg Rand index:
    avgRand <- apply(d,1,mean)
    idx <- which(max(avgRand)==avgRand)
    solns[[idx[1]]] #take first best
}

learnBinMix.single <- function(dat,n.groups=2,init.seed=2012,n.iter=10,plot=F,pause=F) {
    
    p.ask <- par()$ask
    if (pause) par(ask=T)
    if (!is.null(init.seed)) set.seed(init.seed)
    n <- length(dat)
    g <- sample(1:n.groups,n,replace=T) #equal prior
    x <- jitter(rep(0,n))
    g.means <- tapply(dat,g,mean)
    for (i in 1:n.iter) {
        if (plot) {
            plot(x,dat,col=1+g,pch=16,cex=2,xlim=c(-.02,.025))
            arrows(.025,g.means,.021,g.means,col=1+(1:n.groups),length=.15)
        }
        #reassign based on group means (a bit kludgy)
        for (j in 1:n) {
            z <- abs(dat[j]-g.means)
            g[j] <- (1:n.groups)[min(z)==z][1]
        }
        g.means <- tapply(dat,g,mean)
    }
    par(ask=p.ask) #reset to prior value
    list(probs=g.means,cluster=g)
}


par(mfrow=c(1,1))
dat <- simBinomMix(c(50,50),100,c(.25,.75),2011)
plot(density(dat[,1]),main='Density: two groups',xlab='Binominal Draw (out of 100)')
dat <- simBinomMix(c(25,25,50),100,c(.3,.5,.7),2011)
plot(density(dat[,1],bw=3),main='Density: three groups',xlab='Binominal Draw (out of 100)')
par(mfrow=c(2,2))
learnBinMix.single(dat[,1],3,plot=T,n.iter=4)
par(mfrow=c(1,1))
learnBinMix.single(dat[,1],3,plot=T,n.iter=10,pause=T)



########################### IN CLASS ################################################################
#Harder set of params to solv:
dat <- simBinomMix(2*c(15,20,20,45),100,c(.1,.25,.5,.65),2011)
plot(density(dat[,1],bw=3),main='Density: four groups',xlab='Binominal Draw (out of 100)')
par(mfrow=c(2,2))
learnBinMix.single(dat[,1],4,plot=T,n.iter=4,pause=F)
par(mfrow=c(1,1))
fit.binmix <- learnBinMix.single(dat[,1],4,plot=T,n.iter=10,pause=F)
#fit.binmix <- learnBinMix(dat[,1],4,plot=T,n.iter=10,pause=T)

#bring in old code:
require(gtools)
optLabel <- function(src,trg) {
    #input two sets of labels, find permuation that maximizes agreement
    #to be complete search, and handle simpler diag eval, trg must have larger # of labels
    n1 <- length(unique(src))
    n2 <- length(unique(trg))
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
    list(best.match=best.match,best.perm=best.perm,best.tbl=tbl[,best.perm])
}
#

#try lots of starts:
fit.binmix <- learnBinMix(dat[,1],4,plot=T,n.iter=10,nstart=10,pause=F)


xtabs(~fit.binmix$cluster+dat[,2])
RRand(fit.binmix$cluster,dat[,2])
#confusion matrix:
optLabel(fit.binmix$cluster,dat[,2])
###########################################################################################


#now, more formally, with flexmix:
library(flexmix)
newDat <- data.frame(successes=dat[,1],failures=100-dat[,1]) #this is how you specify binomial trials
fitMix3 <- flexmix(cbind(successes,failures)~1,data=newDat,k=3, model=list(FLXMRglm(cbind(successes,failures)~.,family='binomial')))
summary(fitMix3)
print(lapply(fitMix3@components,"[[",1))

mcl.iris <- Mclust(iris[,-5])
mcl.crabs <- Mclust(crabs[,-(1:3)])
mcl.euro<-Mclust(eurowork[,-1])
mcl.life<-Mclust(maleLifeExp[,2:5])
pc.crabs <- princomp(crabs[,-(1:3)],cor=T)$scores

plot(mcl.iris)
plot(mcl.iris,dimen=c(1,2))

cols <- mclust.options("classPlotColors")
syms <- mclust.options("classPlotSymbols")
plot(mcl.crabs,dimen=c(2,3))
plot(pc.crabs[,c(2,3)],col=cols[mcl.crabs$class],pch=syms[mcl.crabs$class],main='Classification')
plot(pc.crabs[,c(2,3)],col=cols[mcl.crabs$class],pch=16,cex=pmax(.3,sqrt(6*mcl.crabs$uncertainty)),main='Classification Uncertainty')

plot(mcl.life)
plot(mcl.life,dimen=c(1,3))
pc.euro <- princomp(eurowork[,-1])$scores
plot(mcl.euro)

plot(pc.euro[,2:3],col=1+mcl.euro$class,pch='.',type='n')
text(pc.euro[,2:3],as.character(eurowork[,1]),col=cols[mcl.euro$class],cex=.8)

#################### IN CLASS #######################################################################
#try Mclust on animals:
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
mcl.animals <- Mclust(animals)
plot(mcl.animals)
mds.animals <-cmdscale(dist(animals),k=3)
par(mfrow=c(1,2))
plot(mds.animals[,c(1,2)]+c(-.05,0,+.05,0),type='n',xlab='MDS-1',ylab='MDS-2')
text(mds.animals[,c(1,2)]+c(-.05,0,+.05,0),labels=a.nms,col=(2:3)[mcl.animals$class])
plot(mds.animals[,c(2,3)]+c(-.05,0,+.05,0),type='n',xlab='MDS-2',ylab='MDS-3')
text(mds.animals[,c(2,3)]+c(-.05,0,+.05,0),labels=a.nms,col=(2:3)[mcl.animals$class])
par(mfrow=c(1,1))


###########################################################################################
# EXTRA MATERIAL ON Silhouette and PAM
###########################################################################################


##PAM:
require(cluster)
data(votes.repub)
lbls0 <- dimnames(votes.repub)[[1]]
lbls <- c("AL","AK","AZ","AR","CA","CO","CT","DE","FL","GA","HI","ID","IL","IN","IA","KS","KY","LA","ME","MD","MA","MI","MN","MS","MO","MT","NE","NV","NH","NJ","NM","NY","NC","ND","OH","OK","OR","PA","RI","SC","SD","TN","TX","UT","VT","VA","WA","WV","WI","WY")
len <- length(lbls)
blueStates <- c("MA","RI","NY","HI","VT","MD","IL","CT","CA","DE","ME","NJ","WA","MI","MN","OR","PA","NM","WI","IA","NH")
purpleStates <- c("NV","WV","AR","CO","FL","MO","OH")
b.blue <- !is.na(match(lbls,blueStates))
b.purp <- !is.na(match(lbls,purpleStates))
col.state <- 1+1*b.blue+2*b.purp

mds3 <- cmdscale(dist(votes.repub[,27:31],method='manhattan'),3)
plot(mds3[,2:3],type='n',xlab='MDS Dimension 2',ylab='MDS Dimension 3')
for (i in 1:len) text(mds3[i,2],mds3[i,3],lbls[i],cex=1,col=c("red","blue","purple")[col.state[i]])

#cluster - look at g=2-10 groups; report only through g=5
par(mfrow=c(2,2))
pam.fits <- vector('list',9)
G <- 2:10
for (k in 1:length(pam.fits)) {
    pam.fits[[k]] <- pam(dist(votes.repub[,27:31],method='manhattan'),diss=T,k=G[k])
    if (G[k] <= 5) {
        plot(mds3[,2:3],type='n',xlab='MDS Dimension 2',ylab='MDS Dimension 3')
        new.lbl <- pam.fits[[k]]$clust
        for (i in 1:len) text(mds3[i,2],mds3[i,3],new.lbl[i],cex=1,col=c("red","blue","purple")[col.state[i]])
        med.idx <- match(pam.fits[[k]]$medoid,lbls0) #find which cell is the medoid
        points(mds3[med.idx,2:3],pch=1,cex=2.2,col='green')
    }
}


#separate SW plots
par(mfrow=c(1,1))
for (k in 1:4) {
    idx.state <- match(dimnames(pam.fits[[k]]$silinfo$widths)[[1]],dimnames(votes.repub)[[1]])
    plot(silhouette(pam.fits[[k]]),col=c("red","blue","purple")[col.state[idx.state]]) #silhouette plot
    locator() # to pause the plot
}

#diminishing returns to increasing # groups (reqs running 9 different models previously):
avg.sw<-vector("numeric",9);for(i in 1:9) avg.sw[i] <-pam.fits[[i]]$silinfo$avg
plot(G,avg.sw,type='l')


#look closer at 5 cluster soln
idx.state <- match(dimnames(pam.fits[[4]]$silinfo$widths)[[1]],dimnames(votes.repub)[[1]]) #these are the indices of states in cluster order
sil <- silhouette(pam.fits[[4]])
#cluster 1
b <- sil[,"cluster"]==1 #this is a boolean selection of rows (states) corresp. to cluster 1.
cbind(votes.repub[idx.state[b],27:31],sil[b,])
#cluster 4
b <- sil[,"cluster"]==4 #this is a boolean selection of rows (states) corresp. to cluster 1.
cbind(votes.repub[idx.state[b],27:31],sil[b,])

# b is a boolean selection of rows (states) corresp. to clusters 1 & 4.
b <- sil[,"cluster"]==1 | sil[,"cluster"]==4
d.mat <- as.matrix(dist(votes.repub[idx.state[b],27:31],method='manhattan'))
avg.dist.a <- mean(d.mat[3,1:2])
avg.dist.b <- mean(d.mat[3,4:10])
sw <- (avg.dist.b-avg.dist.a)/max(avg.dist.a,avg.dist.b)
sw

###########################################################################################
#good use of PAM:
#try proving that LA does not belong in cluster 4, while VA does...

state.nms <-dimnames(votes.repub)[[1]] #alpha order
idx.LA <- match("Louisiana",state.nms)
idx.VA <- match("Virginia",state.nms)
sub.mat <- votes.repub[c(idx.LA,idx.VA,pam.fits[[4]]$id.med),27:31]
dimnames(sub.mat)[[1]] <- c("LA","VA",paste(pam.fits[[4]]$medoid,1:5,sep="-"))
dist(sub.mat,method='manhattan')



