## Citation----------------------
# Matsuyoshi D, Watanabe K (submitted) People have modest, not good, 
# insight into their face recognition ability: 
# a comparison between developmental prosopagnosia questionnaires.


library(dplyr)
library(psych)
library(cocor)
library(gtools)
library(RColorBrewer)
library(cocron)
library(MBESS)
# using heatmap.3 function
# https://gist.github.com/tleja/7783150#file-heatmap-3-r

## Read data----
dp_survey <- read.table("dp_survey.data",header=T) #
dp_survey$Sex <- factor(dp_survey$Sex)
dp_survey$Hand <- factor(dp_survey$Hand)

Revcols = c("HK1", "HK4", "HK6", "HK7", "HK8", "HK9", "HK10", "HK12", "CONF","PI8", "PI9", "PI13", "PI17", "PI19")
dp_survey[,Revcols] <- 6 - dp_survey[,Revcols]

## Create summary variables ----
dp_survey$HKDP <- dplyr::select(dp_survey,starts_with("HK")) %>% dplyr::select(-c(HK10:HK13)) %>% apply(1,sum)
dp_survey$PIDP <- dplyr::select(dp_survey,starts_with("PI")) %>% apply(1,sum)
dp_survey$HKDummy <- dplyr::select(dp_survey,c(HK10:HK13)) %>% apply(1,sum)
dp_survey$HKall <- dplyr::select(dp_survey,starts_with("HK")) %>% apply(1,sum)

DP.fe <- psych::describe(dp_survey[dp_survey$Sex=="Female",c(2,(ncol(dp_survey)-3):ncol(dp_survey))]) %>% print(digits=4)
DP.ma <- psych::describe(dp_survey[dp_survey$Sex=="Male",c(2,(ncol(dp_survey)-3):ncol(dp_survey))]) %>% print(digits=4)
DP.all <- psych::describe(dp_survey[c(2,(ncol(dp_survey)-3):ncol(dp_survey))]) %>% print(digits=4)

summary(dp_survey[3:4])

## Sex differences ----
x <- dp_survey$HKDP[dp_survey$Sex=="Female"]
y <- dp_survey$HKDP[dp_survey$Sex=="Male"]
t.test(x,y)
md  <- mean(x,na.rm=T) - mean(y,na.rm=T)
diff_sd <- sqrt(((length2(x,na.rm=T)-1)*var(x,na.rm=T)+(length2(y,na.rm=T)-1)*var(y,na.rm=T))/(length2(x,na.rm=T)+length2(y,na.rm=T)-2))  ## denominator -2
smd <- md/diff_sd # d
MBESS::ci.smd(smd=smd,n.1=length(x), n.2=length(y))
ttestBF(x,y,r=1)
ks.test(x,y)

x <- dp_survey$PIDP[dp_survey$Sex=="Female"]
y <- dp_survey$PIDP[dp_survey$Sex=="Male"]
t.test(x,y)
md  <- mean(x,na.rm=T) - mean(y,na.rm=T)
diff_sd <- sqrt(((length2(x,na.rm=T)-1)*var(x,na.rm=T)+(length2(y,na.rm=T)-1)*var(y,na.rm=T))/(length2(x,na.rm=T)+length2(y,na.rm=T)-2))  ## denominator -2
smd <- md/diff_sd # d
MBESS::ci.smd(smd=smd,n.1=length(x), n.2=length(y))
ttestBF(x,y,r=1)
ks.test(x,y)

cor.test(dp_survey$PIDP,dp_survey$HKDP)
cor.test(dp_survey$PIDP[dp_survey$Sex=="Female"],dp_survey$HKDP[dp_survey$Sex=="Female"])
cor.test(dp_survey$PIDP[dp_survey$Sex=="Female"],dp_survey$HKDP[dp_survey$Sex=="Female"])$p.value
cor.test(dp_survey$PIDP[dp_survey$Sex=="Male"],dp_survey$HKDP[dp_survey$Sex=="Male"])
cor.test(dp_survey$PIDP[dp_survey$Sex=="Male"],dp_survey$HKDP[dp_survey$Sex=="Male"])$p.value

cocor::cocor.indep.groups(cor(dp_survey$PIDP[dp_survey$Sex=="Male"],dp_survey$HKDP[dp_survey$Sex=="Male"]),
                          cor(dp_survey$PIDP[dp_survey$Sex=="Female"],dp_survey$HKDP[dp_survey$Sex=="Female"]),
                          length(dp_survey$PIDP[dp_survey$Sex=="Male"]),
                          length(dp_survey$PIDP[dp_survey$Sex=="Female"]))
cocor::cocor.indep.groups(cor(dp_survey$PIDP[dp_survey$Sex=="Male"],dp_survey$HKDP[dp_survey$Sex=="Male"],method = "spearman"),
                          cor(dp_survey$PIDP[dp_survey$Sex=="Female"],dp_survey$HKDP[dp_survey$Sex=="Female"],method = "spearman"),
                          length(dp_survey$PIDP[dp_survey$Sex=="Male"]),
                          length(dp_survey$PIDP[dp_survey$Sex=="Female"]))


## PCA ----
pcamat <- data.matrix(dplyr::select(dp_survey,c(PIDP,HKDP)))
pcdp <- prcomp(pcamat,scale=T)
pcdp
summary(pcdp)

pca.res <- prcomp(~PIDP+HKDP,data.frame(pcamat))
slp <- with(pca.res,rotation[2,1] / rotation[1,1])
int <- with(pca.res,center[2] - slp*center[1])
summary(pca.res)

## Polychoric----
pcor <- psych::cor.ci(as.matrix(dplyr::select(dp_survey,HK1:PI20)),n.iter = 10,poly = TRUE)
pcor.PI <- psych::cor.ci(as.matrix(dplyr::select(dp_survey,PI1:PI20)),n.iter = 10,poly = TRUE)
pcor.HK <- psych::cor.ci(as.matrix(dplyr::select(dp_survey,c(HK1:HK9,HK14:HK15))),n.iter = 10,poly = TRUE)

## Basic scatterplot----
def.par <- par(no.readonly = TRUE)
plot(dp_survey$PIDP,dp_survey$HKDP,xlab="", ylab="",col="#00000022",
     xlim=c(0,100),ylim=c(0,50),xaxs="i",yaxs="i",pch=19,las=1)
abline(int,slp,col=4)
text(18,30,sprintf("N = %d",nrow(dp_survey)),font=1)
mtext(text=expression(paste(bold("PI20"))), side=1, font=2, line=3)
mtext(text=expression(paste(bold("HK11"))), side=2, font=2, line=3)

## Correlation matrix and dendrogram----
distCor <- function(x) as.dist(1-cor(t(x)))
hclustAvg <- function(x) hclust(x, method="average")
heatmap.3(pcor$rho, trace="none", scale="none", zlim=c(-1,1), reorder=FALSE,
          distfun=distCor, hclustfun=hclustAvg, col=rev(colorRampPalette(brewer.pal(10, "RdBu"))(256)), symbreak=FALSE) 

## Reliability coefficients----
om.HK <- psych::omega(pcor.HK$rho,fm="ml",flip=F,plot=F)
om.HK$alpha
om.HK$omega.tot
om.PI <- psych::omega(pcor.PI$rho,fm="ml",flip=F,plot=F)
om.PI$alpha
om.PI$omega.tot

diff.alpha <- cocron::cocron.two.coefficients(c(om.HK$alpha,om.PI$alpha),
                                              n=nrow(dp_survey), dep = TRUE, r = pearscor$estimate, los = 0.05, alternative = "two.sided")
diff.alpha.p <- 2*pt(-abs(diff.alpha@statistic),df=diff.alpha@df)
diff.alpha
sprintf("%g",diff.alpha.p)
diff.omega <- cocron::cocron.two.coefficients(c(om.HK$omega.tot,om.PI$omega.tot),
                                              n=nrow(dp_survey), dep = TRUE, r = pearscor$estimate, los = 0.05, alternative = "two.sided")
diff.omega.p <- 2*pt(-abs(diff.omega@statistic),df=diff.omega@df)
diff.omega
sprintf("%g",diff.omega.p)

## Brute-force calculation of subsets' reliability coefficients----
Mat <- combn(1:20,11)
R <- choose(20,11)
al <- matrix(0,R,1)
o.tot <- matrix(0,R,1)

for (i in 1:R) {
  poly11 <- pcor.PI$rho[c(Mat[,i]),c(Mat[,i])]
  al[i] <- suppressWarnings(psych::alpha(poly11)$total$raw_alpha)
}

for (i in 1:R) {
  poly11 <- pcor.PI$rho[c(Mat[,i]),c(Mat[,i])]
  o.tot[i] <- suppressWarnings(psych::omega(poly11,plot=F,nfactors=3,fm="ml",flip=F)$omega.tot)
}

psych::describe(al) %>% print(digits=4)
psych::describe(o.tot) %>% print(digits=4)

## CI bootstrap with bca----
source("ci.reliability2.R",encoding="UTF-8")
rel.hk.a.bca <- ci.reliability2(data = dplyr::select(dp_survey,c(HK1:HK9,HK14:HK15)), N = nrow(dp_survey), type = "alpha",
                                interval.type = "bca", B = 10000, conf.level = 0.95) 
rel.hk.ot.bca <- ci.reliability2(data = dplyr::select(dp_survey,c(HK1:HK9,HK14:HK15)), N = nrow(dp_survey), type = "omega",
                                 interval.type = "bca", B = 10000, conf.level = 0.95) 
rel.pi.a.bca <- ci.reliability2(data = dplyr::select(dp_survey,PI1:PI20), N = nrow(dp_survey), type = "alpha",
                                interval.type = "bca", B = 10000, conf.level = 0.95) 
rel.pi.ot.bca <- ci.reliability2(data = dplyr::select(dp_survey,PI1:PI20), N = nrow(dp_survey), type = "omega",
                                 interval.type = "bca", B = 10000, conf.level = 0.95) 
