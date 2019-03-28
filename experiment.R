## Citation----------------------
# Matsuyoshi D, Watanabe K (submitted) People have modest, not good, 
# insight into their face recognition ability: 
# a comparison between developmental prosopagnosia questionnaires.

library("dplyr")
library("ggplot2")
library("psych")
library("cocor")
library("MBESS")
library("rgr")
library("coin")
colorset <- c("#FF0000","#0000FF")

## Load DATA ----------------------
qdata <- read.table("facememory_questionnaire.data",header=T)
bdata <- read.table("facememory_behavior.data",header=T)

## Preprocessing ----
bdata$mean <- apply(bdata[,c("Stage1","Stage2","Stage3")],1,sum)/72

# Reverse items
HKcols = c("HK1", "HK4", "HK6", "HK7", "HK8", "HK9", "HK10", "HK12", "CONF")
qdata[,HKcols] <- 6 - qdata[,HKcols]
PIcols = c("PI8", "PI9", "PI13", "PI17", "PI19")
qdata[,PIcols] <- 6 - qdata[,PIcols]

# Create HK summary statistics
qdata$HKDP <- apply(qdata[,paste("HK",c(1:9,14:15),sep="")],1,sum)
qdata$HKDummy <- apply(qdata[paste("HK",c(10:13),sep="")],1,sum)
qdata$HKall <- apply(qdata[,paste("HK",1:15,sep="")],1,sum)
# Create PIDP summary statistics
qdata$PIDP <- apply(qdata[,paste("PI",1:20,sep="")],1,sum)

qdata$All <- qdata$HKDP + qdata$PIDP + qdata$CONF

## Data summary ----
summary(qdata)
summary(qdata[qdata$Sex=="Female",])
summary(qdata[qdata$Sex=="Male",])
psych::describe(qdata)
psych::describeBy(qdata,qdata$Sex)

summary(bdata)
summary(bdata[bdata$Sex=="Female",])
summary(bdata[bdata$Sex=="Male",])
psych::describe(bdata)
psych::describeBy(bdata,bdata$Sex)

## Correlations ----
# Pearson
cor.pears <- cor.test(qdata$PIDP,qdata$HKDP)
cor.pears
cor.pears.HK <- cor.test(qdata$HKDP,bdata$mean)
cor.pears.HK
cor.pears.PI <- cor.test(qdata$PIDP,bdata$mean)
cor.pears.PI
# Spearman
cor.spear.HK <- cor.test(qdata$HKDP,bdata$mean,method="spearman")
cor.spear.HK
cor.spear.PI <- cor.test(qdata$PIDP,bdata$mean,method="spearman")
cor.spear.PI
# All data
cor.pears.All <- cor.test(qdata$All,bdata$mean)
cor.pears.All
cor.spear.All <- cor.test(qdata$All,bdata$mean,method="spearman")
cor.spear.All

cor.spear.HK$conf.int[1] <- tanh(atanh(cor.spear.HK$estimate) - qt(0.95/2 + .5, length(qdata$HKDP)) * 1/sqrt(length(qdata$HKDP) - 3) )
cor.spear.HK$conf.int[2] <- tanh(atanh(cor.spear.HK$estimate) + qt(0.95/2 + .5, length(qdata$HKDP)) * 1/sqrt(length(qdata$HKDP) - 3) )
cor.spear.PI$conf.int[1] <- tanh(atanh(cor.spear.PI$estimate) - qt(0.95/2 + .5, length(qdata$PIDP)) * 1/sqrt(length(qdata$PIDP) - 3) )
cor.spear.PI$conf.int[2] <- tanh(atanh(cor.spear.PI$estimate) + qt(0.95/2 + .5, length(qdata$PIDP)) * 1/sqrt(length(qdata$PIDP) - 3) )

coin::spearman_test(qdata$HKDP ~ bdata$mean, distribution = "approximate")
coin::spearman_test(qdata$PIDP ~ bdata$mean, distribution = "approximate")

## MISC ----
# Correlation with all items
cor.test(qdata$All,bdata$mean)
# Correlation with dummy items
cor.test(qdata$HKDummy,bdata$mean)
cor.test(qdata$HK10,bdata$mean)
cor.test(qdata$HK11,bdata$mean)
cor.test(qdata$HK12,bdata$mean)
cor.test(qdata$HK13,bdata$mean)



## HK11 ggplot ----------------------
QBdata <- data.frame(HKDP=qdata$HKDP,PIDP=qdata$PIDP,Sex=qdata$Sex,Behavior=bdata$mean*100)
cortextHK <- sprintf("r = %.4f [95%% CI: %.4f, %.4f]\n(p = %g)\n%s = %.4f [95%% CI: %.4f, %.4f]\n(p = %g)",cor.pears.HK$estimate,cor.pears.HK$conf.int[1],cor.pears.HK$conf.int[2],cor.pears.HK$p.value,paste("\u03C1"),cor.spear.HK$estimate,cor.spear.HK$conf.int[1],cor.spear.HK$conf.int[2],cor.spear.HK$p.value,paste("\u03C1"))
theme0 <- function(...) theme( legend.position = "none",
                               panel.background = element_blank(),
                               panel.grid.major = element_blank(),
                               panel.grid.minor = element_blank(),
                               panel.spacing = unit(0,"null"),
                               axis.ticks = element_blank(),
                               axis.text.x = element_blank(),
                               axis.text.y = element_blank(),
                               axis.title.x = element_blank(),
                               axis.title.y = element_blank(),
                               panel.border=element_rect(color=NA),...)

hk_p1A <- ggplot(QBdata, aes(x=HKDP, y=Behavior,colour=Sex)) + 
  geom_point(shape=16,size=3,alpha=1) +
  # plot a regression line
  geom_smooth(method=lm, se=FALSE) +
  # scale settings
  scale_x_continuous("HK11", expand=c(0,0), limits = c(10, 45), breaks=seq(10, 45, by=10)) +
  scale_y_continuous("Face Memory (%)", expand=c(0,0), limits = c(40, 100), breaks=seq(40, 100, by=10)) +
  scale_colour_manual(values=colorset) + 
  scale_fill_manual(values=colorset) + 
  # theme settings  
  #coord_fixed()  + 
  #geom_abline(slope=slp,intercept=int,color="black",size=1) + #PCA
  #geom_abline(slope=result$coefficients[2],intercept=result$coefficients[1],color="black",size=1) + #LM
  annotate("text", x=-Inf,y=Inf,hjust=-.2,vjust=1.1, label=cortextHK) +
  theme_bw() +
  theme(legend.position="none",plot.margin=unit(c(0,0,0,0),"points")) +
  theme(plot.background = element_blank(),
        panel.background = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(),
        axis.title.x = element_text(face="bold", size=12),
        axis.title.y = element_text(face="bold", size=12, angle=90),
        axis.text.y=element_text(angle=0, hjust=0.5), # rotate the Y axis text anticlockwise 90 degrees, and centre it (0 left, 0.5 centre, 1 right)
        legend.title = element_blank(),
        legend.key.size = unit(1.5, "lines"),
        legend.position = c(0.85,0.15),
        legend.key = element_blank() # provides a border to the coloured squares in the legend
  )
hk_p1A


## PI20 ggplot ----------------------
cortextPI <- sprintf("r = %.4f [95%% CI: %.4f, %.4f]\n(p = %g)\n%s = %.4f [95%% CI: %.4f, %.4f]\n(p = %g)",cor.pears.PI$estimate,cor.pears.PI$conf.int[1],cor.pears.PI$conf.int[2],cor.pears.PI$p.value,paste("\u03C1"),cor.spear.PI$estimate,cor.spear.PI$conf.int[1],cor.spear.PI$conf.int[2],cor.spear.PI$p.value,paste("\u03C1"))

pi_p1A <- ggplot(QBdata, aes(x=PIDP, y=Behavior,colour=Sex)) + 
  geom_point(shape=16,size=3,alpha=1) +
  # plot a regression line
  geom_smooth(method=lm, se=FALSE) +
  # scale settings
  scale_x_continuous("PI20", expand=c(0,0), limits = c(25, 85), breaks=seq(30, 80, by=10)) +
  scale_y_continuous("Face Memory (%)", expand=c(0,0), limits = c(40, 100), breaks=seq(40, 100, by=10)) +
  scale_colour_manual(values=colorset) + 
  scale_fill_manual(values=colorset) + 
  # theme settings  
  #coord_fixed()  + 
  #geom_abline(slope=slp,intercept=int,color="black",size=1) + #PCA
  #geom_abline(slope=result$coefficients[2],intercept=result$coefficients[1],color="black",size=1) + #LM
  annotate("text", x=-Inf,y=Inf,hjust=-.2,vjust=1.1, label=cortextPI) +
  theme_bw() +
  theme(legend.position="none",plot.margin=unit(c(0,0,0,0),"points")) +
  theme(plot.background = element_blank(),
        panel.background = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(),
        axis.title.x = element_text(face="bold", size=12),
        axis.title.y = element_text(face="bold", size=12, angle=90),
        axis.text.y=element_text(angle=0, hjust=0.5), # rotate the Y axis text anticlockwise 90 degrees, and centre it (0 left, 0.5 centre, 1 right)
        legend.title = element_blank(),
        legend.key.size = unit(1.5, "lines"),
        legend.position = c(0.85,0.15),
        legend.key = element_blank() # provides a border to the coloured squares in the legend
  )
pi_p1A

## Sex differences in behavior ----
psych::describe(QBdata)
psych::describeBy(QBdata,QBdata$Sex)
t.test(QBdata$HKDP[qdata$Sex=="Female"],QBdata$HKDP[qdata$Sex=="Male"],var.equal = T)
t.test(QBdata$PIDP[qdata$Sex=="Female"],QBdata$PIDP[qdata$Sex=="Male"],var.equal = T)

x <- QBdata$Behavior[qdata$Sex=="Female"]
y <- QBdata$Behavior[qdata$Sex=="Male"]
t.test(x,y,var.equal = T)
md  <- mean(x,na.rm=T) - mean(y,na.rm=T)
diff_sd <- sqrt(((length(x)-1)*var(x,na.rm=T)+(length(y)-1)*var(y,na.rm=T))/(length(x)+length(y)-2))  ## denominator -2
smd <- md/diff_sd # d
MBESS::ci.smd(smd=smd,n.1=length(x), n.2=length(y))

rgr::gx.ks.test(QBdata$HKDP[qdata$Sex=="Female"],QBdata$HKDP[qdata$Sex=="Male"])
rgr::gx.ks.test(QBdata$PIDP[qdata$Sex=="Female"],QBdata$PIDP[qdata$Sex=="Male"])
rgr::gx.ks.test(QBdata$Behavior[qdata$Sex=="Female"],QBdata$Behavior[qdata$Sex=="Male"])

## Questionnaire difference (HK11 vs PI20) in behavior-score correlation
cocor::cocor.dep.groups.overlap(cor(bdata$mean,qdata$PIDP),
                                cor(bdata$mean,qdata$HKDP),
                                cor(qdata$PIDP,qdata$HKDP),
                                length(bdata$mean))

## Sex differences in the score-behavior correlation
# HK
cor.test(qdata$HKDP[qdata$Sex=="Female"],bdata$mean[qdata$Sex=="Female"])
cor.test(qdata$HKDP[qdata$Sex=="Male"],bdata$mean[qdata$Sex=="Male"])
cocor::cocor.indep.groups(cor(qdata$HKDP[qdata$Sex=="Female"],bdata$mean[qdata$Sex=="Female"]),
                          cor(qdata$HKDP[qdata$Sex=="Male"],bdata$mean[qdata$Sex=="Male"]),
                          sum(qdata$Sex=="Female"),sum(qdata$Sex=="Male"))
# PI
cor.test(qdata$PIDP[qdata$Sex=="Female"],bdata$mean[qdata$Sex=="Female"])
cor.test(qdata$PIDP[qdata$Sex=="Male"],bdata$mean[qdata$Sex=="Male"])
cocor::cocor.indep.groups(cor(qdata$PIDP[qdata$Sex=="Female"],bdata$mean[qdata$Sex=="Female"]),
                          cor(qdata$PIDP[qdata$Sex=="Male"],bdata$mean[qdata$Sex=="Male"]),
                          sum(qdata$Sex=="Female"),sum(qdata$Sex=="Male"))
