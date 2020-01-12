#################################################################################
#################################################################################
################### Script for HCN Meter Manuscript - PLOTS #####################
#################################################################################
#################################################################################

# Colin Richard Morrison 
# PhD Candidate
# The University of Texas at Austin 
# Department of Integrative Biology 
# Graduate Program In Ecology, Evolution and Behavior
# crmorrison@utexas.edu


getwd()
setwd("~/Desktop/HCN")



### ------------------------------------------------------------------------
### ------------------------------------------------------------------------
####  ----------- FIGURE 2 - Method comparison study 

# using Non-transformed [CN] values plot

comp=read.csv("method.comparison3.csv", header=T)
head(comp,17)
comp2=comp[-c(16),]
#

### Plot data from both experiments separately 
# set plot graphical parameters
par(oma = c(0, 1, 0, 0))# = c(Top, Right, Bottom, Left)
par(mfrow=c(1,2))
# set colors for different Passiflora species.
# cols = gray.colors(8) 
# plot CUP v Spec
plot(cupPAIR~spec.cup,data=comp2,xlab='',ylab=expression(paste("meter [CN] (", mu, "mol/g)")),
     main='A. Plastic Cup',pch=19,xlim=c(0,17),ylim=c(0,17))
R2.exp <- expression(paste(" ",R^2 ," = 0.63"))
text(x=15.0,y=1.5, labels=R2.exp,cex=1.0)
text(x=15.15,y=0.5, labels='P < 0.001',cex=1.0)
abline(lm(cupPAIR~spec.cup,data=comp), col="blue")
# plot MP v Spec
plot(mpPAIR~spec.mp,data=comp2,xlab='',ylab='',
     col='black', yaxt='n',main='B. Closed Mortar - Pestle',pch=19,xlim=c(0,17),ylim=c(0,17))
axis(side=2,labels=F) 
R2.exp2 <- expression(paste(" ",R^2 ," = 0.76"))
text(x=15.0,y=1.5, labels=R2.exp2,cex=1.0)
text(x=15.15,y=0.5, labels='P < 0.001',cex=1.0)
abline(lm(mpPAIR~spec.mp,data=comp), col="blue")
# plot tite
mtext(expression(paste("colorimetric [CN] (", mu, "mol/g)")), side = 3, outer = TRUE,  line = -30.25, cex=1.0)


### TRY  with log transformations of the MP and CUP data 
#comp=read.csv("method.comparison.csv", header=T)
#head(comp)
#colnames(comp)

#### MP vs SPEC
#comp2=read.csv("method.comparisonMP.csv", header=T)

# MP data
#mpLOG <- log(comp2$mpPAIR)
#mpspecLOG <- log(comp2$spec.mp)
# stats on log MP data
#corrlog=lm(mpLOG~mpspecLOG,data=comp2)
#summary(corrlog)
# Multiple R-squared:  0.5707,	Adjusted R-squared:  0.5548 
# F-statistic:  35.9 on 1 and 27 DF,  p-value: 2.164e-06

# CUP data
#colnames(comp3)
#cupLOG <- log(comp3$cupPAIR)
#cupspecLOG <- log(comp3$spec.cup)
# stats on log CUP data
#corrlog2=lm(cupLOG~cupspecLOG)
#summary(corrlog2)
# Multiple R-squared:  0.5499,	Adjusted R-squared:  0.5349 
# F-statistic: 36.65 on 1 and 30 DF,  p-value: 1.2e-06

#par(oma = c(2.5, 1, 1, 0.75))  # c(1, 1, 0, 0) = c(Top, Right, Bottom, Left)
#par(mfrow=c(1,2))

# natural log plot of these CUP data 
#plot(cupLOG~cupspecLOG,xlab='',ylab='meter log[CN]',
#     main='A. Solo Cup',pch=19,ylim=c(-3.2,2.6))
#abline(lm(mpLOG~mpspecLOG), col="blue")
#R2.exp <- expression(paste(" ",R^2 ," = 0.53"))
#text(x=-3.4,y=2.35, labels=R2.exp,cex=1.0)
#text(x=-3.3,y=1.9, labels='P < 0.001',cex=1.0)

# natural log plot of these MP data 
#plot(mpLOG~mpspecLOG,xlab='',ylab='',
#     main='B. Closed Mortar/Pestle',pch=19,ylim=c(-3.2,2.6))
#abline(lm(mpLOG~mpspecLOG), col="blue")
#R2.exp2 <- expression(paste(" ",R^2 ," = 0.55"))
#text(x=-3.4,y=2.35, labels=R2.exp2,cex=1.0)
#text(x=-3.3,y=1.9, labels='P < 0.001',cex=1.0)

# mtext(expression(bold('Method Validation')), side = 1, outer = T,  line = -28.25, cex=1.75)
#mtext('colorimetric log[CN]', side = 3, outer = TRUE,  line = -27.5, cex=1.0)


### ------------------------------------------------------------------------
### ------------------------------------------------------------------------
####  -----------FIGURE 3 - Dessicator x Cup Accuracy Study 
# Here I am regressing values of leaves that were sampled on with the dessicator,ethods and cup/meter methods. 

dess=read.csv('dessicate.csv', header=T)
dess2=dess[-c(1:2),] # outliers from first 2 attempts at method calibration experiment
head(dess)
colnames(dess)


# log10 transformed for the ratio calculation with y=mx+b equation
par(oma = c(0, 1, 0, 0)) # graphical parameters
plot(log10dessicator~log10cup,data=dess2,pch=19,col="black",xlim=c(),ylim=c(),
     xlab=expression(paste("cup - log10 [CN] (", mu, "mol/g)")),
     ylab=expression(paste("desiccator - log10 [CN] (", mu, "mol/g)")),
     main="")
abline(lm(log10dessicator~log10cup,data=dess2), lty=2,col="black")
# R^2 text 
r2.dess <- expression(paste(" ",R^2 ," = 0.97"))
text(x=-0.6,y=1.275, labels=r2.dess,cex=1.0)
#text(x=-0.59,y=1.225, labels='P < 0.001',cex=1.0)
text(x=-0.59,y=1.225, labels='y = 0.95x + 1.01',cex=1.0)
mtext(expression(bold("Cyanide Measurement Method Calibration")), side = 1, 
      outer = TRUE,  line = -30.00, cex=1.3)
mtext(expression(italic("Passiflora biflora")), side = 1, 
      outer = TRUE,  line = -28.75, cex=1.3)


## untransformed data 
#par(oma = c(0, 1, 0, 0)) # graphical parameters
#plot(dessicator~cup,data=dess2,pch=19,col="black",xlim=c(),ylim=c(),
#     xlab=expression(paste("cup [CN] (", mu, "mol/g)")),
#     ylab=expression(paste("desiccator [CN] (", mu, "mol/g)")),
#     main="Cyanide Measurement Method Calibration")
#abline(lm(dessicator~cup,data=dess2), lty=2,col="black")
## R^2 text 
#r2.dess <- expression(paste(" ",R^2 ," = 0.93"))
#text(x=0.3,y=19.5, labels=r2.dess,cex=1.0)
#text(x=0.311,y=18.5, labels='P < 0.001',cex=1.0)


### ------------------------------------------------------------------------
### ------------------------------------------------------------------------
####  -----------FIGURE 4 - Standard Curve with Amygdalin 
# Here I am creating cyanogenic reactions in the lab with known amounts of the commercially available 
#   cyanogenic glycoside amygdalin, and endogenous beta-glucosidase enzyme isolated from almond seeds
# Aliquots of the reaction mixture are placed in the plastic cup and measured with the HCN meter to 
#   create a standard curve for user reference. 

amy=read.csv('amygdalin.csv', header=T)
colnames(amy)
head(amy)

### subset cup and MP data for easy plotting
cup <- amy[which(amy$chamber == 'cup'), names(amy) %in% c('sample','chamber','ppmTOTAL','ppmAVG','mg','umol','ug.hcn')]
MP <- amy[which(amy$chamber == 'MP'), names(amy) %in% c('sample','chamber','ppmTOTAL','ppmAVG','mg','umol','ug.hcn')]

#plot
par(oma = c(0, 1, 0, 3)) # graphical parameters
plot(mg~ppmAVG,data=cup,pch=19,col="blue",xlim=c(),ylim=c(),
     xlab="HCN meter (ppm)",
     ylab="amygdalin (mg)",
     main="HCN Standard Curve")
points(mg~ppmAVG,data=MP,pch=19,col="red")
abline(lm(mg~ppmAVG,data=cup), lty=2,col="blue")
abline(lm(mg~ppmAVG,data=MP), lty=2,col="red")
# R^2 text 
r2.cup <- expression(paste(" ",R^2 ," = 0.97"))
text(x=135,y=3.35, labels=r2.cup,cex=1.0,col = "blue")
r2.MP <- expression(paste(" ",R^2 ," = 0.99"))
text(x=75,y=4.6, labels=r2.MP,cex=1.0,col = "red")
# linear equations text
text(x=135,y=3.1, labels='y = 0.031x - 0.0025',cex=1.0,col = "blue")
text(x=75,y=4.35, labels='y = 0.043x - 0.1977',cex=1.0,col = "red")
# overlay new 'plot' so that there is y-axis on right side
par(new = T)
plot(ug.hcn~ppmAVG,data=cup,pch=19,col='blue',axes=F, xlab='', ylab='')
axis(side = 4)
mtext(side = 4, line = 3, expression(paste("HCN (", mu, "g)")))

# legend
legend('bottomright', legend=c('cup','MP'),  
       text.font=1,lty=2, pch=19, lwd=1.0, 
       cex=1.0,col=c('blue','red'))



### ------------------------------------------------------------------------
### ------------------------------------------------------------------------
####  ----------- FIGURE 5 - Comparison with Engler & Gilbert (2007) colorimetric CN data 
eng=read.csv("engler.data.csv", header=T)
head(eng,10)
eng2<-eng[1:8,] # subset to remove unused rows

### ----- load color pallette 
library(viridis)
#cols <- viridis(8)
cols2 <-c("#440154FF", "#365C8DFF", "coral2", "#277F8EFF",
          "#1FA187FF", "#4AC16DFF","#9FDA3AFF", "#FDE725FF")

# change factor level order before plotting 
eng2$species <- factor(eng2$species, levels=c("AUR","BI","LOB","MEN","OER","PIT",
                                              "QUAD","VIT"),
                       labels=c("P.auriculata","P.biflora","P.lobata",
                                "P.menispermifolia","P.oerstedii","P.pittieri",
                                "P.quadrangularis","P.vitifolia"))

head(eng2,8)
leg<-eng2[,"species"]

# plot
par(oma = c(0, 1, 0, 0)) # graphical parameters 
plot(ENmean~PCmean.10,data=eng2,
     xlab=expression(paste("Smiley & Morrison Data [CN] (", mu, "mol/g)")),
     ylab=expression(paste("Engler & Gilbert (2007) Data [CN] (", mu, "mol/g)")),
     main='Study Results Comparison',pch=19,col = cols)
legend('bottomright', legend=leg,  
       text.font=3,lty=0, pch=19, lwd=2.5, 
       cex=.9,col=cols2)
abline(lm(ENmean~PCmean.10,data=eng),col="black",lty=2)
R2.exp4 <- expression(paste(" ",R^2 ," = 0.93"))
text(x=4.0,y=34.0, labels=R2.exp4,cex=1.0)
text(x=4.3,y=32.25, labels='P < 0.01',cex=1.0)



### ------------------------------------------------------------------------
### ------------------------------------------------------------------------
#### ----------- FIGURE 6 - Inter- and Intraspecific Variation Figure 
# Goal is to visualize intra- and interspecific variation in of all samples
# CONSIDER visualizing leaves separately from other tissues types (fruit, seeds, etc.)

all=read.csv("figure3data2.csv")
head(all)
colnames(all)
nrow(all) # 1808 observations 
levels(all$tissue.type) # variable names in column 'type'

### ----- load color pallette 
library(viridis)
color<-palette(viridis(20))
cols <- viridis(8)
cols2 <-c("#440154FF", "#365C8DFF", "coral2", "#277F8EFF",
          "#1FA187FF", "#4AC16DFF","#9FDA3AFF", "#FDE725FF")
#

library(ggplot2)
# change factor order of labels for legend before plotting
all$tissue.type <- factor(all$tissue.type, levels=c("new leaf","mature leaf","old leaf","stem","tendril","root","flower","fruit"),
                          labels=c("new leaf","mature leaf","old leaf","stem","tendril","root","flower","fruit"))
# plot with all tissues
hcn<-ggplot(all, aes(x=species, y=HCN, color= tissue.type, group="")) + 
  geom_point() + geom_jitter(position=position_jitter(0.2)) +
  ggtitle(expression(paste(italic("Passiflora "),"HCN Variation"))) +
  scale_y_continuous(trans='log2',labels = scales::number_format(accuracy = 0.001),breaks=c(0.001,0.01, 0.05, 0.2,0.5,1.0,2.0,5.0,10.0,20.0,40.0)) +
  scale_color_manual(values = cols2) +
  scale_fill_discrete(breaks=c("new leaf","mature leaf","old leaf","stem","tendril","root","flower","fruit")) +
  xlab("La Selva species") + ylab(expression(paste("log HCN (", mu, "mol/g)"))) + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        legend.title = element_blank(),
        legend.key = element_rect(fill = "white"),
        legend.box.background = element_rect(colour = "black",size = 1.15),
        axis.title.y = element_text(margin = margin(t = 20,r = 20,b = 20, l = 20)),
        axis.text.x = element_text(face = "italic", angle = 70,hjust=1),
        axis.title.x = element_text(margin = margin(t = 20)),
        plot.title = element_text(size = rel(1.75), hjust = 0.5,vjust=1),
        plot.margin = margin(t = 15, r = 90, b = 10, l = 0, unit = "pt"),
        axis.line = element_line(colour = "black"))
hcn

# plot with just new leaves 
# levels(all$tissue.type) # 'New Leaf'
# new <-all[which(all$tissue.type=='New Leaf'), 
#          names(all) %in% c('species','date','tissue.type','HCN')]
# head(new)
#
# hcn2<-ggplot(new, aes(x=species, y=HCN)) + 
#  geom_point() + geom_jitter(position=position_jitter(0.2)) +
#  ggtitle(expression(paste(italic("Passiflora "),"New Leaf HCN Variation"))) +
#  scale_y_continuous(trans='log2',labels = scales::number_format(accuracy = 0.001),breaks=c(0.001,0.01, 0.05, 0.2,0.5,1.0,2.0,5.0,10.0,20.0,40.0)) +
#  xlab("La Selva species") + ylab(expression(paste("HCN (", mu, "mol/g)"))) + 
#  theme(panel.grid.major = element_blank(), 
#        panel.grid.minor = element_blank(),
#        panel.background = element_blank(),
#        legend.title = element_blank(),
#        axis.title.y = element_text(margin = margin(t = 20,r = 20,b = 20, l = 20)),
#        axis.text.x = element_text(face = "italic", angle = 70,hjust=1),
#        axis.title.x = element_text(margin = margin(t = 20)),
#        plot.title = element_text(size = rel(1.75), hjust = 0.5,vjust=1),
#        plot.margin = margin(t = 15, r = 90, b = 10, l = 0, unit = "pt"),
#        axis.line = element_line(colour = "black"))
# hcn2
#


### ------------------------------------------------------------------------
### ------------------------------------------------------------------------
####  ----------- FIGURE 7 - HCN Kinetics Figure 
# This figure is to demonstrate fast release of HCN in certain Passiflora species, 
# such as P. ambigua (Panel A) and P. menispermiflora (Panel B). 
# AND.... the slow release of HCN in certain Passiflora species, such as P. coriacea (Panel B). 

# Call all the data into memory, apply time series and subset:
### P. coriacea release in minutes (SLOW RELEASE)
slow=read.csv("figure8dataCOR.csv")
names(slow) <- c("time","leaf 2","leaf 4","leaf 6","leaf 8")
head(slow)
# subset P. coriacea
library(zoo)
cor = zoo(ts(slow, frequency = 0.1, start = 0,end=180))
cor2 <- cor[,-1]
cor3 <- cor2[-c(18:19),]
head(cor2)

### P. ambigua release in minutes 
fast=read.csv("figure7dataAMB.csv")
names(fast) <- c("leaf 1","leaf 3","leaf 5","leaf 7","leaf 9","leaf 11")
head(fast)
#subset P. ambigua
fast2 = zoo(ts(fast, frequency = 0.2, start = 0))
fast3 <- fast2[,-1]
fast4 <- fast3[,-c(5:6)]

### P. menispermifolia release in seconds 
mensec=read.csv("menkin.csv")
head(mensec)
# subset
mensec2=mensec[,-1]
names(mensec2) <- c("mature leaf","new leaf")
mensec3 = zoo(ts(mensec2, frequency = 0.05, start = 0))

### ------------------------------------------------------------------------
# color palette 
library(RColorBrewer)
cols <- brewer.pal(4, "Spectral")

# plot both panels of figure 
layout(matrix(c(1,1,2,3), 2, 2, byrow = TRUE))
par(oma = c(1, 1, 0, 0))
# panel A (P. coriacea)
plot(cor3, plot.type="single", col = cols,lwd=2.5,
     xlab='',ylab=expression(paste("HCN (", mu, "mol/g)")), main='A. Slow HCN Release')
text(x=125,y=0.2,labels=expression(paste(italic("Passiflora coriacea"))),cex=1.2)
legend(x=148.25,y=0.38, colnames(cor3), col=cols, lty=1, lwd=2.5, cex=.6)
#
# panel B (P. ambigua)
plot(ambsec5, plot.type="single", col = cols,lwd=2.5,
     xlab='',ylab=expression(paste("HCN (", mu, "mol/g)")), main='')
text(x=365,y=0.2,labels=expression(paste(italic("P. ambigua"))),cex=0.8)
legend(x=272,y=0.353, colnames(ambsec5), col=cols, lty=1, lwd=2.5, cex=.6)
#
# panel C (P. menispermifolia)
plot(mensec3, plot.type="single", col = cols,lwd=2.5,
     xlab='',ylab="", main='')
text(x=310,y=1.7,labels=expression(paste(italic("P. menispermifolia"))),cex=0.8)
legend(x=271,y=3.12, colnames(mensec3), col=cols, lty=1, lwd=2.5, cex=0.6)
mtext('minutes after crushing leaf', side = 3, outer = TRUE,  line = -18, cex=0.8)
mtext('seconds after cruching leaf', side = 1, outer = TRUE,  line = -1, cex=0.8)
mtext(expression(bold('B. Fast HCN Release')), side = 1, outer = TRUE, line = -18, cex=1)
#



### ------------------------------------------------------------------------
### ------------------------------------------------------------------------
####  ----------- APPENDIX S9 - FIGURE - Intraspecific / Within Branch Figure 
# Goal is to visualize within branch HCN variation among individual P. auriculata plants
# column HCN1 is the data for which meter couldn't detect, so mass multiplied times lower detection limit (0.3ppm)

aur=read.csv("figure4dataAUR4.csv")

colnames(aur)
head(aur,40)
levels(aur$branch)

# load color palette
library(RColorBrewer)
colorblindFriendly = TRUE
cols <- brewer.pal(4, "Dark2")
  
library(ggplot2)

# scatterplot with colored points
# intra2<-ggplot(aur, aes(x=plant, y=logHCN,color=branch)) + 
#    geom_point() + guides(fill=FALSE) +
#    geom_jitter(position=position_jitter(0.3)) + scale_y_continuous(trans='log2') +
#    ggtitle(expression(paste("HCN Variation Among ", italic("P. auriculata "), "Individuals"))) + 
#    xlab("plant") + theme(plot.title = element_text(size = rel(1.5),hjust=0.5)) + 
#    scale_color_manual(values = cols) +
#    scale_fill_discrete(breaks=c("1","2","3","4")) +
#    ylab(expression(paste("log HCN (", mu, "mol/g)"))) + 
#    theme(panel.grid.major = element_blank(), 
#          panel.grid.minor = element_blank(),
#          panel.background = element_blank(), 
#          plot.title = element_text(vjust=5),
#          axis.line = element_line(colour = "black"),
#          axis.title.y = element_text(margin = margin(t = 20,r = 20,b = 20, l = 20)),
#          axis.title.x = element_text(margin = margin(t = 20)),
#          legend.title = element_blank(),
#          legend.key = element_rect(fill = "white"),
#          legend.box.background = element_rect(colour = "black",size = 1.15),
#          plot.margin = margin(t = 30, r = 20, b = 20, l = 0, unit = "pt")) 
#  intra2

# set color palette 
library(viridis)
cols <- viridis(5)

  # scatterplot with colored shapes --> SHOWS INTRA-BRANCH variation the BEST
intra3<-ggplot(aur, aes(x=plant, y=HCN,color=branch,shape=branch,size=branch)) + 
  geom_point() + guides(fill=FALSE) +
    geom_jitter(position=position_jitter(0.2)) + scale_y_continuous(trans='log10') +
    scale_color_manual(values = cols) +
    scale_shape_manual(values=c(3, 19, 17, 15)) +
    scale_size_manual(values=c(2,2,2,2))+
    ggtitle(expression(paste("HCN Spatial Variation Among and Within ", italic("P. auriculata ")))) + 
    xlab("individual plant") + theme(plot.title = element_text(size = rel(1.5),hjust=0.5)) + 
    ylab(expression(paste("log HCN (", mu, "mol/g)"))) + 
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          panel.background = element_blank(), 
          plot.title = element_text(vjust=5),
          axis.line = element_line(colour = "black"),
          axis.title.y = element_text(margin = margin(t = 20,r = 20,b = 20, l = 20)),
          axis.title.x = element_text(margin = margin(t = 20)),
          legend.title = element_blank(),
          legend.key = element_rect(fill = "white"),
          legend.box.background = element_rect(colour = "black",size = 1.15),
          plot.margin = margin(t = 30, r = 20, b = 20, l = 0, unit = "pt")) 
intra3

# scale_color_manual(values = cols) +
# 
#scale_fill_discrete(breaks=c("1","2","3","4")) +
# ,color=branch,shape=branch

  
### ------------------------------------------------------------------------
### ------------------------------------------------------------------------
#### -----------  APPENDIX S10 - FIGURE - HCN Induction with Mechanical Damage Figure  
# Goal is to visualize induction or lack thereof with P.ambigua (inducible) and P. auriculata (not-inducible)

# create objets out of AMB data vectors 
amb=read.csv("ambind.csv", header=T)
head(amb,40)
colnames(amb)

o <- amb$X0~amb$leaf.pos
o24<- amb$X24~amb$leaf.pos
o24tip <-amb$X24alttip~amb$leaf.pos
o24mid <-amb$X24altmid~amb$leaf.pos
o48<-amb$X48~amb$leaf.pos

# create objets out of AUR data vectors 
aur=read.csv("aurind2.csv", header=T)
head(aur,40)
colnames(aur)

z <- aur$X0~aur$leaf.pos
z24<- aur$X24~aur$leaf.pos
z24alt <-aur$X24alt~aur$leaf.pos
z48<-aur$X48~aur$leaf.pos
z48alt<-aur$X48alt~aur$leaf.pos

# plot both figures 
par(oma = c(1.0, 1.0, 1.5, 0.0)) # graphical parameters
par(mfrow=c(1,2)) # bottom, left, top, right
# AMB
plot(o,pch=4,col='black',ylim=c(0.3,3.1),xlim=c(1,11),xlab='',
     ylab=expression(paste("HCN (", mu, "mol/g)")),
     main=expression(paste("A. ", italic("Passiflora ambigua "))),
     xaxt='n')
points(o24,pch=16,col="black")
points(o24tip,pch=15,col="black")
points(o24mid,pch=17,col="black")
points(o48,pch=1,col='black',bg='black')
legend('topright', legend=c("0 h cut leaf","24 h cut leaf","24 h alternate tip",
                                "24 h alternate mid-vein","48 h cut leaf"), 
       pch=c(4,16,15,17,1),col = c("black", "black", "black", "black","black"),
       lty=c(NA,NA,NA,NA,NA),lwd=1.0, cex=0.8)
axis(1, xaxp=c(1, 11, 10), las=1)
# AUR
plot(z,pch=4,ylim=c(0.0,1.0),xlim=c(0,12),xlab='',ylab='',
     main=expression(paste("B. ", italic("Passiflora auriculata "))))
points(z24,pch=16,col="black",size=5.0)
points(z24alt,pch=15,col="black")
points(z48,pch=17,col="black")
points(z48alt,pch=1,col="black")
legend('topleft', legend=c("0 h","24 h cut leaf","24 h alternate leaf",
                                  "48 h cut leaf","48 h alternate leaf"), 
       pch=c(4,16,15,17,1),col = c("black", "black", "black", "black","black"),
       lty=c(NA,NA,NA,NA,NA), lwd=1.0, cex=0.8)
# plot title
mtext("Variation in HCN Inducibility Over Time", side = 1, 
      outer = TRUE,  line = -29.0, cex=1.3)
# x-axis title 
mtext('leaf position along branch (starting with tip)', side = 3, outer = TRUE,  line = -28.0, cex=1.0)



### ------------------------------------------------------------------------
### ------------------------------------------------------------------------
#### ----------- APPENDIX A 11 - FIGURE - P. biflora HCN spatial/temporal distribution   
# Goal here is to visualizae the distribution of HCN in leaves over time (0 h to 54 h)

quartz()
par(oma = c(1.0, 1.0, 1.5, 1.0))
par(mfrow=c(2,1))

dist=read.csv("distribution.csv", header=T)
head(dist)
colnames(dist)

# dist$hour<-as.numeric(dist$hour)
# ggplot(dist, aes(x=leaf, y=HCN, group=hour)) +
#  geom_point()

# create objects out of leaves sampled at different times
b0 <- dist[c(1:4),]
b14.5<- dist[c(5:7),]
b24 <-dist[c(8:10),]
b54 <-dist[c(11:12),]

# set color palette 
library(viridis)
cols <- viridis(24) # "#440154FF" "#3B528BFF" "#21908CFF" "#5DC863FF" 

# panel A
plot(HCN~leaf,data=b0,pch=19,col="#440154FF",ylim=c(0,13),xlim=c(0,25),
     xlab="",ylab=expression(paste("HCN (", mu, "mol/g)")),
     main=expression(paste("A. Distribution of ", italic("P. biflora")," Leaf HCN with Age and Sampling Time")))
points(HCN~leaf,data=b14.5,pch=19,col="#228B8DFF")
points(HCN~leaf,data=b24,pch=19,col="#6FCF57FF")
points(HCN~leaf,data=b54,pch=19,col="#C4E021FF")
legend('topright', legend=c("cut 0 h","cut 14.5 h","cut 24 h","cut 54 h"), 
       pch=19,col = c("#440154FF","#228B8DFF", "#6FCF57FF", "#C4E021FF"),
       lty=c(NA,NA,NA,NA), lwd=2.5, cex=0.8)

# panel B 
dyn=read.csv("dynamic.csv", header=T)
dyn2=dyn[-c(13,27),] # redundant measurements
head(dyn2,30)
# create objects out of leaves sampled at different times
b2 <- dyn2[1:3,]
b14<- dyn2[4:11,]
b24 <-dyn2[12:26,]

# PLOT
# par(oma = c(0, 1, 0, 0)) # graphical parameters
plot(biflora~leaf,data=b2,pch=19,col="red",ylim=c(0,7),xlim=c(0,27),
     xlab="leaf position",ylab=expression(paste("HCN (", mu, "mol/g)")),
     main=expression(paste("B. Dynamic HCN Activity in ", italic("P. biflora"))))
points(biflora~leaf,data=b14,pch=19,col="blue")
points(biflora~leaf,data=b24,pch=19,col="black")

legend('topright', legend=c("2 h cut leaf","14 h cut leaf","24 h cut leaf"), 
       pch=19,col = c("red", "blue", "black"),
       lty=c(NA,NA,NA), lwd=2.5, cex=0.8)




### ------------------------------------------------------------------------
### ------------------------------------------------------------------------
#### ----------- APPENDIX S12 - FIGURE - Heliconius doris Feeding Iduced Reduction in Cyanogenesis
# Goal is to visualize induction of HCN by H.doris caterpillars feeding on P. ambigua leaves

sync=read.csv("Hdoris.feeding.csv", header=T)
head(sync)
sync2<-sync[-c(21:23),]
nrow(sync)
#94
sum(sync$treatment == 'no herbivory') # 47
sum(sync$treatment == 'larvae feeding') # 47

library(ggplot2)
induce<-ggplot(sync2, aes(x=treatment, y=HCN,color=treatment)) + 
  geom_boxplot() + guides(fill=FALSE) +
  geom_jitter(position=position_jitter(0.1)) + 
  labs(title='Feeding-Induced Cyanogenesis Reduction',
       subtitle=expression(paste("by " ,italic("H. doris "), "Larvae"))) +
  annotate(geom="text", x=1.5, y=1.35, label="***",
           color="black",size=8) +
  xlab("treatment") + theme(plot.title = element_text(size = rel(1.5),hjust=0.5)) + 
  scale_color_manual(values = c("grey28","grey48")) +
  scale_y_continuous(labels = scales::number_format(accuracy = 0.01),breaks=c(0.0,0.25, 0.5, 0.75,1.0,1.25,1.5)) +
  scale_fill_discrete(breaks=c("larvae feeding","no herbivory")) +
  ylab(expression(paste("HCN (", mu, "mol/g)"))) + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        plot.subtitle = element_text(size=14,hjust = 0.5,vjust=8),
        legend.position = "none",
        plot.title = element_text(size=14,vjust=8),
        axis.line = element_line(colour = "black"),
        axis.title.y = element_text(margin = margin(t = 20,r = 20,b = 20, l = 20)),
        axis.title.x = element_text(margin = margin(t = 20)),
        plot.margin = margin(t = 30, r = 20, b = 20, l = 0, unit = "pt")) 
        
induce



################# FIGURE not in main text ####################################


### ------------------------------------------------------------------------
### ------------------------------------------------------------------------
####  ----------- FIGURE S1 - Spectrophotometer (intra-leaf) comparison 
# Here I am regressing the [CN] values from an individual leaf against one another to assess inter-leaf variation 
    #  .... and potential variation inherenet to method. 

# comp=read.csv("method.comparison.csv", header=T)
comp=read.csv("method.comparison3.csv", header=T)
head(comp,100)
colnames(comp)


#cols = gray.colors(8) # set colors for different Passiflora species.
par(oma = c(0, 1, 0, 0)) # graphical parameters 
plot(spec.cup~spec.mp,data=comp,
     xlab=expression(paste("left side leaf [CN] (", mu, "mol/g)")),
     ylab=expression(paste("right side leaf [CN] (", mu, "mol/g)")),
     main='',pch=19)
R2.exp3 <- expression(paste(" ",R^2 ," = 0.54"))
text(x=3.0,y=30.0, labels=R2.exp3,cex=1.0)
text(x=3.0,y=28.5, labels='P < 0.01',cex=1.0)
abline(lm(spec.cup~spec.mp,data=comp), col="black",lty=2)
mtext(expression(bold('Colorimetric Data Comparison')), side = 1, outer = T,  line = -30.5, cex=1.0)
#mtext('Intra-Leaf Variation', side = 1, outer = T,  line = -29.5, cex=1.0)



### ------------------------------------------------------------------------
#### ----------- Table S2 - Sample size of each tissue type by species
#### ---- columns = tissue types, rows = species, cell content = N
# 
summary(all$species)
#        P.ambigua      P.arbelaezii      P.auriculata         P.biflora        P.coriacea 
#         406                92               451               446                84 
#        P.costaricensis   P.lobata   P.menispermifolia       P.oerstedii        P.pittieri 
#          43                15                73                17               118 
#        P.quadrangularis       P.vitifolia 
#                   39                24 

amb <- all[which(all$species == 'P.ambigua'), names(all) %in% c('species','tissue.type','HCN')]
amb2<-summary(amb$tissue.type)
#
arb <- all[which(all$species == 'P.arbelaezii'), names(all) %in% c('species','tissue.type','HCN')]
arb2<-summary(arb$tissue.type)
#
aur <- all[which(all$species == 'P.auriculata'), names(all) %in% c('species','tissue.type','HCN')]
aur2<-summary(aur$tissue.type)
#
bi <- all[which(all$species == 'P.biflora'), names(all) %in% c('species','tissue.type','HCN')]
bi2<-summary(bi$tissue.type)
#
cor <- all[which(all$species == 'P.coriacea'), names(all) %in% c('species','tissue.type','HCN')]
cor2<-summary(cor$tissue.type)
#
cos <- all[which(all$species == 'P.costaricensis'), names(all) %in% c('species','tissue.type','HCN')]
cos2<-summary(cos$tissue.type)
#
lob <- all[which(all$species == 'P.lobata'), names(all) %in% c('species','tissue.type','HCN')]
lob2<-summary(lob$tissue.type)
#
men <- all[which(all$species == 'P.menispermifolia'), names(all) %in% c('species','tissue.type','HCN')]
men2<-summary(men$tissue.type)
#
oer <- all[which(all$species == 'P.oerstedii'), names(all) %in% c('species','tissue.type','HCN')]
oer2<-summary(oer$tissue.type)
#
pit <- all[which(all$species == 'P.pittieri'), names(all) %in% c('species','tissue.type','HCN')]
pit2<-summary(pit$tissue.type)
#
qua <- all[which(all$species == 'P.quadrangularis'), names(all) %in% c('species','tissue.type','HCN')]
qua2<-summary(qua$tissue.type)
#
vit <- all[which(all$species == 'P.vitifolia'), names(all) %in% c('species','tissue.type','HCN')]
vit2<-summary(vit$tissue.type)


plants<-rbind(amb2,arb2,aur2,bi2,cor2,cos2,lob2,
              men2,oer2,pit2,qua2,vit2)
# plants2<-t(plants)  # species as rows 

levels(all$species)
rownames(plants) <- c("P.ambigua","P.arbelaezii","P.auriculata","P.biflora",        
                      "P.coriacea","P.costaricensis","P.lobata","P.menispermifolia",
                      "P.oerstedii","P.pittieri","P.quadrangularis","P.vitifolia")

# write plants data to A Tab Delimited Text File
write.table(plants,sep="\t") 



#################################################################################
#################################################################################
