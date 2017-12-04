
## Script dato coprofagos

setwd()
#First we read our data matrix and assign it to the "data" vector
datos <- read.csv("morfo.csv", header =T)
class(datos)

#assign to a vector each column of the data.frame "data"
# each vector corresponds to the abundance of the different Morphs (28) in each sampling site
PinarA <- datos$Pinar.A
BsecunB <- datos$Segundario.B
Yar <- datos$Bosque.Yarigui.es

# Now we graph, and we show the 3 graphs in a single image with the function "par ()"
png("1.png",width = 790,height = 490)
par(mfrow=c(1,3))
barplot(PinarA,space=0,width = 1.1, main = "Pinar A",ylab = "Abundancia",  
        col = c(1:28),names=c(seq(1:28)),ylim=c(0,200),xlim = c(1,28),bty="n",cex.lab=1.5,cex.axis = 1.2,cex.names = 1.2)
barplot(BsecunB,space=0,width = 1.1,main = "Bosque secundario",xlab = "Morfos",
        col = c(1:28),names=c(seq(1:28)), ylim=c(0,200),xlim = c(1,28),bty="n",cex.lab=1.5,cex.axis = 1.2,cex.names = 1.2)
barplot(Yar,space=0,width = 1.1,main = "Yariguíes",
        col = c(1:28),names=c(seq(1:28)), ylim=c(0,200),xlim = c(1,28),bty="n",cex.axis = 1.2,cex.names = 1.2)
dev.off()
# Now in the next part we are going to calculate the morph richness of
# coprophagous beetles, and of arthropod orders in the three sampling sites of the Naples farm
# We read the data matrix and assign it to the "dat" vector
dat <- read.csv("riq.csv")
# class of the vector "dat"
class(dat)
#Now with the replace () function we replace in the data.frame the addendum of each morph
# by presence or presence at each site, leaving 0 when it is not found and 1 when present, and
# we assign it in the "morf" vector
morf<-replace(dat, dat>0, 1)
class(morf)
# now with the function apply (x, MARGIN, FUNC) we perform the sumated rows of the vector "morf" of class data.frame, for
#calculate the wealth of each of the sites
riq<-apply(morf, 1, sum)
riq # EL site 1 (Pinar A), with riqieza of 23 morphs, Secondary Forest B with 21, and Yariguíes with 20
# The above was done in the same way to see the richness of the orders of arthropods
# we graph in riches of morphs "riq", and rich orders "riqord" in a same image
png("2.png", width = 790,height = 490)
par(mfrow=c(1,2))
Graphic graph of morph wealth
barplot(riq, beside = TRUE, border = "white", 
        col = c("darkgreen","forestgreen","chartreuse3"), 
        main = "Riqueza de morfos de escarabajos corprófagos", ylab = "No.Morfos",xlab = "Sitios de muestreo",
        names=c("Pinar A","Bosque secundario","Yariguíes"), ylim=c(0,28),bty="n")
#data for wealth of arthropod orders
ord <- read.csv("riqord.csv")
ord
ordenes<-replace(ord, ord>0, 1)
riqord <- apply(ordenes,1, sum)
riqord
# graph of riches of arthropod orders
barplot(riqord, beside = TRUE, border = "white", 
      col = c("red","orange","yellow"), 
      main = "Órdenes de artrópodos", ylab = "No. Órdenes",xlab = "Sitios de muestreo",
      names=c("Pinar A","Bosque secundario","Yariguíes"), ylim=c(0,28),bty="n")

dev.off()
## We will now calculate Shannon and Simpson diversity indices with morph data found at each site
# for this we need the package "vegan"
#  load "vegan"
library(vegan)
# read our data matrix
dat <- read.csv("riq.csv")
# Now with the diversity function () we calculate the Shannon index
?diversity
# Shannon's equity index
# Assume that all species are represented in the samples and that all
#individuals were sampled at random. You can acquire values between zero (0) when there is
#a single species and the logarithm of S when all species are represented by
#the same number of individuals. It can be strongly influenced by the species
# more abundant.
Shannon<-diversity(dat, index = "shannon")
Shannon
png("3.png", width = 700,height = 700)
par(mfrow=c(2,2))
# Graphed species richness index vs Shannon
plot(riq,Shannon,type = "p", col=c("darkgreen","forestgreen","chartreuse3"),
     xlab="species richness", ylab="Shannon Index",pch=16, cex=2)
# index of shannon in each site
barplot(Shannon,col = c("darkgreen","forestgreen","chartreuse3"), 
        main = "Índice de Shannon", ylab = " Shannon index",xlab = "Sitios de muestreo",
        names=c("Pinar A","Bosque secundario","Yariguíes"),bty="n")


# species richness, which previously had already been shown
Riqueza<-specnumber(dat)
Riqueza
# Simpson Index
# Show the probability that two individuals drawn at random from
#a sample correspond to the same species.

x<-diversity(dat, index = "simpson")
Simpson <- 1-x

# Graphed species richness index vs Simpson
plot(riq,Simpson,type = "p",xlab="species richness", col=c("darkgreen","forestgreen","chartreuse3"),
     ylab="Simpson Index",pch=16, cex=2)
# Simpson index in each site
barplot(Simpson,col = c("darkgreen","forestgreen","chartreuse3"), 
        main = "Índice de Simpson", ylab = " Simpson index",xlab = "Sitios de muestreo",
        names=c("Pinar A","Bosque secundario","Yariguíes"),bty="n")

dev.off()

