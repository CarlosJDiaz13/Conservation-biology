
## Script dato coprofagos

setwd("~/Descargas/informeconservacion")
#Primero leemos nuestra matriz de datos y la asignamos al vector "datos"
datos <- read.csv("morfo.csv", header =T)
class(datos)

#asignamos a un vector cada columna del data.frame "datos"
# cada vector correponde a l abundancia de los diferentes Morfos(28) en cada sitio de muestreo
PinarA <- datos$Pinar.A
BsecunB <- datos$Segundario.B
Yar <- datos$Bosque.Yarigui.es

# ahora graficamos, y mostramos las 3 graficas en una sola imagne con la funcion "par()"
png("1.png",width = 790,height = 490)
par(mfrow=c(1,3))
barplot(PinarA,space=0,width = 1.1, main = "Pinar A",ylab = "Abundancia",  
        col = c(1:28),names=c(seq(1:28)),ylim=c(0,200),xlim = c(1,28),bty="n",cex.lab=1.5,cex.axis = 1.2,cex.names = 1.2)
barplot(BsecunB,space=0,width = 1.1,main = "Bosque secundario",xlab = "Morfos",
        col = c(1:28),names=c(seq(1:28)), ylim=c(0,200),xlim = c(1,28),bty="n",cex.lab=1.5,cex.axis = 1.2,cex.names = 1.2)
barplot(Yar,space=0,width = 1.1,main = "Yariguíes",
        col = c(1:28),names=c(seq(1:28)), ylim=c(0,200),xlim = c(1,28),bty="n",cex.axis = 1.2,cex.names = 1.2)
dev.off()
# Ahora en la siguiente parte vamos calcular la riqueza de morfos de 
# escarabajos coprofagos, y de orddenes de artropodos en los tres sitios de muestreo de la Finca Napoles
#Leemos la matrix de datos y la asignamos al vector "dat"
dat <- read.csv("riq.csv")
# clase del vector "dat"
class(dat)
#ahora con la funcion replace() remplazamos en el data.frame la adundancia de cada morfo
# por presencia o asusencia en cada sitio, dejando 0 cuando no se encuntra y 1 cuando esta presente, y
# lo asignamos en del vector "morf"
morf<-replace(dat, dat>0, 1)
class(morf)
# ahora con la funcion apply(x,MARGIN,FUNC) realizamos la sumade las filas del vector "morf" de clase data.frame, para 
#calcular la riqueza de cada uno de los sitios
riq<-apply(morf, 1, sum)
riq # EL sitio 1 (Pinar A), con riqieza de 23 morfos, Bosque secundario B con 21, y Yariguies con 20
# Lo anterior se realizo de la isma forma para ver la riqueza de los ordenes de artropodos
# graficamos en riqueza de morfos "riq", y riqueza ordenes "riqord" en una misma imagen 
png("2.png", width = 790,height = 490)
par(mfrow=c(1,2))
# grafica de riqueza de morfos
barplot(riq, beside = TRUE, border = "white", 
        col = c("darkgreen","forestgreen","chartreuse3"), 
        main = "Riqueza de morfos de escarabajos corprófagos", ylab = "No.Morfos",xlab = "Sitios de muestreo",
        names=c("Pinar A","Bosque secundario","Yariguíes"), ylim=c(0,28),bty="n")

#datos para riqueza de ordenes de artropodos
ord <- read.csv("riqord.csv")
ord
ordenes<-replace(ord, ord>0, 1)
riqord <- apply(ordenes,1, sum)
riqord
# grafida de riqueza de ordenes de artropodos
barplot(riqord, beside = TRUE, border = "white", 
      col = c("red","orange","yellow"), 
      main = "Órdenes de artrópodos", ylab = "No. Órdenes",xlab = "Sitios de muestreo",
      names=c("Pinar A","Bosque secundario","Yariguíes"), ylim=c(0,28),bty="n")

dev.off()
## Ahora calcularemos los indeces de diversidad Shannon y Simpson con datos de morfos encontrados en cada sitio
# para esto necesitamos el paquete "vegan"
# cargamos "vegan"
library(vegan)
# Leemos nuestra matrix de datos
dat <- read.csv("riq.csv")
# Ahora con la funcion diversity() calculamos el indice de Shannon
?diversity
#indice de equidad de Shannon
#Asume que todas las especies están representadas en las muestras y que todos los
#individuos fueron muestreados al azar. Puede adquirir valores entre cero (0) cuando hay
#una sola especie y el logaritmo de S cuando todas las especies están representadas por
#el mismo número de individuos. Puede verse fuertemente influenciado por las especies
#más abundantes.
Shannon<-diversity(dat, index = "shannon")
Shannon
png("3.png", width = 700,height = 700)
par(mfrow=c(2,2))
# graficamos riqueza de especies vs indice de Shannon
plot(riq,Shannon,type = "p", col=c("darkgreen","forestgreen","chartreuse3"),
     xlab="species richness", ylab="Shannon Index",pch=16, cex=2)
# indice de shannon en cada sitio
barplot(Shannon,col = c("darkgreen","forestgreen","chartreuse3"), 
        main = "Índice de Shannon", ylab = " Shannon index",xlab = "Sitios de muestreo",
        names=c("Pinar A","Bosque secundario","Yariguíes"),bty="n")


# riqueza de espcies, que anteriormente ya se habia mostrado
Riqueza<-specnumber(dat)
Riqueza
# Indice de Simpson
#Muestra la probabilidad de que dos individuos sacados al azar de
#una muestra correspondan a la misma especie.

x<-diversity(dat, index = "simpson")
Simpson <- 1-x

# graficamos riqueza de especies vs indice de Simpson
plot(riq,Simpson,type = "p",xlab="species richness", col=c("darkgreen","forestgreen","chartreuse3"),
     ylab="Simpson Index",pch=16, cex=2)
# indice de Simpson en cada sitio
barplot(Simpson,col = c("darkgreen","forestgreen","chartreuse3"), 
        main = "Índice de Simpson", ylab = " Simpson index",xlab = "Sitios de muestreo",
        names=c("Pinar A","Bosque secundario","Yariguíes"),bty="n")

dev.off()

