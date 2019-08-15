
library(visNetwork)
require("igraph")#librer?a requerida para crear 
normalize <- function( x){
  return ((x - min(x))/ (max( x) - min (x)))
}


MyData <- read.csv(file="/home/ospcx/GitProyects/BCDR-D01_kruskal_primm_alg/data/characteristics/perimetro.csv", header=TRUE, sep=",", colClasses=c(NA, NA,NA))

MyData

#caracteristica <- as.data.frame(sapply(perimetro[,c(2)], normalize))
caracteristica <- as.data.frame(sapply(MyData[2], normalize))
#caracteristica<-caracteristica[order(caracteristica$s_perimeter),]
#caracteristica <- as.data.frame(sapply(s_circularity[,c(2)], normalize))
#caracteristica <- as.data.frame(sapply(s_elongation[,c(2)], normalize))
#caracteristica <- as.data.frame(sapply(s_y_center_mass[,c(2)], normalize))
#caracteristica <- as.data.frame(sapply(skewness[,c(2)], normalize))
#caracteristica <- as.data.frame(sapply(t_denth[,c(2)], normalize))
#caracteristica <- as.data.frame(sapply(t_homo[,c(2)], normalize))


caracteristica
dist_hc<-dist(caracteristica,method = "euclidean")
dist_hc
distancia = as.matrix(dist_hc)
distancia

inversa <- 1/distancia
inversa
diag(inversa) <- 0
inversa

#hc<- hclust(dist_hc,method = "ave")
#hc
#grupos<-cutree(hc,k = 3)
#set.seed(200)
grupos<-kmeans(dist_hc,3)
grupos$cluster
x<-cbind(inversa,grupos$cluster)

x1<-subset(x,grupos$cluster==1)
x2<-subset(x,grupos$cluster==2)
x3<-subset(x,grupos$cluster==3)
x1
x2
x3

valores<-cbind(MyData,grupos$cluster)
valores1<-subset(valores,grupos$cluster==1)
valores2<-subset(valores,grupos$cluster==2)
valores3<-subset(valores,grupos$cluster==3)
#ifelse(sum(x1[1])>sum(x2[1]),altos<-x1,altos<-x2)
#ifelse(sum(altos[1])>sum(x3[1]),altos<-altos,altos<-x3)
#altos
valores1

datos <- as.data.frame(sapply(valores1[2],normalize))
datos
distancia <- suppressWarnings(dist(datos,method = "euclidean"))

distancia = as.matrix(distancia)


#inversa de la distancia
inversa <- 1/distancia

diag(inversa) <- 0

matriz = as.matrix(inversa)


red = graph.adjacency(matriz, mode="undirected", weighted=TRUE)
E(red)$weight





E(red)$width <- E(red)$weight/50
V(red)$degree <- graph.strength(red,vids = V(red),mode = c("out"),loops = FALSE)

fastgreedy.community(red,merges = TRUE, modularity = TRUE,membership = TRUE,weights = E(red)$weight)
ceb <- fastgreedy.community(red,merges = TRUE, modularity = TRUE,membership = TRUE,weights = E(red)$weight)
#length(ceb)

V(red)$clasificacion = valores1$classification
V(red)$clasificacion
#V(red)$color<-ifelse(V(red)$clasificacion == 1,"gray","black")

#membership(ceb)

data<-toVisNetworkData(red)

my.nodes<-data$nodes
my.edges<-data$edges
my.nodes$size<-V(red)$degree/25
my.nodes$groups<-ceb$membership
my.nodes$label<-valores1$caso
my.nodes$font.color<-ifelse(V(red)$clasificacion == 1,c("pink"),c("brown"))
my.nodes$font.size<-ifelse(V(red)$clasificacion == 1,c(55),c(55))
my.nodes$label.font.color<-c("white","blue")[my.nodes$label]
my.nodes$color.background<-c("red","blue","green","yellow","purple")[my.nodes$groups]
my.nodes$color.border<-c("red","blue","green","yellow","purple")[my.nodes$groups]
vn<-visNetwork(my.nodes,my.edges,width = "1000px",height = "800px")%>%visIgraphLayout()%>%visPhysics(enabled=FALSE)
vn
visSave(vn, file = "/home/ospcx/GitProyects/BCDR-D01_kruskal_primm_alg/data/characteristics/network.html", background = "white")

data1<-toVisNetworkData(mst(red, algorithm = 'prim'))
my.nodes<-data1$nodes
my.edges<-data1$edges
my.nodes$size<-V(mst(red))$degree/25
my.nodes$groups<-ceb$membership
my.nodes$label<-valores1$caso
my.nodes$font.color<-ifelse(V(red)$clasificacion == 1,c("pink"),c("brown"))
my.nodes$font.size<-ifelse(V(red)$clasificacion == 1,c(55),c(55))
my.nodes$label.font.color<-c("white","blue")[my.nodes$label]
my.nodes$color.background<-c("red","blue","green","yellow","purple")[my.nodes$groups]
my.nodes$color.border<-c("red","blue","green","yellow","purple")[my.nodes$groups]
vn1<-visNetwork(my.nodes,my.edges,width = "1000px",height = "800px")%>%visIgraphLayout()%>%visPhysics(enabled=FALSE)
vn1
visSave(vn1, file = "/home/ospcx/GitProyects/BCDR-D01_kruskal_primm_alg/data/characteristics/network2.html", background = "white")


#########################
valores3
V(red)$degree[is.infinite(V(red)$degree)]<-0
bind<-cbind(valores3,V(red)$degree)
bind

#bind<-bind[order(bind[5]),]
malignos<-subset(bind,as.integer(V(red)$clasificacion) == 2)
benignos<-subset(bind,as.integer(V(red)$clasificacion) == 1)

benignos<-benignos[order(-benignos[5]),]
benignos
malignos<-malignos[order(-malignos[5]),]
malignos
#x,y

casosb<-benignos$caso[order(benignos$caso)]
casosb
casosm<-malignos$caso[order(malignos$caso)]
casosm
plot(casosb,benignos$`V(red)$degree`,type = "o",xlab = "No. de caso", ylab = "Centralidad de grado", 
     main = "Centralidad de grado por benignos (Azul) y malignos (Rojo).",col="blue",
     xlim=range(benignos$caso,malignos$caso),ylim=range(benignos$`V(red)$degree`,malignos$`V(red)$degree`))
lines(casosm,malignos$`V(red)$degree`,type = "o",col="red")


plot(c(0,length(bind$caso)),c(0,max(bind$`V(red)$degree`)),type = "n",xlab = "N?mero de casos", ylab = "Centralidad de grado", 
     main = "Centralidad de grado para benignos y malignos.",col="blue")

lines(malignos$`V(red)$degree`,type = "o",col="red")
lines(benignos$`V(red)$degree`,type = "o",col="blue")
##########################


#correlaciones con centralidad de grado. 191
#agregar los n?emros a el an?slisis.
#graficas por benigno y maligno.

#x no casos
#y degree
#sacar los casos de menor grado al principio
#ordenar los casos.


#tal caso tiene tanl grado, ordenado

###------------------------------------
