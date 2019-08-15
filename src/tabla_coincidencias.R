
library(visNetwork)
require("igraph")#librera requerida para crear 
red<-function(coincidencias){
  datos <- as.data.frame(sapply(coincidencias[6],normalize))
  datos
  distancia <- suppressWarnings(dist(datos,method = "euclidean"))
  distancia
  distancia = as.matrix(distancia)
  distancia
  
  #inversa de la distancia
  inversa <- 1/distancia
  inversa
  diag(inversa) <- 0
  inversa
  #inversa[1:13, 1:13]
  matriz = as.matrix(distancia)
  matriz
  
  red = graph.adjacency(matriz, mode="undirected", weighted=TRUE)
  red
  
  data<-toVisNetworkData(red)
  data
  my.nodes<-data$nodes
  my.edges<-data$edges
  my.nodes$class<-coincidencias$classification
  my.nodes$label<-coincidencias$caso
  
  my.nodes$font.size<-c(90)
  #my.nodes$label.font.color<-c("white","blue")[my.nodes$label]
  coincidencias$s_circularity
  my.nodes$color.background<-ifelse(is.na(coincidencias$s_circularity),c("red"),c("yellow"))
  
  visNetwork(my.nodes,my.edges,width = "1000px",height = "800px")%>%visIgraphLayout()%>%visPhysics(enabled=FALSE)%>%
    visOptions(selectedBy = "class")
}

normalize <- function( x){
  return ((x - min(x))/ (max( x) - min (x)))
}
grupo_alto<-function(datos){
  caracteristica <- as.data.frame(sapply(datos[2], normalize))
  
  caracteristica
  dist_hc<-dist(caracteristica,method = "euclidean")
  
  set.seed(200)
  grupos<-kmeans(dist_hc,3)
  grupos$cluster
  x<-cbind(datos,grupos$cluster)
  x1<-subset(x,grupos$cluster==1)
  x2<-subset(x,grupos$cluster==2)
  x3<-subset(x,grupos$cluster==3)
  x1
  x2
  x3
  return(x3)
}

library(plyr)

perimetro <- read.csv(file="/home/ospcx/GitProyects/BCDR-D01_kruskal_primm_alg/data/characteristics/perimetro.csv", header=TRUE, sep=",", colClasses=c(NA, NA,NA))
perimetro<-perimetro[order(perimetro[2]),]
perimetro
perimetro=grupo_alto(perimetro)
perimetro_benignos<-subset(perimetro,as.integer(perimetro$classification) == 1)
perimetro_benignos
count(perimetro_benignos)
#perimetro_malignos<-subset(perimetro,as.integer(perimetro$classification) == 2)
#perimetro_malignos


circularity <- read.csv(file="/home/ospcx/GitProyects/BCDR-D01_kruskal_primm_alg/data/characteristics/s_circularity.csv", header=TRUE, sep=",", colClasses=c(NA, NA,NA))
circularity<-circularity[order(circularity[2]),]
circularity
circularity=grupo_alto(circularity)
circularity_benignos<-subset(circularity,as.integer(circularity$classification) == 1)
count(circularity_benignos)


elongation <- read.csv(file="/home/ospcx/GitProyects/BCDR-D01_kruskal_primm_alg/data/characteristics/s_elongation.csv", header=TRUE, sep=",", colClasses=c(NA, NA,NA))
elongation<-elongation[order(elongation[2]),]
elongation
elongation=grupo_alto(elongation)
elongation_class<-subset(elongation,as.integer(elongation$classification) == 1)
count(elongation_class)

mass <- read.csv(file="/home/ospcx/GitProyects/BCDR-D01_kruskal_primm_alg/data/characteristics/s_y_center_mass.csv", header=TRUE, sep=",", colClasses=c(NA, NA,NA))
mass<-mass[order(mass[2]),]

mass=grupo_alto(mass)
mass_benignos<-subset(mass,as.integer(mass$classification) == 1)
count(mass_benignos)

skewness <- read.csv(file="/home/ospcx/GitProyects/BCDR-D01_kruskal_primm_alg/data/characteristics/skewness.csv", header=TRUE, sep=",", colClasses=c(NA, NA,NA))
skewness<-skewness[order(skewness[2]),]
skewness
skewness=grupo_alto(skewness)
skewness_class<-subset(skewness,as.integer(skewness$classification) == 1)
skewness_class

denth <- read.csv(file="/home/ospcx/GitProyects/BCDR-D01_kruskal_primm_alg/data/characteristics/t_denth.csv", header=TRUE, sep=",", colClasses=c(NA, NA,NA))
denth<-denth[order(denth[2]),]
denth
denth=grupo_alto(denth)
denth
denth_class<-subset(denth,as.integer(denth$classification) == 1)
count(denth_class)

homo <- read.csv(file="/home/ospcx/GitProyects/BCDR-D01_kruskal_primm_alg/data/characteristics/t_homo.csv", header=TRUE, sep=",", colClasses=c(NA, NA,NA))
homo<-homo[order(homo[2]),]
homo
homo=grupo_alto(homo)
homo_benignos<-subset(homo,as.integer(homo$classification) == 2)
count(homo_benignos)
#sacar coincidencias entre casos
#tres_caracteristicas<-Reduce(intersect,list(perimetro$caso,circularity$caso,elongation$caso,mass$caso))
#tres_caracteristicas
p_c<-merge(cicularidad,elongation)
class<-subset(p_c,as.integer(p_c$classification) == 1)
count(class)
subset(p_c,as.integer(p_c$classification) == 1)
#combinar caracter?sticas
combined <- rbind.fill(circularity,elongation)
combined
coincidencias<-cbind(combined,coinciden=ifelse(combined$caso %in% p_c$caso,1,0))
coincidencias
#coincidencias<-distinct(coincidencias)
#coincidencias
#coincidencias_benignas<-subset(coincidencias,as.integer(coincidencias$classification) == 1)
#coincidencias_benignas
#coincidencias_malignas<-subset(coincidencias,as.integer(coincidencias$classification) == 2)
red(coincidencias)
#red(coincidencias_malignas)


