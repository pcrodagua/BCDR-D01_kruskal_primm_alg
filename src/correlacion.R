library(visNetwork)
require("igraph")#librer?a requerida para crear

normalize <- function( x){
  return ((x - min(x))/ (max( x) - min (x)))
}




perimetro <- read.csv(file="/home/ospcx/GitProyects/BCDR-D01_kruskal_primm_alg/data/characteristics/perimetro.csv", header=TRUE, sep=",", colClasses=c(NA, NA,NA))
perimetro<-perimetro[order(perimetro[2]),]

circ <- read.csv(file="/home/ospcx/GitProyects/BCDR-D01_kruskal_primm_alg/data/characteristics/s_circularity.csv", header=TRUE, sep=",", colClasses=c(NA, NA,NA))
circ<-circ[order(circ[2]),]

elong <- read.csv(file="/home/ospcx/GitProyects/BCDR-D01_kruskal_primm_alg/data/characteristics/s_elongation.csv", header=TRUE, sep=",", colClasses=c(NA, NA,NA))
elong<-elong[order(elong[2]),]

mass <- read.csv(file="/home/ospcx/GitProyects/BCDR-D01_kruskal_primm_alg/data/characteristics/s_y_center_mass.csv", header=TRUE, sep=",", colClasses=c(NA, NA,NA))
mass<-mass[order(mass[2]),]

skewness <- read.csv(file="/home/ospcx/GitProyects/BCDR-D01_kruskal_primm_alg/data/characteristics/skewness.csv", header=TRUE, sep=",", colClasses=c(NA, NA,NA))
skewness<-skewness[order(skewness[2]),]

denth <- read.csv(file="/home/ospcx/GitProyects/BCDR-D01_kruskal_primm_alg/data/characteristics/t_denth.csv", header=TRUE, sep=",", colClasses=c(NA, NA,NA))
denth<-denth[order(denth[2]),]

homo <- read.csv(file="/home/ospcx/GitProyects/BCDR-D01_kruskal_primm_alg/data/characteristics/t_homo.csv", header=TRUE, sep=",", colClasses=c(NA, NA,NA))
homo<-homo[order(homo[2]),]

area <- read.csv(file="/home/ospcx/GitProyects/BCDR-D01_kruskal_primm_alg/data/relevant/s_area.csv", header=TRUE, sep=",", colClasses=c(NA, NA,NA))
area<-area[order(area[2]),]

form <- read.csv(file="/home/ospcx/GitProyects/BCDR-D01_kruskal_primm_alg/data/relevant/s_form.csv", header=TRUE, sep=",", colClasses=c(NA, NA,NA))
form<-form[order(form[2]),]

mass_x <- read.csv(file="/home/ospcx/GitProyects/BCDR-D01_kruskal_primm_alg/data/relevant/s_x_center_mass.csv", header=TRUE, sep=",", colClasses=c(NA, NA,NA))
mass_x<-mass_x[order(mass_x[2]),]

contr <- read.csv(file="/home/ospcx/GitProyects/BCDR-D01_kruskal_primm_alg/data/relevant/t_contr.csv", header=TRUE, sep=",", colClasses=c(NA, NA,NA))
contr<-contr[order(contr[2]),]

corr <- read.csv(file="/home/ospcx/GitProyects/BCDR-D01_kruskal_primm_alg/data/relevant/t_corr.csv", header=TRUE, sep=",", colClasses=c(NA, NA,NA))
corr<-corr[order(corr[2]),]

dvarh <- read.csv(file="/home/ospcx/GitProyects/BCDR-D01_kruskal_primm_alg/data/relevant/t_dvarh.csv", header=TRUE, sep=",", colClasses=c(NA, NA,NA))
dvarh<-dvarh[order(dvarh[2]),]

energy <- read.csv(file="/home/ospcx/GitProyects/BCDR-D01_kruskal_primm_alg/data/relevant/t_energ.csv", header=TRUE, sep=",", colClasses=c(NA, NA,NA))
energy<-energy[order(energy[2]),]

inf2 <- read.csv(file="/home/ospcx/GitProyects/BCDR-D01_kruskal_primm_alg/data/relevant/t_inf2h.csv", header=TRUE, sep=",", colClasses=c(NA, NA,NA))
inf2<-inf2[order(inf2[2]),]

data<-data.frame(perimetro$s_perimeter , circ$s_circularity,elong$s_elongation,
                 mass$s_y_center_mass,skewness$i_skewness, 
                 denth$t_denth, homo$t_homo,area$s_area,form$s_form, 
                 mass_x$s_x_center_mass, contr$t_contr, corr$t_corr, dvarh$t_dvarh, 
                 energy$t_energ, inf2$t_inf2h)
data
cor(data,method = "p")
