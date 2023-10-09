#################################
# Analisis psicometrico pruebas #
#################################

# Librerias
library(ltm)
library(dplyr)
library(nortest)

# Recursos
scripts.dir <- dir()[grep(".R",dir())]
scripts.dir <- scripts.dir[-grep(".Rproj",scripts.dir)]
scripts.dir <- scripts.dir[-grep("Analisis",scripts.dir)]
for(i in 1:length(scripts.dir)){
  source(scripts.dir[i])
}

# Función
stats.item <- function(data,I1=1,tipo = 1,conf = 1, componentes,dif = 1, disc = 1, conv = "no",
                       nas = NULL,claves=NULL,key.calif = NA,tot.opc = NULL,str.opc = NULL,
                       min.disc = 0.2, min.dif = 0.1, max.dif = 0.9,pb.norm = TRUE){
  stats <- data.frame()
  n <- NULL
  dif.trad <- NULL
  rp.bis <- NULL
  cor.kendall <- NULL
  conf.it <- NULL
  dif.conf <- NULL
  eliminar <- NULL
  correl <- NULL
  fluj.opc <- data.frame()
  if(conv == "si"){
    str.items <- data[,-c(1:(I1-1))]
    if(tipo == 1){
      items <- califica(data,key=claves,key.data = key.calif,I1 = I1)
    } else {
      items <- calif.pol(data,keys=claves,key.data = key.calif,I1 = I1)
    }
    items <- items[,-c(1:(I1-1))]
  } else {
    if(I1 == 1){
      items <- data
    } else {
      items <- data[,-c(1:(I1-1))]
    } 
  }
  if(is.character(items[[1]])==TRUE){
    stop("Los datos no corresponden a la base de datos calificada")
  }
  Item <- names(items)
  if(conf == 1){
    conf.tot <- round(al.c(items,1),3)
  } else {
    if(conf == 2){
      conf.tot <- round(kr20(items,1),3)
    } else {
      conf.tot <- round(beta(items,1,componentes),3)
    }
  }
  for(i in 1:ncol(items)){
    dif.trad[i] <- round(mean(items[,i]),3)
    if(conf == 1){
      conf.it[i] <- round(al.c(items[,-i],1),3)
    } else {
      if(conf == 2){
        conf.it[i] <- round(kr20(items[,-i],1),3)
      } else {
        comp.adj <- componentes
        comp.adj[2] <- ifelse(componentes$Pos.Inicial > i,
                              componentes$Pos.Inicial - 1,
                              componentes$Pos.Inicial)
        comp.adj[3] <- ifelse(componentes$Pos.Final >= i,
                              componentes$Pos.Final - 1,
                              componentes$Pos.Final)
        conf.it[i] <- round(beta(items[,-i],1,as.data.frame(comp.adj)),3)
      }
    }
    subitems <- subset(items,is.na(items[,i]) == FALSE)
    pb <- rowSums(subitems)
    n[i] <- nrow(subitems)
    if(tipo == 1){
      rp.bis[i] <-  round(biserial.cor(pb,subitems[,i])*-1,3)
    } else {
      cor.kendall[i] <- round(cor(pb,subitems[,i],method = "spearman"),3)
    }
    if(n[i]>30){
      correl[i] <- round(cor(rowSums(subitems),subitems[,i], method = "pearson"),3)
    } else {
      correl[i] <- round(cor(rowSums(subitems),subitems[,i], method = "kendall"),3)
    }
    dif.conf <- conf.tot - conf.it
    if(conv == "si"){
      opciones <- data[,-c(1:(I1-1))]
      subopc <- subset(opciones,is.na(opciones[,i]) == FALSE)
      for(l in 1:length(str.opc)){
        assign(paste("opc",l,sep = "."),sum(subopc[,i]==str.opc[l]))
      }
      for(l in 1:length(str.opc)){
        assign(paste("nr",l,sep = "."),sum(subopc[,i]==nas[l]))
      }
      fo.it <- data.frame(get(paste("opc",1,sep = ".")))
      for(l in 2:(length(str.opc))){
        fo.it <- cbind.data.frame(fo.it,get(paste("opc",l,sep = ".")))
      }
      for(l in 1:(length(nas))){
        fo.it <- cbind.data.frame(fo.it,get(paste("nr",l,sep = ".")))
      }
      names(fo.it) <- c(str.opc,nas)
      fluj.opc <- rbind.data.frame(fluj.opc,fo.it)
    }
  }
  dif.grupos <- round(dif.si(items, I1 = 1)$Indice,3)
  disc.grupos <- round(disc.si(items, I1 = 1)$Indice,3)
  dific <- switch(dif,dif.trad,dif.grupos)
  if(nrow(items)>5){
    nor.pb <- lillie.test(pb)
    if(pb.norm == TRUE & tipo == 1){
      disc <- ifelse(nor.pb$p.value > 0.05,1,2)
      nor.pv <- round(rep(nor.pb$p.value,ncol(items)),3)
      nor.est <- round(rep(nor.pb$statistic,ncol(items)),3)
    } else {
      disc <- disc
      nor.pv <- round(rep(nor.pb$p.value,ncol(items)),3)
      nor.est <- round(rep(nor.pb$statistic,ncol(items)),3)
    }
  } else {
    nor.pv <- rep("N/A",ncol(items))
    nor.est <- rep("N/A",ncol(items))
  }
  discrim <- switch(disc,rp.bis,disc.grupos,cor.kendall)
  for(i in 1:ncol(items)){
    if(is.na(discrim[i])==TRUE | discrim[i]<min.disc | discrim[i]>=min.disc & dific[i]<min.dif | discrim[i]>=min.disc & dific[i]>max.dif){
      eliminar[i] <- "Si"
    } else {
      eliminar[i] <- "No"
    }
  }
  n.conf <- switch(conf,
                   c("Alfa_item","Alfa_Prueba"),
                   c("KR20-item","KR20_Prueba"),
                   c("Beta-item","Beta_Prueba"))
  n.dif <- switch(dif,"Dif_Tradicional","Dif_Grupos")
  n.disc <-rep(ifelse(disc == 1,"RP_biserial",ifelse(disc == 2,"Disc_Grupos","Cor_Kendall")),ncol(items))
  if(conv == "si"){
    stats <- data.frame(Item,n,dific,discrim,n.disc,correl,conf.it,conf.tot,dif.conf,fluj.opc,eliminar,nor.est,nor.pv)
    names(stats) <- c("ID_ITEM","N",n.dif,"Discriminacion","Metodo_Disc","Pearson/Kendall",n.conf,"Efecto_Conf",
                      paste("Tot_Opc",c(str.opc,nas),sep = "_"),"Eliminar","Normalidad","Valor_p")
  } else {
    stats <- data.frame(Item,n,dific,discrim,n.disc,correl,conf.it,conf.tot,dif.conf,eliminar,nor.est,nor.pv)
    names(stats) <- c("ID_ITEM","N",n.dif,"Discriminacion","Metodo_Disc","Pearson/Kendall",
                      n.conf,"Efecto_Conf","Eliminar","Normalidad","Valor_p")
  }
  items <- cbind.data.frame(data[,1:(I1-1)],items)
  stats <- list(stats,items)
}

## *data* debe ser igual a un string con la lectura óptica si el párametro *conv* es igual a "si",
## en caso contrario, debe ser una base de datos con la lectura óptica transformada a 1 y 0.
## El parámetro *nas* corresponde a los caracteres o marcas usadas para identificar valores perdidos
## o no validos para la calificación.
## El parámetro *str.opc* corresponde a los caracteres con los cuales se identificarán las opciones de
## respuesta ("a","b","c")
## El parámetro *tot.opc* corresponde al total de opciones de respuestas con las que cuentan los ítems
## en la prueba.