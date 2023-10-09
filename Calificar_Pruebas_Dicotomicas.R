#####################################
# Calificacion de ítems dicótomicos #
#####################################

# Librerias
library(dplyr)

# Función
califica <- function(data,key,key.data,I1){
  items <- ncol(data[,-c(1:(I1-1))])
  calif <- left_join(data,key,by=key.data)# Union bases de datos
  for(i in I1:((items-1)+I1)){
    calif[,(2*items+i)] <- calif[,i]==calif[,(i+items)]#Calificacion
  }
  calif <- as.data.frame(calif)
  for(j in (2*items+I1):ncol(calif)){
    calif[,j] <- as.numeric(calif[,j])#cambiar a 1 y 0
  }
  calif <- calif[,c(1:(I1-1),(2*items+I1):(2*items+I1+items-1))]
  names(calif) <- names(data)
  calif
}
## Se debe incluir una base de datos con el string de opciones de respuesta
## *key* corresponde a la base de datos de las claves de los ítems
## *key.data* corresponde a la llave de unión de la base de datos de las claves y del string de respuesta.
## *I1* corresponde a la posición del primer ítem en el string de opciones de respuesta