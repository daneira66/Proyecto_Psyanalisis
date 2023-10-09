#######################################
# Calificación de pruebas politómicas #
#######################################

# Función
calif.pol <- function(data,keys,key.data,I1){
  prub <- unique(data[key.data])
  data.calif <- data.frame()
  for(i in 1:nrow(prub)){
    dat.sub <- as.data.frame(data[data[key.data] == as.vector(prub[i,1]),])
    key.sub <- as.data.frame(keys[keys[key.data] == as.vector(prub[i,1]),])
    for(j in I1:ncol(dat.sub)){
      for(k in 1:nrow(dat.sub)){
        grad <- which(dat.sub[k,j]==names(key.sub))
        if(length(grad) == 0){
          dat.sub[k,j] <- 0
        } else {
          dat.sub[k,j] <- key.sub[(j-(I1-1)),grad]
        }
      }
    }
    data.calif <- rbind.data.frame(data.calif,dat.sub)
  }
  for(i in I1:ncol(data.calif)){
    data.calif[,i] <- as.numeric(data.calif[,i])
  }
  data.calif
}
## Se debe incluir una base de datos con el string de opciones de respuesta
## *keys* corresponde a la base de datos de las claves de los ítems
## *key.data* corresponde a la llave de unión de la base de datos de las claves y del string de respuesta.
## *I1* corresponde a la posición del primer ítem en el string de opciones de respuesta
