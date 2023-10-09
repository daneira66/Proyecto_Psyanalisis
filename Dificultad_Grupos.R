######################################
# Dificultad por grupos de desempeño #
######################################

# Función
dif.si <- function(data, I1 = 1){
  if(I1 == 1){
    items <- data
  } else {
    items <- data[,-c(1:(I1-1))]
  }
  if(is.character(items[[1]])==TRUE){
    stop("Los datos no corresponden a la base de datos calificada")
  }
  items$pb <- rowSums(items)
  if(nrow(items)<16){
    it.sup <- subset(items,pb > quantile(items$pb)[3] | pb == quantile(items$pb)[3])
    it.inf <- subset(items,pb < quantile(items$pb)[3])
  } else {
    it.sup <- subset(items,pb > quantile(items$pb)[4] | pb == quantile(items$pb)[4])
    it.inf <- subset(items,pb < quantile(items$pb)[2] | pb == quantile(items$pb)[2])
  }
  id.it <- NULL
  dif.it <- NULL
  for(i in 1:(ncol(items)-1)){
    id.it[i] <- names(items)[i]
    dif.it[i] <- (sum(it.sup[,i])+sum(it.inf[,i]))/(nrow(it.sup)+nrow(it.inf))
  }
  dif <- data.frame(item = id.it,
                    Indice = dif.it)
  dif
}
## Se debe incluir una base de datos de 1 y 0
## El parámetro *I1* indica la primera posición en la cuál se encuentran ítems
