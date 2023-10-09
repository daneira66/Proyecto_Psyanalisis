########################
# Kudder-Richardson 20 #
########################

# Función
kr20 <- function(data, I1 = 1){
  pq <- NULL
  if(I1 == 1){
    items <- data
  } else {
    items <- data[,-c(1:(I1-1))]
  }
  if(is.character(items[[1]])==TRUE){
    stop("Los datos no corresponden a la base de datos calificada")
  }
  n <- ncol(items)
  for(i in 1:ncol(items)){
    pq[i] <- mean(items[,i])*(1-mean(items[,i]))
  }
  vpb <- var(rowSums(items))
  p1 <- n/(n-1)
  p2 <- 1 - (sum(pq)/vpb)
  coef <- p1*p2
  coef
}
## Se debe incluir una base de datos de 1 y 0
## El parametro *I1* indica la primera posición en la cuál se encuentran ítems