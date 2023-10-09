#######################
# Truncar y redondear #
#######################

# Función extraer decimales
d <- function(x){
  d <- ifelse(x>0,x-as.integer(x),x+(-as.integer(x)))
  d
}

# Redondear
redondear <- function(x,dec = 0){
  pd <- ifelse(x>0,1-d(x),-1-(d(x)))
  rx <- x+(d(pd*10^dec)/10^dec)
  rx
}

# truncar
truncar <- function(x,dec = 0){
  tx <- as.integer(x)+as.integer(d(x)*10^dec)*10^-dec
  tx
}
