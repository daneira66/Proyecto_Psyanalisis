#######################################
# Aleatorización de claves de pruebas #
#######################################

rkey <- function(pruebas,items,opc){
  prueba <- c(1:pruebas) #numero de prueba rta
  it <- matrix(sample(letters[1:opc],pruebas*items,replace = T),
               nrow = pruebas,ncol = items) #Claves de los items
  colnames(it) <- c(paste("I",1:items,sep = "")) #nombres de los items
  claves <- data.frame(prueba,it) #Base de datos final
  for(i in 2:ncol(claves)){
    claves[,i] <- as.character(claves[,i])
  }
  claves
}

# pruebas = cantidad de pruebas aplicadas, items = cant items por prueba
# opc = cantidad de opcioens de respuesta