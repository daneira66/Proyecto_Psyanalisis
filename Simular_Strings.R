########################################
# Sumulación de Bases de datos Pruebas #
########################################

bdi <- function(n,pruebas,items,opc,prob){
  ID <- c(1:n) #ID personas
  prueba <- c(sample(c(1:pruebas),n,replace = T)) #numero de prueba rta
  it <- matrix(sample(letters[1:opc],n*items,replace = T,prob = prob1(opc)),
               nrow = n,ncol = items) #respuestas de las personas
  colnames(it) <- c(paste("I",1:items,sep = "")) #nombres de los items
  respuestas <- data.frame(ID,prueba,it) #Base de datos final
  for(i in 3:ncol(respuestas)){
    respuestas[,i] <- as.character(respuestas[,i])
  }
  respuestas
}