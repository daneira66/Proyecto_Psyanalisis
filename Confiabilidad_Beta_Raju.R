############################
# Coeficiente Beta de Raju #
############################

# Función
beta <- function(data, I1 = 1, componentes){
  if(I1 == 1){
    items <- data
  } else {
    items <- data[,-c(1:(I1-1))]
  }
  if(is.character(items[[1]])==TRUE){
    stop("Los datos no corresponden a la base de datos calificada")
  }
  n <- ncol(items)
  for(i in 1:nrow(componentes)){
    a <- componentes[i,2]
    b <- componentes[i,3]
    assign(paste("comp",i,sep = "_"),
           items[,c(a:b)])
  }
  for(i in 1:nrow(componentes)){
    assign(paste("n",i,sep = "_"),
           ncol(get(paste("comp",i,sep = "_"))))
  }
  for(i in 1:nrow(componentes)){
    assign(paste("var",i,sep = "_"),
           var(rowSums(get(paste("comp",i,sep = "_")))))
  }
  var.x <- var(rowSums(items))
  assign("list.var", paste("var",1:nrow(componentes),sep = "_"))
  assign("list.n", paste("n",1:nrow(componentes),sep = "_"))
  sum.vc <- get(list.var[1])
  for(i in 2:nrow(componentes)){
    sum.vc <- sum.vc + get(list.var[i])
  }
  p1 <- var.x - sum.vc
  sum.cjn2 <- (get(list.n[1])/n)^2
  for(i in 2:nrow(componentes)){
    sum.cjn2 <- sum.cjn2+(get(list.n[i])/n)^2
  }
  p2 <- var.x*(1-sum.cjn2)
  coef <- p1/p2
  coef[coef < -1 | coef > 1] <- NA
  coef
}
## Se debe incluir una base de datos de 1 y 0
## El parámetro *I1* indica la primera posición en la cuál se encuentran ítems
## El parámetro *componentes* corresponde a un data.frame compuesto por tres variables: Nombre, Posición inicial y Posición Final
