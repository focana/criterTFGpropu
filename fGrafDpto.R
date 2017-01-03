fGrafDpto <- function (dCrit) {
######################################
# Crea un gráfico con la distibución de la carga por Departamento
#  dCrit	Tabla con los recuentos y porcentajes de las cargas estimadas distribuidos por Departamentos.
######################################
par(mar=c(5,15,1,5), cex.axis=1) 
vi <- dCrit$Carga > 0
vnombdpto <- dCrit$nombdpto

for (i in 1:nrow(dCrit)) {
 lnomDpto <- nchar(vnombdpto[i])
 if (lnomDpto > 25) {
   vnombdpto[i] <- substr(dCrit$nombdpto[i],1,25)
   vnombdpto[i] <- paste(vnombdpto[i],".",sep="")
#   if (lnomDpto > 30) {
#     vnombdpto[i] <- paste(vnombdpto[i], ".", substr(dCrit$nombdpto[i],lnomDpto-2,lnomDpto), sep="")
#   }
 }
}


barplot(dCrit$PCarga[vi], horiz=TRUE, names.arg=vnombdpto[vi], las=2)

}