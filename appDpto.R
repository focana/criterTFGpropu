appDpto <- function (mCrit, Codes) {
##########################
# Obtiene la distribución por Departamentos a la matriz con el criterio calculado por materia.
# Input
#	mCrit	matriz con el criterio calculado por materia,
#   Codes	matriz con los códigos identificativos de los Departamentos
# Output
#	ddpto	matriz con la suma agregada por Departamentos de las cargas estimadas (ordenados alfabéticamente)
##########
ddpto <- aggregate(Carga ~ Cd, data = mCrit, sum)

total <- sum(ddpto$Carga)

if (total != sum(mCrit$Carga)) {print("Error: suma no validada")}

ddpto$PCarga <- ddpto$Carga * 100/total;

ddpto$nombdpto <- ""

for (i in 1:nrow(ddpto)) {
  for (d in 1:nrow(Codes)) {
    if (ddpto$Cd[i] == Codes$COD[d]) {
	  ddpto$nombdpto[i] <- Codes$DEPARTAMENTO[d]
	  break;
	}	
  }
  
}

#Reordenar los recuentos por Departamento
ddpto <- ddpto[order(ddpto$nombdpto),]
ddpto <- ddpto[, c(1,4,2,3)]

return (ddpto)
}