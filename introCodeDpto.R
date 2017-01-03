introCodeDpto <- function (mdato, Codes) {
# #################################
# Identifica los Departamentos que imparten cada una de las asignaturas.
# Input
# 	mdato: matriz con los datos de las asignaturas,
# 	Codes: matriz con los códigos identificativos de los Departamentos.
# 
# Introduce los códigos identificativos de un Departamento, añadiendo en "mdato" una columna, denominada "Cd",
# que contiene el código identificativo de cada Departamento que imparte la docencia.
#######################################
mdato$Cd <- 0;

for (i in 1:nrow(mdato)) {
  for (d in 1:nrow(Codes)) {
    if (mdato$Area[i] == Codes$Area[d]) {
	  mdato$Cd[i] <- Codes$COD[d]
	  break
	}
  }
}

Controla <- which(mdato$Cd == 0)
if (length(Controla) > 0) {cat("Error: revisar filas: ", Controla,"\n")}

return (mdato)
}