CritVig <-  function (mdato) {
# #######
# Criterio Vigente
# Estima la carga docente de cada asignatura utilizando el criterio vigente, consistente en el número de asignaturas, es decir,
# sólo en el número de créditos de teoría y prácticas del plan de estudios que son impartidos.
# Input
# 	mdato: matriz con los datos de las asignaturas.
# Output
# 	Crit: nueva matriz con la carga docente estimada para cada asignatura y una breve información sobre las asignaturas.
#         Es un espejo de mdato.
####################

Crit <- data.frame(Asignatura=mdato$Asignatura, Cd=mdato$Cd, stringsAsFactors = FALSE)
Crit$Carga <- 0; # Carga estimada

for (i in 1:nrow(mdato)) {
  if (mdato$Caracter[i] == "Optativa") {
    peso <- 0.5  
  } else {
    peso <- 1
  }
  
  Crit$Carga[i] <- peso*(mdato$Cr_amp[i] + mdato$Cr_sub[i])
}

s <- sum(Crit$Carga)
Crit$PCarga <- Crit$Carga * 100/s; # % Carga estimada

return (Crit)
}