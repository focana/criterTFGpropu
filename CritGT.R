CritGT <-  function (mdato) {
# #######
# Criterio considerando grupos de Teoría (propuesto por el Dpto. de Nutrición y Bromatología)
# Estima la carga docente de cada asignatura utilizando el criterio propuesto por el Depto. de Nutrición, consistente en 
# considerar, además del número de créditos de teoría y prácticas del plan de estudios que son impartidos, el número 
# de grupos de teoría (grupo ampliado).
# Input
# 	mdato: matriz con los datos de las asignaturas.
# Output
# 	Crit: nueva matriz con la carga docente estimada para cada asignatura y una breve información sobre las asignaturas.
#         Es un espejo de mdato.
####################

Crit <- data.frame(Asignatura=mdato$Asignatura, Cd=mdato$Cd, stringsAsFactors = FALSE)
Crit$Carga <- 0; # Carga estimada

for (i in 1:nrow(mdato)) {  
  Crit$Carga[i] <- (mdato$Cr_amp[i] + mdato$Cr_sub[i])*mdato$Gr_amp[i]
}

s <- sum(Crit$Carga)
Crit$PCarga <- Crit$Carga * 100/s; # % Carga estimada

return (Crit)

}