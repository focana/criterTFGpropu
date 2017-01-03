fguardaFichcsv <- function (dCrit, nfdCrit=deparse(substitute(dCrit)) ){
######################################
# Crea un fichero CSV con los recuentos por Departamento.
#  dCrit	Tabla con los recuentos y porcentajes de las cargas estimadas distribuidos por Departamentos.
#  nfdCrit  Nombre del fichero CSV que será creado (sin extensión).
######################################
 nfdCrit <- paste(nfdCrit, ".csv", sep="")
 write.table(dCrit, file=nfdCrit, sep=";", dec=",", row.names = FALSE)
 cat("", "\"Total\"", format(sum(dCrit$Carga),decimal.mark=","), format(sum(dCrit$PCarga), decimal.mark=","), sep=";", file=nfdCrit, append=TRUE)
}