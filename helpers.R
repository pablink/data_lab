# Función que carga todas las hojas de un libro de excel.
# hace uso del paquete readxl
read_excel_allsheets <- function(filename) {
  sheets <- readxl::excel_sheets(filename) # vector con el nombre de las hojas del excel
  x <- lapply(sheets, function(X) readxl::read_excel(filename, sheet = X, 
                                                    col_names = FALSE, skip = 5))
  names(x) <- sheets
  x
}

# Función para estandarizar los datos capturados en formato 00º00'00\"
clean_chars <- function(x) {
  y <- gsub("\\D|º|ª|\\|", "-", x) # todo lo que no sea digito se remplaza por -
  y <- gsub("-+", " ", y)
  y <- sub(" ", "º", y)
  y <- sub(" ", "'", y)
  y <- sub(" ", ".", y)
  y <- sub(" ", "\"", y)
  return(y)
}

# Función para convertir coordenadas geográficas en formato grado, minuto y 
# segundo a formato decimal
# source: https://modtools.wordpress.com/2013/09/25/dms2dec/
dms2dec <- function(dms, separators = c("º", "\\|", "°", "\'", "\"")) {
  # version 1.0 (25 Sep 3013)
  # dms: a vector (or column) of latitude or longitude in degrees-minutes-seconds-hemisfere, e.g. 41° 34' 10.956" N (with or without spaces)
  # separators: the characters that are separating degrees, minutes and seconds in dms
  
  dms <- as.character(dms)
  dms <- gsub(pattern = " ", replacement = "", x = dms)
  for (s in separators) dms <- gsub(pattern = s, replacement = "_splitHere_", x = dms)
  
  splits <- strsplit(dms, split = "_splitHere_")
  n <- length(dms)
  deg <- min <- sec <- hem <- vector("character", n)
  
  for (i in 1:n) {
    deg[i] <- splits[[i]][1]
    min[i] <- splits[[i]][2]
    sec[i] <- splits[[i]][3]
    hem[i] <- splits[[i]][4]
  }
  
  dec <- as.numeric(deg) + (as.numeric(min) / 60) + (as.numeric(sec) / 3600)
  sign <- ifelse (hem %in% c("N", "E"), 1, -1)
  dec <- sign * dec
  return(dec)
}  # end dms2dec function 