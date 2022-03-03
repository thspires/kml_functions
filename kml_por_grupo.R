gerar_kmlao <- function(grupo=fulano) {
  
  nome_arquivo <- paste0("kml_raioD_", grupo, ".kml")
  
  if (file.exists(nome_arquivo)) stop("tem kml unificado already")
  
  # ler todos KMLs que tem "talhao" no nome
  kmls <- list.files(pattern=".kml")
  kmls <- kmls[grep("figref", kmls)]
  myfiles = lapply(kmls, readr::read_file) # lapply pra deixar o Gabriel feliz
  
  no_propostas <- gsub("_.*", "", kmls)
  
  kml_header <- '<?xml version="1.0" encoding="UTF-8"?>
  <kml xmlns="http://www.opengis.net/kml/2.2" xmlns:gx="http://www.google.com/kml/ext/2.2" xmlns:kml="http://www.opengis.net/kml/2.2" xmlns:atom="http://www.w3.org/2005/Atom">
  <Document id="KML_RAIO_D">
  	<name>%s</name>
  	<open>1</open>'
  
  kml_header <- sprintf(kml_header, grupo)
  
  footer <- '\n</Document>\n</kml>'
  
  style_template <- '
  <Style id=%1$s>
    <LineStyle>
      <color>%2$s</color>
      <width>2</width>
      <fill>1</fill>
    </LineStyle>
    <PolyStyle>
      <color>7dff0000</color>
      <fill>0</fill>
    </PolyStyle>
  </Style>'
  
  n_propostas <- length(unique(no_propostas))
  
  gerar_cores_propostas <- function() {
    n_cores <- n_propostas
    cores <- sample(rainbow(n_cores, alpha=1), n_cores)
    cores <- tolower(gsub('#', '', cores))
    return(cores)
  }
  cores <- gerar_cores_propostas()
  
  # cores iguais para a mesma proposta
  cores <- rep(cores, table(no_propostas))
  # A ordem das cores em KML nao eh rrggbbaa, eh aaggbbrr
  cores <- stringi::stri_reverse(cores) # esse eh um jeito meio tosco de resolver o problema, 
  # uma funcao para converter rrggbbaa -> aabbggrr precisa ser feita ou usar do pacote plotKML
                         
  # Criar estilos, nomes sao os numeros das propostas e tem uma cor para cada proposta
  n_talhoes <- length(no_propostas)
  styles <- rep(style_template, n_talhoes)
  styles <- sprintf(styles, paste0('"', 1:n_talhoes, '"'), cores)

  # funcao para pegar apenas o <Placemark> desses arquivos
  grab_placemark <- function(kml) {
    placemark <- sub(".*<Placemark", "", kml)
    placemark <- sub("</Placemark>\n.*", "", placemark)
    placemark <- paste0("<Placemark", placemark, "</Placemark>\n")
    return(placemark)
  }
  
  # aplicar grab_placemark em toos os kmls de talhoes
  placemarks <- lapply(myfiles, grab_placemark) # tem um sorriso no Gabriel ao ver mais um lapply

  # modificar nome dos talhoes <name>talhao_1</name> para <name>PROPOSTAtalhao_1</name>
  for(i in seq_along(placemarks)){
    placemarks[[i]] <- sub("(<name>)(t)", paste0("\\1", "_", no_propostas[i], "\\2"), placemarks[[i]])
  }
  
  # Adicionar novos campos "no_proposta" e "grupo_Raio_D" para cada placemark
  xml_data <- '
  <styleUrl>#%s</styleUrl>
  <ExtendedData>
    <Data name="no_proposta">
      <displayName>no_proposta</displayName>
      <value>%s</value>
    </Data>
  <Data name="Gupo">
    <displayName>Grupo_Raio_D</displayName>
    <value>%s</value>
  </Data>'

  # sprintf adicionando no_proposta para cada <Placemark>
  placemark_style_data <- list()
  for(i in 1:length(no_propostas)) {
      placemark_style_data[[i]] <- sprintf(xml_data, i, no_propostas[i], grupo)
  }
  
  for(i in seq_along(no_propostas)) {
    placemarks[[i]] <- gsub("<style.*<ExtendedData>", placemark_style_data[[i]], placemarks[[i]]) 
  }
  
  # funcao para escrever arquivo
  criar_kmlao <- function(filename) {
    sink(filename)
    cat(kml_header)
    cat(styles)
    cat(unlist(placemarks))
    cat(footer)
    sink()
  }
  criar_kmlao(nome_arquivo)
  print(paste("Arquivo criado:", nome_arquivo))
}
