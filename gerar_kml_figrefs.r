gerar_kml_figrefs <- function() {
  template <- '<Data name="fig">
     <displayName></displayName>
     <value>
       <img style="max-width:500px;" src="%s"></img>
     </value>
   </Data>'
  #
  # repetir template para cada arquivo kml de talhao
  meumapa$figfiles[which(meumapa$kmlfile==meumapa$kmlfile[1])]
  # para cada kmlfile
  # para cada figura
  n_figs_each_kml <- table(meumapa$kmlfile)
  
  mapa2 <- data.frame(n_figs_each_kml)
  names(mapa2) <- c("kmlfile", "n")
  mapa2$texto_add <- NA
  
  for(i in 1:nrow(mapa2)) {
    mapa2$texto_add[i] <- str_c(rep(template, mapa2$n[i]), sep = "", collapse = T)
  }
  
  # para cadata texto_add, fazer sprintf com nomes dos arquivos
  for(i in 1:nrow(mapa2)){
    preenchimento <- meumapa$figfiles[which(meumapa$kmlfile==mapa2$kmlfile[i])]
    mapa2$texto_add[i] <- do.call(sprintf, c(fmt = mapa2$texto_add[i], as.list(preenchimento)))
  }
  
  # Preparar para gsub nos arquivos kml
  mapa2$texto_add <- paste0("<ExtendedData>", mapa2$texto_add)
  
  # ler KML e substituir 
  kmls <- list.files(pattern=".kml")
  mapa2$kmls <- kmls[grep("talhao", kmls)]
  
  # Gerar novos KMLs de cada talhao
  novos_nomes <- gsub("_[0-9]{4}.*", "", mapa2$kmlfile)
  novos_nomes <- paste0(novos_nomes,"_figref.kml")
  
  for(i in 1:nrow(mapa2)) {
    kml <- readr::read_file(kmls[i])
    kml_com_figrefs <- gsub("<ExtendedData>", mapa2$texto_add[i], kml)
    sink(novos_nomes[i])
    cat(kml_com_figrefs)
    sink()
  }
}
