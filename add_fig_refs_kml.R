# mapear nomes KML com nomes das figuras em um dataframe
# Extrai figuras das paginas 4 ate o fim do PDF e move-las para subdiretorio "figuras"


extrair_figuras_pdf <- function(pdf) {
  dados_pdf <- pdftools::pdf_data(pdf)
  n_pages <- length(dados_pdf)
  no_processo <- str_match(pdf, "[0-9]{5}")
  figtitles <- c()
  for (i in 4:n_pages) {
    figtitles <- append(figtitles, dados_pdf[[i]]$text[1]) }
  talhoes_antes_ndvi <- paste0("talhao_", 1:(min(grep("NDVI", figtitles))-1))
  figtitles[which(figtitles=="NDVI")] <- paste0(talhoes_antes_ndvi, "_NDVI")
  nomes_arq <- paste(no_processo, figtitles, 4:n_pages, ".png", sep="_")
  pdftools::pdf_convert(pdf, pages=4:n_pages, format="png", filenames=nomes_arq)
  novapasta <- paste0("figuras_", no_processo)
  dir.create(novapasta)
  file.copy(nomes_arq, novapasta)
  file.remove(nomes_arq)
}

#setwd("//w00101pnas0/Shared/AgroBTG/shared/RAIO_D_analise/Minozzo")
setwd("/Users/tiagopires/Desktop/RAIO_QO_PARTA/Minozzo")
pdfs <- list.files(pattern=".pdf")
sapply(pdfs, extrair_figuras_pdf)

# MAPEAR kmls e figuras
mapear_kml_figuras <- function() {
  no_processos <- str_match(pdfs, "[0-9]{5}")
  # mapear quais figuras vao para quais KML
  figfiles <- list.files(pattern=".png", recursive=T)
  mapeamento <- data.frame(proc=str_match(figfiles, "[0-9]{5}"), figfiles)
  mapeamento$talhao <- str_match(mapeamento$figfiles, "talhao..")
  mapeamento$kmlname <- paste(mapeamento$proc,mapeamento$talhao, sep="_")
  kmls <- list.files(pattern=".kml")
  kmls <- kmls[grep("talhao", kmls)]
  mapeamento$kmlfile <- NA
  for(i in 1:nrow(mapeamento)){
    # arquivo kml com combinacao de talhao e processo
    meukml <- kmls[str_detect(kmls, mapeamento$proc[i]) & str_detect(kmls, mapeamento$talhao[i])]
    # numero de linhas com combinacao de talhao e processo
    linhas <- grepl(mapeamento$proc[i],   mapeamento$proc) & grepl(mapeamento$talhao[i], mapeamento$talhao)
    mapeamento$kmlfile[linhas] <- meukml
  }
  return(mapeamento)
}

meumapa <- mapear_kml_figuras()

# em cada KML lido, adicionar:
# abaixo de <ExtendedData>:
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
