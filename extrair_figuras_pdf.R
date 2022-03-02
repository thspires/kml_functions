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
  pdf_convert(pdf, pages=4:n_pages, format="png", filenames=nomes_arq)
  novapasta <- paste0("figuras_", no_processo)
  dir.create(novapasta)
  file.copy(nomes_arq, novapasta)
  file.remove(nomes_arq)
}
