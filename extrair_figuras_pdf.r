library(pdftools)
pdf <- "/Users/tiagopires/Desktop/kml_functions-main/REPORT_BTG_BTG-LOTE-34480-BTG-LOTE-34480-22442105020.pdf"
setwd("/Users/tiagopires/Desktop/kml_functions-main/")

extrair_figuras_pdf <- function(pdf) {
  n_pages <- length(pdf_data(pdf))
  no_processo <- str_match(pdf, "[0-9]{5}")
  nomes_arq <- paste(no_processo, "fig", 4:n_pages, sep="_")
  pdf_convert(pdf, pages=4:n_pages, format="png", filenames=nomes_arq)
}
