gerar_extendedData_figuras <- function(arq_full_path) {
  xml <- rep('<Data name="test">
    <displayName></displayName>
      <value> 
        <img style="max-width:500px;" src="%s"></img>
      </value>
  </Data>', n_pages-3)
  
  for(i in seq_along(xml)) {
    xml[i] <- sprintf(xml[i], arq_full_path[i]) }
  return(unlist(xml))
}
