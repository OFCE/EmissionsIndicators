loadBridge <- function(from, to, sht, transverse = NULL) {
  
  if(is.null(transverse)){transverse = FALSE}
  
  
  path <- ifelse(Sys.info()['user']=="leasy",
                 str_c(path_codedata,"data_in/bridges/", from, ".to.", to, ".xlsx"),
                 str_c("data_in/bridges/", from, ".to.", to, ".xlsx"))
  
  # Industry names
  row.names <- openxlsx::read.xlsx(path, sheet = sht, cols = 1, colNames = FALSE) %>% .[[1]] 
  # Keep only non-NA cells
  row.count <- sum(!is.na(row.names))
  #row.names <- row.names[2:(1 + row.count)]
  
  # Product names
  col.names <- openxlsx::read.xlsx(path, sheet = sht, rows = 1, colNames = FALSE) %>% t %>% as.vector
  # Keep only non-NA cells
  col.count <- sum(!is.na(col.names))
  #col.names <- col.names[2:(1 + col.count)]
  
  # Correspondence matrix
  mat <- openxlsx::read.xlsx(path, sheet = sht, rows = c(2:(1+length(row.names))),cols = c(2:(1+length(col.names))), colNames = FALSE)  %>% as.matrix
  rownames(mat) <- row.names
  colnames(mat) <- col.names
  mat[is.na(mat)] <- 0
  
  if(transverse ==TRUE){mat <- t(mat)}
  
  mat
}