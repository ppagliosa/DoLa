encode_xml2 <- function(txt) {
  txt %>%
    str_replace_all("ã", "a") %>%
    str_replace_all("á", "a") %>%
    str_replace_all("à", "a") %>%
    str_replace_all("ä", "a") %>%
    str_replace_all("â", "a") %>%
    
    str_replace_all("Ã", "A") %>%
    str_replace_all("Á", "A") %>%
    str_replace_all("À", "A") %>%
    str_replace_all("Ä", "A") %>%
    str_replace_all("Â", "A") %>%
    
    str_replace_all("é", "e") %>%
    str_replace_all("ê", "e") %>%
    str_replace_all("É", "E") %>%
    str_replace_all("Ê", "E") %>%
    
    str_replace_all("í", "i") %>%
    str_replace_all("Í", "I") %>%
    
    str_replace_all("õ", "o") %>%
    str_replace_all("ó", "o") %>%
    str_replace_all("ö", "o") %>%
    str_replace_all("ô", "o") %>%
    
    str_replace_all("Õ", "O") %>%
    str_replace_all("Ó", "O") %>%
    str_replace_all("Ö", "O") %>%
    str_replace_all("Ô", "O") %>%
    
    str_replace_all("ú", "u") %>%
    str_replace_all("ü", "u") %>%
    str_replace_all("Ú", "U") %>%
    str_replace_all("Ü", "U") %>%
    
    str_replace_all("ç", "c") %>%
    str_replace_all("Ç", "C") %>%
    
    str_replace_all("ñ", "n") %>%
    str_replace_all("Ñ", "N")
}
