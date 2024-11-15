#' DoLa: Do Lattes para o PPG
#'
#' esta função extrai dados do curriculo Lattes dos docentes
#' de um determinado Programa de Pós-graduação
#'
#' @param id_especifico description
#' @param ano_ini description
#' @param ano_fim description
#' @param nome_instituicao description
#' @param nome_curso description
#' @param nome_area description
#' @param nome_comissao description
#' @param xlsx_qualis description
#' @param xlsx_qualis_livros description
#' @param doce_cv description
#' @param dice_cv description
#' @param fl_classificacao description
#'
#'
#'
#'
#'@export
dola<- function(id_especifico = NULL,
                ano_ini, ano_fim,
                nome_instituicao, nome_curso, nome_area,
                nome_comissao = NULL,
                xlsx_qualis, xlsx_qualis_livros = NULL,
                doce_cv, dice_cv,
                fl_classificacao) {
  rmarkdown::render("DoLa.Rmd")
  browseURL("DoLa.html")
}
