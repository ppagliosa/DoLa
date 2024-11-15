#############################################
# PONTUACAO CAPES - DOCENTE
#############################################

# criterios de pontuacao para artigos
pontos_artigo <- function(eq){ # eq: estrato qualis
   valor <- c(1, 0.875, 0.75, 0.625, 0.50, 0.375, 0.25, 0.125, 0.1, 0.1)
  names(valor) <- c("A1", "A2", "A3", "A4", "B1", "B2", "B3", "B4", "NC", "NP") # novo qualis
  valor[eq]
}

# criterios de pontuacao de livros
pontos_livro <- function(eqcp = NULL, tipo){ # eq: estrato qualis
  
  # pontuacao CAPES
  if(tipo == "LIVRO_PUBLICADO"){
    valor <- c(1*3, 0.875*3, 0.75*3, 0.5*3, 0)
    names(valor) <- c("L4", "L3", "L2", "L1", "--")
    as.numeric(valor[eqcp])
    
  } else if(tipo == "LIVRO_ORGANIZADO_OU_EDICAO"){
    valor <- c(1, 0.875, 0.75, 0.5, 0)
    names(valor) <- c("L4", "L3", "L2", "L1", "--")
    as.numeric(valor[eqcp])
    
  } else if(tipo == "Capitulo de livro publicado") {
    valor <- c(1, 0.875, 0.75, 0.5, 0)
    names(valor) <- c("L4", "L3", "L2", "L1", "--")
    as.numeric(valor[eqcp])
  }  
}


# criterios de pontuacao publicacao em eventos
pontos_anais <- function(natureza){ 
  if(natureza == "completo"){
    0.25
  } else 0.125
}


# criterios de pontuacao orientacoes NO PROGRAMA
pontos_orientacao <- function(item, orientacao, nivel){
  
  # orientacoes concluidas
  if(item == 1){
    
    if(nivel == "dissertacao de mestrado"){
      if(orientacao == "orientador principal") {
        1
      } else if(orientacao == "co orientador"){
        1
      } else 1
    } else 1
    
    
    # orientacoes em andamento
  } else if(item == 2){
    
    if(nivel == "dissertacao de mestrado"){
      if(orientacao == "orientador principal") {
        0.5
      } else if(orientacao == "co orientador"){
        0.5
      } else 0.5
    } else 0.5
  }    
}


# criterios de pontuacao de bancas
pontos_banca <- function(item, nivel = NULL){
  
  # bancas no Programa
  if(item == 1){
    
    if(nivel == "mestrado"){
      0.25
    } else if(nivel == "exame de qualificacao de mestrado"){
      0.25
    } else if(nivel == "doutorado"){
      0.25
    } else if(nivel == "exame de qualificacao de doutorado"){
      0.25
    } else 0.25
    
    
    # bancas em outros programas
  } else if(item == 2){
    
    if(nivel == "mestrado"){
      0.25
    } else if(nivel == "exame de qualificacao de mestrado"){
      0.25
    } else if(nivel == "doutorado"){
      0.25
    } else if(nivel == "exame de qualificacao de doutorado"){
      0.25
    } else 0
    
    # quando for processo seletivo DISCENTE DO PROGRAMA
  } else if(item == 3){
    0
    
    # quando for outro tipo de banca julgadora
  } else if(item == 4){
    
    if(nivel == "professor titular"){
      0.25
    } else if(nivel == "concurso publico"){
      0.25
    } else if(nivel == "livre-docencia"){
      0.25
    } else if(nivel == "avalizacao de cursos"){
      0.25
    } else if(nivel == "outra"){
      0.25
    } else 0
  }
  
}


# criterios de pontuacao de projetos de pesquisa
pontos_projeto <- function(fl_responsavel, fl_financiado, tipo){
  
  # coordenador
  if(fl_responsavel == "SIM"){
    
    # financiado
    if(fl_financiado == "SIM"){
      1
      
      # nao financiado
    } else 0.125
    
    # participante
  } else {
    
    # financiado
    if(fl_financiado == "SIM"){
      0.25
      
      # nao financiado
    } else 0.125
  }
}


# criterios de pontuacao de projetos de pesquisa
#pontos_projeto <- function(fl_responsavel, fl_financiado, tipo){
#  
#  # coordenador
#  if(fl_responsavel == "SIM"){
#    
#    # financiado
#    if(fl_financiado == "SIM"){
#      1
#      
#      # nao financiado
#    } else {
#      if(tipo == "<GPDP>"){
#        0
#      } else if(tipo == "<GPII>"){
#        0
#      } else if(tipo == "<GPEC>"){
#        0
#      } else 0
#    }
#    
#    # participante
#  } else {
#    
#    # financiado
#    if(fl_financiado == "SIM"){
#      1
#      
#      # nao financiado
#    } else {
#      if(tipo == "<GPDP>"){
#        0
#      } else if(tipo == "<GPII>"){
#        0
#      } else if(tipo == "<GPEC>"){
#        0
#      } else 0
#    }
#  }
#  
#}


# criterios de pontuacao de disciplinas
pontos_disciplina <- function(fl_programa){
  if(fl_programa){
    0
  } else 0
}


# criterios de pontuacao de disciplinas
#pontos_disciplina <- function(fl_programa){
#  if(fl_programa){
#    1
#  } else 0
#  
#}

# criterios de pontuacao colaboracao tecnica
pontos_coltec <- function(item, fl_qualificado, fl_editor){
  
  # Membro de comite assessor
  if(item == 1){
    0.5
    
    # Membro de corpo editorial qualificado
  } else if(item == 2){
    
    if(fl_qualificado){
      # editor
      if(fl_editor){
        1
      } else{
        0.5
      }
    } else 0.5
  }
}

# criterios de pontuacao colaboracao tecnica
#pontos_coltec <- function(item, fl_qualificado, fl_editor){
#  
#  # Membro de comite assessor
#  if(item == 1){
#    1
#    
#    # Membro de corpo editorial qualificado
#  } else if(item == 2){
#    
#    if(fl_qualificado){
#      # editor
#      if(fl_editor){
#        1
#      } else{
#        1
#      }
#    } else 0
#  }
#  
#}


# criterios de pontuacao tecnica
pontos_tec <- function(item, param = NULL){
  
         valor <- c(1, 0.75, 0.50, 0.25, 0.125, 0)
  names(valor) <- c("T4", "T3", "T2", "T1", "T0", "--")
  
  # se item 0, retorna a pontuacao dos estratos em param
  if(item == 0){
    return(as.numeric(valor[param]))
  }  
  
  # criterios de pontuacao
  
  if(item == 1){
    # organizacao de evento
  if(param == "<INTERNACIONAL>") {
    list("T4", as.numeric(valor["T4"]))
  } else list("T4", as.numeric(valor["T4"]))  # Nacional 

  } else if(item == 2){
  # 2 - apresentacao de trabalho OU "Outras palestras e apresenta??es
  list("T1", as.numeric(valor["T1"]))   
  
} else if(item == 3){
  # 3 - participacao em eventos e congressos
  if(param[1] == "convidado") { 
    list("T1", as.numeric(valor["T1"]))
  } else if(param[1] == "participante" & param[2] == "apresentacao oral"){ 
    list("T1", as.numeric(valor["T1"]))
  } else list("T0", as.numeric(valor["T0"])) # Ouvinte e partic. poster/painel

} else if(item == 4){
  # 4 - material didatico
  list("T4", as.numeric(valor["T4"]))    

} else if(item == 5){
  # 5 - Curso de forma??o profissional e atividade de capacita??o
  if(param == "organizador") {
    list("T1", as.numeric(valor["T1"]))
  } else if(param == "docente") {
    list("T4", as.numeric(valor["T4"]))
  } else list("--", as.numeric(valor["--"])) # outros

} else if(item == 6){
  # 6 - Produ??o e participa??o em m?dias (entrevista, mesas redondas, programas e coment?rios)
  if(param == "<ORGANIZADOR>") {
  list("T2", as.numeric(valor["T2"]))
  } else list("T0", as.numeric(valor["T0"]))

} else if(item == 7){
  # 7 - redes sociais, websites e blogs
  list("T2", as.numeric(valor["T2"]))
  
} else if(item == 8){
  # 8 - Editoracao
  list("T2", as.numeric(valor["T2"])) 
  
} else if(item == 9){
  # 9 - artigo publicado em revista de divulgacao; artigo em jornal
  if(param == "Brasil"){ 
    list("T1", as.numeric(valor["T1"]))
    } else list("T1", as.numeric(valor["T1"])) # internacional

} else if(item == 10){
  # 10 - Artigo publicado em revista tecnica, resenha
  if(param == "revista tecnica"){ 
    list("T3", as.numeric(valor["T3"])) 
  } else if(param == "resenha"){
    list("T0", as.numeric(valor["T0"])) 
  } else list("--", as.numeric(valor["--"])) # outra producao bliografica
  
} else if(item == 11){
  # 11 - prefacio/posfacio de obra tecnica
    list("T0", as.numeric(valor["T0"]))

} else if(item == 12){
  # 12 - traducao
  list("T4", as.numeric(valor["T4"]))
  
} else if(item == 13){
  # 2 - relatorio de pesquisa
  list("T1", as.numeric(valor["T1"]))  
  
} else if(item == 14){
  # 14 - Relatorio tecnico conclusivo
  if(param[1] == "Parecer de artigo de revista" & param[2] != "--") {
  if(param[2] %in% c("A1","A2","B1", "B2"))  #      if(param[2] %in% c("A1","A2","A3","A4")) { 
     list("T1", as.numeric(valor["T1"]))
   } else list("T1", as.numeric(valor["T1"]))

} else if(item == 15){
  # 15 - OUTRA-PRODUCAO-TECNICA
  if(param == "Manual ou protocolo"){
    list("T2", as.numeric(valor["T2"]))
  } else if(param == "Base de dados tecnico-cientifica"){
    list("T4", as.numeric(valor["T4"]))
  } else if(param == "Norma ou marco regulatorio"){
    list("T4", as.numeric(valor["T4"]))
  } else if(param == "Acervo"){
    list("T3", as.numeric(valor["T3"]))
  } else if(param == "Empresa ou organizacao social inovadora"){
    list("T4", as.numeric(valor["T4"]))
  } else if(param == "Tecnologia social"){
    list("T4", as.numeric(valor["T4"]))
  } else if(param == "Taxonomia, ontologias e tesauros"){ 
    list("T0", as.numeric(valor["T0"]))
  } else if(param == "Relatorio tecnico conclusivo: Nota tecnica ou laudo tecnico"){
    list("T1", as.numeric(valor["T1"]))
  } else list("--", as.numeric(valor["--"])) # Outro tipo de produto

} else if(item == 16){
  # 16 - carta, mapa ou similar
  list("T2", as.numeric(valor["T2"]))   
  
} else if(item == 17){ 
  # 17 - software
  if(param == "registro de software"){ 
    list("T4", as.numeric(valor["T4"]))
  } else list("T4", as.numeric(valor["T4"])) # n?o registrado  
  
} else if(item == 18){
  # 18 - Produto nao patenteavel
  list("T4", as.numeric(valor["T4"]))
  
} else if(item == 19){
  # 19 - Processo ou tecnica
  list("T4", as.numeric(valor["T4"]))  

} else if(item == 20){
  # 20 - Patente
  list("T4", as.numeric(valor["T4"])) 

} else if(item == 21){
  # 21 - Cultivar
  list("T4", as.numeric(valor["T4"])) 
}
  } # FIM


# criterios de pontuacao de posdoc e livre-docencia
pontos_posdoc <- function(item){
  if(item == 1){
    1
  } else 0
}  # FIM


# criterios de pontuacao de prof. visitante e estagio de curta duracao
pontos_pvecd <- function(item){
  if(item == 1){
    1
  } else 0
}  # FIM


# criterios de pontuacao de premios
pontos_premio <- function(item){
  if(item == 1){
    0.5
  } else 0
}  # FIM

