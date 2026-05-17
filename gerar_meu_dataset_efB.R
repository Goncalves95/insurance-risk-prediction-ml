# encoding: UTF-8

gerar_meu_dataset <- function(numero_estudante,
                              n = NULL,
                              guardar_csv = TRUE,
                              mostrar_resumo = TRUE) {
  
  if (missing(numero_estudante)) {
    stop("Indique o número de estudante. Exemplo: gerar_meu_dataset(20231234)")
  }
  
  if (!is.numeric(numero_estudante) || length(numero_estudante) != 1 || is.na(numero_estudante)) {
    stop("O número de estudante deve ser um único valor numérico.")
  }
  
  numero_estudante <- as.integer(numero_estudante)
  set.seed(numero_estudante)
  
  if (is.null(n)) {
    n <- as.integer(sample(c(75, 80, 85), 1))
  }
  
  if (!is.numeric(n) || length(n) != 1 || is.na(n) || !(n %in% c(75, 80, 85))) {
    stop("O argumento 'n' deve ser 75, 80 ou 85.")
  }
  
  temas <- c(
    "DesempenhoAcademico",
    "CreditoBancario",
    "Saude",
    "AbandonoClientes",
    "JogoOnline",
    "Ecommerce",
    "Seguros",
    "RecursosHumanos",
    "Energia",
    "Transportes"
  )
  
  tema <- temas[(numero_estudante %% length(temas)) + 1]
  
  if (tema == "DesempenhoAcademico") {
    
    HorasEstudo <- round(runif(n, 0, 20), 1)
    Materiais <- pmin(3, pmax(1, round(1 + HorasEstudo / 9 + rnorm(n, 0, 0.6))))
    Atividades <- pmin(3, pmax(1, round(0.6 * Materiais + 0.08 * HorasEstudo + rnorm(n, 1, 0.6))))
    Intervencoes <- pmin(3, pmax(1, round(0.4 * Atividades + rnorm(n, 1.2, 0.7))))
    Avaliacoes <- pmin(3, pmax(1, round(0.5 * Atividades + 0.4 * Materiais + 0.1 * Intervencoes + rnorm(n, 0.9, 0.5))))
    
    dados <- data.frame(Materiais, Atividades, Intervencoes, Avaliacoes, HorasEstudo)
    
    score <- 0.45 * Materiais + 0.75 * Atividades + 0.25 * Intervencoes +
      1.1 * Avaliacoes + 0.08 * HorasEstudo + rnorm(n, 0, 0.5)
    
    dados$Classe <- ifelse(score > median(score), "Aprovado", "Reprovado")
  }
  
  else if (tema == "CreditoBancario") {
    
    Idade <- round(runif(n, 18, 70))
    Emprego <- sample(1:3, n, replace = TRUE, prob = c(0.20, 0.35, 0.45))
    Historico <- sample(1:3, n, replace = TRUE, prob = c(0.20, 0.45, 0.35))
    Dependentes <- pmin(4, pmax(0, round(rnorm(n, mean = pmax(0, (Idade - 25) / 15), sd = 1))))
    
    Rendimento <- round(pmax(
      500,
      650 + 22 * Idade + 400 * Emprego + 180 * Historico - 120 * Dependentes + rnorm(n, 0, 220)
    ))
    
    Divida <- round(pmax(
      0,
      Rendimento * runif(n, 0.05, 0.85) + 250 * Dependentes - 180 * Historico + rnorm(n, 0, 150)
    ))
    
    dados <- data.frame(Rendimento, Divida, Historico, Emprego, Dependentes)
    
    score <- 0.0022 * Rendimento - 0.0026 * Divida + 0.9 * Historico +
      0.6 * Emprego - 0.18 * Dependentes + rnorm(n, 0, 0.5)
    
    dados$Classe <- ifelse(score > median(score), "Aprovado", "Rejeitado")
  }
  
  else if (tema == "Saude") {
    
    Idade <- round(runif(n, 20, 85))
    AtividadeFisica <- sample(1:3, n, replace = TRUE, prob = c(0.35, 0.40, 0.25))
    Fumador <- sample(0:1, n, replace = TRUE, prob = c(0.72, 0.28))
    
    IMC <- round(pmin(42, pmax(18, rnorm(
      n,
      mean = 27 + 1.7 * Fumador - 1.4 * AtividadeFisica + 0.03 * (Idade - 45),
      sd = 3.2
    ))), 1)
    
    Pressao <- round(pmin(190, pmax(90,
      92 + 0.65 * Idade + 0.8 * IMC + 7 * Fumador - 4 * AtividadeFisica + rnorm(n, 0, 8)
    )))
    
    Colesterol <- round(pmin(340, pmax(130,
      135 + 1.0 * Idade + 2.0 * IMC + 15 * Fumador - 7 * AtividadeFisica + rnorm(n, 0, 18)
    )))
    
    dados <- data.frame(Idade, Pressao, Colesterol, AtividadeFisica, Fumador)
    
    score <- 0.025 * Idade + 0.020 * Pressao + 0.010 * Colesterol -
      0.7 * AtividadeFisica + 0.8 * Fumador + rnorm(n, 0, 0.8)
    
    dados$Classe <- ifelse(score > median(score), "Risco", "SemRisco")
  }
  
  else if (tema == "AbandonoClientes") {
    
    TempoCliente <- round(runif(n, 1, 72))
    PlanoPremium <- sample(0:1, n, replace = TRUE, prob = c(0.65, 0.35))
    UsoServico <- pmin(3, pmax(1, round(rnorm(n, 1.7 + 0.4 * PlanoPremium + 0.01 * TempoCliente, 0.7))))
    
    Satisfacao <- pmin(3, pmax(1, round(rnorm(
      n,
      mean = 1.5 + 0.4 * UsoServico + 0.35 * PlanoPremium + 0.01 * TempoCliente,
      sd = 0.6
    ))))
    
    Reclamacoes <- pmin(8, rpois(
      n,
      lambda = pmax(0.2, 4.2 - 0.9 * Satisfacao - 0.45 * PlanoPremium - 0.02 * TempoCliente)
    ))
    
    dados <- data.frame(TempoCliente, Reclamacoes, Satisfacao, UsoServico, PlanoPremium)
    
    score <- -0.03 * TempoCliente + 0.75 * Reclamacoes - 0.9 * Satisfacao -
      0.25 * UsoServico - 0.35 * PlanoPremium + rnorm(n, 0, 0.5)
    
    dados$Classe <- ifelse(score > median(score), "Sai", "Fica")
  }
  
  else if (tema == "JogoOnline") {
    
    HorasJogo <- round(runif(n, 0, 20), 1)
    Experiencia <- pmin(3, pmax(1, round(1 + HorasJogo / 8 + rnorm(n, 0, 0.6))))
    Cooperacao <- sample(1:3, n, replace = TRUE, prob = c(0.25, 0.45, 0.30))
    Estrategia <- pmin(3, pmax(1, round(0.5 * Experiencia + 0.3 * Cooperacao + rnorm(n, 1.0, 0.6))))
    
    Falhas <- pmin(10, pmax(0, round(
      7 - 1.0 * Experiencia - 0.8 * Estrategia - 0.4 * Cooperacao + rnorm(n, 0, 1)
    )))
    
    dados <- data.frame(HorasJogo, Estrategia, Cooperacao, Experiencia, Falhas)
    
    score <- 0.15 * HorasJogo + 0.75 * Estrategia + 0.6 * Cooperacao +
      0.7 * Experiencia - 0.45 * Falhas + rnorm(n, 0, 0.5)
    
    dados$Classe <- ifelse(score > median(score), "Ganha", "Perde")
  }
  
  else if (tema == "Ecommerce") {
    
    Frequencia <- sample(1:12, n, replace = TRUE)
    Promocoes <- sample(0:1, n, replace = TRUE, prob = c(0.55, 0.45))
    Avaliacoes <- sample(1:5, n, replace = TRUE, prob = c(0.08, 0.12, 0.30, 0.30, 0.20))
    
    TempoSite <- round(pmax(1, rnorm(
      n,
      mean = 3 + 1.2 * Frequencia + 1.8 * Promocoes + 0.8 * Avaliacoes,
      sd = 3
    )), 1)
    
    Paginas <- pmin(35, pmax(1, round(
      2 + TempoSite / 1.7 + 1.5 * Promocoes + rnorm(n, 0, 3)
    )))
    
    dados <- data.frame(TempoSite, Paginas, Promocoes, Avaliacoes, Frequencia)
    
    score <- 0.09 * TempoSite + 0.05 * Paginas + 0.8 * Promocoes +
      0.25 * Avaliacoes + 0.12 * Frequencia + rnorm(n, 0, 0.5)
    
    dados$Classe <- ifelse(score > median(score), "Compra", "NaoCompra")
  }
  
  else if (tema == "Seguros") {
    
    Idade <- round(runif(n, 18, 80))
    
    IdadeCarta <- sapply(Idade, function(x) {
      idade_max_carta <- min(35, x)
      sample(18:idade_max_carta, 1)
    })
    
    AnosCarta <- Idade - IdadeCarta
    
    TipoVeiculo <- sample(1:3, n, replace = TRUE, prob = c(0.35, 0.40, 0.25))
    
    UsoAnual <- round(pmax(2000, rnorm(
      n,
      mean = 6000 + 120 * AnosCarta + 1300 * TipoVeiculo,
      sd = 2300
    )))
    
    Acidentes <- pmin(6, rpois(
      n,
      lambda = pmax(0.15, 2.0 - 0.035 * AnosCarta + 0.25 * TipoVeiculo + 0.00001 * UsoAnual)
    ))
    
    dados <- data.frame(Idade, AnosCarta, Acidentes, TipoVeiculo, UsoAnual)
    
    score <- 0.02 * Idade - 0.045 * AnosCarta + 0.95 * Acidentes +
      0.7 * TipoVeiculo + 0.00004 * UsoAnual + rnorm(n, 0, 0.6)
    
    dados$Classe <- ifelse(score > median(score), "AltoRisco", "BaixoRisco")
  }
  
  else if (tema == "RecursosHumanos") {
    
    Idade <- round(runif(n, 21, 60))
    Educacao <- sample(1:3, n, replace = TRUE, prob = c(0.20, 0.45, 0.35))
    
    idade_inicio_trabalho <- ifelse(Educacao == 1, 18, ifelse(Educacao == 2, 21, 23))
    ExperienciaMax <- pmax(0, Idade - idade_inicio_trabalho)
    Experiencia <- round(runif(n, 0, ExperienciaMax), 1)
    
    TesteTecnico <- pmin(5, pmax(1, round(rnorm(
      n,
      mean = 1.8 + 0.10 * Experiencia + 0.45 * Educacao,
      sd = 0.8
    ))))
    
    Entrevista <- pmin(5, pmax(1, round(rnorm(
      n,
      mean = 2.0 + 0.08 * Experiencia + 0.35 * Educacao,
      sd = 0.8
    ))))
    
    dados <- data.frame(Experiencia, Educacao, Entrevista, TesteTecnico, Idade)
    
    score <- 0.18 * Experiencia + 0.45 * TesteTecnico + 0.55 * Entrevista +
      0.35 * Educacao + 0.005 * Idade + rnorm(n, 0, 0.5)
    
    dados$Classe <- ifelse(score > median(score), "Contratar", "Rejeitar")
  }
  
  else if (tema == "Energia") {
    
    Area <- round(runif(n, 40, 350), 1)
    Pessoas <- sample(1:6, n, replace = TRUE, prob = c(0.18, 0.22, 0.22, 0.18, 0.12, 0.08))
    
    Equipamentos <- pmin(15, pmax(2, round(
      2 + 1.1 * Pessoas + Area / 85 + rnorm(n, 0, 1.8)
    )))
    
    Isolamento <- sample(1:3, n, replace = TRUE, prob = c(0.30, 0.45, 0.25))
    Temperatura <- round(runif(n, 5, 35), 1)
    
    dados <- data.frame(Area, Pessoas, Equipamentos, Isolamento, Temperatura)
    
    score <- 0.012 * Area + 0.55 * Pessoas + 0.22 * Equipamentos -
      0.8 * Isolamento + 0.08 * abs(Temperatura - 21) + rnorm(n, 0, 0.5)
    
    dados$Classe <- ifelse(score > median(score), "AltoConsumo", "BaixoConsumo")
  }
  
  else if (tema == "Transportes") {
    
    Distancia <- round(runif(n, 1, 60), 1)
    HoraPico <- sample(0:1, n, replace = TRUE, prob = c(0.55, 0.45))
    Clima <- sample(0:1, n, replace = TRUE, prob = c(0.75, 0.25))
    
    Trafego <- pmin(3, pmax(1, round(
      1.2 + 0.9 * HoraPico + 0.5 * Clima + 0.01 * Distancia + rnorm(n, 0, 0.5)
    )))
    
    VelocidadeMedia <- round(pmax(15, pmin(120, rnorm(
      n,
      mean = 88 - 11 * Trafego - 12 * HoraPico - 9 * Clima - 0.20 * Distancia,
      sd = 7
    ))), 1)
    
    dados <- data.frame(Distancia, Trafego, HoraPico, Clima, VelocidadeMedia)
    
    score <- 0.05 * Distancia + 0.75 * Trafego + 0.8 * HoraPico +
      0.6 * Clima - 0.04 * VelocidadeMedia + rnorm(n, 0, 0.4)
    
    dados$Classe <- ifelse(score > median(score), "Atraso", "Pontual")
  }
  
  dados$Classe <- as.factor(dados$Classe)
  
  nome_ficheiro <- paste0("dataset_", tema, "_", numero_estudante, ".csv")
  
  if (guardar_csv) {
    write.csv(dados, nome_ficheiro, row.names = FALSE)
  }
  
  attr(dados, "tema") <- tema
  attr(dados, "numero_estudante") <- numero_estudante
  attr(dados, "ficheiro") <- nome_ficheiro
  
  if (mostrar_resumo) {
    cat("\n====================================\n")
    cat("Dataset gerado com sucesso\n")
    cat("Número de estudante:", numero_estudante, "\n")
    cat("Tema:", tema, "\n")
    cat("Número de observações:", n, "\n")
    cat("Ficheiro:", nome_ficheiro, "\n")
    cat("Distribuição da classe:\n")
    print(table(dados$Classe))
    cat("====================================\n\n")
  }
  
  return(dados)
}