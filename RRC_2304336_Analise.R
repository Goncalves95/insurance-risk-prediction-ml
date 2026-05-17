# ==============================================================================
# E-fólio B - Aprendizagem Automática (Estudante 2304336)
# Aluno: Fernando Gonçalves - 2304336
# Licenciatura de Engenharia e Informatica
# Unidade Corricular: Raciocínio e Representação do Conhecimento
# Script de Análise e Modelação - Tema: Seguros
# ==============================================================================

if(!require(rpart)) install.packages("rpart")
if(!require(rpart.plot)) install.packages("rpart.plot")
if(!require(caret)) install.packages("caret")
if(!require(class)) install.packages("class")
if(!require(neuralnet)) install.packages("neuralnet")

library(rpart)
library(rpart.plot)
library(caret)
library(class)
library(neuralnet)

# Conexão aos Dados (Leitura do ficheiro gerado)
meu_dataset <- read.csv("dataset_Seguros_2304336.csv")

# Converte a variável alvo para fator (Essencial para classificação)
meu_dataset$Classe <- as.factor(meu_dataset$Classe)

# Divisão do Dataset (80% Treino / 20% Teste)
set.seed(2304336)
indices <- createDataPartition(meu_dataset$Classe, p = 0.8, list = FALSE)
dados_treino <- meu_dataset[indices, ]
dados_teste  <- meu_dataset[-indices, ]

# ==============================================================================
# ÁRVORE DE DECISÃO
# ==============================================================================

modelo_arvore <- rpart(Classe ~ ., data = dados_treino, method = "class")

previsoes_arvore <- predict(modelo_arvore, dados_teste, type = "class")
print("--- MATRIZ DE CONFUSÃO: ÁRVORE DE DECISÃO ---")
confusionMatrix(previsoes_arvore, dados_teste$Classe)

# Guardar Gráfico de Alta Resolução em png
png("Arvore_2304336.png", width = 4000, height = 2500, res = 300)
rpart.plot(modelo_arvore, type = 2, extra = 104, fallen.leaves = TRUE, 
           main = "Árvore de Decisão - Estudante 2304336", tweak = 0.8)
dev.off()

# ==============================================================================
# k-NN (k=3)
# ==============================================================================

# Normalização obrigatória para k-NN
normalizar <- function(x) { return ((x - min(x)) / (max(x) - min(x))) }
dados_norm <- as.data.frame(lapply(meu_dataset[,1:5], normalizar))

treino_knn <- dados_norm[indices, ]
teste_knn  <- dados_norm[-indices, ]

previsao_knn <- knn(train = treino_knn, test = teste_knn, 
                    cl = dados_treino$Classe, k = 3)

print("--- MATRIZ DE CONFUSÃO: k-NN (k=3) ---")
confusionMatrix(previsao_knn, dados_teste$Classe)

# ==============================================================================
# REDE NEURONAL
# ==============================================================================

# Preparação de dados numéricos para a rede
treino_nn <- dados_treino
treino_nn$Classe <- ifelse(treino_nn$Classe == "AltoRisco", 1, 0)
teste_nn <- dados_teste
teste_nn$Classe <- ifelse(teste_nn$Classe == "AltoRisco", 1, 0)

set.seed(2304336)
modelo_rede <- neuralnet(Classe ~ Idade + AnosCarta + Acidentes + TipoVeiculo + UsoAnual, 
                         data = treino_nn, hidden = 3, linear.output = FALSE)

previsao_nn_prob <- compute(modelo_rede, teste_nn[, 1:5])
previsao_nn_classe <- ifelse(previsao_nn_prob$net.result > 0.5, "AltoRisco", "BaixoRisco")
print("--- MATRIZ DE CONFUSÃO: REDE NEURONAL ---")
confusionMatrix(as.factor(previsao_nn_classe), as.factor(dados_teste$Classe))

# Guardar Gráfico de Alta Resolução em png
png("Rede_Neuronal_2304336.png", width = 4000, height = 2500, res = 300)
plot(modelo_rede, show.weights = FALSE, information = FALSE, fill = "lightblue",
     col.entry = "darkblue", col.hidden = "darkgreen", col.out = "darkred",
     radius = 0.12, dimension = 6)
dev.off()

print("Processamento concluído com sucesso!")