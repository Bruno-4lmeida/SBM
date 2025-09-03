
#### PASSO 1: INSTALAÇÃO DE PACOTES (rodar apenas uma vez no console) ####

# O 'tidyverse' é para manipulação geral de dados.
# O 'rsample' é parte do ecossistema 'tidymodels' e é excelente para divisões de dados.
#install.packages(c("tidyverse", "rsample", "randomForest, caret"))

#### Chamar os pacotes necessários

library(randomForest)
library(tidyverse)
library(rsample
library(caret)

### ler o arquivo .csv do data set completo

diabetes_set = read.csv("diabetes_prediction_dataset.csv")

summary(diabetes_set)

message("Valores únicos na coluna 'gender':")
print(unique(diabetes_set$gender))

message("\nValores únicos na coluna 'smoking_history':")
print(unique(diabetes_set$smoking_history))

### para transformar uma variável de uma classe para outra, nesse caso está transformando para fator ####
###         dados_treino$diabetes <- as.factor(dados_treino$diabetes) ###
###          dados_teste$diabetes <- as.factor(dados_teste$diabetes) ###

diabetes_transformado = diabetes_set %>%
  mutate(
    gender = as.factor(gender),
    smoking_history = as.factor(smoking_history),
    diabetes = as.factor(diabetes)
  )

message("\n--- Resumo do Dataset Transformado ---")
summary(diabetes_transformado)

### PASSO 2 VERIFICAR VALORES AUSENTES (NA) ####

message("\n--- Contagem de Valores Ausentes (NA) por Coluna ---")
sapply(diabetes_transformado, function(x) sum(is.na(x)))

### PASSO 3: DIVIDIR OS DADOS EM TREINO E TESTE (80/20 ESTRATIFICADO) ####

# 'initial_split' cria um objeto que contém as "instruções" para a divisão.
# prop = 0.8 significa que 80% dos dados irão para o conjunto de treino.
# strata = diabetes garante que a proporção de 0s e 1s seja a mesma nos dois conjuntos.

split_obj = initial_split(diabetes_transformado, prop = 0.8, strata = diabetes)

# Extrai os dataframes de treino e teste a partir do objeto de divisão
dados_treino = training(split_obj)
dados_teste  = testing(split_obj)


### PASSO 4: VERIFICAR A DIVISÃO    ####

# Verifica as dimensões (linhas, colunas) de cada conjunto
message("Dimensões do dataset completo:")
print(dim(diabetes_set)) # Saída: 100000 linhas

message("\nDimensões do dataset de treino:")
print(dim(dados_treino)) # Saída: 80000 linhas

message("\nDimensões do dataset de teste:")
print(dim(dados_teste)) # Saída: 20000 linhas

# Verifica a proporção da variável 'diabetes' em cada conjunto

message("\nProporção de 'diabetes' no dataset completo:")
print(prop.table(table(diabetes_set$diabetes)))

message("\nProporção de 'diabetes' no dataset de treino:")
print(prop.table(table(dados_treino$diabetes)))

message("\nProporção de 'diabetes' no dataset de teste:")
print(prop.table(table(dados_teste$diabetes)))


### PASSO 5: TREINANDO O MODELO  ####


# Define a fórmula do modelo
formula_diabetes = as.formula("diabetes ~ .") # O "." significa "usar todas as outras colunas"

# Treina o modelo Random Forest
# Usamos dados_treino, que contém a resposta 'diabetes'
modelo_rf <- randomForest(
  formula = formula_diabetes,
  data = dados_treino,
  ntree = 100,
  importance = T
)

print(modelo_rf)

plot(modelo_rf)


### PASSO 6: Analisando a Importância das Variáveis ####

# Gera a matriz de importância das variáveis (MeanDecreaseAccuracy)
# type=1 mede o quanto a acurácia do modelo piora se a variável for removida
importancia_modelo = importance(modelo_rf, type = 1)

# Visualiza a matriz de importância
print(importancia_modelo)

# Transforma a matriz em um dataframe para facilitar a criação de gráficos com ggplot2
tabela_importancia = data.frame(
  variavel = row.names(importancia_modelo),
  importancia = importancia_modelo[, 1]
)

# Visualiza a tabela formatada
print(tabela_importancia)


### PASSO 7: Gerando o Gráfico de Importância das Variáveis ####

grafico_importancia <- ggplot(tabela_importancia,
                              aes(x = reorder(variavel, importancia), y = importancia)) +
  geom_bar(stat = "identity", fill = "#008080") + 
  geom_text(aes(label = round(importancia, 2)), hjust = -0.2, size = 4) + 
  coord_flip() +
  theme_light(base_size = 16) +
  labs(
    x = "Variáveis Preditoras",
    y = "Importância (Diminuição Média da Acurácia)",
    title = "Importância das Variáveis na Predição de Diabetes (Modelo RF)",
    caption = "Fonte: o autor"
  ) +
  theme(plot.title = element_text(size = 18, hjust = 0.5))

# Mostra o gráfico
print(grafico_importancia)


# USANDO o modelo treinado ('modelo_rf') para fazer previsões no conjunto de teste ('dados_teste')

previsoes <- predict(modelo_rf, newdata = dados_teste)

# Cria um dataframe final que combina os dados de teste originais com a coluna de previsões
resultados_finais = dados_teste
resultados_finais$previsao_diabetes = previsoes

# Visualiza as primeiras linhas do dataframe de resultados para verificar
head(resultados_finais)

# Salva o dataframe com as previsões em um arquivo .csv no seu diretório de trabalho
write.csv(resultados_finais, file = "previsoes_diabetes.csv", row.names = FALSE)

### PASSO 8: Certifique-se de que seus dados de teste e previsões estão no formato 'factor'####
# (o que já fizemos anteriormente)
previsoes <- as.factor(resultados_finais$previsao_diabetes)
valores_reais <- as.factor(resultados_finais$diabetes)

# Crie a matriz de confusão 
matriz_confusao <- confusionMatrix(data = previsoes, reference = valores_reais, positive = "1")

# Imprima o resultado completo
print(matriz_confusao)


### PASSO 9: EXTRAIR A TABELA E CONVERTER PARA DATAFRAME ####

# A matriz em si está armazenada no elemento "$table" do objeto
# Usamos as.data.frame() para converter essa tabela em um dataframe limpo

matriz_df <- as.data.frame(matriz_confusao$table)

# RENOMEIA AS COLUNAS
colnames(matriz_df) <- c("Previsão", "Realidade", "Frequência")


print(matriz_df)


# VISUALIZANDO A MATRIZ 

ggplot(data = matriz_df, aes(x = Previsão, y = Realidade, fill = Frequência)) +
  geom_tile() + # Cria os "azulejos" do heatmap
  geom_text(aes(label = Frequência), color = "white", size = 8) + # Adiciona os números no centro
  scale_fill_gradient(low = "lightblue", high = "darkblue") + # Define a escala de cores
  labs(
    title = "Matriz de Confusão - Performance do Modelo de Predição de Diabetes",
    x = "O que o Modelo Previu",
    y = "O que Realmente Aconteceu"
  ) +
  theme_minimal(base_size = 16) +
  theme(plot.title = element_text(hjust = 0.5)) # Centraliza o título
