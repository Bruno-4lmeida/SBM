
# PASSO 1: CARREGAR PACOTES E PREPARAR O AMBIENTE####


# Instale os pacotes se ainda não os tiver (rode no console apenas uma vez)
install.packages(c("tidyverse", "factoextra", "corrplot"))

library(tidyverse)    # Para manipulação de dados (dplyr) e gráficos (ggplot2)
library(factoextra)   # Para visualizações de PCA e clustering
library(corrplot)     # Para matriz de correlação (opcional, mas útil)


# PASSO 2: PREPARAÇÃO DOS DADOS ####

# ler o dataset .csv

syntetic_dna = read.csv("synthetic_dna_dataset.csv")

# 2.1 - Selecionar apenas as variáveis numéricas relevantes para a PCA.
# Vamos excluir colunas de texto (IDs, sequências, rótulos).
dados_numericos <- syntetic_dna %>%
  select_if(is.numeric) %>% 
  select(-"Sequence_Length")

# 2.2 - Guardar os rótulos para usar nos gráficos depois.

labels_e_ids <- syntetic_dna %>%
  select(Sample_ID, Class_Label, Disease_Risk)

# 2.3 - Definir os IDs das amostras como nomes das linhas para identificação.
# Isso ajuda a identificar pontos específicos nos gráficos, se necessário.
row.names(dados_numericos) <- syntetic_dna$Sample_ID

# 2.4 - Normalizar os dados.
# Este é um passo CRÍTICO para a PCA, pois garante que variáveis com escalas
# diferentes (ex: uma de 0-1 e outra de 100-1000) tenham o mesmo peso.
dados_normalizados <- scale(dados_numericos)


# PASSO 3: ANÁLISE DE COMPONENTES PRINCIPAIS (PCA) ####

# 3.1 - Realizar a PCA
pca_result <- prcomp(dados_normalizados, center = TRUE, scale. = TRUE)

# 3.2 - Analisar o resultado da PCA
# O summary mostra a importância de cada componente principal (PC).
message("--- Resumo da PCA ---")
summary(pca_result)

# 3.3 - Visualizar a variância explicada por cada componente (Scree Plot)
# Este gráfico ajuda a decidir quantos componentes principais são necessários
# para capturar a maior parte da informação dos dados.
fviz_eig(pca_result, addlabels = TRUE, barfill = "steelblue", barcolor = "black") +
  ggtitle("Variância Explicada por Componente Principal") +
  theme_minimal()


# 3.4 - Preparar dados para visualização
# Vamos criar um dataframe com os "scores" (as novas coordenadas) de cada amostra
# e juntar com os rótulos que guardamos.
pca_scores <- as.data.frame(pca_result$x) %>%
  bind_cols(labels_e_ids) # Junta os scores com os rótulos originais

# 3.5 - Criar gráficos da PCA
# Gráfico 1: Amostras coloridas por 'Class_Label'
ggplot(pca_scores, aes(x = PC1, y = PC2, color = Class_Label)) +
  geom_point(alpha = 0.8, size = 3) +
  ggtitle("Amostras no Espaço da PCA - Colorido por Classe") +
  xlab(paste0("PC1 (", round(summary(pca_result)$importance[2,1]*100, 1), "%)")) +
  ylab(paste0("PC2 (", round(summary(pca_result)$importance[2,2]*100, 1), "%)")) +
  theme_minimal()

# Gráfico 2: Amostras coloridas por 'Disease_Risk'
ggplot(pca_scores, aes(x = PC1, y = PC2, color = Disease_Risk)) +
  geom_point(alpha = 0.8, size = 3) +
  ggtitle("Amostras no Espaço da PCA - Colorido por Risco de Doença") +
  xlab(paste0("PC1 (", round(summary(pca_result)$importance[2,1]*100, 1), "%)")) +
  ylab(paste0("PC2 (", round(summary(pca_result)$importance[2,2]*100, 1), "%)")) +
  theme_minimal()

# Gráfico 3: Biplot (mostra a influência das variáveis originais)
fviz_pca_biplot(pca_result,
                label = "var",      # Mostra os nomes das variáveis
                repel = TRUE,       # Evita que os nomes se sobreponham
                col.ind = "#2c7fb8", # Cor das amostras
                col.var = "#d95f02") + # Cor das variáveis
  ggtitle("Biplot da PCA: Amostras e Variáveis")

# PASSO 4: ANÁLISE DE CLUSTER (K-MEANS) ####

# 4.1 - Determinar o número ideal de clusters (Método do Cotovelo - Elbow Method)
# Este método calcula a soma dos quadrados dentro dos clusters para diferentes
# valores de k. O "cotovelo" no gráfico sugere o número ideal de clusters.
fviz_nbclust(dados_normalizados, kmeans, method = "wss") +
  ggtitle("Método do Cotovelo para Determinar K ideal")

# 4.2 - Aplicar o algoritmo K-Means
# Com base no gráfico acima, escolha um número de clusters (vamos usar 3 como exemplo).
# Usamos set.seed para que o resultado seja sempre o mesmo ao rodar o código.
set.seed(123)
kmeans_result <- kmeans(dados_normalizados, centers = 3, nstart = 25)

# 4.3 - Adicionar os resultados do cluster ao nosso dataframe de scores da PCA
pca_scores$Cluster_KMeans <- as.factor(kmeans_result$cluster)

# 4.4 - Visualizar os clusters no espaço da PCA
# Este gráfico nos mostra se os grupos encontrados pelo K-Means
# correspondem à separação visual que a PCA nos deu.
ggplot(pca_scores, aes(x = PC1, y = PC2, color = Cluster_KMeans)) +
  geom_point(alpha = 0.8, size = 3) +
  ggtitle("Clusters (K-Means) no Espaço dos Componentes Principais") +
  scale_color_brewer(palette = "Set1") +
  theme_minimal()

# PASSO 5: INTERPRETAÇÃO BIOLÓGICA DOS CLUSTERS ####

# Objetivo: Entender o que caracteriza cada cluster que o K-Means encontrou.

# 5.1 - Extrair os "centros" dos clusters.
# Cada centro representa o valor médio (normalizado) de cada variável para aquele cluster.
cluster_centers <- kmeans_result$centers

# 5.2 - Transformar os centros em um dataframe "longo" para facilitar a visualização
cluster_centers_df <- as.data.frame(cluster_centers) %>%
  mutate(Cluster = as.factor(1:nrow(.))) %>% # Adiciona uma coluna com o número do cluster
  pivot_longer(
    cols = -Cluster, # Empilha todas as colunas, exceto a do Cluster
    names_to = "Variavel",
    values_to = "Valor_Medio_Normalizado"
  )

# 5.3 - Visualizar os centros com um Heatmap
# O heatmap é a melhor forma de comparar os perfis dos clusters.
# Cores quentes = valor acima da média; Cores frias = valor abaixo da média.
ggplot(cluster_centers_df, aes(x = Variavel, y = Cluster, fill = Valor_Medio_Normalizado)) +
  geom_tile(color = "white") + # geom_tile cria o heatmap
  scale_fill_gradient2(
    low = "blue",      # Valor baixo
    mid = "white",     # Valor médio (zero)
    high = "red",      # Valor alto
    midpoint = 0
  ) +
  labs(
    title = "Perfil das Variáveis por Cluster (Centros do K-Means)",
    x = "Variável Genômica",
    y = "Cluster",
    fill = "Valor Médio\nNormalizado"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) # Rotaciona os nomes das variáveis


