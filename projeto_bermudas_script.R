# Instalação e Carregamento de pacotes ------------------------------------
# dplyr = ajuda a manipular os dados
# readxl = permite que importe a base xlsx
# ggplot2 = permite crie e visualize graficos de forma otimizado
# install.packages("dplyr")
# install.packages("readxl")
# install.packages("ggplot2")

library(dplyr)
library(readxl)
library(ggplot2)

# Importação e Conhecimento dos Dados -------------------------------------
vendas_df <- read_excel("Vendas.xlsx")

View(vendas_df)

# head() = consegue ver as 6 primeiras linhas da tabela
head(vendas_df)

#str() = mostra a estrutura dos dados(se é String/numerico/char)
str(vendas_df)

# summary() = resumo estatistico dos dados
summary(vendas_df)

# Manipulação dos Dados ---------------------------------------------------

# calcular faturamento
faturamento <- sum(vendas_df$Valor_Final)
print(faturamento)

# calcular faturamento por loja SEM O PIPE
faturamento_loja <- group_by(vendas_df, ID_Loja)

# summarise = combinação do group_by com summarise (vem quando usa o group_by irá agrupar e o summarise ira fazer contas estatisticas com esses grupos)
faturamento_loja <- summarise(faturamento_loja, Faturamento_Total = sum(Valor_Final))

# calcular faturamento por loja COM O PIPE (ctrl + shift M)
# o operador PIPE vai pegar toda a informação que tirar a esquerda dele e colocar com primeiro argumento %>%  

faturamento_loja2 <-  vendas_df %>%  
  group_by(ID_Loja) %>% 
  summarise(Faturameto_Total = sum(Valor_Final))

#Calcular faturamento por produto

faturamento_produto <- vendas_df %>% 
  group_by(ID_Loja, Produto) %>% 
  summarise(Faturamento_Total = sum(Valor_Final))

# Visualização dos Dados --------------------------------------------------

ggplot(data = faturamento_loja, mapping = aes(x = ID_Loja, y = Faturamento_Total)) +
 
  # geom_bar(stat = "identity") ou geom_col() = para aparecer as barras do grafico
  # theme_minimal = theme_ = tema
  # labs() = Permite adicionar titulos(title), subtitulos(subtitle), renomear os eixos
  # fill = preenche as barras com cores
  # geom_col(position = "dodge") - position = posição das barras / dodge(lado a lado)
  
  geom_col() +
  theme_minimal() +
  labs(
    title = "Faturamento por Loja",
    subtitle = "Iguatemi Camopias é a loja responsável pelo maior faturamento da empresa.",
    x = NULL,
    y = "Faturamento") 

ggplot(faturamento_produto, aes(x = ID_Loja, y = Faturamento_Total, fill = Produto)) +
  geom_col(position = "dodge") +
  theme_minimal() +
  labs(
    title = "Faturamento por Produto",
    subtitle = "A bermuda liso é o produto de maior sucesso.",
    x = NULL,
    y = "Faturamento",
    fill = NULL
  )
  
  


