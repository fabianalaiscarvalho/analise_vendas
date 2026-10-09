
# Instalação e Carregamento dos Pacotes -----------------------------------
# install.packages("dplyr")
# install.packages("readxl")
# install.packages("ggplot2")

library(dplyr)
library(readxl)
library(ggplot2)

# Importação e Conhecimento dos Dados -------------------------------------
vendas_df <- read_excel("Vendas.xlsx")

View(vendas_df)
head(vendas_df)
str(vendas_df)
summary(vendas_df)

# Manipulação dos Dados ---------------------------------------------------

# Calcular faturamento
faturamento <- sum(vendas_df$Valor_Final)
faturamento

# Calcular faturamento por loja sem pipe
faturamento_loja <- group_by(vendas_df, ID_Loja)
faturamento_loja <- summarise(faturamento_loja, Faturamento_Total = sum(Valor_Final))

# Calcular faturamento por loja com pipe
faturamento_loja2 <- vendas_df %>% 
  group_by(ID_Loja) %>% 
  summarise(Faturamento_Total = sum(Valor_Final))

# Calcular faturamento por produto
faturamento_produto <- vendas_df %>% 
  group_by(ID_Loja, Produto) %>% 
  summarise(Faturamento_Total = sum(Valor_Final))

# Visualização dos Dados --------------------------------------------------

ggplot(data = faturamento_loja, mapping = aes(x = ID_Loja, y = Faturamento_Total)) +
  geom_col() +
  theme_minimal() +
  labs(
    title = "Faturamento por Loja",
    subtitle = "Iguatemi Campinas é a loja responsável pelo maior faturamento da empresa.",
    x = NULL,
    y = "Faturamento"
    )

ggplot(faturamento_produto, aes(x = ID_Loja, y = Faturamento_Total, fill = Produto)) +
  geom_col(position = "dodge") +
  theme_minimal() +
  labs(
    title = "Faturamento por Produto",
    subtitle = "A bermuda liso é o produto de maior sucesso.",
    x = NULL,
    y = "Faturamento",
    fill = NULL

