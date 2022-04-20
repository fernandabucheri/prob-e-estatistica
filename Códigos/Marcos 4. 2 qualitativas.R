# Importando planilha
library(readxl)
respostas <- read_excel("Documents/Estudos/UNIFESP/Materias/Prob e Estatistica/Trabalho Final/Impacto dos jogos eletrônicos na vida acadêmica (respostas).xlsx")

require(summarytools)
# Tabela de dupla entrada com percentuais calculados em relação ao total de cada linha
view(ctable(respostas$"Por quantas horas do dia você joga?", respostas$"Qual é o seu coeficiente de rendimento (CR)?", prop = 'r', headings = FALSE))

# Tabela de dupla entrada com percentuais calculados em relação ao total de cada coluna
view(ctable(respostas$"Por quantas horas do dia você joga?", respostas$"Qual é o seu coeficiente de rendimento (CR)?", prop = 'c', headings = FALSE))

# Tabela de dupla entrada com percentuais calculados em relação ao total geral
view(ctable(respostas$"Por quantas horas do dia você joga?", respostas$"Qual é o seu coeficiente de rendimento (CR)?", prop = 't', headings = FALSE))

# Gerando grafico de barras na vertical
library(dplyr)
percentData <- respostas %>% group_by(`Por quantas horas do dia você joga?`) %>% count(`Qual é o seu coeficiente de rendimento (CR)?`) %>%  mutate(ratio=scales::percent(n/sum(n)))
percentData

require(ggplot2)
ggplot(respostas, aes(x=factor(`Por quantas horas do dia você joga?`),fill=factor(`Qual é o seu coeficiente de rendimento (CR)?`))) + 
  geom_bar(position="fill") + 
  geom_text(data=percentData, aes(y=n,label=ratio), position=position_fill(vjust=0.5))+
  xlab("Por quantas horas do dia você joga?") +
  ylab("Proporção de alunos") + 
  scale_fill_manual(name="Qual é o seu coeficiente de rendimento (CR)?", values = c("indianred2", "gold1", "seagreen3", "lightsalmon3")) +
  theme(axis.text.x = element_text(angle = 0, vjust = .5, size = 12)) + # tamanho fonte eixo x
  theme(text = element_text(size = 12)) + # tamanho fonte título eixo x e y
  theme(axis.text.y = element_text(size = 12)) + # tamanho fonte eixo y
  theme(legend.text=element_text(size = 11)) # tamanho fonte legenda

# Gerando grafico de barras na horizontal
ggplot(respostas, aes(x=factor(`Por quantas horas do dia você joga?`),fill=factor(`Qual é o seu coeficiente de rendimento (CR)?`))) + 
  geom_bar(position="fill") + 
  geom_text(data=percentData, aes(y=n,label=ratio), position=position_fill(vjust=0.5))+
  xlab("Por quantas horas do dia você joga?") +
  ylab("Proporção de alunos") + 
  scale_fill_manual(name="Qual é o seu coeficiente de rendimento (CR)?", values = c("indianred2", "gold1", "seagreen3", "lightsalmon3")) +
  theme(axis.text.x = element_text(angle = 0, vjust = .5, size = 13)) + # tamanho fonte eixo x
  theme(text = element_text(size = 13)) + # tamanho fonte título eixo x e y
  theme(axis.text.y = element_text(size = 13)) + # tamanho fonte eixo y
  theme(legend.text=element_text(size = 11)) + # tamanho fonte legenda
  theme(legend.position="top") + # fonte na parte de cima
  coord_flip() # barras na horizontal
