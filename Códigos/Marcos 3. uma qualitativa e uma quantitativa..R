# Importando planilha
library(readxl)
respostas <- read_excel("Documents/Estudos/UNIFESP/Materias/Prob e Estatistica/Trabalho Final/Impacto dos jogos eletrônicos na vida acadêmica (respostas).xlsx")

# Renomeando a coluna
names(respostas)[names(respostas)=="Idade"]="idade"

# Gerando grafico de barras na vertical
percentData <- respostas %>% group_by(idade) %>% count(`Por quantas horas do dia você joga?`) %>% mutate(ratio=scales::percent(n/sum(n)))
percentData

require(ggplot2)
ggplot(respostas, aes(x=factor(idade),fill=factor(`Por quantas horas do dia você joga?`))) + 
  geom_bar(position="fill") + 
  geom_text(data=percentData, aes(y=n,label=ratio), position=position_fill(vjust=0.5))+
  xlab("Idade (em anos)") +
  ylab("Percentual de Alunos") + 
  scale_fill_manual(name="Por quantas horas do dia você joga? x Idade", values = c("indianred2", "gold1", "seagreen3", "lightsalmon3")) +
  labs(title = "Por quantas horas do dia você joga? x Idade") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x = element_text(angle = 0, vjust = .5, size = 12)) + # tamanho fonte eixo x
  theme(text = element_text(size = 12)) + # tamanho fonte título eixo x e y
  theme(axis.text.y = element_text(size = 12)) + # tamanho fonte eixo y
  theme(legend.text=element_text(size = 11)) # tamanho fonte legenda

# Gerando grafico de barras na horizontal
ggplot(respostas, aes(x=factor(idade),fill=factor(`Por quantas horas do dia você joga?`))) + 
  geom_bar(position="fill") + 
  geom_text(data=percentData, aes(y=n,label=ratio), position=position_fill(vjust=0.5))+
  xlab("Idade (em anos)") +
  ylab("Percentual de Alunos") + 
  scale_fill_manual(name="Por quantas horas do dia você joga? x Idade", values = c("indianred2", "gold1", "seagreen3", "lightsalmon3")) +
  labs(title = "Por quantas horas do dia você joga? x Idade") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x = element_text(angle = 0, vjust = .5, size = 12)) + # tamanho fonte eixo x
  theme(text = element_text(size = 12)) + # tamanho fonte título eixo x e y
  theme(axis.text.y = element_text(size = 12)) + # tamanho fonte eixo y
  theme(legend.text=element_text(size = 11)) + # tamanho fonte legenda
  theme(legend.position="top") + # fonte na parte de cima
  coord_flip() # barras na horizontal

# Diagrama de caixas
boxplot(respostas$idade ~ respostas$`Por quantas horas do dia você joga?`, xlab = "Por quantas horas do dia você joga?", ylab = "Idade")
points(1:nlevels(respostas$`Por quantas horas do dia você joga?`), tapply(respostas$idade, respostas$`Por quantas horas do dia você joga?`, mean),  pch=3)
