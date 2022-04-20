library(readxl)
respostas <- read.csv("(...)\\Impacto dos jogos eletrônicos na vida acadêmica (respostas) - Respostas ao formulário 1.csv")
respostas <- read_excel("(...)\\Impacto dos jogos eletrônicos na vida acadêmica (respostas).xlsx")

     # Análise das variáveis quantitativas idade e período (de forma conjunta)

# Diagrama de dispersão
names(respostas)
par(mfrow=c(1,1))
plot(respostas$Idade~respostas$`Em que período da faculdade você se encontra?`, xlab = "Período", ylab = "Idade", col = "blue")

# Coeficientes de correlação
  #Coeficiente de correlação de Pearson
pearson <- cor(respostas$Idade,respostas$`Em que período da faculdade você se encontra?`); pearson

  #Coeficiente de correlação de Spearman
spearman <- cor(respostas$Idade,respostas$`Em que período da faculdade você se encontra?`, method = "spearman"); spearman

# Diagrama de dispersão e reta de mínimos quadrados
par(mfrow=c(1,1))
plot(respostas$Idade~respostas$`Em que período da faculdade você se encontra?`, xlab = "Período", ylab = "Idade", col = "blue") 
abline(lm(respostas$Idade~respostas$`Em que período da faculdade você se encontra?`), col= "red") #reta ajustada

#Gráfico de barras verticais
library(dplyr)
percentData <- respostas %>% group_by(`Idade`) %>% count(`Em que período da faculdade você se encontra?`) %>% mutate(ratio=scales::percent(n/sum(n)))
percentData

require(ggplot2)
ggplot(respostas, aes(x=factor(`Idade`),fill=factor(`Em que período da faculdade você se encontra?`))) +
  geom_bar(position="fill") +
  geom_text(data=percentData, aes(y=n,label=ratio), position=position_fill(vjust=0.5))+
  xlab("Idade") +
  ylab("Proporção de alunos") +
  scale_fill_manual(name="Em que período da faculdade\nvocê se encontra?", values = c("indianred2", "gold1", "seagreen3", "grey")) +
  theme(axis.text.x = element_text(angle = 0, vjust = .5, size = 12)) + # tamanho fonte eixo x
  theme(text = element_text(size = 12)) + # tamanho fonte título eixo x e y
  theme(axis.text.y = element_text(size = 12)) + # tamanho fonte eixo y
  theme(legend.text=element_text(size = 11)) # tamanho fonte legenda

