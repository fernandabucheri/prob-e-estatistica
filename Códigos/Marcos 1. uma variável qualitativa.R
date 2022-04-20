# Importando planilha
library(readxl)
respostas <- read_excel("Documents/Estudos/UNIFESP/Materias/Prob e Estatistica/Trabalho Final/Impacto dos jogos eletrônicos na vida acadêmica (respostas).xlsx")

# Renomeando a coluna
names(respostas)[names(respostas)=="Por quantas horas do dia você joga?"]="jogos"

# Removendo valores duplicados para a legenda
valores_unicos <- unique(respostas$jogos)

# Alterando margens
par(mfrow=c(3,1)) # 3 linhas e 1 coluna
par(oma=c(0.5,0.5,0.5,0.5)) # 0.5 de espaço das margens
par(mar=c(0,19,1,0)) # aumentando espaço para a esquerda

# Definindo cores do grafico de setores
cores <- c("purple", "yellow", "blue", "red", "green", "gray16", "pink", "orangered4", "lightsalmon3")

# Gerando grafico de setores
pielabels<- paste(round(table(respostas$jogos)/length(respostas$jogos)*100), "%", sep="")
pie(round((table(respostas$jogos)/length(respostas$jogos)*100), 2),labels=pielabels, cex=1.4, col=cores, radius = 1, main = "Horas")
legend(-5, 1, c(sort(valores_unicos)), cex = 1.1, fill = cores, xpd=TRUE)

# Gerando grafico de frequencia absoluta
par(las=1) # nomes dos eixos perpendicular 
par(mar=c(4,19,1,0.5))
barplot(sort(table(respostas$jogos), increasing = T), horiz=TRUE, main = "Frequencia absoluta:", xlab = "nº Alunos", cex.lab=1.1, cex.names=1.1, cex.axis=1.1, col="lightgreen", xlim=c(0,15))

# Gerando grafico de frequencia relativa
barplot(sort(table(respostas$jogos), increasing = T)/length(respostas$jogos)*100, horiz=TRUE, main = "Frequencia relativa:", xlab = "% Alunos", cex.lab=1.1, cex.names=1.1, cex.axis=1.1, col="lightblue", xlim=c(0,70))
