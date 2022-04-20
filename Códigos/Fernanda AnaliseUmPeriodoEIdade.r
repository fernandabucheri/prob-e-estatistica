library(readxl)
respostas <- read.csv("(...)\\Impacto dos jogos eletrônicos na vida acadêmica (respostas) - Respostas ao formulário 1.csv")
respostas <- read_excel("(...)\\Impacto dos jogos eletrônicos na vida acadêmica (respostas).xlsx")

    # Análise das variáveis quantitativas idade e período

#PERÍODO

# Gráfico de barras verticais
p<-(table(respostas$`Em que período da faculdade você se encontra?`)/length(respostas$`Em que período da faculdade você se encontra?`))*100
x<-barplot(p, ylab ="% de alunos", xlab="Período", ylim=c(0,100), col="skyblue2")
text(x, p+3, labels=paste(p, "%", sep=""), col="black", cex=1.0)

# Gráfico de frequências acumuladas
plot((ecdf(respostas$`Em que período da faculdade você se encontra?`)), main="", ylab="Proporção de alunos", xlab="Período")

# Os dois gráficos (de barras verticais e de frequências acumuladas) em uma mesma janela
par(mfrow=c(1,2))
x<-barplot(p, ylab ="% de alunos", xlab="Período", ylim=c(0,100), col="skyblue2")
text(x, p+3, labels=paste(p, "%", sep=""), col="black", cex=1.0)
plot((ecdf(respostas$`Em que período da faculdade você se encontra?`)), main="", ylab="Proporção de alunos", xlab="Período")

par(mfrow=c(1,1))

descr(respostas$`Em que período da faculdade você se encontra?`)

#IDADE

sort(respostas$Idade)
tab=fdt(respostas$Idade, start=18,h=2,end=26)
tab # para visualizar a tabela

# Histograma com polígono de frequências
plot(tab, type='fh', xlab="Idade",ylab="nº de alunos", col = "lightpink")
par(new=TRUE)
plot(tab,type='fp', xlab="Idade",ylab="nº de alunos", col = "black")

# Gráfico de frequências acumuladas
plot(tab,type='cfpp', xlab="Idade",ylab="% de alunos", ylim=c(0,100), col = "black" ) 

# Diagrama de caixas
boxplot(respostas$Idade, ylab = "Idade")
points(mean(respostas$Idade), pch=3)

# Os três gráficos (histograma com polígono de frequências, frequências acumuladas e diagrama de caixas) em uma mesma janela
par(mfrow=c(1,3))
boxplot(respostas$Idade, ylab = "Idade") #diagrama de caixas
points(mean(respostas$Idade), pch=3)
plot(tab, type='rfph', xlab="Idade",ylab="% de alunos", col = "lightpink")
par(new=TRUE)
plot(tab,type='rfpp', xlab="Idade",ylab="% de alunos", col = "black") # histograma e #poligono de frequencia
plot(tab,type='cfpp', xlab="Idade",ylab="% acumulados", ylim=c(0,100), col = "black") #ogiva de Galton (% acumulados)

# Medidas Resumo
descr(respostas$Idade)
