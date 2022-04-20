library(readr)
Impacto_dos_jogos <- read_csv("Documentos/BKP/Wendell/01-UNIFESP/Sem-01.2020/PE/trab/Impacto_dos_jogos.csv")
View(Impacto_dos_jogos)

#Plotando os Gráficos
library(ggplot2)

#Sexo frequência relativa
barplot((table(Impacto_dos_jogos$Sexo)/length(Impacto_dos_jogos$Sexo)*100), xlab = "Sexo", ylab = "% Alunos", col="pink", ylim=c(0,100), width = c(0.5, 0.5))

#Sexo total
ggplot(Impacto_dos_jogos) +
 aes(x = Sexo) +
 geom_bar(fill = "#c6dbef") +
 labs(y = "Número de Alunos") +
 coord_flip() +
 theme_bw()

#Curso pizza frequencia relativa
cols <- c("deepskyblue1","turquoise3","mediumpurple3","tan1")
pielabels<- paste(round(table(Impacto_dos_jogos$`Qual curso você faz ou pretende fazer?`)/length(Impacto_dos_jogos$`Qual curso você faz ou pretende fazer?`)*100), "%", sep="")
pie(round((table(Impacto_dos_jogos$`Qual curso você faz ou pretende fazer?`)/length(Impacto_dos_jogos$`Qual curso você faz ou pretende fazer?`)*100),2),labels=pielabels, cex=1.2, col=cols)
legend("topright", c("BCT", "Engenharia da Computação", "Engenharia Biomédica", "Ciência da Computalção"), cex = 0.8,
       fill = c("deepskyblue1","turquoise3","mediumpurple3","tan1"))

#Numero total por curso
par(mfrow=c(1,1))

Sim =c(3, 0, 4, 9, 4, 0, 0)

par(las=1) # nomes dos eixos perpendicular
par(mar=c(5,15,1,1)+0.1) # para aumentar a margem a esquerda 

x <- barplot(Sim, xlab = "Número de alunos", col="skyblue1",
             names.arg=c(row.names =c("BCT",
                                      "Biotecnologia",
                                      "Ciência da Computação",
                                      "Engenharia Biomédica",
                                      "Engenharia da Computação",
                                      "Engenharia de Materiais",
                                      "Matemática Computacional")), horiz=TRUE) 
text(Sim-0.5*Sim, x,labels=round(Sim), col="black", cex=1.0)


#Coeficiente de Rendimento freq relativa
barplot((table(Impacto_dos_jogos$`Qual é o seu coeficiente de rendimento (CR)?`)/length(Impacto_dos_jogos$`Qual é o seu coeficiente de rendimento (CR)?`)*100), xlab = "CR", ylab = "% alunos", col="turquoise", ylim=c(0,100)) + theme_classic()

#numero total
ggplot(Impacto_dos_jogos) +
 aes(x = `Qual é o seu coeficiente de rendimento (CR)?`) +
 geom_bar(position = "dodge", fill = "#fc9272") +
 labs(x = "Coeficiente de Rendimento", y = "Número de Alunos") +
 coord_flip() +
 theme_classic()

#Estado Civil
pie((table(Impacto_dos_jogos$`Qual é o seu estado civil?`)/length(Impacto_dos_jogos$`Qual é o seu estado civil?`)*100), xlab = "Estado Civil", col="deepskyblue1", ylim=c(0,100))
legend("bottomright", c("Solteiro", "Casado"), cex = 0.8, fill = c("deepskyblue1", "black"))

#PLOT Tipos de Jogos
sistema <- data.frame(sim=c(25, 25, 20, 35, 10, 35, 15, 80, 10, 5, 5), nao=c((100-25), (100-25), (100-20), (100-35), (100-10), (100-35), (100-15), (100-80), (100-10), (100-5), (100-5)))

par(mfrow=c(1,1), mar=c(5,4,4,4)+0.1, oma=c(0,0,0,4)) # para aumentar a margem da direita.

x <- barplot(t(data.matrix(sistema)),  col=c("deepskyblue1","palegreen"), border=NA,  cex.lab=1.0, cex.axis =1.0, cex.names=1.0, ylab="% de alunos", xlab="Tipos de Jogos", names.arg=c(row.names =c(
  "MOBA", "FPS", "RTS", "RPG", "MMORPG", "BR", "PVP", "JC", "NA", "JDH", "FIFA"
  )), width = c(1, 1.5, 1, 1, 2.2, 1, 1, 1, 1, 1, 1))

par(mar = c(0,0,0,0), oma = c(0,0,0,0), new = TRUE)
legend(x = "right", colnames(sistema), fill = c("deepskyblue1","palegreen"), bty = "n")

par(mar=c(5,4,4,4)+0.1, oma=c(0,0,0,4), new=TRUE)  

library(dplyr) # para usar na_if
text(x, sistema$sim-0.5*sistema$sim,  labels=na_if(round(sistema$sim),0), col="black", cex=1.0)
text(x, sistema$sim+sistema$nao-0.5*sistema$nao, labels=na_if(round(sistema$nao),0), col="black", cex=1.0)
