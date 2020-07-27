# instala pacotes

install.packages(c("Hmisc","nortest","car","faraway","lattice","survival","Formula","ggplot2"))
install.packages(c("e1071","glm2"))
install.packages("caret")
install.packages("GGally")
install.packages("lattice")
install.packages("caTools")
install.packages("ROCR")

# importa dataset dos pacientes e carrega libraries

library(glm2)# Pacote para criar o modelo
library(readxl)
library(Hmisc)
library(nortest)
library(car)
library(faraway)
library(e1071)
library(GGally)
library(caTools)
library(dplyr)
library(ROCR)
library(boot)

#import data
credito <- read_excel("credito2.xlsx")
View(credito)

#usa o dataset
attach(credito)

#coloca risco credito
credito$risco_credito = as.factor(risco_credito)

#numero de linhas e colunas do dataset
dim(credito)

#analisa primeiras linhas
head(credito)

#valida existencia de nulos
any(is.na(credito))

#declara variaveis
variaveis_modelo = c("idade", "salario", "sexo", "estado_civil", "n_filhos", "n_cartoes", "pagto_salario", "hipoteca", "emprestimos")

#analisando variavel dependente a quantidade de registros 0 e 1
table(credito$risco_credito)

#acuracia do modelo - taxa media fifth fifth
559/(559+421)

#prepara amostra e teste
> #Slipt dos dados em treinamento e teste (training and test)
  set.seed(123)
split <- sample.split(credito$risco_credito,SplitRatio = 0.57)
split

# true e apenas para definir quem sera a base de treino
treino <- subset(credito, split == "TRUE")
teste <- subset(credito, split == "FALSE")

#verifica variaveis do modelo e calcula os betas
modelteste <- glm(risco_credito~n_cartoes+n_filhos+estado_civil+pagto_salario+hipoteca+emprestimos,family = binomial(link = "logit"),data = treino)
summary(modelteste)

#com isso montamos o modelo com o que queremos sobrescrevendo o anterior
model <- glm(risco_credito~n_cartoes+n_filhos+estado_civil+pagto_salario+emprestimos,family = binomial(link = "logit"),data = treino)
summary(model)

#nao da para desmembrar estado civil... entao vou escolhemos ele mesmo com a constante solteiro

#coluna probabilidade. Essa variável está recebendo a probabilidade prevista do modelo, colocando a probabilidade na resposta
probabilidade <-predict(model, treino, type = "response")
summary(probabilidade)

treino$probabilidade <-probabilidade # colocando a probabilidade no arquivo.
##teste$probabilidade <-probabilidade # colocando a probabilidade na amostra

#caso a probabilidade seja maior que 0.5 defina como 1 no predito
predito <- ifelse(probabilidade >= 0.5,1,0)
predito

#colocando predito no arquivo
treino$predito <- predito # colocando a classificação no arquivo peso normal "0" ou bx peso "1".
treino

#ajustando visualizacao pois as colunas ficam escondidas
View(treino[,-(1:10)])

#posicionando o arquivo
attach(treino)

#matrix de confusao
table(predito,risco_credito) # tabela cruada serve para saber quantos acertara e quantos erraram.

#Sensibilidade
sensibilidade = 230/(71+230)
sensibilidade

#Especificidade
especificidade = 169/(169+89)
especificidade

#teste de equacao

#equacao = (1+e^y)/(e^y)
#y= -1.9754+(0.5926*n_cartoes)+(0.4503*n_filhos)+(-4.9161*estado_civildivorciad)+(0.4743*estado_civilsolteiro)
#+(1.1664*pagto_salariosemanal)+(0.6043*emprestimos)

#calcula a taxa de acertividade dentro do conjunto de dados
attach(treino)
resumo <-table(predito,risco_credito)#crio a tabela

#regra de 3
tx_acerto <-(resumo[1]+resumo[4])/sum(resumo)
tx_acerto#uma taxa de acerto bacana

#instala pacote ResourceSelection
install.packages("ResourceSelection")

#teste estatístico de qualidade do ajuste para modelos de regressão logística.
require(ResourceSelection)
bondade_ajuste <- hoslem.test(risco_credito, predito, g=2)
bondade_ajuste #o ajuste ficou interessante abaixo de 0.05

#Deve-se enfatizar que um valor-p grande não significa que o modelo se encaixa bem,
#uma vez que a falta de evidência contra uma hipótese nula não é equivalente à evidência
#a favor da hipótese alternativa. Em particular, se o tamanho da amostra for pequeno, um alto 
#valor de p do teste pode ser simplesmente uma conseqüência do teste ter menor poder para 
#detectar erros de especificação, em vez de indicar um bom ajuste.

#curva ROC

ROCRpred <- prediction(probabilidade,treino$risco_credito)
ROCRperf <- performance(ROCRpred,"tpr","fpr")
plot(ROCRperf)
plot(ROCRperf, colorize=TRUE)
plot(ROCRperf, colorize=TRUE, print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(-0.2,1.7))
abline(a=0, b=1, lwd=2, lty=2)

#no ponto de escolher o melhor threshold utilizaria o 0.6 pois ele me garante uma taxa de aproximadamente 65%
#dos acertos com chances de falsos positivos em aproximadamente 0.24, mas para o teste nao sera considerado.

#previsao aplicando a modelagem nos dados teste
previsao <- predict(model, teste)
previsao <- ifelse(previsao >= 0.5,1,0)
previsao
teste$previsao <- previsao
head(teste)

#resultado
View(teste[,-(1:10)])
