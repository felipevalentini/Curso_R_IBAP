library(psych)
library(lavaan)


### Análise Fatorial Confirmatória ###
# fazer o download do banco de dados #
dataURL<-"https://raw.githubusercontent.com/felipevalentini/Curso_R_IBAP/master/banco_exemplo_AFC.dat"
data_exemplo<- read.table(dataURL, header = T, sep='\t')

# exemplo 1#
exemplo1<-'f1 =~ A1 + A2 + A3 + A4' #configura o modelo
cfa_ex1<- cfa(model = exemplo1, data=data_exemplo) #roda o modelo
summary(cfa_ex1) # output com o resumo do modelo
summary(cfa_ex1, fit.measures=TRUE) # output com indicadores de ajuste
summary(cfa_ex1, fit.measures=TRUE, standardized=TRUE) # output com parametros padronizados


# exemplo 2#
exemplo2<-'f1 =~ A1 + A2' #configura o modelo
cfa_ex2<- cfa(model = exemplo2, data=data_exemplo) #roda o modelo
summary(cfa_ex2, fit.measures=TRUE) # output com indicadores de ajuste


# exemplo 3#
exemplo3<-'f1 =~ A1 + A2 + A3 + A4
           f2 =~ D1 + D2 + D3 + D4 
           f3 =~ V1 + V2 + V3 + V4' #configura o modelo
cfa_ex3<- cfa(model = exemplo3, data=data_exemplo) #roda o modelo
summary(cfa_ex3, fit.measures=TRUE) # output com indicadores de ajuste


# exemplo 3B
modindices(cfa_ex3) # solicita indices de modificacao


# exemplo 3C
summary(cfa_ex1, standardized=TRUE) # parametros padronizados


# exemplo 4
dataURL2<-"https://raw.githubusercontent.com/felipevalentini/Curso_R_IBAP/master/banco_exemplo_cat.txt"
data_exemplo_cat<- read.table(dataURL2, header = T, sep='\t')
#para dados categoricos é necessario especificar os thresholds no modelo
exemplo4<- 'f1 =~ i1 + i2 + i3 + i4
              i1 | t1 + t2 + t3 + t4
              i2 | t1 + t2 + t3 + t4
              i3 | t1 + t2 + t3 + t4
              i4 | t1 + t2 + t3 + t4'
cfa_ex4<- cfa(model = exemplo4, data=data_exemplo_cat, ordered = T, estimator = "DWLS") #informar que os itens sao categoricos ordinais
summary(cfa_ex4, fit.measures=TRUE)


#exemplo5
exemplo5<-'f1 =~ A1 + A2 + A3 + A4
           f2 =~ D1 + D2 + D3 + D4 
           f3 =~ V1 + V2 + V3 + V4' #configura o modelo
cfa_ex5<- cfa(model = exemplo5, data=data_exemplo, se="bootstrap", bootstrap=100) #roda o modelo com bootstrap
summary(cfa_ex5, fit.measures=TRUE, standardized=TRUE) # output com indicadores de ajuste

#exemplo5b
cfa_ex5b<- cfa(model = exemplo5, data=data_exemplo)
summary(cfa_ex5, fit.measures=TRUE, standardized=TRUE)
cfa_ex5bBoot<-bootstrapLavaan(cfa_ex5b, R=100) #bootstrap para modelo já estimado previamente
summary(cfa_ex5bBoot, fit.measures=TRUE, standardized=TRUE)


#exemplo6
library(semTools)
exemplo6<-'f1 =~ A1 + A2 + A3 + A4
           f2 =~ D1 + D2 + D3 + D4 
           f3 =~ V1 + V2 + V3 + V4' #configura o modelo
cfa_ex6<- measurementInvariance(model = exemplo6, data=data_exemplo, 
                                group="genero") #roda invariancia

#exemplo6b
cfa_ex6b<- measurementInvariance(model = exemplo6, data=data_exemplo, 
                                group="genero", 
                                group.partial=c("f1 =~ A2", "D2~1")) #roda invariancia parcial

  

