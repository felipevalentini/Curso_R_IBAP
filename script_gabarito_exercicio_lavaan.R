#gabarito exercicio Lavaan

# buscar o banco
dataURL2<-"https://raw.githubusercontent.com/felipevalentini/Curso_R_IBAP/master/banco_exemplo_cat.txt"
bancoExerc<- read.table(dataURL2, header = T, sep='\t')

#configurar o modelo
exercicio<- 'f1 =~ i1 + i2 + i3 + i4
             f2 =~ i5 + i6 + i7 + i8
             f3 =~ i9 + i10 + i11 + i12
              i1 | t1 + t2 + t3 + t4
              i2 | t1 + t2 + t3 + t4
              i3 | t1 + t2 + t3 + t4
              i4 | t1 + t2 + t3 + t4
              i5 | t1 + t2 + t3 + t4
              i6 | t1 + t2 + t3 + t4
              i7 | t1 + t2 + t3 + t4
              i8 | t1 + t2 + t3 + t4
              i9 | t1 + t2 + t3 + t4
              i10 | t1 + t2 + t3 + t4
              i11 | t1 + t2 + t3 + t4
              i12 | t1 + t2 + t3 + t4'

#rodar o modelo
cfa_exerc<- cfa(model = exercicio, data=bancoExerc, ordered = T, estimator = "DWLS") #informar que os itens sao categoricos ordinais
summary(cfa_exerc, fit.measures=TRUE, standardized=TRUE)


#passo 2, recofigurar o modelo retirando o item 6 (belo)
#configurar o modelo
exercicioV2<- 'f1 =~ i1 + i2 + i3 + i4
             f2 =~ i5 + i7 + i8
             f3 =~ i9 + i10 + i11 + i12
              i1 | t1 + t2 + t3 + t4
              i2 | t1 + t2 + t3 + t4
              i3 | t1 + t2 + t3 + t4
              i4 | t1 + t2 + t3 + t4
              i5 | t1 + t2 + t3 + t4
              i7 | t1 + t2 + t3 + t4
              i8 | t1 + t2 + t3 + t4
              i9 | t1 + t2 + t3 + t4
              i10 | t1 + t2 + t3 + t4
              i11 | t1 + t2 + t3 + t4
              i12 | t1 + t2 + t3 + t4'

#rodar o modelo
cfa_exercV2<- cfa(model = exercicioV2, data=bancoExerc, ordered = T, estimator = "DWLS") #informar que os itens sao categoricos ordinais
summary(cfa_exercV2, fit.measures=TRUE, standardized=TRUE)
modindices(cfa_exercV2)



#passo 3, recofigurar o modelo incluindo um cross-loading
#configurar o modelo
exercicioV3<- 'f1 =~ i1 + i2 + i3 + i4
               f2 =~ i5 + i7 + i8
               f3 =~ i9 + i10 + i11 + i12 + i1
              i1 | t1 + t2 + t3 + t4
              i2 | t1 + t2 + t3 + t4
              i3 | t1 + t2 + t3 + t4
              i4 | t1 + t2 + t3 + t4
              i5 | t1 + t2 + t3 + t4
              i7 | t1 + t2 + t3 + t4
              i8 | t1 + t2 + t3 + t4
              i9 | t1 + t2 + t3 + t4
              i10 | t1 + t2 + t3 + t4
              i11 | t1 + t2 + t3 + t4
              i12 | t1 + t2 + t3 + t4'

#rodar o modelo
cfa_exercV3<- cfa(model = exercicioV3, data=bancoExerc, ordered = T, estimator = "DWLS") #informar que os itens sao categoricos ordinais
summary(cfa_exercV3, fit.measures=TRUE, standardized=TRUE) 
modindices(cfa_exercV3)