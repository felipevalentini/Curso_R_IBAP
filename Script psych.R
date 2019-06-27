#Pacote psych

#instalando e abrindo
install.packages(psych)
install.packages(GPArotation)
library(psych)
library(GPArotation)

--------------------------------------------------------
  
#carregando bancos
  
banco1<-read.table(choose.files(),header=FALSE,sep="\t")


#carregando rótulos dos itens
labels1<-read.table(choose.files(), header=FALSE, sep="\t")

--------------------------------------------------------
  
# Análise fatorial 
  
#Qualidade do banco de dados 
KMO(banco1)  

# Retenção fatorial
## Very Simple Structure e Minimum Average Partial
vss<-vss(banco1, n = 3, rotate = "oblimin", diagonal = FALSE, fm = "wls", cor="poly",
         n.obs=NULL,plot=T,title="Very Simple Structure")
vss 


## Análise paralela
fa.parallel(banco1, n.obs = NULL,fm="ml", fa="fa",cor="poly",
            main = "Parallel Analysis Scree Plots", n.iter=20,error.bars=FALSE,
            SMC=FALSE,ylabel=NULL,show.legend=TRUE,sim=TRUE)

# Análise de clusters de itens
iclust(banco1)

# Análise fatorial
fit <- fa.poly(banco1,nfactors=2,rotate="oblimin", fm = "wls")
print(fit,sort=TRUE)


# Fidedignidade -----------------------------------------------------------
alpha(banco, check.keys = TRUE)
omega(banco1, poly=TRUE,nfactors=2)


# Construir uma variável com o escore total de um instrumento ou escala
itens<-banco1[c(9,1,10,3)] # selecionar todos os itens do instrumento
keys.list <- list(c(1,2,3,4)) # selecionar os itens a serem somados
keys <- make.keys(itens,keys.list)
scores <- scoreItems(keys,itens,totals=TRUE, min=1,max=4)#if "totals=TRUE" then total scores; if "totals=FALSE" then mean scores
scores
banco1$escore.F1<-scores$scores

