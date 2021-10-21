rm(list = ls())


#-------------------------------------- Pacotes----------------------------------------------------------------------
library(tidyverse)
library(plm)
library(car,'rgl') # teste durbinWatsonTest e scatterplot3d! PODE GIRAR!!
library(moments) # assimetria e curtose
library(stargazer)

#-----------------------Royalties Share----------------------------------------------------------
# (Amostra baseada no quociente royalties/Receita_orçamentária_Total)
setwd('./Dados')

load('royalties_PibIfdmEducRAIS_Investimentos.RData') 
royalties_PibIfdmEducRAIS_Investimentos$ANO <- str_extract(royalties_PibIfdmEducRAIS_Investimentos$MUNICIPIO_id, pattern = "20..") %>% as.integer()
royalties_panel <- royalties_PibIfdmEducRAIS_Investimentos
royalties_panel$MUNICIPIO_UF <- gsub(royalties_panel$MUNICIPIO_id,pattern = "_20..", replacement = "")


#_________Atribuindo volatilidade a cada ano ####
volatilidade_wide <-  select(royalties_panel, everything()) %>% 
   filter(ANO<=2006 & ANO>=2004) %>% 
   group_by(MUNICIPIO_UF) %>% 
   summarise(VOLATILIDADE2006=sd(ROYALTIES_MUNICIPIO)) %>% left_join(
      
      select(royalties_panel, everything()) %>% 
         filter(ANO<=2007 & ANO>=2004) %>% 
         group_by(MUNICIPIO_UF) %>% 
         summarise(VOLATILIDADE2007=sd(ROYALTIES_MUNICIPIO))) %>% left_join(
            
            select(royalties_panel, everything()) %>% 
               filter(ANO<=2008 & ANO>=2005) %>% 
               group_by(MUNICIPIO_UF) %>% 
               summarise(VOLATILIDADE2008=sd(ROYALTIES_MUNICIPIO))) %>% left_join(
                  
                  select(royalties_panel, everything()) %>% 
                     filter(ANO<=2009 & ANO>=2006) %>% 
                     group_by(MUNICIPIO_UF) %>% 
                     summarise(VOLATILIDADE2009=sd(ROYALTIES_MUNICIPIO))) %>% left_join(
                        
                        select(royalties_panel, everything()) %>% 
                           filter(ANO<=2010 & ANO>=2007) %>% 
                           group_by(MUNICIPIO_UF) %>% 
                           summarise(VOLATILIDADE2010=sd(ROYALTIES_MUNICIPIO))) %>% left_join(
                              
                              select(royalties_panel, everything()) %>% 
                                 filter(ANO<=2011 & ANO>=2008) %>% 
                                 group_by(MUNICIPIO_UF) %>% 
                                 summarise(VOLATILIDADE2011=sd(ROYALTIES_MUNICIPIO))) %>% left_join(
                                    
                                    select(royalties_panel, everything()) %>% 
                                       filter(ANO<=2012 & ANO>=2009) %>% 
                                       group_by(MUNICIPIO_UF) %>% 
                                       summarise(VOLATILIDADE2012=sd(ROYALTIES_MUNICIPIO))) %>% left_join(
                                          
                                          select(royalties_panel, everything()) %>% 
                                             filter(ANO<=2013 & ANO>=2010) %>% 
                                             group_by(MUNICIPIO_UF) %>% 
                                             summarise(VOLATILIDADE2013=sd(ROYALTIES_MUNICIPIO))) %>% left_join(
                                                
                                                select(royalties_panel, everything()) %>% 
                                                   filter(ANO<=2014 & ANO>=2011) %>% 
                                                   group_by(MUNICIPIO_UF) %>% 
                                                   summarise(VOLATILIDADE2014=sd(ROYALTIES_MUNICIPIO))) %>% left_join(
                                                      
                                                      select(royalties_panel, everything()) %>% 
                                                         filter(ANO<=2015 & ANO>=2012) %>% 
                                                         group_by(MUNICIPIO_UF) %>% 
                                                         summarise(VOLATILIDADE2015=sd(ROYALTIES_MUNICIPIO))) 

# mudando para formato Long, painel     ----       
volatilidade_long <-gather(volatilidade_wide, key = ANO, value = VOLATILIDADE, -MUNICIPIO_UF)
volatilidade_long$ANO <- volatilidade_long$ANO %>% gsub(pattern = "VOLATILIDADE", replacement =  "") %>% as.integer()




# unindo volatilidade à base de dados----
# _____ atribuindo chave primária
#royalties_panel$MUNICIPIO_id <- paste(royalties_panel$MUNICIPIO_UF, royalties_panel$ANO, sep = "_")
volatilidade_long$MUNICIPIO_id <- paste(volatilidade_long$MUNICIPIO_UF, volatilidade_long$ANO, sep = "_")
# unindo
royalties_panel <- royalties_panel %>% left_join(volatilidade_long, by = "MUNICIPIO_id")
# delimitando variáveis de interesse----
royalties_panel <- royalties_panel[c(2,4,6:14,17)]
colnames(royalties_panel)[c(11,10)] <- c("MUNICIPIO_UF","ano")

#_________ Delimitando % royalties/orçamento--------

# IMPONDO ANO BASE ----
ano0 <- 2007 
royalties_panel <- royalties_panel[royalties_panel$ano >= ano0,] 


# IMPONDO PERCENTUAL ROYALTIES/RECEITA_ORÇAMENTÁRIA ----
royalties_share <- select(royalties_panel, everything()) %>% 
   filter(royalties_panel$ROYALTIES_MUNICIPIO/royalties_panel$REC_ORCAMENTARIA > 0.10 & 
             royalties_panel$ROYALTIES_MUNICIPIO/royalties_panel$REC_ORCAMENTARIA < 1) %>% 
   select(MUNICIPIO_UF) %>% unique() %>% data.frame() %>% left_join(royalties_panel, by = 'MUNICIPIO_UF') 

# Delimitando anos completos (painel balanceado)####
royalties_share <- na.omit(royalties_share)
royalties_share <- royalties_share[royalties_share$Investimentos > 0,]

royalties_share <- select(data.frame(table(royalties_share$MUNICIPIO_UF)), everything()) %>% 
   filter(Freq==length(unique(royalties_share$ano))) %>% left_join(royalties_share, by = c('Var1' = 'MUNICIPIO_UF'))
colnames(royalties_share)[1] <- c('MUNICIPIO_UF')

#ajustando a linha 134, ITATIAIAUÇU_MG. Falta um '6' na frente 
royalties_share[134,6] <- royalties_share[134,6] + 60000000


# substituindo municípios por índice númerico----
numeracao_municipios <- royalties_share$MUNICIPIO_UF %>% unique() %>% data.frame()
numeracao_municipios$individuo <- c(1:length(unique(royalties_share$MUNICIPIO_UF)))
colnames(numeracao_municipios)[1] <- c("MUNICIPIO_UF")
royalties_share <- left_join(royalties_share,numeracao_municipios, by = "MUNICIPIO_UF")

painel <- royalties_share[,c("individuo",
                             "PIB",
                             "ano",
                             "POPULACAO",
                             "ROYALTIES_MUNICIPIO",
                             "VOLATILIDADE",
                             "REC_ORCAMENTARIA",
                             "Investimentos",
                             "pe",
                             "EDUCACAO",
                             "anosDeEscolaridade",
                             "ifdm")]

painel <- pdata.frame(x = painel, index = c("individuo", "ano"))

ano <- painel$ano
pib <- painel$PIB
pib_pc <- log(painel$PIB) - log(painel$POPULACAO)
pop <- painel$POPULACAO
royalties <- log(painel$ROYALTIES_MUNICIPIO)
volatilidade <- log(painel$VOLATILIDADE)
IFDM <- log(painel$ifdm) 
pop_empregada <- log(painel$pe)
educacao <- log(painel$anosDeEscolaridade) #(anos de escolaridade)
OrcamentoDaEducacao <- log(painel$EDUCACAO) 
orcamento <- log(painel$REC_ORCAMENTARIA)
investimento <- log(painel$Investimentos)


#__________Rodando o modelo----

plm_within <- plm(formula = pib_pc ~ 
                     volatilidade
                  + investimento
                  + educacao
                  + pop_empregad
#                 + IFDM
                  ,data = painel, model = 'within')
summary(plm_within)




# ______ -----
equacao1 <- plm(formula = pib_pc ~ volatilidade ,data = painel, model = 'within')
equacao2 <- plm(formula = pib_pc ~ volatilidade + investimento, data = painel, model = 'within')
equacao3 <- plm(formula = pib_pc ~ volatilidade + investimento + educacao, data = painel, model = 'within')
equacao4 <- plm(formula = pib_pc ~ volatilidade + investimento + educacao + pop_empregada,data = painel, model = 'within')
equacao5 <- plm(formula = pib_pc ~ volatilidade + investimento + educacao + pop_empregada + IFDM ,data = painel, model = 'within')


stargazer(equacao1, type = 'html')
stargazer(equacao2, type = 'html')
stargazer(equacao3, type = 'html')
stargazer(equacao4, type = 'html')
stargazer(equacao5, type = 'html')








# rascunho


#_________Atribuindo variável de crescimento ----
# delimitando anos 2005:2015
#royalties_panel <- royalties_panel[royalties_panel$ANO %in% c(2005:2015), ]

# Delimitando anos completos (painel balanceado)####
royalties_panel <- select(data.frame(table(royalties_panel$MUNICIPIO_UF)), everything()) %>% 
   filter(Freq==length(unique(royalties_panel$ANO))) %>% left_join(royalties_panel, by = c('Var1' = 'MUNICIPIO_UF'))
# Crescimento anual do PIB PER CAPTA----
colnames(royalties_panel)[1] <- c('MUNICIPIO_UF')
royalties_panel$Gpib_pc <- NA
for (i in unique(royalties_panel$MUNICIPIO_UF)) {
   for (j in c(2006:2015)) {
      royalties_panel[royalties_panel$MUNICIPIO_UF==i & royalties_panel$ANO==j,]$Gpib_pc  <- 
         (royalties_panel[royalties_panel$MUNICIPIO_UF==i & royalties_panel$ANO==j,]$PIB /
             royalties_panel[royalties_panel$MUNICIPIO_UF==i & royalties_panel$ANO==j,]$POPULACAO) / 
         (royalties_panel[royalties_panel$MUNICIPIO_UF==i & royalties_panel$ANO==j-1,]$PIB /
             royalties_panel[royalties_panel$MUNICIPIO_UF==i & royalties_panel$ANO==j-1,]$POPULACAO) #- 1
   } }

# Crescimento do PIB ABSOLUTO anual----
royalties_panel$Gpib <- NA
for (i in unique(royalties_panel$MUNICIPIO_UF)) {
   for (j in c(2006:2015)) {
      royalties_panel[royalties_panel$MUNICIPIO_UF==i & royalties_panel$ANO==j,]$Gpib  <- 
         royalties_panel[royalties_panel$MUNICIPIO_UF==i & royalties_panel$ANO==j,]$PIB /
         royalties_panel[royalties_panel$MUNICIPIO_UF==i & royalties_panel$ANO==j-1,]$PIB  #- 1
   } }

# Crescimento  POPULACIONAL-ANO----
royalties_panel$Gpop <- NA
for (i in unique(royalties_panel$MUNICIPIO_UF)) {
   for (j in c(2006:2015)) {
      royalties_panel[royalties_panel$MUNICIPIO_UF==i & royalties_panel$ANO==j,]$Gpop  <- 
         royalties_panel[royalties_panel$MUNICIPIO_UF==i & royalties_panel$ANO==j,]$POPULACAO /
         royalties_panel[royalties_panel$MUNICIPIO_UF==i & royalties_panel$ANO==j-1,]$POPULACAO  #- 1
   } }


# Crescimento  na População_empregada-ANO----
royalties_panel$Gpe <- NA
for (i in unique(royalties_panel$MUNICIPIO_UF)) {
   for (j in c(2006:2015)) {
      royalties_panel[royalties_panel$MUNICIPIO_UF==i & royalties_panel$ANO==j,]$Gpe  <- 
         royalties_panel[royalties_panel$MUNICIPIO_UF==i & royalties_panel$ANO==j,]$pe /
         royalties_panel[royalties_panel$MUNICIPIO_UF==i & royalties_panel$ANO==j-1,]$pe  #- 1
   } }


