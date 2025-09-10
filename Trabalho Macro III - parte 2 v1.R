# UNIVERSIDADE ESTADUAL DE CAMPINAS – INSTITUTO DE ECONOMIA

# CE572 – MACROECONOMIA III

# TRABALHO FINAL DO CURSO

###### PARTE 2: Modelo de Crescimento com Restrição no Balanço de Pagamentos #########

############## QUESTÃO 1 ############## 

# 1)Estime o crescimento com restrição do balanço de pagamentos para o Brasil e compare com o crescimento efetivo para o período 1990-2021. 
# Podemos dizer que o Brasil apresenta um crescimento do PIB com restrição do balanço de pagamentos? 


# Para a parte 2 do trabalho, vamos utilizar o banco de dados do Banco Mundial.

# Também existe um pacote para diretamente fazer o download do dados do banco mundial direto para o R, o "wstats"

# Instalação do pacote
# install.packages("wbstats")

# Biblioteca
library("wbstats")
library("dplyr")
library("ggplot2")
library("flextable")



# Para vermos o que há dentro do pacote:
str(wb_cachelist, max.level = 1)

wb_cachelist$indicators$indicator


### Procurar séries de interesse:

wb_search("NY.GDP.MKTP.CD") # indicadores almejados
wb_search("NE.EXP.GNFS.KD")

# NY.GDP.MKTP.CD    GDP (constant 2010 US$) 

wb_search("Exports of goods and services")

# NE.IMP.GNFS.CD    Imports of goods and services (current constant 2010 US$)

# Once you have found the set of indicators that you would like to explore further, the next step is downloading the data with wb_data().

isocode = "BRA"

start_date = 1989
end_date = 2021

gdp_bra <- wb_data("NY.GDP.MKTP.KD", country = isocode,
                   start_date = start_date, end_date = end_date) # PIB

imp_bra <- wb_data("NE.IMP.GNFS.KD", country = isocode,
                   start_date = start_date, end_date = end_date) # IMPORTACAO

exp_bra <- wb_data("NE.EXP.GNFS.KD", country = isocode,
                   start_date = start_date, end_date = end_date) # EXPORTACAO


# Crio variáveis apenas com as séries. 
gdp_bra_serie <- gdp_bra$NY.GDP.MKTP.KD
imp_bra_serie <- imp_bra$NE.IMP.GNFS.KD
exp_bra_serie <- exp_bra$NE.EXP.GNFS.KD


# Obtendo as taxas de variação do PIB, Exportações e Importações utilizando o log:


g_gdp <- log(gdp_bra_serie/lag(gdp_bra_serie))  # log Y_t / Y_(t-1)
g_gdp <- g_gdp[ !is.na(g_gdp) ] # Retiro o NA do 1º ano da série

g_imp <- log(imp_bra_serie/lag(imp_bra_serie))
g_imp <- g_imp[ !is.na(g_imp) ]

g_exp <- log(exp_bra_serie/lag(exp_bra_serie))
g_exp <- g_exp[ !is.na(g_exp) ]

ano <- 1990:2021 # crio vetor para os anos da amostra

df <- data.frame(g_gdp,g_imp,g_exp,ano) # Reuno taxas de variação e ano em um mesmo data.frame

# Colocar um label

ggplot(df) +
  geom_line(aes(x = ano, y = g_gdp*100, colour="PIB"), size= 1.5) +
  geom_line(aes(x = ano, y = g_imp*100, colour = "Imp"), size= 1.5) +
  geom_line(aes(x = ano, y = g_exp*100, colour="Exp"), size= 1.5) +
  scale_color_manual(name = "", values = c("PIB" = "black", "Exp" = "blue" , "Imp" = "red")) +
  ylab("Taxa de variação (%)") +
  theme_bw() 
  


# Regressão efeito multiplicar sobre o pib
elast_pib_exp_BRA <- lm(g_gdp ~ g_exp, data=df)

# Regressão crescimento PIB vs. crescimento importaçõe
elast_renda_imp_reg_BRA <- lm(g_imp ~ g_gdp, data = df)

# Resultados da última
results <- summary(elast_renda_imp_reg_BRA)


# Elasticidade renda da demanda por importações :

elas_renda_imp_BRA <- elast_renda_imp_reg_BRA$coefficients[2] # 

# transformando em tabela bonita
library(broom)
library(flextable)

flextable(tidy(elast_pib_exp_BRA)) # regressão Gdp ~ Exp
flextable(tidy(elast_renda_imp_reg_BRA)) # regressão Imp ~ Gdp


# Crescimento com restrição no BP:

# Estimativa: taxa de crescimento das exportações/elasticidade renda da demanda por importações :

g_rest_bp <- g_exp/elas_renda_imp_BRA


# QUal a correlação das taxas?

cor(g_rest_bp,g_gdp)
cor(g_exp,g_gdp)

### Gráfico 

df <- data.frame(df,g_rest_bp)

my_plot <- ggplot(df) +
  geom_line(aes(x = ano, y= g_gdp*100, colour = "PIB efetivo"),linewidth=1.2) +
  geom_line(aes(x = ano, y = g_rest_bp*100, colour = "PIB c/ rest. BP"),linewidth=1.2) +
  scale_color_manual(name = "", values = c("PIB efetivo" ="blue", "PIB c/ rest. BP" = "red" )) +
  theme_bw(base_size = 18) +
  ylab("Taxa de variação (%)") +
  xlab("Ano")

my_plot


############## QUESTÃO 2 ############## 

# 2)	Estime o crescimento com restrição do balanço de pagamentos para um país selecionado e compare com o crescimento efetivo para o período 1980-2021.
# Podemos dizer que este país apresenta um crescimento do PIB com restrição do balanço de pagamentos?


# Vou repetir o código da questão 1. Apenas vou trocar o isocode para o país desejado:

isocode = "MEX"

gdp_iso <- wb_data("NY.GDP.MKTP.KD", country = isocode,
                   start_date = 1989, end_date = 2021)

imp_iso <- wb_data("NE.IMP.GNFS.KD", country = isocode,
                   start_date = 1989, end_date = 2021)

exp_iso <- wb_data("NE.EXP.GNFS.KD", country = isocode,
                   start_date = 1989, end_date = 2021)


  # Crio variáveis apenas com as séries. 
gdp_iso_serie <- gdp_iso$NY.GDP.MKTP.KD
imp_iso_serie <- imp_iso$NE.IMP.GNFS.KD
exp_iso_serie <- exp_iso$NE.EXP.GNFS.KD


# Obtendo as taxas de variação do PIB, Exportações e Importações utilizando o log:


g_gdp <- log(gdp_iso_serie/lag(gdp_iso_serie))  # log Y_t / Y_(t-1)
g_gdp <- g_gdp[ !is.na(g_gdp) ] # Retiro o NA do 1º ano da série

g_imp <- log(imp_iso_serie/lag(imp_iso_serie))
g_imp <- g_imp[ !is.na(g_imp) ]

g_exp <- log(exp_iso_serie/lag(exp_iso_serie))
g_exp <- g_exp[ !is.na(g_exp) ]

ano <- 1990:2021 # crio vetor para os anos da amostra

df <- data.frame(g_gdp,g_imp,g_exp,ano) # Reuno taxas de variação e ano em um mesmo data.frame

# Colocar um label

ggplot(df) +
  geom_line(aes(x = ano, y = g_gdp*100, colour="PIB"), size= 1.5) +
  geom_line(aes(x = ano, y = g_imp*100, colour = "Imp"), size= 1.5) +
  geom_line(aes(x = ano, y = g_exp*100, colour="Exp"), size= 1.5) +
  scale_color_manual(name = "", values = c("PIB" = "black", "Exp" = "blue" , "Imp" = "red")) +
  ylab(paste("Taxas de variação (%) de",isocode)) +
  xlab("Ano") +
  theme_bw() 

# Regressão efeito multiplicar sobre o pib
elast_pib_exp_MEX <- lm(g_gdp ~ g_exp, data=df)

# Regressão crescimento PIB vs. crescimento importaçõe
elast_renda_imp_reg_MEX <- lm(g_imp ~ g_gdp, data = df)

# Resultados da última
summary(elast_renda_imp_reg_MEX)

flextable(tidy(elast_pib_exp_MEX)) # regressão Gdp ~ Exp
flextable(tidy(elast_renda_imp_reg_MEX)) # regressão Imp ~ Gdp

# Elasticidade renda da demanda por importações :

elas_renda_imp_MEX <- elast_renda_imp_reg_MEX$coefficients[2] # 


# Crescimento com restrição no BP:

# Estimativa: taxa de crescimento das exportações/elasticidade renda da demanda por importações :

g_rest_bp_MEX <- g_exp/elas_renda_imp_MEX

# QUal a correlação das taxas?

cor(g_rest_bp,g_gdp)
cor(g_exp,g_gdp)

### Gráfico 

df <- data.frame(df,g_rest_bp)

my_plot <- ggplot(df) +
  geom_line(aes(x = ano, y= g_gdp*100, colour = "PIB efetivo"),linewidth=1.2) +
  geom_line(aes(x = ano, y = g_rest_bp*100, colour = "PIB c/ rest. BP"),linewidth=1.2) +
  scale_color_manual(name = "", values = c("PIB efetivo" ="blue", "PIB c/ rest. BP" = "red" )) +
  theme_bw(base_size = 18) +
  ylab("Taxa de variação (%)") +
  xlab("Ano")

my_plot

############## QUESTÃO 3 #################

#3)	Faça a estimação separando o período de análise antes e após 2003, quando há o boom de commodities. 
# Analisando o comportamento das exportações e da elasticidade-renda da demanda por importações, 
# como você explica o crescimento do país em questão? Houve diferença entre os períodos? 
# O que isso significa em termos de crescimento do PIB?


# Veja que a questão exige que possamos repetir a rotina da questão 2, apenas alterando alguns inputs. 
# Por isso, criei uma função, que vai nos ajudar com a questão 3 e 4.

Q3Macro <- function(isocode,start_date,end_date){

isocode = isocode
start_date = start_date
end_date = end_date

gdp_iso <- wb_data("NY.GDP.MKTP.KD", country = isocode,
                   start_date = start_date, end_date = end_date)

imp_iso <- wb_data("NE.IMP.GNFS.KD", country = isocode,
                   start_date = start_date, end_date = end_date)

exp_iso <- wb_data("NE.EXP.GNFS.KD", country = isocode,
                   start_date = start_date, end_date = end_date)


# Crio variáveis apenas com as séries. 
gdp_iso_serie <- gdp_iso$NY.GDP.MKTP.KD
imp_iso_serie <- imp_iso$NE.IMP.GNFS.KD
exp_iso_serie <- exp_iso$NE.EXP.GNFS.KD

# Obtendo as taxas de variação do PIB, Exportações e Importações utilizando o log:


g_gdp <- log(gdp_iso_serie/lag(gdp_iso_serie))  # log Y_t / Y_(t-1)
g_gdp <- g_gdp[ !is.na(g_gdp) ] # Retiro o NA do 1º ano da série

g_imp <- log(imp_iso_serie/lag(imp_iso_serie))
g_imp <- g_imp[ !is.na(g_imp) ]

g_exp <- log(exp_iso_serie/lag(exp_iso_serie))
g_exp <- g_exp[ !is.na(g_exp) ]

# Cálculo da taxa de variação média da taxa de exportações no período:
mean_g_exp <- mean(g_exp)
mean_g_gdp <- mean(g_gdp)

# Criação de vetor para os anos da amostra:
ano <- round((start_date+1):end_date,0)

# Reuno taxas de variação e ano em um mesmo data.frame
df <- data.frame(g_gdp,g_imp,g_exp,ano) 

# Colocar um label

ggplot(df) +
  geom_line(aes(x = ano, y = g_gdp*100, colour="PIB"), size= 1.5) +
  geom_line(aes(x = ano, y = g_imp*100, colour = "Imp"), size= 1.5) +
  geom_line(aes(x = ano, y = g_exp*100, colour="Exp"), size= 1.5) +
  scale_x_continuous(breaks = 0:3000) +
  scale_color_manual(name = "", values = c("PIB" = "black", "Exp" = "blue" , "Imp" = "red")) +
  ylab(paste("Taxas de variação (%) de",isocode)) +
  xlab("Ano") +
  theme_bw() +
  theme(axis.text.x=element_text(angle=60, hjust=1))

# Regressão efeito multiplicar sobre o pib
elast_pib_exp_iso <- lm(g_gdp ~ g_exp, data=df)

# Regressão crescimento PIB vs. crescimento importaçõe
elast_renda_imp_reg_iso <- lm(g_imp ~ g_gdp, data = df)

# Resultados da última
reg_summ <- summary(elast_renda_imp_reg_iso)

# Elasticidade renda da demanda por importações :

elas_renda_imp_iso <- unname(elast_renda_imp_reg_iso$coefficients[2]) # 

# Crescimento com restrição no BP:

# Estimativa: taxa de crescimento das exportações/elasticidade renda da demanda por importações :

g_rest_bp <- g_exp/elas_renda_imp_iso

### Gráfico 

df <- data.frame(df,g_rest_bp)

my_plot <- ggplot(df) +
  geom_line(aes(x = ano, y= g_gdp*100, colour = "PIB efetivo"),linewidth=1.2) +
  geom_line(aes(x = ano, y = g_rest_bp*100, colour = "PIB c/ rest. BP"),linewidth=1.2) +
  scale_color_manual(name = "", values = c("PIB efetivo" ="blue", "PIB c/ rest. BP" = "red" )) +
  scale_x_continuous(breaks = (start_date+1):end_date) +
  theme_bw() +
  ylab("Taxa de variação (%)") +
  xlab("Ano") +
  theme(axis.text.x=element_text(angle=60, hjust=1))

print(my_plot)

# Lista com os resultados da função
resultados <- list("regression_summary" = reg_summ,"coef_imp" = elas_renda_imp_iso, "mean_g_exp" = mean_g_exp ,"mean_g_gdp" = mean_g_gdp, "g_gdp" = g_gdp)

return(resultados)

}

Q3Macro(isocode = "MEX",start_date= 1980,end_date = 2021)

# Função criada nos dá:
# resultados da regressão: $regression_summary
# Elasticidade média das importações do período: $coef_imp
# Crescimento médio das exportações: $mean_g_exp
# Crescimento médio do PIB: $mean_g_gdp
# A série de variação anual do PIB: $g_gdp

# Voltando ao exercício:

# 1º período:

periodo_1 = Q3Macro(isocode = "MEX",start_date= 1989,end_date = 2002)

periodo_2 = Q3Macro(isocode = "MEX",start_date= 2002,end_date = 2021) # Preciso começar em 2002 para calcular a taxa de crescimento de 2023

tabela <- data.frame(Período = c("Pré-2003","Pós-2003"),
                     Cresc._médio_das_Exp = c(periodo_1$mean_g_exp,periodo_2$mean_g_exp),
                     Elast._renda_das_Imp = c(periodo_1$coef_imp,periodo_2$coef_imp),
                     Cresc._restrição_BP = c(periodo_1$mean_g_exp/periodo_1$coef_imp, periodo_2$mean_g_exp/periodo_2$coef_imp),
                     Cresc._efetivo = c(periodo_1$mean_g_gdp,periodo_2$mean_g_gdp))

tabela

flextable(tabela)

############## QUESTÃO 4 #################

# 4)	Comparando com o caso brasileiro, as taxas de crescimento desses dois países são semelhantes? 
#  Como o modelo considerado ajuda a explicar as possíveis diferenças? 


# Comparar as taxas de crescimento de ambos os países:

result.MEX <- Q3Macro(isocode = "MEX",start_date= 1989,end_date = 2021)
result.BRA <- Q3Macro(isocode = "BRA",start_date= 1989,end_date = 2021)
ano <- 1990:2021

df <- data.frame(result.BRA$g_gdp,result.MEX$g_gdp,ano)

ggplot(df) +
  geom_line(aes(x = ano, y=result.BRA.g_gdp*100, colour="Brasil"),size=1.2) +
  geom_line(aes(x = ano, y=result.MEX.g_gdp*100, colour="México"),size=1.2) +
  scale_color_manual(name = "", values = c("Brasil" ="green", "México" = "red" )) +
  theme_bw(base_size = 18) +
  ylab("Taxa de variação (%)") +
  xlab("Ano")


tabela <- data.frame(Período = c("Brasil","México"),
                     Cresc._médio_das_Exp = c(result.BRA$mean_g_exp,result.MEX$mean_g_exp),
                     Elast._renda_das_Imp = c(result.BRA$coef_imp,result.MEX$coef_imp),
                     Cresc._restrição_BP = c(result.BRA$mean_g_exp/result.BRA$coef_imp, result.MEX$mean_g_exp/result.MEX$coef_imp),
                     Cresc.Efet.PIB = c(result.BRA$mean_g_gdp,result.MEX$mean_g_gdp))

tabela

flextable(tabela)



