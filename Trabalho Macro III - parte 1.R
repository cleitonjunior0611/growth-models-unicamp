# UNIVERSIDADE ESTADUAL DE CAMPINAS – INSTITUTO DE ECONOMIA

# CE572 – MACROECONOMIA III

# TRABALHO FINAL DO CURSO

# Parte I: Modelo de crescimento neoclássico  

# A partir do modelo de Solow, analise a dinâmica do PIB per capita dos países selecionados em
# relação aos Estados Unidos entre os anos 2000 e 2021.

###### QUESTÃO 1 #####

# QUESTÃO 1: A partir da relação entre as taxas médias de crescimento ao longo de todo o período analisado e
# o PIB per capita inicial dos países em sua amostra, discuta se há convergência destes países em relação à 
 # economia dos Estados Unidos. 

###### Inicialização de pacotes ######

# Instalação das bibliotecas que vamos utilizar:

install.packages("pwt10")
install.packages("dplyr")
install.packages("ggplot2")

# pwt8 = pacote que faz o downoad da base de dados Penn Tables
# dplyr = manuseio de dados. É um pacote com diversas funções que facilita a manipulação de dados no R. 
# ggplot2 = pacote para criar gráficos no R


# Inicializar bibliotecas:

library("pwt10")
library("dplyr")
library("ggplot2")

###### Importação da base de dados #####

# Importar a base de dados pwt

data("pwt10.01")

pwt <- pwt10.01 # Denomina a base de dados de pwt

View(pwt) # Ver a base de dados

colnames(pwt) # Variáveis que constam na base de dados 

??pwt # Documentação help dá o nome de cada uma das variáveis dentro da base de dados.

###### Manipulação da base de dados ######

# Variáveis que vamos precisar:
# year = ano
# isocode = actor with ISO 3166-1 alpha-3 country code. 
# rgdpna = Real GDP at constant 2005 national prices (in million 2005 USD).
# emp = Number of persons engaged (in millions).

# Usando o dplyr, vamos selecionar apenas as colunas desejadas:

data <- pwt %>%
  select(year, isocode, rgdpo, rkna, emp, labsh, pop) %>% # selecionar colunas...
  filter(year >= 1999, # Filtrar do ano 1999 ... (a partir de 1999 para calcular qual foi a taxa de crescimento de 99 para 2000)
         year <= 2019) %>% # ... até até 2021
  filter(!is.na(rgdpo)) # Desconsiderar países em que em algum ano há valor vazio

# Não temos o as relações produto por trabalhar e capital por trabalhador. Nem as taxas de crescimento.
# Vamos calcular elas usando o "dplyr" e já adicionar elas na base de dados "data"

data <- data %>%
  mutate( y_emp = rgdpo / pop ) %>% # PIB por trabalhador
  arrange(year) %>% # Ordenar por país
  group_by(isocode) %>% # Para cada país, calcular...
  mutate( g = (y_emp / lag(y_emp) - 1) * 100 ) # ...a taxa de crescimento do PIB per capita

# Precisamos da taxa de crescimento média do produto per capita por trabalhador:

gdp.g.l <- data %>%
  na.omit() %>%
  summarise(g = mean(g))

# Print output
gdp.g.l

# Selecionar qual era o PIB per capita no ano base (2000)


pib2000 <- data %>% 
  filter(year==2000) %>% # Selecionar qual era o PIB per capita no ano base (2000)
  select(year,isocode,y_emp) %>% # Selecionar year, isocode e y_emp
  mutate(y_norm = y_emp / data$y_emp[ data$year == 2000 & data$isocode == "USA" ] ) %>% # Criar nova variável PIB normalizado ("y_norm")
  na.omit() %>%
  filter(y_norm < 2) # retirada de outlier que atrapalhava a amostra


  

# data$y_emp[ data$year == 2000 & data$isocode == "USA" ] == selecionar dos dados variável "y_emp", [] é um condicional para selecionar apenas TRUE
# data$year == 2000 precisa ser igual a 2000, & é o operador lógico "E" e data$isocode == "USA"

# Observe que em 2000 não há dados para todos os países. Por outro lado, não temos também a taxa média para todos os países. 
# Assim, vamos limpar os vetores para termos apenas os países com informações completas:
# Vamos filtrar bases ( é um checklist):

gdp.g.l <- gdp.g.l %>% filter(isocode %in% pib2000$isocode)
pib2000 <- pib2000 %>% filter(isocode %in% gdp.g.l$isocode)

length(gdp.g.l$g)
length(pib2000$isocode)

# Base de dados que será usada no gráfico e regressão:

g <- gdp.g.l$g # Taxa média de crescimento de 2000 até 2021 dos países da amostra
yl <- pib2000$y_norm # PIB per capita por trabalhador normalizado pelo PIB per capita dos EUA


###### Análise dos dados #####

# g = Taxa média de crescimento de 2000 até 2021 dos países da amostra
# yl = PIB per capita por trabalhador normalizado pelo PIB per capita dos EUA

data.reg <- data.frame(g,yl) # Junto "g" e "yl" em um mesmo data-frame

names(data.reg) <- c("g","yl") # Dar nomes para as séries

data.reg # Visualizar

# Gráfico de dispersão

ggplot(data.reg, aes(y = g, x = yl)) + # aes() dá a estética do gráfico. x, variáveis 
  geom_point(size = 3, alpha = .7) + # Tamanho dos pontos
  theme_classic(base_size = 18) + # Estilo do gráfico
  xlab(" PIB per capita inicial (EUA = 1)") +  # Nome eixo X
  ylab("Taxa de crescimento média (%)") + # Nome eixo Y 
  geom_smooth(method = "lm", # Colocar uma linha de regressão no gráfioco, calculada por OLS
              formula = y ~ x) # Fòrmula da regressão

# Regressão por MQO:

reg <- lm( g ~ yl , data = data.reg)

# Resultados da regressão

summary(reg)


###### Análise com base limitada de países #####

### Selecionar países:

# Países do G8:

#############################################################################
################# ÚNICA PARTE DO CÓDIGO QUE DEVE SER ALTERADA ###############
#############################################################################

grupo_paises = c("USA","DEU","ESP","IRL","NOR","JPN","MEX", # Colocar 7 países da OCDE
                 "PAK","IND","DZA","UGA","THA","ECU","BRA") # Colocar 7 páises não OCDE
     


# Lista do código dos países
# CONSIDERAR 3 DÍGITOS

# https://www.dadosmundiais.com/codigos-de-pais.php

# Vamos usar o banco de dados "data" já criado anteriormente.

###### Manipulação da base de dados #####
# Precisamos da taxa de crescimento média do produto per capita por trabalhador do grupo de países selecionado:

gdp.g.l <- data %>%
  select(year,isocode,rgdpo,emp,pop,y_emp,g) %>%
  filter(isocode %in% grupo_paises) %>% # Seleciona apenas os países do "grupo_paises"
  group_by(isocode) %>%
  na.omit() %>%
  summarise(g = mean(g))
  

# Print output
gdp.g.l

# Selecionar qual era o PIB per capita no ano base (2000)

pib2000 <- data %>%
  filter(isocode %in% grupo_paises) %>% # Seleciona apenas os países do "grupo_paises"
  filter(year == 2000) %>%
  select(year, isocode, y_emp) %>%
  mutate(y_norm = y_emp / data$y_emp[ data$year == 2000 & data$isocode == "USA" ]  )

# Print PIB 2000
pib2000

# Observe que em 2000 não há dados para todos os países. Por outro lado, não temos também a taxa média para todos os países. 
# Por isso, é importante verificar se os dados foram calculados antes de prosseguir.
# Um simples print de "gdp.g.l" e "pib2000" já permite vermos isso.
# Assim, vamos limpar os vetores para termos apenas os países com informações completas:
# Vamos filtrar bases:

gdp.g.l <- gdp.g.l %>% filter(isocode %in% pib2000$isocode)
pib2000 <- pib2000 %>% filter(isocode %in% gdp.g.l$isocode)

length(gdp.g.l$g)
length(pib2000$isocode)

# Base de dados que será usada no gráfico e regressão:

g <- gdp.g.l$g # Taxa média de crescimento de 2000 até 2021 dos países da amostra
codepaises <- gdp.g.l$isocode
yl <- pib2000$y_emp/pib2000$y_emp[ pib2000$isocode == "USA"] # PIB per capita por trabalhador normalizado pelo PIB per capita dos EUA
data.reg <- data.frame(g,yl,codepaises)

# Base de dados não vem com classificação entre OCDE e não OCDE

# For-loop para classificação rápida, dada as posições dos países no vetor "grupo_paises":

region <- c(rep("OCDE",7),rep("ñ-OCDE",7))

data.reg <- data.reg %>% mutate(1) 

data.reg <- data.reg %>% mutate(region) 

for(i in 1:length(data.reg$g)){
  
    if( data.reg$codepaises[i] %in% grupo_paises[1:7] ){
      
      data.reg$region[i] <- "OCDE"
      
    }else{
      
      data.reg$region[i] <- "ñ-OCDE"
      
    }
}


###### Análise de dados ######

data.reg 

# criando uma tabela bonita 
install.packages("flextable")
library(flextable)

data.reg %>% select (codepaises, g, yl, region) %>% arrange (region) %>% flextable()

g # Taxa média de crescimento de 2000 até 2021 dos países da amostra
yl # IB per capita por trabalhador normalizado pelo PIB per capita dos EUA
data.reg$codepaises # ISOCODE dos países da amostra
data.reg$region # região a que pertencem


# Gráfico de dispersão

ggplot(data.reg , aes(y = g, x = yl, label=codepaises)) + # aes() dá a estética do gráfico. x, variáveis 
  labs(colour = "Região") + # Título da legenda
  geom_point(size = 3, aes(colour=factor(region))) + # Tamanho dos pontos e cor dos pontos
  geom_text(hjust=0, vjust=0) +
  scale_color_manual(values = c("OCDE" = "blue", # Determinar quais as cores de cada região
                                "ñ-OCDE" ="red")) + 
  theme_classic(base_size = 18) + # Estilo do gráfico
  xlab(" PIB per capita inicial (normalizado EUA)") +  # Nome eixo X
  ylab("Taxa de crescimento média (%)") + # Nome eixo Y 
  geom_smooth(method = "lm", # Colocar uma linha de regressão no gráfioco, calculada por OLS
              formula = y ~ x) # Fòrmula da regressão
  

# Regressão por MQO:

reg <- lm( g ~ yl , data=data.reg)

# Resultados da regressão

summary(reg)

# transformando em tabela bonita
install.packages("broom")
library(broom)
summary_reg <- tidy(reg)
flextable(summary_reg)

###### QUESTÃO 2 ######

# QUESTÃO 2)	Analise graficamente a relação entre as taxas de crescimento do produto per capita entre 
# os países selecionados (incluindo os Estados Unidos). Qual seria a expectativa, a partir do modelo de Solow,
# para esta relação? Os resultados corroboram esta previsão? Como podemos explicar esse resultado à luz do modelo de Solow?


# Selecionamos apenas os dados que desejamos para a questão 2:
data.crescimento <- data %>%
  filter(isocode %in% grupo_paises) %>%# Seleciona apenas os países do "grupo_paises"
  arrange(isocode) %>%
  select(year,isocode,g) %>%  # Organiza os países em ordem alfabética
  mutate(region = ifelse(isocode %in% grupo_paises[1:7], "OCDE", "ñ-OCDE")) %>% # Classifica os países por sua região, À depender da posição do isocode no vetor "grupo_países"
  na.omit()


# Calculo a taxa de crescimento média anual por grupo
data.crescimento_2 <- data.crescimento %>%  
  group_by(region, year) %>% # Agrupo por região e por ano
  summarise(mean_taxa = mean(g)) # Cálculo a estatística "média" por grupo
  
col1 <- data.crescimento_2 %>% filter(region == "OCDE") # Filtro países da OCDE em uma tabela
col2 <- data.crescimento_2 %>% filter(region == "ñ-OCDE") # Filtro países ñ-OCDE

data.crescimento_final <- data.frame(col1, col2$mean_taxa) # Junto à tabela OCDE apenas a coluna da taxa de crescimento dos ñ-OCDE

names(data.crescimento_final) <- c("Região","Ano","OCDE","nOCDE") # Dou um novo nome para o data.frame

# Resultado final:
data.crescimento_final

# tabela bonita

data.crescimento_final %>% select (Ano, OCDE, nOCDE) %>% flextable()


# Gráfico de linhas para comparar o resultado dos países OCDE e não OCDE
ggplot(data.crescimento_final) +
  geom_line(aes(x=Ano, y = OCDE,colour="OCDE"), size=1.5) +
  geom_line(aes(x=Ano, y = nOCDE,colour="ñ-OCDE"), size=1.5) +
  theme_light() + 
  scale_color_manual(name = "", values = c("OCDE" = "blue", "ñ-OCDE" = "red")) +
  xlab("Ano") +
  ylab("Taxa de crescimento média por região (%)")
  

###### QUESTÃO 3 ###########	

# Analise graficamente o nível de produto per capita dos países selecionados para o ano de 2021. 
# Quais são as principais diferenças observadas em relação ao nível de PIB per capita dos Estados Unidos neste ano? 

# Selecionamos os dados que vamos usar em um data-frame:
data.2019 <- data %>%
  filter(isocode %in% grupo_paises) %>% # Seleciona apenas os países do "grupo_paises"
  filter(year == 2019) %>% # Seleciona o ano
  arrange(isocode) %>% # Organiza os países em ordem alfabética
  mutate(region = ifelse(isocode %in% grupo_paises[1:7], "OCDE", "ñ-OCDE")) # Classifica os países por sua região, À depender da posição do isocode no vetor "grupo_países"

# tabela bonita

flextable(data.2019)

# Transformar isocode em fator para controlar a ordem
data.2019$isocode <- factor(data.2019$isocode, levels = data.2019$isocode[order(data.2019$region)])

# Criação do gráfico de coluna:
ggplot(data.2019) +
  aes( x = isocode, y = y_emp , fill=region) + # Variáveis que serão usadas no gráfico, para determinar eixo X, Y e cor 
  geom_bar(stat = "identity" , position = "dodge") + # Escolher estética bar. Stat = "identity" define que queremos os nomes no eixo X, "y = y_emp" = PIB per capita no eixo y e "fill=region" altera a corda da barra à depender da região
  ylab("PIB per capita em 2019 ( preços em PPP-2017) ") + # Legenda eixo Y
  xlab("País") + # Legenda eixo X
  scale_fill_discrete(name="Região") # Título da legenda


