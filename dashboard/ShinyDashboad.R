library(shinydashboard)
library(tidyverse)
library(readxl)
library(caret)
library(lubridate)
library(geobr)
library(ggplot2)
library(sf)
library(viridis)
library(tmap)

# dados = read_csv("basePM2.5_mensal.csv")
# dados$mesano=paste(dados$mes,dados$ano)
# dados$mesano=my(dados$mesano)

dados <- read_csv("C:/Users/diogo/Downloads/PM2.5_diario_2023.csv")
dados_donkelar <- read_excel("C:/Users/diogo/Downloads/dados_completos_consolidado_donkelar.xlsx")

dados$Data <- ymd(dados$Date)
dados$mes <- month(dados$Data)
dados$ano <- year(dados$Data)
dados$dia <- weekdays(dados$Data)
dados$UF <- substr(dados$Cod, 1, 2)

dados_ceara <- subset(dados, UF == 23)

dados_ceara_1 <- dados_ceara %>%
  group_by(Cod, mes) %>%
  summarise(pm2.5 = mean(PM2.5, na.rm = TRUE))

dados_ceara_uf <- dados_ceara %>%
  group_by(mes) %>%
  summarise(pm2.5 = mean(PM2.5, na.rm = TRUE)) %>%
  mutate(Ano = 2023, Fonte = "Copernicus")

dados_donkelar_ceara <- dados_donkelar %>% filter(SIGLA_UF == 23)

Media_mensal_donkelar_ceara <- dados_donkelar_ceara %>%
  group_by(Mes) %>%
  summarise(pm2.5 = mean(Media_PM25, na.rm = TRUE)) %>%
  rename(mes = Mes) %>%
  mutate(Fonte = "Donkelar", Ano = 2023)

donkelar_copernicus <- bind_rows(Media_mensal_donkelar_ceara, dados_ceara_uf)


####

dados_ceara_1$Fonte <- "Copernicus"
dados_donkelar_ceara$Fonte <- "Donkelar"
dados_donkelar_ceara$pm2.5 <- dados_donkelar_ceara$Media_PM25

dados_combinados <- bind_rows(
  dados_ceara_1 %>% select(mes, pm2.5, Fonte),
  dados_donkelar_ceara %>% select(Mes, `pm2.5`, Fonte) %>% rename(mes = Mes)
)

dados_wide <- donkelar_copernicus %>%
  pivot_wider(names_from = Fonte, values_from = pm2.5)


########### Mapa ##########

map <- st_read("C:/Users/diogo/OneDrive/Estatística 1.R/Pratica Estatistica/CE_Municipios_2024/CE_Municipios_2024.shp")
dados_ceara_1$Cod <- as.character(dados_ceara_1$Cod)

map_dados <- left_join(map, dados_ceara_1, join_by("CD_MUN" == "Cod"))

# Simplificação para evitar erro de memória
map_simplificado <- st_simplify(map_dados, dTolerance = 100)



dados_donkelar_ceara$CD_MUN <- as.character(dados_donkelar_ceara$CD_MUN)

map_dados_donkelar <- left_join(map, dados_donkelar_ceara, join_by("CD_MUN"))

# Simplificação para evitar erro de memória
map_simplificado_2 <- st_simplify(map_dados_donkelar, dTolerance = 100)



ui <- dashboardPage(
  header<-dashboardHeader(title = "Material Particulado"),
  sidebar<-dashboardSidebar(
    sidebarMenu(
      menuItem("Análise Mensal", icon=icon("bar-chart"),
               menuSubItem("Histograma", tabName="subitem1"),
               menuSubItem("Gráfico de Linha", tabName="subitem2"),
               menuSubItem("Boxplot", tabName="subitem3"),
               menuSubItem("Gráfico de Dispersão", tabName="subitem4"),
               menuSubItem("Mapa - Copernicus", tabName="subitem5"),
               menuSubItem("Mapa - Donkelar", tabName="subitem6")
               ))
  ),
  body<-dashboardBody(
    tabItems(
      tabItem("subitem1",
              plotOutput("distPlot")),
      tabItem("subitem2", plotOutput("distPlot2")),
      tabItem("subitem3", plotOutput("distPlot3")),
      tabItem("subitem4", plotOutput("distPlot4")),
      tabItem("subitem5", plotOutput("distPlot5")),
      tabItem("subitem6", plotOutput("distPlot6"))
    )
  )
)

server <- function(input, output) {
  output$distPlot <- renderPlot({
    #Grafico de barras 
    ggplot(donkelar_copernicus, aes(x = factor(mes), y = pm2.5, fill = Fonte)) +
      geom_col(position = "dodge") +
      labs(title = "(A) Médias mensais - PM2,5",
           x = "Mês",
           y = "PM2.5 (µg/m³)") +
      scale_fill_manual(values = c("Copernicus" = "black", "Donkelar" = "grey70")) +
      theme_minimal() +
      theme(plot.title = element_text(face = "bold"))
  })
  
  #Grafico de linhas 
  
  output$distPlot2 = renderPlot({
    ggplot(donkelar_copernicus, aes(x = mes, y = pm2.5, color = Fonte, fill = Fonte)) +
      geom_line() +
      geom_point() +
      labs(title = "Tendência Mensal PM2,5 - Ceará",
           x = "Mês", y = "PM2.5 (µg/m³)", color = "Fonte") +
      theme_minimal()
  })
  
  
  
  #Box plot
  output$distPlot3 = renderPlot({
    ggplot(dados_ceara_1, aes(x = factor(mes), y = pm2.5)) +
      geom_boxplot() +
      labs(title = "Boxplot Copernicus - Ceará",
           x = "Mês", y = "PM2.5 (µg/m³)") +
      theme_minimal()
  })
  
  #Estatísticas descritivas e Boxplot
  output$distPlot3 = renderPlot({
    ggplot(dados_donkelar_ceara, aes(x = factor(Mes), y = Media_PM25)) +
      geom_boxplot() +
      labs(title = "Boxplot Donkelar - Ceará",
           x = "Mês", y = "PM2.5 (µg/m³)") +
      theme_minimal()
    
    ggplot(dados_combinados, aes(x = factor(mes), y = pm2.5, fill = Fonte)) +
      geom_boxplot() +
      labs(title = "Boxplot Donkelar / Copernicus - Ceará",
           x = "Mês", y = "PM2.5 (µg/m³)") +
      theme_minimal()
    
    
  })
  
  #Grafico dispersão
  
  output$distPlot4 = renderPlot({
    # dados_wide <- donkelar_copernicus %>%
    #   pivot_wider(names_from = Fonte, values_from = pm2.5)
    
    ggplot(dados_wide, aes(x = Copernicus, y = Donkelar)) +
      geom_point(color = "blue", size = 3) +
      geom_smooth(method = "lm", se = FALSE, linetype = "dashed", color = "black") +
      geom_text(aes(label = mes), vjust = -1, size = 3) +
      labs(title = "Dispersão entre Copernicus e Donkelar - PM2,5",
           x = "Copernicus (µg/m³)",
           y = "Donkelar (µg/m³)") +
      theme_minimal()
  })
  
  #Mapa
  output$distPlot5 = renderPlot({
    tm_shape(map_simplificado) +
      tm_fill(col = 'pm2.5',
              palette = "Red",
              breaks = c(0, 10, 15, 20, 30, 40),  # cobrem toda a faixa
              na.show = FALSE,
              # colorNA = "red",
              textNA = "Missing") +
      tm_borders() +
      tm_layout(legend.outside = TRUE) +
      tm_credits("https://atmosphere.copernicus.eu/charts/packages/cams/", position = c("left", "bottom"), size = 0.7)
  })
  
  output$distPlot6 = renderPlot({
    
    tm_shape(map_simplificado_2) +
      tm_fill(col = 'pm2.5',
              palette = "Red",
              breaks = c(0, 10, 15, 20, 30, 40),  # cobrem toda a faixa
              na.show = FALSE,
              # colorNA = "red",
              textNA = "Missing") +
      tm_borders() +
      tm_layout(legend.outside = TRUE) +
      tm_credits("https://atmosphere.copernicus.eu/charts/packages/cams/", position = c("left", "bottom"), size = 0.7)
  })

}

shinyApp(ui, server)
