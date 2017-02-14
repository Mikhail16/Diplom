
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(plotly)
library(ggplot2movies)
 
shinyUI(fluidPage(
#  tags$head(
#    tags$link(rel = "stylesheet", type = "text/css", href = "main.css")
#  ),
  # Application title
  titlePanel("Дослідження відбиваючих властивостей акустичної моделі ділянки трубопроводу"),   

  sidebarLayout(
  sidebarPanel(
    
      selectInput("radio","Cередовище транспортування",choices = list("Повітря" = "povitrya", "Метан" = "metan", "Аміак" = "amiak","Природній газ" = "gas")),
      fluidRow(
        column(6,
               wellPanel(
                tags$h5("Технологічні умови транспортування речовини"),
                selectInput("textT", label = h6("Введіть температуру T, К"), 
                      choices = list("273" = 273, "283" = 283, "293" = 293, "303" = 303, "313" = 313), 
                      selected = 273),
                textInput("textP", label = h6("Введіть тиск P, Па"), value = "101325")
              #  submitButton("Update View")
              )),
        column(6,wellPanel(
          tags$h5("Параметри трубопроводу"),
          textInput("textd", label = h6("Введіть діаметр d, м"), value = 0.15),
          selectInput("textdel", label = h6("Введіть товщину стінки δ, м"), 
                      choices = list("0.002" = 0.002, "0.00325" = 0.00325), 
                      selected = 0.00325),
          textInput("textE", label = h6("Введіть модуль пружності матеріалу E"), value = 2.08*10^11)
          
        ))
        ),
        tags$h5("Характеристики речовини, що транспортується"),
      fluidRow(  
      column(6, 
          
               textInput("textc", label = h6("Введіть швидкість звуку C, м/с"), value = ""),
               textInput("textro", label = h6("Введіть густину ρ, кг/м3"), value = "")
               
          
        ),
      column(6,
             textInput("textksi", label = h6("Введіть ξ"), value = ""),
             textInput("texteta", label = h6("Введіть η"), value = "")
             )),
      hr(),
      h4("Максимальна дальність виявлення витоку"),
      fluidRow(column(6, wellPanel(
              textInput("textPower", label = h6("Вкажіть допустимі втрати сигналу для виявлення витоку, дБ"), value = 95)
                            )),
              column(6, wellPanel(
                      h6("Максимальна відстань виявлення витоку заданої конфігурації, км"),
                      verbatimTextOutput("rezultPower")
                     
              ))
              )
      

      
       
  ),
  mainPanel(
        
    fluidRow( 
      
      column(6,wellPanel(
        sliderInput("rezultf_sl", "Діапазон значень частоти за умовою поширення плоских хвиль",
                    min = 20, max = 200, value = 50),
        h5("Врахування температурної залежності Cm(T), м/c"),
        verbatimTextOutput("rezultC"),
        h5("Критичне значення частоти fкр, Гц"),
        verbatimTextOutput("rezultf"),
        h5("Коефіцієнт згасання на заданій частоті β, дБ/км"),
        verbatimTextOutput("rezultBet"),
        h5("Врахування частотної залежності С(ω), м/c"),
        verbatimTextOutput("rezultCw")
      )),
      column(6,wellPanel(
              h5("Характеритистики неоднорідності"),
              radioButtons("radio1", label = h6("Вид неоднорідності"),
                           choices = list("Витік у формі кола" = "otvir", "Витік у формі еліпса" = "elips", "Закритий відвід" = "close"), 
                           selected = "otvir"),
              textInput("textdotv", label = h6("Введіть діаметр отвору (мала вісь еліпса) або відводу, м"), value = 0.005),
              textInput("textlotv", label = h6("Введіть довжину відгалуження або товщину стінки труби, м"), value = 0.00325),
              h6("Згасання на неоднорідності, дБ"),
              verbatimTextOutput("rezult")
      ))
      #column(12,
       #      tableOutput('table2'))
    ),  
        
    fluidRow(
            column(6, 
                   
                   textInput("textfreq1", label = h6("Введіть нижню межу частоти, Гц "), value = 50)
                   
                   
            ),
            column(6,
                   textInput("textfreq2", label = h6("Введіть верхню межу частоти, Гц "), value = 200)
            )    
    ),
    

    # Show a plot of the generated distribution
  
      fluidRow(column(12,
                      tabsetPanel(type = "tabs", 
                                  tabPanel("Коефіцієнт поглинання, дБ/км", plotlyOutput("trendPlot")), 
                                  tabPanel("Швидкість звуку, м/с",plotlyOutput("trendс1")),
                                  tabPanel("Коефіцієнт відбивання від витоку",plotlyOutput("trendVabs")),
                                  tabPanel("Коефіцієнт відбивання від неоднорідності", plotlyOutput("trendVabs1"))
                                 # tabPanel("Temp", plotOutput("TempPlot")), 
                                 # tabPanel("Temp1",  plotOutput("TempPlot1"))   
                      
                      
             )         ),
             column(12,
                    tableOutput('table1')),
             column(12,
                    tableOutput('table3'))
          
    
  )))
))
