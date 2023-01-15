#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyWidgets)
library(ggplot2)
library(dplyr)
library(groupdata2)


Coronakennzahlen <- na.omit(read.csv("C:/Users/Fatma/OneDrive/Dokumente/Hausarbeit/data/Coronakennzahlen.csv"))


# Define UI for application that draws a histogram
ui <- fluidPage(
setBackgroundColor("#CDC9C9"),
  
  #source: https://shiny.rstudio.com/tutorial/written-tutorial/lesson2/
    # Application title
    titlePanel("Hausarbeit RKI Covid 19"),


tabsetPanel(
  tabPanel("Altersgruppen,Infizierte(männlich)"), 
  tabPanel("Altersgruppen,Infizierte(weiblich)"), 
  tabPanel("Bezirke, Infizierte"),
  tabPanel("BezirkeInf, Fläche"),
  tabPanel("Geschlecht gesamt"), 
),
    h5("Autoren: Fatmagül Tokcan, Seda Delikaya, Sinem Kurtoglu"),
    
  
    fluidRow(splitLayout(cellWidths = c("33%", "67%"), 
                         img(src = "https://media.defense.gov/2020/Apr/21/2002285330/1088/820/0/200421-D-EX074-002.JPG",height = 240, width = 545), 
                         h3("Chancen einer Covid-Infektion von Frauen und Männern ",
                         h2("Anzahl der Infizierten: ",
                         h4("Davon : 290 (40,62%)"), h4(" Davon : 424 (59,38%)"))
                           
)),


# source: https://shiny.rstudio.com/tutorial/written-tutorial/lesson3/
column(3, 
       checkboxGroupInput("checkGroup", 
                          h3("Altersgruppe wählen"), 
                          choices = list("A00-A04", 
                                         "A05-A14", 
                                         "A15-A34",
                                         "A35-A59",
                                         "A60-A79",
                                         "A80+"),
                          selected = 1)),


),



    # Sidebar with a slider input for number of bins 
    sidebarLayout(

  
        sidebarPanel(
          style = "color:black",
      # source: https://shiny.rstudio.com/tutorial/written-tutorial/lesson4/       
          
          selectInput("Landkreis", 
                      label = "Wähle ein Landkreis aus",
                      choices = c("SK Berlin Mitte", 
                                  "SK Berlin Friedrichshain-Kreuzberg",
                                  "SK Berlin Pankow", 
                                  "SK Berlin Charlottenburg-Wilmersdorf",
                                  "SK Berlin Spandau",
                                  "SK Berlin Tempelhof-Schöneberg",
                                  "SK Berlin Neukölln",
                                  "SK Berlin Treptow-Köpenick",
                                  "SK Berlin Marzahn-Hellersdorf",
                                  "SK Berlin Lichtenberg",
                                  "SK Berlin Reinickendorf"),
                      selected = "SK Berlin Mitte"),
          
    
      # source: https://shiny.rstudio.com/tutorial/written-tutorial/lesson1/      
          sliderInput(inputId = "bins",
                      label = "Anzahl Gruppen der Genesenen(Histogramm):",
                      min = 1,
                      max = 30,
                      value = 26),
          radioButtons("lbHist", "Labels anzeigen? (Histogramm):",
                       choices = list("Ja" = "true",
                                      "Nein" = "false"),
                       selected = "false"),
          selectInput("dropd", "Absolute oder Relative Werte? (Parch, SipSp):",
                      c("Absolute" = "abso",
                        "Relative" = "rela"))
        ),

        # Show a plot of the generated distribution
       
        mainPanel(
           plotOutput("histCor"),
           plotOutput("distPlot"),
           textOutput("selected_Landkreis"),
          
           )
          
        )
    )


# source: 
# Define server logic required to draw a histogram
server <- function(input, output) {

  
  output$selected_Landkreis <- renderText({ 
    paste("Du hast Landkreis:", input$Landkreis, "ausgewählt")
    
  })  

  
  Genesene <- as.data.frame(Coronakennzahlen[which(Coronakennzahlen$AnzahlGenesen == ">0"), ,c(1-100)])
  
 
  
  output$histCor <- renderPlot({
    lbHist <- switch(input$lbHist,
                     true = TRUE,
                     false = FALSE)
  
    
    
    
    x    <- na.omit(Coronakennzahlen$AnzahlFall)
    hist(x, breaks = input$bins, labels = lbHist, ylim=c(0,200), xlim = c(0,350), col = "#00C5CD", border = "black",
      
            
         xlab = "Anzahl der Fälle",
         main = "Anzahl der Corona Fälle in Abhängigkeit zur Anzahl der Genesenen", ylab="Anzahl de Genesenen")
    
    
 
    })
  
  
  CoronaInfizierte <- as.data.frame(Coronakennzahlen[which(Coronakennzahlen$IstErkrankungsbeginn == "1"),c(1-300)])

  
  output$distPlot <- renderPlot({
    lbHist <- switch(input$lbHist,
                     true = TRUE,
                     false = FALSE)
    
    
    x    <- na.omit(Coronakennzahlen$AnzahlFall)
    hist(x, breaks = input$bins, labels = lbHist, ylim=c(0,300), xlim = c(0,250), col = "#00C5CD", border = "black",
         
         
         xlab = "Anzahl Corona Fälle",
         main = "Anzahl der Corona Fälle in Abhängigkeit von Corona Erkrankten", ylab="Corona Erkrankte")
    
    
    
  })
  
  

  

    }
  


    
    
  

# Run the application 
shinyApp(ui = ui, server = server)
runGitHub("<Fatmaguel-Tokcan>","<https://github.com/Fatmaguel-Tokcan/Statistik/>")

