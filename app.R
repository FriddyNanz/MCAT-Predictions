library(shiny)
library(tidyverse)
library(rsconnect)
Scores2019 <- read_csv("Scores2019.csv")


ui <- fluidPage(

    titlePanel("FriddyNanz MCAT Score Predictor"),

    sidebarLayout(
        sidebarPanel(
            radioButtons("test", h3("Select test"),
                         choices = list("AAMC FL 1" = 1, "AAMC FL 2" = 2,
                                        "AAMC FL 3" = 3),selected = 1),
            sliderInput("CPscore",
                        "C/P Score:",
                        min = 118,
                        max = 132,
                        value = 125),
            sliderInput("CARSscore",
                        "CARS Score:",
                        min = 118,
                        max = 132,
                        value = 125),
            sliderInput("BBscore",
                        "B/B Score:",
                        min = 118,
                        max = 132,
                        value = 125),
            sliderInput("PSscore",
                        "P/S Score:",
                        min = 118,
                        max = 132,
                        value = 125),
            submitButton("Submit")
        ),
        
        mainPanel(
            textOutput("score"),
            plotOutput("bootPlot")
        )
    )
)


server <- function(input, output) {

    numBoots <- 1000
    
    FL1 <- c("RealShit","AAMC1_CP","AAMC1_CARS","AAMC1_BB","AAMC1_PS")
    FL2 <- c("RealShit","AAMC2_CP","AAMC2_CARS","AAMC2_BB","AAMC2_PS")
    FL3 <- c("RealShit","AAMC3_CP","AAMC3_CARS","AAMC3_BB","AAMC3_PS")
    
    preds <- matrix(nrow = numBoots,ncol = 1)
    
    bootstrapFL <- function(test) {
        data <- na.omit(select(Scores2019, all_of(test)))
        colnames(data) <- c("RealShit","CPscore","CARSscore","BBscore","PSscore")
        N <- nrow(data)
        for(i in 1:numBoots){
            samp <- sample(N,numBoots,rep=T)
            boots <- data[samp,]
            lm <- lm(RealShit ~ ., data = boots)
            preds[i,] <- predict(lm, newdata = data.frame(
                CPscore = input$CPscore,
                CARSscore = input$CARSscore,
                BBscore = input$BBscore,
                PSscore = input$PSscore))
        }
        return(preds)
    }
    
    scoreModel <- reactive({
        if(input$test == 1) {
            bootstrapFL(FL1)
        } else if(input$test == 2) {
            bootstrapFL(FL2)
        } else if(input$test == 3) {
            bootstrapFL(FL3)
        }
    })
    score <- reactive({sort(scoreModel())})
    guessLow <- reactive({
        round(score()[(numBoots - 0.95*numBoots)],1)
    })
    guessHigh <- reactive({
        round(score()[(numBoots - 0.05*numBoots)],1)
    })
    output$score <- renderText({
        paste("Based on predictions from 1000 random samples of the data, my best guess is that you will score in the ballpark of",guessLow(),"-",guessHigh())
    })
    output$bootPlot <- renderPlot({
        predictions  <- score()
        hist(predictions, col = 'blue', border = 'white')
    })
}

shinyApp(ui = ui, server = server)
