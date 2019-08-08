library(shiny)
library(tidyverse)
library(Peptides)

# Define UI for application that draws a histogram
ui <- navbarPage(

    # Application title
    "Protein sequence pI and charge predictor",

    # Sidebar with a slider input for number of bins 
    tabPanel("Prediction",
                             
                        
                    
    sidebarLayout(
        sidebarPanel(
            # protein sequence entry
            textInput("seq", 
                      label = h3("Protein Sequence"),
                      value = "CHEMISTRY"),
            # pKa scale to use
            selectInput("scale",
                        label = h3("Select a pKa scale to use:"),
                        choices = c("EMBOSS", "Stryer", "Lehninger", "Bjellqvist", "Murray", "Sillero", "Solomon", "Dawson", "Rodwell"),
                        selected = "Murray"
                        ),
            br(),
            br(),
            br(),
            "pI and charges calculated using the Peptides package by Daniel Osorio, Paola Rondon-Villarreal, and Rodrigo Torres", tags$a(href="https://cran.r-project.org/web/packages/Peptides/index.html", "REF"),
            br(),
            br(),
            br(),
           
            "Shiny app created by C.E. Berndsen, 2019",
            br()
        ),
        

        # Show a plot of the generated distribution
        mainPanel(
           h3("Plot of Charge vs. pH"),
           plotOutput("chargePlot"),
           h4("Charge at select pH values"),
           tableOutput("pI")
           
           
        )
    )
),
tabPanel("More Information",
         includeHTML("protcharge.html"))
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$chargePlot <- renderPlot({
        seq <- as.character(input$seq)
        scale <- as.character(input$scale)
        
        df <- tibble("pH" = c(seq(from = 1, to = 14, by = 0.2)))
        
        # calculate the charge
        df <- df %>%
            mutate(charge = charge(seq = seq, pH = pH, pKscale = scale))
        
        # calculate the zero value
        zero <- pI(seq, pKscale = scale)
        
        # calculate 0.2 * the max value to position text correctly
        x20 <- max(df$charge) * 0.15
        
        # make the plot
        ggplot(df, aes(x = pH, y = charge)) +
            geom_point(aes(color = charge)) +
            # add crosshairs at the zero value
            geom_hline(yintercept = 0) +
            geom_vline(xintercept = zero) +
            # get the color scale right
            scale_color_gradient2() +
            # make it look pretty
            scale_x_continuous(expand = c(0.01, 0.01)) +
            theme_bw() +
            theme(axis.title = element_text(size = 24, face = "bold", color = "black"),
                  axis.text = element_text(size = 20, face = "bold", color = "black"),
                  plot.caption = element_text(size = 10),
                  legend.position = "none") +
            # label the axes and the pI value
            labs(x = "pH", y = "Charge", caption = paste("pKa scale used was", input$scale)) +
            annotate("text", x =  zero + 1.5, y = x20, label = paste("pI = ", round(zero, 2)), size = 6, color = "black")
    }
)
    output$pI <- renderTable({
        # make the pH table
        pHint <- tibble("pH" = seq(from = 1, to = 14, by = 1))
        
        # make sequence variable
        seq <- as.character(input$seq)
        
        # make scale variable
        scale <- as.character(input$scale)
        
        # make a table
        pHint %>% 
            mutate(charge = charge(seq = seq, pH = pH, pKscale = scale)) %>%
            spread(., pH, charge, 1:2)
    })
}


# Run the application 
shinyApp(ui = ui, server = server)
