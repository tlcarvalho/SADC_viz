library(dplyr)
library(tidyverse)
library(plotly)
library(shiny)
library(here)
library(readxl)
library(shinyWidgets)
library(dslabs)
library(lubridate)
library(reshape2)

df <- read_excel("CDS initiatives - lasts.xlsx") %>% rename(Axis=axis, Issue=issue, Outcome=outcome, Leader=leader,
           Proposition=first_ap, Result=result)
df$num <- 1
df$Axis[df$Axis==1] <- "Defense Policies"
df$Axis[df$Axis==2] <- "Milit. Coop., Hum. Action, and Peace Op."
df$Axis[df$Axis==3] <- "Defense Industry"
df$Axis[df$Axis==4] <- "Education and Capacity Building"
df$Axis[df$Axis=="CBM"] <- "Conf. Build. Measures"
df$proposition[df$proposition == "n/a"] <- NA
df$proposition <- as.numeric(df$proposition)
df$proposition <- as.Date(df$proposition, origin = "1899-12-30")

df$Year <- year(df$proposition)
df <- df %>% select(transl_simpl_name, Axis, Issue, Outcome, Leader, Proposition, Result, num)
df <- melt(df, id.vars = "transl_simpl_name")

variables <- c("Axis", "Issue", "Outcome", "Leader", "Proposition", "Result")
fill <- c("Axis", "Issue", "Outcome", "Leader", "Proposition", "Result")

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("SADC Data"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput("variables", label = "Variable:", 
                        choices = variables),
            selectInput("fill", label= "Fill:",
                        choices=fill)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotlyOutput("SADCplot")
        )
    )
)
# Define server logic required to draw a histogram
server <- function(input, output) {
        d <- reactive({
            df %>% filter(value != "n/a" & value != "NA" & value != "AMC" &
                              value != "HAI"  & value != "TP") %>%
                filter(variable %in% input$variables |
                              variable %in% input$fill) %>%
                dcast(transl_simpl_name ~ variable) %>%
                mutate(num=1) 
        })
        
        f <- reactive({
            d() %>%
                group_by(Variable = d()[[input$variables]], Fill=d()[[input$fill]]) %>% 
                summarize(Total = sum(num)) %>%
                na.omit()
        })

        output$SADCplot <- renderPlotly({
            pt <- ggplot(f(), aes(x=Variable, y=Total,
                                  fill=Fill))+
                geom_bar(stat="identity") +
                labs(x="Variable", y="Proportion of initiatives",
                     fill="Fill") +
                theme(panel.background = element_rect(fill='white', 
                                                      colour='black'), 
                      legend.position = "bottom",
                      axis.text.x=element_text(angle=45))
            ggplotly(pt)
        })
    }
# Run the application 
shinyApp(ui = ui, server = server)