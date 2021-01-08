library(data.table)
library(tidyverse)
library(shiny)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
path <- "WA_Fn-UseC_-HR-Employee-Attrition.csv"
ui <- fluidPage(
  
  titlePanel("Job Attrition Analytics"),
  h5("Created by: DNN - Version: 1.0"),
  sidebarLayout(
    
    sidebarPanel(
      
      
    ), #endsidebarpanel
    
    mainPanel(
      "First some rows of data:",
      dataTableOutput("data_head_DT"),
      selectInput("cat_compare", "Choose category column to compare", choices=NULL, selected=NULL),
      plotOutput("cat_vs_cat_chart"),
      selectInput("num_compare", "Choose numeric column to compare", choices=NULL, selected=NULL),
      plotOutput("cat_vs_num_chart"),
    )#end mainpanel
  )# end sidebarlayout
)

server <- function(input, output, session) {
  data <- reactive({fread(path)})
  
  output$data_head_DT<-renderDataTable(data(),
                                       options =list(pageLength = 5)
  )
  observeEvent(data(),updateSelectInput(session, "cat_compare", 
                                                choices=names(data() %>% select_if(is.character)),selected='BusinessTravel'))
  
  observeEvent(data(),updateSelectInput(session, "num_compare", 
                                        choices=names(data() %>% select_if(is.numeric)),selected='Age'))
  # Plot categorical vs catagorica
  output$cat_vs_cat_chart<-renderPlot(
    Categorical_vs_categorical_plot(data(),~Attrition,input$cat_compare)
  )
  
  # Plot Categorical vs. Quantitative
  output$cat_vs_num_chart<-renderPlot(
    Categorical_vs_quantitative_plot(data(),~Attrition,input$num_compare)
  )
  
  # Function
  Categorical_vs_categorical_plot <- function(data,group_col,fill_col){
    data %>%
      group_by_(group_col, fill_col) %>%
      summarize(n = n()) %>% 
      mutate(pct = n/sum(n),lbl = scales::percent(pct))%>% 
      ggplot(aes_(x = group_col,y = ~pct,
                  fill = fill_col)) +
      geom_bar(stat = "identity",
               position = "fill") +
      scale_y_continuous(breaks = seq(0, 1, .2),label =scales::percent) +
      geom_text(aes(label = lbl), 
                size = 3, 
                position = position_stack(vjust = 0.5)) +
      #scale_fill_brewer(palette = "Set2") +
      labs(y = "Percent",x = "Attrition",title = "Compare attrition accross category")+
      theme_minimal()  
    
  }
  
  Categorical_vs_quantitative_plot <- function(data,categorical_col,quantitative_col){
    # plot the distribution using violin and boxplots
    ggplot(data, aes_(x = categorical_col, 
                      y = quantitative_col)) +
      geom_violin(fill = "cornflowerblue") +
      geom_boxplot(width = .2, 
                   fill = "orange",
                   outlier.color = "orange",
                   outlier.size = 2) 
  }
  
}


shinyApp(ui = ui, server = server)