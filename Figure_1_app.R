################################################################
### DESIRE - Nature Ecology and Evolution 
### February 2019
### 
### Figure 1
### 
### Ines S. Martins (ines.martins@idiv.de) 
#####################################################################################


library(ggplot2)
library(reshape2)
library(network)
library(wordcloud)
library(plotrix)
library(plyr)
library(gridExtra)
library(grid)
library(stringr)
library(graphics)
library(shiny)
library(shinydashboard)


df3<-read.csv("Figure_1a.csv")

df3<-df3[,2:5]

df3<-droplevels(df3)


###### RUN SHINY #####

ui <- fluidPage(
  
  titlePanel(h3("Production impacts on biodiversity and carbon sequestration per economic sector")
  ),
  
  sidebarLayout(
    
    sidebarPanel(position = "right",
                 fluidRow(selectInput("bins", h4("Impacts in absolute terms for the year 2011"), 
                                      choices = list("Region" = df3$WR), selected = 1)),
                 # fluidRow(helpText("Annual changes in production impacts relative to 2000")),
                 fluidRow(helpText("Biodiversity impacts are measured in terms of impending global bird extinctions.")),
                 fluidRow(helpText(em("Based on Figure 1a in Marques et al. (2019) Trends on higher land-use efficiency insufficient to mitigate impacts on nature from population and consumption growth. Nat.Ecol.Evo."))),
                 fluidRow(helpText("Graph by I.S. Martins."))
                 
    ),
    
    mainPanel(
      
      plotOutput(outputId = "distPlot")
    )
    
    #)
    # h4("Decomposition of changes in impacts of agriculture and forestry on biodiversity and carbon sequestration into the contribution of the changes
    #    in population, GDP per capita and impact per GDP."),
    
    # Output: Histogram
    #column(2,plotOutput(outputId = "distPlot"))
    #plotOutput(outputId = "distPlot2")
  )
)


#df43[df43$WR==input$bins,]

#Run and store the plot



server <- function(input, output) {
  
  output$distPlot <- renderPlot({
    
    d<-df3[df3$WR==input$bins,]
    SUM<-sum(d$value)
    d<-c('#E69F00')
    
    
    ggplot() + 
      geom_bar(data=df3[df3$WR==input$bins,], aes(x = reorder(LU,value), y = value, fill = WR), stat="identity",width = 0.35)+
      scale_fill_manual(values = d)+
      labs(y="Biodiversity impacts (no. of species)",size = 2)+
      geom_text(aes(label = paste("Total impacts = ", format(round(SUM, 3), nsmall = 3))), x = -Inf, y = -Inf,
                hjust = -1, vjust = -1, col = 'black',size=5)+
      # annotate("text", y= max(f3[df3$WR==input$bins,]), x=max(f3[df3$WR==input$bins,]),label="Custom Title",hjust=1)+
      theme(#axis.title.x = element_blank(), 
        axis.title.y = element_blank(), 
        axis.ticks.y = element_blank(),
        axis.text.y = element_text(size=10),
        axis.text.x = element_text(size=10),
        axis.title=element_text(size=11),
        axis.title.x=element_text(margin = margin(t = 0, r = 0, b = 30, l = 0)),
        legend.position='none'
        ,panel.background = element_blank(),
        axis.line.x = element_line(colour = "black",size = 0.2)
      ) +
      geom_hline(yintercept = 0,alpha =0.5)+
      scale_y_continuous(position = "right",trans = "reverse") +
      #scale_x_continuous(position = "botto",trans = "reverse") 
      #annotate("text", label = "plot mpg vs. wt", x= 2, y = max(df3$value), size = 8, colour = "red")+
      coord_flip()
    
  })
  
}

shinyApp(ui = ui, server = server)


