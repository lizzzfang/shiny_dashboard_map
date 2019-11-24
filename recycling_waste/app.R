library(shiny)
library(leaflet)
library(ggplot2)
library(dplyr)
library(reshape2)
##########Initial##############
# import data
# set the directory of the project
df <- read.csv("Data/Recycling_and_Waste.csv")
# manipulate the data to barplot
c <- df[,-c(1:4,11)]
c[c(40, 58),1] = 'Tallaght'
c <- c %>% mutate(ACCEPTSCAN = ifelse(ACCEPTSCAN == "No",0,1))
c <- c %>% mutate(ACCEPTSGLA = ifelse(ACCEPTSGLA == "No",0,1))
c <- c %>% mutate(ACCEPTSPLA = ifelse(ACCEPTSPLA == "No",0,1))
c <- c %>% mutate(ACCEPTSTEX = ifelse(ACCEPTSTEX == "No",0,1))
c$Private_SDCC <- as.numeric(c$Private_SDCC)
c_p <- subset(c, as.numeric(c$Private_SDCC) == 1)
c_s <- subset(c, as.numeric(c$Private_SDCC) == 2)

quakeIcons <- iconList(A = makeIcon("Icons/0.png", iconWidth = 22, iconHeight =28),
                       B = makeIcon("Icons/1.png", iconWidth = 22, iconHeight =28),
                       C = makeIcon("Icons/2.png", iconWidth = 22, iconHeight =28),
                       D = makeIcon("Icons/3.png", iconWidth = 22, iconHeight =28),
                       E = makeIcon("Icons/4.png", iconWidth = 22, iconHeight =28),
                       G = makeIcon("Icons/6.png", iconWidth = 22, iconHeight =28),
                       H = makeIcon("Icons/7.png", iconWidth = 22, iconHeight =28)
                       )

############Webpage##############
# define UI 
ui <- fluidPage(
  titlePanel("Recycling and Waste in South Dublin"),
  
  # input and Output
  sidebarLayout(position = "right",
   
    sidebarPanel(
      # input:  
      style = "position:fixed;width:inherit;",
      width = 3,
      checkboxGroupInput(inputId ="check", 
                         label = h3("Accept Types:"), 
                         choices = list("Cans" = 1, 
                                        "Glass" = 2, 
                                        "Plastics" = 3,
                                        "Textiles" = 4)
                            ),
      radioButtons(inputId = "radio", 
                   h3("Private or SDCC:"),
                   choices = list("Private" = 1, "SDCC" = 2, "ALL" = 3),
                   selected = 3),
    
      actionButton("recalc", "back")
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # output
      ############
      leafletOutput("mymap"),
      p(),
      br(),
      textOutput("number"),
      column(
        width = 12,
        plotOutput("barplot")
      )
      ############
      
    )
  )
)

# Define server logic
server <- function(input, output) {
  ############
  points <- eventReactive(input$recalc, {
    df
  }, ignoreNULL = FALSE)
  pri <- eventReactive(input$radio, {
      subset(df, df$Private_SDCC == 'Private')
  })
  SD <- eventReactive(input$radio, {
    subset(df, df$Private_SDCC == 'SDCC')
  })
  output$mymap <- renderLeaflet({
      data <- points()
      if(input$radio == 1){data <- pri()}
      if(input$radio == 2){data <- SD()}
      if(length(input$check) >= 1){
        a <- data.frame()
        final <- data.frame()
        i <- 1
        il <- as.integer(input$check)
        while(il[i] %in% il) {
          if(il[i] == 1) {a <- subset(data, data$ACCEPTSCAN == 'Yes')
          final <- rbind(final,a)} 
          if(il[i] == 2) {a <- subset(data, data$ACCEPTSGLA == 'Yes')
          final <- rbind(final,a)} 
          if(il[i] == 3) {a <- subset(data, data$ACCEPTSPLA == 'Yes')
          final<- rbind(final,a)} 
          if(il[i] == 4) {a <- subset(data, data$ACCEPTSTEX == 'Yes')
          final<- rbind(final,a)} 
          i <- i+1
        }
        data <- final
      }
     
      leaflet(data) %>% addTiles() %>%
      #addProviderTiles(providers$Stamen.TonerLite,
      #                 options = providerTileOptions(noWrap = TRUE)
      #) %>%
      addMarkers(lng = data[,1], lat = data[,2], 
                 popup = paste("<h6>",data[,3],data[,4],"</h6>",
                               "Town:",data[,5]),
                 icon = ~quakeIcons[I])

  })
  
  # interactive
  output$number_1<-renderText({ 
    if (length(input$check) >= 1){
    cl <- as.integer(input$check)
    i<- 1
    l <- c()
    while(cl[i] %in% cl) {
     a<- cl[i]
     l <-c(a,l)
     i <- i+1
    }
    return(l)
    }
    return("NULL")
  })
  # barplot
  output$barplot <- renderPlot({
    if (input$radio == 1) {
     data <- aggregate(. ~ TOWNNAME, c_p, sum)
    }
    if (input$radio == 2) {
      data <- aggregate(. ~ TOWNNAME, c_s, sum)
    }
    if (input$radio == 3) {
      data <- aggregate(. ~ TOWNNAME, c, sum)
    }
    #data <- bar_count()
    # Render a barplot
    data <- data %>% tidyr::gather("id", "value", 2:5) 
    ggplot(data, aes(x = TOWNNAME, y = value)) + 
      geom_bar(aes(fill = id),stat = "identity",position = "dodge") + #position = "dodge"
      scale_fill_manual(values = c("#87CEFA","#F0FFFF", "#90EE90", "#DC143C"), name = "Category",
                        labels = c("Cans", "Glass", "Plastics", "Textiles")) +
      theme(
        plot.title = element_blank(),
        axis.text=element_text(size=10,face="bold"),
        axis.title=element_text(size=14),
        #axis.title.x = element_blank(),
        axis.title.y = element_blank())
  },height = 400, width = 1000)
}
#######################
shinyApp(ui = ui, server = server)
