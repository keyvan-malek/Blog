library(shiny)

library(ggpubr)
library(ggplot2)

setwd("~/blog posts/R shiny/") # turnoff for updating the website

# UI 

ui <- fluidPage(  
  # The fluidPage command translates R shiny codes to HTML format 
  
  
  
  titlePanel("Most Downloaded R Packages"), # Here we can define the main title of our web app
  
  
  fluidRow( 
    
    # The fluidRow command gives us the flexibility to define our layout column and offset commands are two of the popular arguments
    # that can be used to generate our layout
    # There are also other predefined layouts available in R-Shiny such as sidebar layout, vertical layout and split layout
    
    
    # Here we width and position of our object in the web app
    column(10, offset = 4,
           
           # Users can have different types of widgets in R-Shiny, such as dropdown menus and date range inputs. One of the popular ways to set a number
           #is through a slider widget, the following two lines define two slider bars to set different values
           # The sliderInputs command allows us to define the range and default value of our slider widget
           
           sliderInput(inputId = "NumLibs1",
                       label = "1- Number of libraries in PDF plot:", min = 30, max = 1000, value = 30),
           
           
           sliderInput(inputId = "NumLibs2",
                       label = "2- Number of libraries in bar plot:", min = 3, max = 30, value = 10)
    ),
    
    # The mainPanel command defines the size and layout structure of our main panel where we show our actual plots
    column(12, 
           mainPanel(
             
             # Creates a plot that will be placed in the main panel
             plotOutput(outputId = "plot",width = "150%", height = "450")
             
           )
    )
    
  ) 
)


# Server 

server<-function(input, output){ 
  # Here we define a server function that does the behind the scene calculations and generates our plot
  
  input_data<- read.table("pkg_by_freq.txt", header = T)
  # Reads the input file from our working directory
  
  reorderd_freq<-input_data[order(input_data$pkg_count, decreasing = T),]
  # This command sorts our input data based on number of downloads in that particular day (11/01/2019)
  
  output$plot <- renderPlot({
    # This part renders our output plot
    
    max_numb<-input$NumLibs1
    num_pop_libs<-input$NumLibs2
    # Here our code receives the number that will be set by users through the slider widget
    
    p1<-ggplot(reorderd_freq[6:max_numb,], aes(x=pkg_count)) +geom_density(fill="lightblue")+
      labs(title = "1- PDF of Numuer of Downloads of R Packages",  x="Number of Downloads", y="") +
      theme_bw() +theme(axis.text.x  = element_text(size = rel(1.8)) )
    
    reorderd_freq$pkg_name <- reorder(reorderd_freq$pkg_name, reorderd_freq$pkg_count)
    p2<-ggplot(reorderd_freq[1:num_pop_libs,])+ geom_bar(aes(x=pkg_name, y=pkg_count), stat="identity", fill="purple1") +
      labs(title = "2- Most Downloaded R Packages", y="Number of Downloads", x="Package Name") +
      coord_flip() +theme_bw() +theme(axis.text.y  = element_text(size = rel(1.4)) )
    # Now we use ggplot2 package to generate two figures a PDF plot (geom_density) and a bar plot (geom_bar)
    # Note that we use the slider input to change the characteristics of our plot
    
    ggarrange(p1, p2)
    # Finally we combine our two plots using ggarange function from the ggpubr package
  })
  
}

shinyApp(ui, server)
# This command connects the UI code with the server code and generates our final output 

