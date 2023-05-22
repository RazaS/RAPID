library(shiny)
library(dplyr)

#set_logging()

# environment
options(scipen = 999)
counter <- reactiveVal(0) #to generate new button names

######################## file uploads ############

csv_file <- "All.csv"
# Read the CSV file into a dataframe
All <- read.csv(csv_file, sep =";")

csv_file <- "Asian.csv"
# Read the CSV file into a dataframe
Asian <- read.csv(csv_file, sep =";")

csv_file <- "Arabic.csv"
# Read the CSV file into a dataframe
Arabic <- read.csv(csv_file, sep =";")

csv_file <- "Black.csv"
# Read the CSV file into a dataframe
Black <- read.csv(csv_file)

csv_file <- "Latin_American.csv"
# Read the CSV file into a dataframe
Latin_American <- read.csv(csv_file, sep =";")

csv_file <- "South_Asian.csv"
# Read the CSV file into a dataframe
South_Asian <- read.csv(csv_file, sep =";")

csv_file <- "Indigenous.csv"
# Read the CSV file into a dataframe
Indigenous <- read.csv(csv_file, sep =";")

csv_file <- "White.csv"
# Read the CSV file into a dataframe
White <- read.csv(csv_file, sep =";")

csv_file <- "Blank.csv"
# Read the CSV file into a dataframe
Blank <- read.csv(csv_file, sep =";")

csv_file <- "Other.csv"
# Read the CSV file into a dataframe
Other <- read.csv(csv_file, sep =";")



############# UI ###################
shinyApp(
  ui = 
    fluidPage(
      
      
      # Display the row for the button clicked
      
      tags$head(
        tags$style(
          HTML("
          .app-title {
            font-size: 30px;
            font-weight: bold;
            text-align: center;
            margin-top: 20px;
            margin-bottom: 3px;
          }
          
          .app-subtitle {
            font-size: 10px;
            font-weight: bold;
            text-align: center;
            margin-top: 1px;
            margin-bottom: 2px;
          }
                    
          .blue-text {
          color: blue;
          }
          

          .pressed-color {
          background-color: gray;  /* Set the pressed button color */
          }
          
        ")
        )
      ),
      tags$div(class = "app-title", "RBC Donor Phenotype Prediction "),
      tags$div(class = "app-subtitle", "Preliminary version. From data by Tordon et. al presented at CSTM 2022"),
      tags$div(class = "app-subtitle", "E: s.raza@utoronto.ca", tags$span(class="blue-text","T: @RazaSN")),
      
      actionButton("reset_btn", "Reset"),
      actionButton("refresh_btn", "Refresh"),
      
      
      
      verbatimTextOutput("details"),
      fluidRow(
        column(width = 3,
               selectInput(inputId ="data_source",
                           label = "Select a data set",
                           choices = c("All", "Asian","Arabic", "Black","Indigenous","Latin_American","South_Asian","White","Blank","Other"))),
        

               
        column(width = 9,
               uiOutput("show_table"))
      )
    ),
  
  
  
  
########################## server starts here #################################
  
  server = 
    shinyServer(function(input, output, session){
      
      
      
      # Store the details of the clicked row
      Data <- reactiveValues(Info = NULL)
      c_prob <- reactiveValues(value = 1)
      reactive_string <- reactiveVal()
      pressed_buttons <- reactiveVal()
      
      
      # One Observer to Rule Them All (evil cackle)
      # Update the Data$Info value.
      observe({
        
        
        
        # Identify all of the buttons in the table.
        # Note that I assumed the same prefix on all buttons, and 
        # they only differ on the number following the underscore
        # This must happen in an observed since the number of rows 
        # in the table is not fixed.
        
        #print(input$data_source)

        #this line gives a unique name to the buttons
        
        input_btn <- paste0(input$data_source,counter(), "btn_", seq_len(nrow(display_table())))

        lapply(input_btn,
               function(x){
                 observeEvent(
                   
                   input[[x]],
                   {
                     # print (x)
                     # print(paste0("btn_", "", x))
                     # print (sub(paste(input$data_source,"btn_"), "", x))
                     
                     #this line identifies the button to pull its data from display table
                     i <- as.numeric(sub(paste0(input$data_source,counter(),"btn_"), "", x))
                     Data$Info <- display_table()[i, -length(display_table())]
                     
                     new_label <- "Selected"  # Set the new label for the button
                     updateActionButton(session, x, label = new_label)
                     
                     pressed_buttons(c(pressed_buttons(), x)) 
                     #keep track of pressed buttons to reset  them later
                     
                     
                    if (length(reactive_string())>0){ 
                      #run only if reactive string isn't empty, to avoid null errors
                       
                      if (!grepl(paste0(Data$Info$Antigen,"-"), reactive_string())) { 
                        #checks if an antigen has been used before, if not, uses it 
                        
                        
                        c_prob$value <-  (100-Data$Info$Frequency)*c_prob$value/100
                        
                        print (reactive_string())
                        print (Data$Info$Antigen)
                        
                        reactive_string(paste0(reactive_string(), Data$Info$Antigen, "- "))
                        
                      }
                      
                    }
                     
                     else { 
                     
                     
                     c_prob$value <-  (100-Data$Info$Frequency)*c_prob$value/100
                     
                     print (reactive_string())
                     print (Data$Info$Antigen)
                     
                     reactive_string(paste0(reactive_string(), Data$Info$Antigen, "- "))
                     
                     }
                     
                     #print ( Data$Info)
                     
                     #print (input_btn)
                     

                   }
                   
                 )
               })
        


        
      })
      
      

      
      # Generate the table of data.  
      display_table <- 
        
        reactive({
          
          tbl <- 
            get(input$data_source) %>% 
            # Add the row names as a column (not always useful)
            cbind(Row_id = rownames(.),
                  .) %>% 
            
            # Add the row names as a column (not always useful)
            cbind(Antigen =  get(input$data_source)$buttonLabel,
                  .) %>% 
            
            # Add the row names as a column (not always useful)
            cbind(Frequency = get(input$data_source)$buttonValue,
                  .) %>% 
            

            
            # Add the action buttons as the last column
            mutate(Select = vapply(row_number(),
                                   function(i){
                                     actionButton(inputId = paste0(input$data_source,counter(), "btn_", i),
                                                  label = "Select") %>% 
                                       as.character()
                                   },
                                   character(1)))
          

        })
      
      observe({
      

      # Render the table with the action buttons
      output$show_table <- 
        
        renderUI({
          
          display_table() %>% 
            select(Antigen, Frequency, Select) %>% 
            knitr::kable(format = "html",
                         # very important to use escape = FALSE
                         escape = FALSE) %>% 
            HTML() %>%
            tagList(tags$style(HTML("
        table {
          font-size: 16px;
          width: 60%;
        }
        th, td {
          padding: 8px;
        }
      "))) 
          

        })
      
      
        c_prob$value <- 1
        reactive_string("")
        
      })
      
      
      
      # Reset button event
      observeEvent(input$reset_btn, {

        reset_all()
        
        
        #session$reload()
        
      })
      
      reset_all <- function() {
        c_prob$value <- 1
        reactive_string(NULL)
        
        for (i in pressed_buttons()) {
          new_label <- "Select"  # Set the new label for the button
          updateActionButton(session, i, label = new_label)
          
        }

        

      }
      
      observeEvent(input$data_source, {
        # Code to execute when the value of input$data_source changes
        # Add your monitoring logic here
        reset_all()
        counter(counter()+1)

      })
      
      
      # Reset button event
      observeEvent(input$refresh_btn, {

        session$reload()
        
      })
      
      
      
      # Print the details to the screen.
      output$details <- 
        renderText({
          
          req(Data$Info)

          
          donors <- round(1/c_prob$value,2)
          
          antigen_rec <- paste("Antigen Negativity Selected: ", reactive_string())
          
          antigen_info <- paste("Estimated Donors:", round(c_prob$value*100,4), "% or ", "1 in", donors, "donors")
  
          paste(antigen_rec, antigen_info, sep="\n")
          

          
          
        })
      
      
      
    })
  
)

