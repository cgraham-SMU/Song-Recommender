library(shiny)
library(shinyjs)
library(stringr)
library(shinydashboard)
library(leaflet)
library(DBI)
library(odbc)
library(DT)
library(shinythemes)
library(shinyWidgets)

source("./credentials_v4.R")

  ui <- fluidPage(
    theme = shinytheme("lumen"),
    useShinyjs(),  # Set up shinyjs
    navbarPage("Group Project - Song Recommender",
        tabsetPanel(
          tabPanel("Welcome!",
                   value = 1,
                   br(),
                   textInput("username", "username"),
                   passwordInput("password", label = "password"),
                   actionBttn(
                     inputId = "login",
                     label = "login",
                     color = "primary",
                     style = "jelly",
                     size = "md"),
                   br(),
                   br(),
                   tags$div(class="header", checked = NA,
                            tags$p("Try username 'itom' and password '6265'.")
                            )
                   
          ), # closes tabPanel
          
          id = "tabselected", type = "pills"
          
        )  # closes tabsetPanel      
        
      )  # closes navbarPage
    
  ) # Closed Fluid Page 
  
  
  # Server ------
  server = function(input, output, session,data,reset){
    
    user_vec <- c("itom" = "6265",
                  "user456" = "password2")
    
    observeEvent(input$login, {
      
      if (str_to_lower(input$username) %in% names(user_vec)) { # is username in user_vec?
        
        if (input$password == unname(user_vec[str_to_lower(input$username)])) {
          
          # > Add MainPanel and Sidebar----------
          shinyjs::show(id = "Sidebar")
          
          appendTab(inputId = "tabselected",
                    
                    tabPanel("User Information",
                          value = 2,
                          sidebarPanel(
                             textInput("user", "User ID:", "Please type in here..."),
                             textInput("pw", "Password:", "Please type in here..."),
                             textInput("Fname", "First Name:", "Please type in here..."),
                             textInput("Lname", "Last Name:", "Please type in here..."),
                             textInput("birth", "Date of Birth:", "Please type in here..."),
                             textInput("address", "Address:", "Please type in here..."),
                             textInput("city", "City:", "Please type in here..."),
                             textInput("state", "State:", "Please type in here..."),
                             textInput("zip", "Zip Code:", "Please type in here..."),
                             actionBttn(inputId = "submit",label = "Submit", color = "primary", style = "jelly",size = "md"),
                             actionBttn(inputId = "update",label = "Update", color = "primary", style = "jelly",size = "md")),
                          mainPanel(
                            DT::dataTableOutput("mytable"),
                            br(),
                            br(),
                            textInput("ID", 
                                        label = "Choose User ID to delete:"),
                            actionButton(inputId = "delete", label = "Delete", icon = icon("minus-circle"),class = "btn-primary")),
                    ) # closes tabPanel,
                    
          ) # closes appendTab 
          
          appendTab(inputId = "tabselected",
                    
                    tabPanel("Search",
                          value = 4,
                          sidebarPanel(
                                        selectInput("searchselect", label = h3("Search by..."), 
                                                  choices = c("track_name", "artist_name", "genre")),
                                        textInput("searchtext", label = h4("With these Keywords:"), value = "e.g. Taylor Swift"),
                                        sliderInput("valence", "Do you prefer songs that sound more positive(e.g. happy, cheerful, euphoric),  or negative(e.g. sad, depressed, angry) Rate from 0(Negative) to 1(Positive)!:", min = 0, max = 1, value = 1),
                                        sliderInput("dance", "Do you like to dance while listening to music? Range yourself from 0 to 1!:", 0, 1, c(0,1)),
                                        sliderInput("energy", "How much do you like energetic tracks? Range yourself from 0 to 1!:", 0, 1, c(0,1)),
                                        actionBttn(
                                          inputId = "Go",
                                          label = "Get Results",
                                          color = "primary",
                                          style = "jelly",
                                          size = "md")),
                          
                          mainPanel(
                                     h4("Search Results Below:"),
                                     DT::dataTableOutput("mytable2")
                                                        
                                )# closes mainpanel
                      ) # closes tabPanel,
                              
                    ) # closes appendTab
          
          appendTab(inputId = "tabselected",
                    tabPanel("Genre",
                             value = 5,
                             mainPanel(
                                      column(width =5 ,
                                      div(style="display:inline-block;horizontal-align:left;",
                                      actionButton("rock",label = img(src="data:image/jpeg;base64,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"),width = "300px"))),
                                      column(width =5 ,
                                      div(style="display:inline-block;horizontal-align:center;", 
                                      actionButton("country",label =img(src="data:image/jpeg;base64,/9j/4AAQSkZJRgABAQAAAQABAAD/2wCEAAkGBxMTEhUTExMWFhUXGR8XFxcXFxcXGBgYGBUYGBsYGBcYHSggGBolHRgXITEhJSkrLi4uFyAzODMtNygtLisBCgoKDg0OGhAQGy0lHyUtLSstKy0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS03K//AABEIAOEA4QMBIgACEQEDEQH/xAAcAAABBQEBAQAAAAAAAAAAAAAFAAIDBAYHAQj/xABPEAACAQIDBAcDBwcKBQIHAAABAhEAAwQSIQUxQVEGEyJhcYGRBzKhFCNCUrHB0RVTVGJykvAWFzOCk6Kz0+HxJGOy0uJDwiU1VXN0g6P/xAAaAQACAwEBAAAAAAAAAAAAAAACBAEDBQAG/8QAJxEAAgIBBAIDAAIDAQAAAAAAAAECEQMEEiExE0EiMlFhcQUUgTP/2gAMAwEAAhEDEQA/
                                      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
                                      PYN25WGx3vGvaVXQ6Al2VqdbpUqOX1O9hGlSpVQWn//Z") ,width ="300px" ))),
                                      column(width = 2,
                                      div(style="display:inline-block;horizontal-align:right;",
                                      actionButton("rap",label = img(src="https://encrypted-tbn0.gstatic.com/images?q=tbn%3AANd9GcQKayUHVseI6CElc3WMhAGC7hy7kJJzDo5mqMldN5xHaSNVdmIj"),width = "300px"))),
                                      tags$br(),
                                      tags$br(),
                                      tags$br(),
                                      tags$br(),
                                      #bottom
                                      column(width = 5,
                                      div(style="display:inline-block;horizontal-align:left;",
                                      actionButton("pop",label = img(src="https://encrypted-tbn0.gstatic.com/images?q=tbn%3AANd9GcSMpK4QUrSDtRye6iR-5xUOTSnQhuIFG4KCLKMtE0ETHzwfpuJL"),width = "300px"))),
                                      column(width =5 ,
                                      div(style="display:inline-block;horizontal-align:center;", 
                                      actionButton("alternative",label =img(src="data:image/jpeg;base64,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
                                      /+x/KqPcfpbf9hr+JoUKcjTj2f30Gav8A2T//ADkf+dVyhQpqOhj2BQoUKkuChQoUAChQoUAChQoUAf/Z") ,width ="300px" ))),
                                      column(width = 2,
                                      div(style="display:inline-block;horizontal-align:right;",
                                      actionButton("jazz",label = img(src="data:image/jpeg;base64,/9j/
                                      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"),width = "300px"))),
                                      
                                      ## Tables
                                      
                                      DT::dataTableOutput("mytable3"),
                                      ## actionButton("country",label ="country",width ="200px",height="30",align="center"),
                                      DT::dataTableOutput("country_table"),
                                      DT::dataTableOutput("jazz_table"),
                                      DT::dataTableOutput("rap_table"),
                                      DT::dataTableOutput("pop_table"),
                                      DT::dataTableOutput("alternative_table")),
                    ) # closes tabPanel,
                    
          ) # closes appendTab
          
          appendTab(inputId = "tabselected",
                    
                    tabPanel("Recommender",
                             value = 6,
                             sidebarPanel(
                               textInput("song", "Enter Song Name:", ""),
                               textInput("size", "Number of Songs in Playlist:", "30"),
                               tags$h5("Action Button:"),
                               actionBttn(
                                 inputId = "Listen",
                                 label = "Search",
                                 color = "primary",
                                 style = "jelly",
                                 size = "md")),
                             mainPanel(
                               h4("Suggested Playlist"),
                               DT::dataTableOutput("mytable9"))
                    ) # closes tabPanel,
                    
          ) # closes appendTab
          
          appendTab(inputId = "tabselected",
                    
                    tabPanel("Trending Artists and Songs",
                             value = 7,
                             fluidRow(
                               h2("Trending in 2018...", align = "center"),
                               box(
                                 column(12,
                                        h3("Top 10 Artists 2018", align = "center"),
                                        DT::dataTableOutput("mytable10"))),
                               box(
                                 column(12,
                                        h3("Top 10 Songs 2018", align = "center"),
                                        DT::dataTableOutput("mytable11"))))
                    ) # Close tabPanel
                    
          ) # closes appendTab
          
          removeTab(inputId = "tabselected",
                    target = "1")
          
        } else { # username correct, password wrong
          
          # specify if needed
          
        } # closes if-clause
        
      } else { # username name wrong 
        
        # specify if needed
        
      } # closes second if-clause
      
    }) # closes observeEvent 
    
## USER INFORMATION 
    
    ### Submission
    
    observeEvent(input$submit, {
    # open DB connection
    db <- dbConnector(
      server   = getOption("database_server"),
      database = getOption("database_name"),
      uid      = getOption("database_userid"),
      pwd      = getOption("database_password"),
      port     = getOption("database_port")
    )
    on.exit(dbDisconnect(db), add = TRUE)
    
    #browser()
    query <- paste("insert into User_Information ([Username],[Password],[First_Name],[Last_Name],[Date_Of_Birth],[Address],[City],[State],[ZipCode]) values('", input$user ,"' , '", input$pw,"' , '", input$Fname, "', '", input$Lname,"' , '", input$birth, "' , '", input$address, "' , '", input$city, "' , '", input$state, "' , '", input$zip,"');", sep="")
    print(query)
    # Submit the fetch query and disconnect
    dbGetQuery(db, query)
    
    })
    
    output$mytable = DT::renderDataTable({
      
      db <- dbConnector(
        server   = getOption("database_server"),
        database = getOption("database_name"),
        uid      = getOption("database_userid"),
        pwd      = getOption("database_password"),
        port     = getOption("database_port")
      )
      on.exit(dbDisconnect(db), add = TRUE)
      
      
      query <- paste("SELECT", "First_Name,Last_Name,Date_Of_Birth,Address,City,State,ZipCode", "FROM", "User_Information")
      
      data <- dbGetQuery(db, query)
      
      data
    })
    
    observeEvent(input$submit, {
      
      output$mytable = DT::renderDataTable({
        
        db <- dbConnector(
          server   = getOption("database_server"),
          database = getOption("database_name"),
          uid      = getOption("database_userid"),
          pwd      = getOption("database_password"),
          port     = getOption("database_port")
        )
        on.exit(dbDisconnect(db), add = TRUE)
        
        query <- paste("SELECT", "First_Name,Last_Name,Date_Of_Birth,Address,City,State,ZipCode", "FROM", "User_Information")
        
        data <- dbGetQuery(db, query)
        
        data
      })
      
    })
    
    ### Deletion
    
    observeEvent(input$delete, {
      db <- dbConnector(
        server   = getOption("database_server"),
        database = getOption("database_name"),
        uid      = getOption("database_userid"),
        pwd      = getOption("database_password"),
        port     = getOption("database_port")
      )
      on.exit(dbDisconnect(db), add = TRUE)
      
      query12 <- paste("DELETE FROM User_Information WHERE Username = '",input$ID ,"';", sep="")
      data12 <- dbGetQuery(db, query12) 
    })
    
    output$mytable = DT::renderDataTable({
      
      db <- dbConnector(
        server   = getOption("database_server"),
        database = getOption("database_name"),
        uid      = getOption("database_userid"),
        pwd      = getOption("database_password"),
        port     = getOption("database_port")
      )
      on.exit(dbDisconnect(db), add = TRUE)
      
      
      query <- paste("SELECT", "First_Name,Last_Name,Date_Of_Birth,Address,City,State,ZipCode", "FROM", "User_Information")
      
      data <- dbGetQuery(db, query)
      
      data
    })
    
    observeEvent(input$delete, {
      
      output$mytable = DT::renderDataTable({
        
        db <- dbConnector(
          server   = getOption("database_server"),
          database = getOption("database_name"),
          uid      = getOption("database_userid"),
          pwd      = getOption("database_password"),
          port     = getOption("database_port")
        )
        on.exit(dbDisconnect(db), add = TRUE)
        
        query <- paste("SELECT", "First_Name,Last_Name,Date_Of_Birth,Address,City,State,ZipCode", "FROM", "User_Information")
        
        data <- dbGetQuery(db, query)
        
        data
      })
      
    })
    
    ### Update
    observeEvent(input$update, {
      db <- dbConnector(
        server   = getOption("database_server"),
        database = getOption("database_name"),
        uid      = getOption("database_userid"),
        pwd      = getOption("database_password"),
        port     = getOption("database_port")
      )
      on.exit(dbDisconnect(db), add = TRUE)
      
      query13 <- paste("UPDATE User_Information SET First_Name = '",input$Fname ,"' WHERE Username = '",input$user ,"';", sep="")
      data13 <- dbGetQuery(db, query13) 
    })
    
    output$mytable = DT::renderDataTable({
      
      db <- dbConnector(
        server   = getOption("database_server"),
        database = getOption("database_name"),
        uid      = getOption("database_userid"),
        pwd      = getOption("database_password"),
        port     = getOption("database_port")
      )
      on.exit(dbDisconnect(db), add = TRUE)
      
      
      query <- paste("SELECT", "First_Name,Last_Name,Date_Of_Birth,Address,City,State,ZipCode", "FROM", "User_Information")
      
      data <- dbGetQuery(db, query)
      
      data
    })
    
    observeEvent(input$update, {
      
      output$mytable = DT::renderDataTable({
        
        db <- dbConnector(
          server   = getOption("database_server"),
          database = getOption("database_name"),
          uid      = getOption("database_userid"),
          pwd      = getOption("database_password"),
          port     = getOption("database_port")
        )
        on.exit(dbDisconnect(db), add = TRUE)
        
        query <- paste("SELECT", "First_Name,Last_Name,Date_Of_Birth,Address,City,State,ZipCode", "FROM", "User_Information")
        
        data <- dbGetQuery(db, query)
        
        data
      })
      
    })
    
    
    
## SEARCH 
    
    observeEvent(input$Go, {
      # open DB connection
      db <- dbConnector(
        server   = getOption("database_server"),
        database = getOption("database_name"),
        uid      = getOption("database_userid"),
        pwd      = getOption("database_password"),
        port     = getOption("database_port")
      )
      on.exit(dbDisconnect(db), add = TRUE)
      
      # browser()
      
      
      query2 <- paste("SELECT artist_name AS ARTIST, track_name AS SONG, genre as GENRE, valence as VALENCE, danceability as DANCEABILITY, energy as ENERGY FROM Song Where"," ", input$searchselect, " ", "LIKE", " '", input$searchtext, "%'", " ", "AND valence <", " ", input$valence," ", "AND danceability >"," ",  input$dance[1]," ","AND danceability <"," ", input$dance[2]," ", "AND energy >"," ",  input$energy[1]," ","AND energy <"," ", input$energy[2]," ","order by valence DESC, danceability DESC, energy DESC "," ", ";", sep="")
      
      print(query2)
      # Submit the fetch query and disconnect
      data2 <- dbGetQuery(db, query2)
      
      output$mytable2 = DT::renderDataTable({
        DT::datatable(data2, options = list(lengthMenu = c(5, 15, 25), pageLength = 15))
      })
    })
  
## GENRES

    # Rock
    observeEvent(input$rock, {
      # open DB connection
      db <- dbConnector(
        server   = getOption("database_server"),
        database = getOption("database_name"),
        uid      = getOption("database_userid"),
        pwd      = getOption("database_password"),
        port     = getOption("database_port")
      )
      on.exit(dbDisconnect(db), add = TRUE)
      
      # browser()
      query3 <- paste("select genre,track_name,artist_name from Song where genre='rock';")
      print(query3)
      # Submit the fetch query and disconnect
      data3 <- dbGetQuery(db, query3)
      output$mytable3 = DT::renderDataTable({
      data3
      })
    })
    
    # country
    observeEvent(input$country, {
      # open DB connection
      db <- dbConnector(
        server   = getOption("database_server"),
        database = getOption("database_name"),
        uid      = getOption("database_userid"),
        pwd      = getOption("database_password"),
        port     = getOption("database_port")
      )
      on.exit(dbDisconnect(db), add = TRUE)
      
      # browser()
      query4 <- paste("select genre,track_name,artist_name from Song where genre='country';")
      print(query4)
      # Submit the fetch query and disconnect
      data4 <- dbGetQuery(db, query4)
      output$country_table = DT::renderDataTable({
        data4
      })
    })
    #pop
    observeEvent(input$pop, {
      # open DB connection
      db <- dbConnector(
        server   = getOption("database_server"),
        database = getOption("database_name"),
        uid      = getOption("database_userid"),
        pwd      = getOption("database_password"),
        port     = getOption("database_port")
      )
      on.exit(dbDisconnect(db), add = TRUE)
      
      # browser()
      query5 <- paste("select genre,track_name,artist_name from Song where genre='pop';")
      print(query5)
      # Submit the fetch query and disconnect
      data5 <- dbGetQuery(db, query5)
      output$pop_table = DT::renderDataTable({
        data5
      })
    })
    
    
    #jazz
    
    observeEvent(input$jazz, {
      # open DB connection
      db <- dbConnector(
        server   = getOption("database_server"),
        database = getOption("database_name"),
        uid      = getOption("database_userid"),
        pwd      = getOption("database_password"),
        port     = getOption("database_port")
      )
      on.exit(dbDisconnect(db), add = TRUE)
      
      # browser()
      query6 <- paste("select genre,track_name,artist_name from Song where genre='jazz';")
      print(query6)
      # Submit the fetch query and disconnect
      data6 <- dbGetQuery(db, query6)
      output$jazz_table = DT::renderDataTable({
        data6
      })
    })
    
    
    #5 Alternative
    
    observeEvent(input$alternative, {
      # open DB connection
      db <- dbConnector(
        server   = getOption("database_server"),
        database = getOption("database_name"),
        uid      = getOption("database_userid"),
        pwd      = getOption("database_password"),
        port     = getOption("database_port")
      )
      on.exit(dbDisconnect(db), add = TRUE)
      
      # browser()
      query7 <- paste("select genre,track_name,artist_name from Song where genre='alternative';")
      print(query7)
      # Submit the fetch query and disconnect
      data7 <- dbGetQuery(db, query7)
      output$alternative_table = DT::renderDataTable({
        data7
      })
    })
    
    
    #rap
    
    observeEvent(input$rap, {
      # open DB connection
      db <- dbConnector(
        server   = getOption("database_server"),
        database = getOption("database_name"),
        uid      = getOption("database_userid"),
        pwd      = getOption("database_password"),
        port     = getOption("database_port")
      )
      on.exit(dbDisconnect(db), add = TRUE)
      
      # browser()
      query8 <- paste("select genre,track_name,artist_name from Song where genre='rap';")
      print(query8)
      # Submit the fetch query and disconnect
      data8 <- dbGetQuery(db, query8)
      output$rap_table = DT::renderDataTable({
        data8
      })
    })
    
    
    
    
## RECOMMENDER Tab
    
    observeEvent(input$Listen, {
      # open DB connection
      db <- dbConnector(
        server   = getOption("database_server"),
        database = getOption("database_name"),
        uid      = getOption("database_userid"),
        pwd      = getOption("database_password"),
        port     = getOption("database_port")
      )
      on.exit(dbDisconnect(db), add = TRUE)
      
      # browser()
      query9 <- paste("SELECT TOP ", input$size, " Song FROM similarity_matrix ORDER BY [", input$song, "];", sep = "")
      print(query9)
      # Submit the fetch query and disconnect
      data9 <- dbGetQuery(db, query9)
      output$mytable9 = DT::renderDataTable({
        DT::datatable(data9, options = list(lengthMenu = c(5, 15, 25), pageLength = 15))
      })
    })


## TRENDING ARTISTS AND SONGS
    
     db <- dbConnector(
      server   = getOption("database_server"),
      database = getOption("database_name"),
      uid      = getOption("database_userid"),
      pwd      = getOption("database_password"),
      port     = getOption("database_port")
    )
    on.exit(dbDisconnect(db), add = TRUE)
    
    ## Table 1
    query10 <- paste("SELECT TOP 10 Artist_Name, Followers From Artist 
                   WHERE Followers >= 7
                   Order by Followers DESC")
    
    data10 <- dbGetQuery(db, query10)
    output$mytable10 = DT::renderDataTable({
    data10
    DT::datatable(data10, options = list(searching = FALSE, paging = FALSE))})
    
    ## Table 2
    query11 <- paste("SELECT distinct top 10  artist_name, track_name,popularity From Song
                   WHERE popularity >= 96
                   Order by popularity DESC;")
    
    data11 <- dbGetQuery(db, query11)
    
    output$mytable11 = DT::renderDataTable({
    data11
    DT::datatable(data11, options = list(searching = FALSE, paging = FALSE))})
    
  } # Closes server
shinyApp(ui, server)