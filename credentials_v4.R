
options(database_name = "ITOM6265_F19_Project_group23")
options(database_userid = "ITOM6265_F2019")
options(database_password = "str0ng!shPassw0rd")

#############################################################
#You don't need to modify any code after this line
#############################################################
# Add the database credentials here.
#Use classroomdb.smu.edu when deploying to shinyapps.io
options(database_server = "classroomdb.smu.edu")
#Use sacad2pdb1.systems.smu.edu 
#options(database_server = "sacad2pdb1.systems.smu.edu")
options(database_port = 55433)

# database connector (edit at your own risk!)

dbConnector <- function(server, database, uid, pwd, port){
  
  ### run app locally
  if(Sys.getenv('SHINY_PORT') == ""){
    os <- Sys.info()['sysname']
    cat("You are running the app locally on", os, "\n")
    cat("You need to be in the SMU network.\n")
    
    if(os == "Linux") {
      cat("Using the FreeTDS database driver. You may need to install the driver.\n")
      DBI::dbConnect(odbc::odbc(),
                     Driver   = "FreeTDS",
                     Database = database,
                     Uid      = uid,
                     Pwd      = pwd,
                     Server   = server,
                     Port     = port,
                     TDS_Version="7.2"
      )
      
    } else if(os == "Windows") {
      DBI::dbConnect(odbc::odbc(), 
                     driver = "ODBC Driver 17 for SQL Server",
                     server = "sacad2pdb1.systems.smu.edu", 
                     database = database, 
                     uid = uid, 
                     pwd = pwd,
                     port = port
      )
    
    } else if(os == "Darwin") {
      DBI::dbConnect(odbc::odbc(), 
                     driver = "ODBC Driver 17 for SQL Server",
                     server = "sacad2pdb1.systems.smu.edu", 
                     database = database, 
                     uid = uid, 
                     pwd = pwd,
                     port = port
      )
      
    }else {
      cat("Unsupported OS. Please install Windows.")
      
    }
    
    ### run on Shinyapps.io (Uses FreeTDS)
  }else{
    DBI::dbConnect(odbc::odbc(),
                   Driver   = "FreeTDS",
                   Database = database,
                   Uid      = uid,
                   Pwd      = pwd,
                   Server   = server,
                   Port     = port,
                   TDS_Version="7.2"
    )
  }
}


#Test if the connection works properly
# dbConnector(
#   server   = getOption("database_server"),
#   database = getOption("database_name"),
#   uid      = getOption("database_userid"),
#   pwd      = getOption("database_password"),
#   port     = getOption("database_port")
# )