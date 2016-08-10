###
# shiny application to run sequential designs.
# After the presentation of the initial design, parameters are updated,
# and new choice sets are generated.
# Results are saved on dropbox account.
###

rm(list=ls())

#libraries
library(choice)
library(shinyjs)
library(mgcv)
library(plyr)
library(DT)
library(xlsx)
library(rdrop2)

### settings choice task ######################################################################
answer_options<-c("None","Alt A","Alt B")
n_alts=2
levels<-c(3,3,3)
outputDir <- "responses"

#level names
names<- vector(mode="list", length(levels))
names[[1]]<-c("$50","$75","$100")
names[[2]]<-c("2min","15min","30min")
names[[3]]<-c("bad","average","good")

#alternatives names
alternatives<-c("Alternative  A","Alternative B")

#attribute names
attributes<-c("price","travel time","comfort")

#PRIOR
p_mod<- c(-1, -1, -1, -1, 1, 1)
p_cov<- diag(c(3, 3, 3, 3, 3, 3))

#design
n_init<-5 #initial
n_total<-5 #total

### initial design (choose)

#random
des<-design.gen(lvls = levels, n_sets = n_init, n_alts = n_alts) #random

#DB efficient
#p_samples<-MASS::mvrnorm(n = 200, mu= p_mod, Sigma=p_cov)
#des_db<-DB_mod_fed(lvls = levels, n_sets = n_init, n_alts= n_alts, par_samples = p_samples, max_iter = 5)
#des<-des_db[[1]]

#read in
#des<-as.matrix(read.xlsx(file = "C:/Users/u0105757/Desktop/sequential design/R.code/designs/des_8(2)3.3.3.xlsx", 1))

#transform
des_ok<-present(design = des, lvl_names = names, n_alts = n_alts)

###############################################################################################


### user interface
ui <- fluidPage(

  useShinyjs(),
  column(12,

         headerPanel("Survey 2.0"),

         mainPanel(align = "center",

                   uiOutput("setnr"),
                   DT::dataTableOutput('choice.set'),
                   uiOutput("MainAction"),
                   actionButton("OK", "OK")

         ))
)


### server
server<-function(input, output) {

#initialize
 resp<-character()
 choice.set<-matrix()
 n_att<-length(levels)
 drops <- c("set","alt")
 n <- 0
 makeReactiveBinding('n')

#dynamic userinterface
 output$MainAction <- renderUI({ dynamicUi() })
 dynamicUi <- reactive({

    #1 explanation.
    if (input$OK == 0 )
    return(
        list(h3("After the instructions you will be presented with a number of choice tasks.
                 Read the specifications of each alternative carefully, afterwards indicate which alternative you prefer.
                 Continue by clicking the 'OK' button. Before continuing please fill in your student ID below."),
             textInput("ID", "student ID:")))
    
    
    #2 Survey
    if (input$OK > 0 & input$OK <= n_total)
      return(list(
        radioButtons("survey", "Please select the option you prefer:",
                     answer_options , inline = T, selected = "None")
      ))

    #3 Endnote
    if (input$OK > n_total)
       return(list(
         h4("Thanks for taking the survey!"),
         actionButton('Save', 'Save and Quit'),
         br()))

  })

#actionbutton
 observeEvent(input$OK, {

    if(n == n_total){
      hide('OK')
    }
    n <<- n + 1
  })

#close app
 observeEvent(input$Save, {
    if (input$Save > 0) stopApp() })

#store responses
 observeEvent(input$OK, {
   if (input$OK > 1){
     resp<<-c(resp, input$survey)
     y_bin<<-map_resp(resp = resp, resp_options = answer_options, n_alts = n_alts)
   }
   
   
 })
 
#produce choice set
 select_set <-eventReactive(input$OK, {

  #initial fase
  if (input$OK > 0 & input$OK <= n_init){

    #select choice set.
    choice.set<-des_ok[des_ok$set==input$OK, !(names(des_ok) %in% drops)]
    choice.set<-t(choice.set)

  }

  #sequential adaptive fase
  else if (input$OK-1 >= n_init & input$OK <= n_total){

  #update parameters
  samples<-imp_sampling(prior_mode = p_mod, prior_covar = p_cov, design = des, n_alts = n_alts, Y=y_bin)
  sam<<-samples[[1]]
  w<<-samples[[2]]
  post_mode<<-samples[[3]]
  post_covar<<-samples[[4]]
  
  #new set based on updated parameters
  new_set<-DB_seq_fed(design = des, lvls = levels, n_alts = n_alts, par_samples = sam, weights = w,
                              prior_covar = p_cov)

  #update
  des<<-rbind(des, new_set)

  choice.set<<-present(design = new_set, lvl_names = names, n_alts = n_alts)
  choice.set<<-t(choice.set[, 1:n_att])


  }
  
  else{}

  #Fill in attribute names and alternatives names
  colnames(choice.set) <- alternatives
  rownames(choice.set) <- attributes

  return(choice.set)
  
  })

#plot choice set
 output$choice.set<- DT::renderDataTable({

   if (input$OK > 0 & input$OK <= n_total){

    set<-select_set()
    datatable(set, filter = 'none', selection="multiple", escape=FALSE,
                                           options = list(sDom  = '<"top"><"bottom">'))}
    })

#plot setnr
 output$setnr <- renderText({

   if (input$OK > 0 & input$OK <= n_total){paste(h5("set: ", input$OK))}
 })
 
#Save data
 saveData <- function(d, Y) {

   data<-cbind(d, Y)
   # Create a unique file name
   fileName <- sprintf("%s_%s.csv", n_alts, input$ID)
   
   # Write the data to a temporary file locally
   filePath <- file.path(tempdir(), fileName)
   write.csv(data, filePath, row.names = FALSE, quote = TRUE)
   
   # Upload the file to Dropbox
   drop_upload(filePath, dest = outputDir)
 }
 observeEvent(input$Save, {
   saveData(d = des, Y= y_bin )

 })

}


shinyApp(ui, server)
