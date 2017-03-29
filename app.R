###
# Basic shiny application to run sequential adaptive designs using the KL criterion
# Results can be remotely saved on dropbox.
#
#
# To run locally: 1 Make sure all the packages under "libraries" are installed.
#                 2 Save script as app.R
#                 3 Change settings section if desired.
#                 4 Press "Run App" top right.
#
# To store the data on dropbox (design matrix + response vector)
#                1 Connect this app with your dropbox account (see package rdrop2).
#                2 Specify the dropbox map in outputDir.
#
# To deploy app to free shiny.io server:
#                1 Create shiny account.
#                2 Connect this app to account (see https://shiny.rstudio.com/deploy/).
#                3 Deploy app (using code below)
###

#clear environment
rm(list=ls())

#libraries
library(IASB)
library(shinyjs)
library(mgcv)
library(plyr)
library(DT)
library(xlsx)
library(rdrop2)

### settings DCE ######################################################################
answer_options<-c("None","Alt A","Alt B")
n_alts=2
levels<-c(3,3,3)

#dropbox map to store data
outputDir <- "ICMC"

#level names
names<- vector(mode="list", length(levels))
names[[1]]<-c("$50","$75","$100")
names[[2]]<-c("2min","15min","30min")
names[[3]]<-c("bad","average","good")

#alternatives names
alternatives<-c("Alternative  A","Alternative B")

#attribute names
attributes<-c("price","travel time","comfort")

#coding used
code<-("contr.treatment") #dummy coding (see ?contrasts for more options)

#PRIOR
p_mean<- c(-1, -1, -1, -1, 1, 1)
var<-3
p_cov<- diag(rep(var, length(p_mean)))

#design
n_total<-10

#introduction text
intro_text<- c("After the instructions you will be presented with a number of choice tasks.
               Read the specifications of each alternative carefully, afterwards indicate which alternative you prefer.
               Continue by clicking the 'OK' button. Before continuing please fill in your student ID below.")

#end text
end_txt<-c("Thanks for taking the survey!")

#title
title<-c(" Title of survey ")

###############################################################################################

#initialise
profs<-profiles(lvls = levels, coding = code)[[2]]    #all possible profiles
combs<- full_sets(lvls = levels, n_alts= n_alts, coding = code) #all possible profile combinations

jscode <- "shinyjs.closeWindow = function() { window.close(); }" #close application after closing window

### user interface
ui <- fluidPage(

  useShinyjs(),
  extendShinyjs(text = jscode, functions = c("closeWindow")),
  column(12,

         headerPanel(title),

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
  n_att<-length(levels)
  drops <- c("set","alt")
  n <- 0
  makeReactiveBinding('n')

  reac_vars = reactiveValues(des = matrix(),
                             y_bin=vector("numeric"),
                             sam=matrix(data = NA, ncol = length(p_mean)),
                             w= vector("numeric"),
                             resp=vector("character"),
                             choice.set=matrix())

  #dynamic userinterface
  output$MainAction <- renderUI({ dynamicUi() })
  dynamicUi <- reactive({

    #1 explanation.
    if (input$OK == 0 )
      return(list(h3(intro_text), textInput("ID", "student ID:")))

    #2 DCE
    if (input$OK > 0 & input$OK <= n_total)
      return(list(
        radioButtons("survey", "Please select the option you prefer:",
                     answer_options , inline = T, selected = "None")))

    #3 Endnote
    if (input$OK > n_total)
      return(list( h4(end_txt), actionButton('Save', 'Save and Quit'), br()))

  })

  #actionbutton
  observeEvent(input$OK, {
    if(n == n_total){ hide('OK') }
    n <<- n + 1
  })

  #close app
  observeEvent(input$Save, {
    saveData(d = reac_vars$des, Y= reac_vars$y_bin )
    js$closeWindow()
  })

  #store responses
  observeEvent(input$OK, {
    if (input$OK > 1){
      reac_vars$resp<-c(reac_vars$resp, input$survey)
      reac_vars$y_bin<-map_resp(resp = reac_vars$resp, resp_options = answer_options, n_alts = n_alts)
    }
  })

  ###Select choice set based on KL criterion###
  select_set <-eventReactive(input$OK, {

    # during DCE fase
    if (input$OK > 0 & input$OK <= n_total){

      #first set (no responses observed)
      if (input$OK < 2){

        #draw samples from prior
        reac_vars$sam<-lattice_mvn(mean=p_mean, cvar = p_cov, m=10)
        reac_vars$w<-rep(1,nrow(reac_vars$sam))/nrow(reac_vars$sam)

        #select new set based on kl info
        reac_vars$des<-KL_info(fp= profs, fcomb= combs, par_samples = reac_vars$sam, weights = reac_vars$w, n_alts = n_alts, print = FALSE )

        #transform coded set to attribute level set.
        reac_vars$choice.set<-present(set = reac_vars$des, lvl_names = names, coding = code)
        reac_vars$choice.set<-t(reac_vars$choice.set[, 1:n_att])
      }

      #after observing responses
      else{
        #update parameters
        samples<-imp_sampling(prior_mean = p_mean, prior_covar = p_cov, des = reac_vars$des, n_alts = n_alts, m=10, Y=reac_vars$y_bin)
        reac_vars$sam<-samples[[1]]
        reac_vars$w<-samples[[2]]

        #select new set based on KL info
        new_set<-KL_info(fp= profs, fcomb= combs, par_samples = reac_vars$sam, weights = reac_vars$w, n_alts = n_alts,print = FALSE )

        #update the full design
        reac_vars$des<-rbind(reac_vars$des, new_set)

        #transform coded set to attribute level set.
        reac_vars$choice.set<-present(set = new_set, lvl_names = names, coding = code)
        reac_vars$choice.set<-t(reac_vars$choice.set[, 1:n_att])

      }

      #Fill in attribute names and alternatives names
      colnames(reac_vars$choice.set) <- alternatives
      rownames(reac_vars$choice.set) <- attributes

      #return choice set
      return(reac_vars$choice.set)

    }

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

    if (input$OK > 0 & input$OK <= n_total){paste(h5("set: ", input$OK,"/",n_total))}
  })

  #Save data
  saveData <- function(d, Y) {

    #combines design and responses in same file
    data<-cbind(d, Y)

    # Create a unique file name
    fileName <- sprintf("%s_%s.txt", n_alts, input$ID)

    # Write the data to a temporary file locally
    filePath <- file.path(tempdir(), fileName)
    write.table(data, filePath, quote=FALSE, sep=" ", row.names = FALSE)

    # Upload the file to Dropbox
    drop_upload(filePath, dest = outputDir)
  }

}

shinyApp(ui, server)

#Uncomment to deploy this app to a shiny.io server
#require('devtools')
#rsconnect::setAccountInfo(name='KEY')
#deployApp()











