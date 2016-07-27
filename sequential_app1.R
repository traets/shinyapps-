###
# shiny application to run sequential designs.
# After the presentation of the initial choice sets, parameters are,
# and new choice sets are generated.
###

#libraries
library(choice)
library(shinyjs)
library(choice)
library(mgcv)
library(plyr)
library(DT)

### settings choice task ######################################################################
answer_options<-c("None","Alt A","Alt B")
n_alts=2
levels<-c(2,3,2)

#level names
names<- vector(mode="list", length(levels))
names[[1]]<-c("$50","$100")
names[[2]]<-c("2min","15min","30min")
names[[3]]<-c("bad","good")

#alternatives names
alternatives<-c("Alternative  A","Alternative B")

#attribute names
attributes<-c("price","travel time","comfort")

#initial design
n_init<-10
n_total<-15
des<-design(lvls = levels, n_sets = n_init, n_alts = n_alts)
des_ok<-present(design = des, lvl_names = names, n_alts = n_alts)

#PRIOR
p_mod<- c(0, 0, 0, 0)
p_cov<- diag(c(3, 3, 3, 3))

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
      return( h3("After the instructions you will be presented with a number of choice tasks. 
                 Read the specifications of each alternative carefully, afterwards indicate which alternative you prefer.
                 Continue by clicking the 'OK' button."))

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
         actionButton('Save', 'click to end the survey'),
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

#produce choice set
 select_set <-eventReactive(input$OK, {

  if (input$OK > 1){
  resp<<-c(resp, input$survey)
  y_bin<<-map_resp(resp = resp, resp_options = answer_options, n_alts = n_alts)
  }

  if (input$OK > 0 & input$OK <= n_init){

    #select choice set.
    choice.set<-des_ok[des_ok$set==input$OK, !(names(des_ok) %in% drops)]
    choice.set<-t(choice.set)

  }
  #sequential adaptive
  else if (input$OK-1 >= n_init & input$OK <= n_total){

  #update parameters
  draws<-imp_sampling(prior_mode = p_mod, prior_covar = p_cov, design = des, n_alts = n_alts, Y=y_bin)
  w<<-draws[[2]]
  sam<<-draws[[1]]

  #new set based on updated parameters
  new_set<-DB_seq_fed(design = des, lvls = levels, n_alts = n_alts, par_samples = sam, weights = w,
                              prior_covar = p_cov)

  #update
  des<<-rbind(des, new_set)
  p_mod<<-modes(samples = sam, weights = w, s=15)

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

}

shinyApp(ui, server)
