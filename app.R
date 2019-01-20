## requirements!!
TZ = "America/New_York"
Sys.setenv(TZ="America/New_York")

if (!require("pacman")) install.packages("pacman")
pacman::p_load(shiny, 
               DT,
               rlang,
               shinyjs,
               shinythemes,
               tidyverse,
               lubridate,
               scales,
               viridis,
               tidytext,
               sentimentr,
               tm,
               topicmodels,
               plotly)

#upload the data and update dataset
source("./NewDataSet.R", local=TRUE)
source("./Plots.R", local = TRUE)
source("./TopicModelling.R", local = TRUE)

require(shiny)
require(DT)
require(rlang)
require(shinyjs)
require(shinythemes)
require(tidyverse)
require(lubridate)
require(scales)
require(viridis)
require(tidytext)
require(sentimentr)
require(tm)
require(topicmodels)
require(plotly)

# Define UI
ui=fluidPage(
  theme = shinytheme("flatly"),
  tags$head(
    tags$style(HTML( #move two tabs over a little
      ".tabbable ul li:nth-child(4) { float: right; }
      .tabbable ul li:nth-child(5) { float: right; }"
    ))
  ),
  useShinyjs(),
  
  # Application title
  titlePanel("PC Dashboard"),
  
  #set up the layout
  sidebarLayout(
    
    #sidebar
    position = "right",
    sidebarPanel = sidebarPanel(width = 3,
                                fluidRow(column(
                                  textOutput("error1"),
                                  width = 10
                                )),
                                fluidRow(column(
                                  dateRangeInput(inputId = "Range",
                                                 label = "Analyze Calls Within Date:"), 
                                  align = "center", width = 12)),
                                fluidRow(
                                  column(
                                    actionButton(inputId = "UpdateRange","Update range!"), 
                                    width = 6,
                                    align = "left"),
                                  column(
                                    actionButton(inputId = "DefaultRange","Use full range!"), 
                                    width = 6,
                                    align = "right")
                                  ),
                                #conditional visualiser options
                                sliderInput("ntopics", "Number of Topics to Extract:",
                                            min = 2, max = 10, value = 3, round = T),
                                radioButtons("counselorCalls", "View Calls By Role:",
                                            choices = c("Primary" = "primary",
                                                        "Secondary" = "secondary",
                                                        "Both" = "both"),
                                            selected = "both"),
                                hr(),
                                #conditional text statistics
                                htmlOutput("stat1"),
                                htmlOutput("stat2"),
                                htmlOutput("stat3"),
                                htmlOutput("stat4"),
                                htmlOutput("stat5")
                                ),
    
    
    
    
    
    #main window
    mainPanel = mainPanel(
      
      #tabset layout
      tabsetPanel(id = "tabset", selected = 4, type = "pills",
          
          
          #activity view
          tabPanel("Activity View", value = 1, icon = icon("phone-square"),
                   fluidRow(column(width=12, wellPanel(plotOutput("traffic", height = "200px")))), #call traffic over time
                   hr(),
                   wellPanel(
                   fluidRow(
                     column(plotOutput("PerQuarter", height = "200px"), width = 4), #call freq per quarter
                     column(plotOutput("PerDay", height = "200px"), width = 4), #call freq per DoW
                     column(plotOutput("PerHour", height = "200px"), width = 4) #call freq per hour of night
                     ))
                   ),
          
          
          
          #counselor view
          tabPanel("Counselor View", value = 2, icon = icon("user"),
                   wellPanel(
                     splitLayout(
                       plotOutput("Calls", height = "600px"),
                       plotOutput("Skills", height = "600px")
                       )
                       )
                     ),
          
          
          #topics view
          tabPanel("Topics View", value = 3, icon = icon("comments"),
                   wellPanel(splitLayout(
                     plotOutput("Topics", height = "600px"),
                     plotOutput("Issues", height = "600px")
                     )
                     )
                   ),
          
          # data loader
          tabPanel("Data", value = 4, icon = icon("upload"),
                   fluidRow(column(helpText("Welcome to PC Dashboard! With this tool, you can run analyses on 
                                              Peer Counseling Helpline data to investigate
                                              call trends, counselor stats, and call topics."), 
                            width = 10,
                            align = "left")),
                   fluidRow(column(fileInput(inputId = "file_in", label = "Choose a Call Log File to Get Started", accept = c(".csv")),
                                   width=12,
                                   align="center")),
                   fluidRow(column(actionButton(inputId = "go", label="Go!"),
                                   width=12,
                                   align="center")),
                   hr(),
                   fluidRow(column(helpText("Then, click the Go button to go to the 
                                            analyses, or upload a different dataset by clicking Browse. 
                                            You can also change the range of data to be analyzed
                                            on the right, or view all of the raw data below."),
                                   width=6)),
                   fluidRow(column(dataTableOutput(outputId = "preview"),
                                   width=12,
                                   align = "center"))
          ),
          
          # help page
          tabPanel("Help", value = "help", icon = icon("info"),
                   fluidRow(
                     column(
                       tags$div(
                         class="header", 
                          tags$p(HTML(
"<h3>Thanks for using <br><strong>PC Dashboard!</strong></h3> This program was built for the <a href='http://drexel.edu/counselingandhealth/counseling-center/peer-counseling/'>Drexel University Peer Counseling Helpline</a>.
It was designed with the purpose of making managing the Helpline easier 
and more engaging by providing the board with <strong>data-driven statistics</strong> 
and <strong>Key Performance Indicators (KPI's)</strong> to support decision making.
<p>This program was built by Tinashe Michael Tapera, an alumnus and former board member on the Helpline. 
<a href='https://dataintensive.quora.com/Data-Dive-The-Peer-Counseling-Helpline-Gmail-Archive'>Click Here</a> to see the work that inspired this project."
                                      )
                              )
                       ), 
                     width = 4),
                     
                     column(
                       tags$div(
                         class="header",
                          tags$p(HTML(
"<h3>How To Use PC Dashboard:</h3>
<ol>
  <li> Load a dataset of call logs using the <strong>Data</strong> tab above. This dataset should be an Excel file exported from our Qualtrics call log system.</li>
  <li> Choose a view from the options up top to view the analytics results.</li>
  <li> Manipulate the date range with the widget in the sidebar to run analyses over different time frames.</li>
  <li> The sidebar also has other analysis widgets that you can manipulate too.</li>
</ol>

<p>The <strong>Activity View</strong> gives a general visualisation and analysis of our call activity over the time range. Use this to understand <i>when</i> our phone is ringing.</p>
<p>Use the <strong>Counselor View</strong> to get analysis of counselors who have taken calls and how they use their skills; this tells us <i>who</i> is answering the phone and how they are handling calls.</p>
<p>Finally, the <strong>Topics View</strong> gives an in-depth analysis of the issues counselors identified, and compares that with the words they use in their call logs. 
This is done through a machine learning technique called <a href='https://en.wikipedia.org/wiki/Topic_model'>Topic Modeling</a>, and can be used to find abstract topics in written documents 
<i>(Hint: We could use this to find words in the topic model that match, or don't match, our tagged call issues)</i>. Additionally, if counselors have given feedback about their calls, 
we use another machine learning technique called <a href='https://en.wikipedia.org/wiki/Sentiment_analysis'>Sentiment Analysis</a> to evaluate how positive or negative their language is across all the feedback.</p>"
                                      )
                              )
                        ), 
                      width = 8))
          )
      )
    )
  ),
hr(),
tags$div(class = "footer",
    tags$p(HTML(
      "<p style='text-align:center; font-size:70%; color:inherit'> Tinashe Michael Tapera | PC Dashboard 2018</p>"
    ))
    )
)


# Define server logic------
server=shinyServer(function(input, output, session) {
  
  values = reactiveValues(df = NULL, subset = NULL)
  
  observeEvent(input$file_in, {
    
    values$df = Load_Data(input$file_in$datapath)
    if(!is.null(values$df)) {
      values$subset = Load_Data(input$file_in$datapath)
      output$error1 = renderText("")
    }
    else{
      output$error1 = renderText("This doesn't look like a Qualtrics CSV file... Please try again.")
    }
  })
  
  #update the range of data if this is punched
  observeEvent(input$UpdateRange, {
    values$subset = values$df%>%
      filter(call_start > input$Range[1] & call_end < input$Range[2])
    if(nrow(values$subset) < 1){
      output$error1 = renderText("It looks like there's no data here. You may want to reset the date range.")
    }else{
      output$error1 = renderText("")
    }
  })
  
  #update the range of data if this is punched
  observeEvent(input$DefaultRange, {
    values$subset = values$df
    output$error1 = renderText("")
  })
  
  
  #following UI goes live only when file is successfully uploaded
  observe({
    toggle(id="go", condition=!is.null(values$df), anim=TRUE, animType = "fade", time = 0.8)
    toggle(id="Range", condition=!is.null(values$df), anim=TRUE, animType = "fade", time = 0.8)
    toggle(id="UpdateRange", condition=!is.null(values$df), anim=TRUE, animType = "fade", time = 0.8)
    toggle(id="DefaultRange", condition=!is.null(values$df), anim=TRUE, animType = "fade", time = 0.8)
    toggle(id="preview", condition=!is.null(values$df), anim=TRUE, animType = "fade", time = 0.8)
    
    
    toggle(id="stat1", condition=!is.null(values$df), anim=TRUE, animType = "fade", time = 0.8)
    toggle(id="stat2", condition=!is.null(values$df), anim=TRUE, animType = "fade", time = 0.8)
    toggle(id="stat3", condition=!is.null(values$df), anim=TRUE, animType = "fade", time = 0.8)
    toggle(id="stat4", condition=!is.null(values$df), anim=TRUE, animType = "fade", time = 0.8)
    toggle(id="stat5", condition=!is.null(values$df), anim=TRUE, animType = "fade", time = 0.8)
    
    toggle(id="traffic", condition=!is.null(values$df), anim=TRUE, animType = "fade", time = 0.8)
    toggle(id="PerQuarter", condition=!is.null(values$df), anim=TRUE, animType = "fade", time = 0.8)
    toggle(id="PerDay", condition=!is.null(values$df), anim=TRUE, animType = "fade", time = 0.8)
    toggle(id="PerHour", condition=!is.null(values$df), anim=TRUE, animType = "fade", time = 0.8)
    toggle(id="Calls", condition=!is.null(values$df), anim=TRUE, animType = "fade", time = 0.8)
    toggle(id="Skills", condition=!is.null(values$df), anim=TRUE, animType = "fade", time = 0.8)
    toggle(id="Topics", condition=!is.null(values$df), anim=TRUE, animType = "fade", time = 0.8)
    toggle(id="Issues", condition=!is.null(values$df), anim=TRUE, animType = "fade", time = 0.8)

  })
  
  #change to activity when go button goes live
  observeEvent(input$go, {
    updateTabsetPanel(session, "tabset", selected = "1")
  })
  
  #show head of table
  output$preview = renderDataTable({
    
    if(!is.null(values$df)){
      values$df%>%
        mutate(call_start = format(call_start, format="%B %d %Y, %H:%M"),
               call_end = format(call_end, format="%B %d %Y, %H:%M"))%>%
        DT::datatable(.,options = )
    }
    
  })
  
  
  #sidebar updated by user
  observeEvent(values$subset, {
    
    #update the size of the subset in the widget
    updateDateRangeInput(session, "Range",
    start = getMin(values$subset),
    end = getMax(values$subset))
  })
  
  #nested ifelse for which stats to show on the sidebar
  observeEvent(input$tabset, {
    
    if(input$tabset=="1"){
      
      output$stat1= renderUI({
        
        stat = values$subset%>%
          filter(answer == "Yes")%>%
          nrow()%>%
          as.character()
        paste0("Total Number of Calls: <strong>", stat, "</strong>")%>%
          HTML()
      })
      
      output$stat2 = renderUI({
        stat = values$subset%>%
          group_by(ask_student)%>%
          summarise(n = n())%>%
          mutate(freq = (n / sum(n))*100)%>%
          filter(ask_student == "Yes")%>%
          select(freq)%>%
          round(2)%>%
          as.character()
        paste0("Drexel Students: <strong>", stat, "%</strong>")%>%
          HTML()
      })
      
      output$stat3 = renderUI({
        stat = values$subset%>%
          group_by(repeat.)%>%
          summarise(n = n())%>%
          mutate(freq = (n / sum(n))*100)%>%
          filter(repeat. == "Yes")%>%
          select(freq)%>%
          round(2)%>%
          as.character()
        paste0("Repeat Callers: <strong>", stat, "%</strong>")%>%
          HTML()
      })
      
      output$stat4 = renderUI({
        stat = values$subset%>%
          select(one_of(c("call_start", "call_end")))%>%
          transmute(duration = call_end-call_start)%>%
          top_n(1)%>%
          as.character()
        paste0("Longest Call: <strong>", stat, " mins</strong>")%>%
          HTML()
      })
      
      output$stat5 = renderUI({
        stat = values$subset%>%
          filter(ask_student == "Yes")%>%
          select(one_of(c("call_start", "call_end")))%>%
          transmute(duration = call_end-call_start)%>%
          transmute(mean_duration = round(mean(duration),2))%>%
          slice(1)%>%
          as.character()
        paste0("Average Call: <strong>", stat, " mins</strong>")%>%
          HTML()
      })
    }
    else if(input$tabset=="2"){
      output$stat1= renderUI({
       stat = values$subset%>%
          select(one_of(c("primary", "secondary")))%>%
          gather()%>%
          select(value)%>%
          unique()%>%
          nrow()%>%
          as.character()
       paste0("Total Number of Counselors on Call: <strong>",stat,"</strong>")%>%
         HTML()
      })
      
      output$stat2 = renderUI({
        stat = values$subset%>%
          select(one_of(c("primary", "secondary", "call_start", "call_end")))%>%
          mutate(duration = call_end-call_start)%>%
          top_n(n = 1,duration)%>%
          select(one_of(c("primary", "secondary")))%>%
          transmute(pair = paste(as.character(primary), as.character(secondary), sep = " & "))%>%
          as.character()
        paste0("Longest Call: <strong>", stat,"</strong>")%>%
          HTML()
      })
      
      output$stat3 = renderUI({
        stat = values$subset%>%
          select(starts_with("referrals"))%>%
          gather(key = "Referral", value = "value", factor_key = TRUE)%>%
          mutate(Referral = gsub("referrals\\.", "", .$Referral))%>%
          filter(value != 0)%>%
          group_by(Referral)%>%
          count(value)%>%
          arrange(desc(n))%>%
          select(Referral)%>%
          ungroup()%>%
          slice(1:3)%>%
          c()
        paste0("Top Referrals:<strong>
                <ol>
                  <li>",gsub("_", " ", stat$Referral[1])," </li>
                  <li>",gsub("_", " ", stat$Referral[2])," </li>
                  <li>",gsub("_", " ", stat$Referral[3])," </li>
                </ol>
                </strong>
               ")%>%
          HTML()
      })
      
      output$stat4 = renderUI({
        return("")
      })
      
      output$stat5 = renderUI({
        return("")
      })
    }
    else if((input$tabset=="3")){
      output$stat1= renderUI({
        
        if(all(values$subset$couns_opinion == "")){
          return("No feedback to analyze!")
        }
        stat = values$subset%>%
          mutate(ave_sentiment = GetSentiment(.$couns_opinion)$ave_sentiment)%>%
          filter(!is.na(couns_opinion))%>%
          select(ave_sentiment)%>%
          summarise(ave = mean(ave_sentiment))%>%
          round(2)%>%
          as.character()
        paste0("Average Counselor Feedback Sentiment Score: <strong>",stat,"</strong>")%>%
          HTML()
      })
      
      output$stat2 = renderUI({
        if(all(values$subset$couns_opinion == "")){
          return("")
        }
        stat = values$subset%>%
          mutate(ave_sentiment = GetSentiment(.$couns_opinion)$ave_sentiment)%>%
          filter(!is.na(couns_opinion))%>%
          arrange(desc(ave_sentiment))%>%
          select(one_of(c("call_start", "ave_sentiment")))
        paste0("Most Positive Counselor Feedback: <strong>", format(stat$call_start[1],format="%B %d %Y, %H:%M")," (", round(stat$ave_sentiment[1], 2), ")</strong>")%>%
          HTML()
      })
      
      output$stat3 = renderUI({
        if(all(values$subset$couns_opinion == "")){
          return("")
        }
        stat = values$subset%>%
          mutate(ave_sentiment = GetSentiment(.$couns_opinion)$ave_sentiment)%>%
          filter(!is.na(couns_opinion))%>%
          arrange(ave_sentiment)%>%
          select(one_of(c("call_start", "ave_sentiment")))
        paste0("Most Negative Counselor Feedback: <strong>", format(stat$call_start[1],format="%B %d %Y, %H:%M")," (", round(stat$ave_sentiment[1], 2), ")</strong>")%>%
          HTML()
      })
      
      output$stat4 = renderUI({
        
        stat = values$subset%>%
          filter(suicidal_thoughts == "Yes")%>%
          nrow()%>%
          as.character()
        paste0("Calls With Suicidal Ideation: <strong>", stat, "</strong>")%>%
          HTML()
        
      })
      
      output$stat5 = renderUI({
        stat = values$subset%>%
          select(starts_with("marketing"))%>%
          gather(key = "Marketing", value = "value", factor_key = TRUE)%>%
          mutate(Marketing = gsub("marketing\\.", "", .$Marketing))%>%
          filter(value != 0 & Marketing != "Did_not_ask/unknown")%>%
          group_by(Marketing)%>%
          count(value)%>%
          arrange(desc(n))%>%
          select(Marketing)%>%
          ungroup()%>%
          slice(1:3)%>%
          c()
        paste0("Top Marketing Strategies:<strong>
               <ol>
               <li>",gsub("_", " ", stat$Marketing[1])," </li>
               <li>",gsub("_", " ", stat$Marketing[2])," </li>
               <li>",gsub("_", " ", stat$Marketing[3])," </li>
               </ol>
               </strong>
               ")%>%
          HTML()
      })
    }
    else{
      output$stat1= renderUI({
        return("")
      })
      
      output$stat2 = renderUI({
        return("")
      })
      
      output$stat3 = renderUI({
        return("")
      })
      
      output$stat4 = renderUI({
        return("")
      })
      
      output$stat5 = renderUI({
        return("")
      })
    }
    
    
  })
  
  
  #activity view outputs-------------
  
  traffic = reactive({
    TrafficPlot(values$subset)
  })
  output$traffic = renderPlot(traffic())
  
  perQuarter = reactive({
    QuarterPlot(values$subset)
  })
  output$PerQuarter = renderPlot(perQuarter())

  perDay = reactive({
    DayPlot(values$subset)
  })
  output$PerDay = renderPlot(perDay())
  
  perHour = reactive({
    HourPlot(values$subset)
  })
  output$PerHour = renderPlot(perHour())
  
  
  #counselor view outputs--------------
  calls = reactive({
    CallsPlot(values$subset, input$counselorCalls)
  })  
  output$Calls = renderPlot(calls())
  
  skills = reactive({
    SkillsPlot(values$subset)
  })
  output$Skills = renderPlot(skills())
  
  #add by call type viewer option conditional
  observeEvent(input$tabset, {
    toggle(id="counselorCalls", condition=input$tabset==2)
  })
  
  #topics view outputs--------------
  topics = reactive({
    TopicsPlot(TidyLDA(values$subset, input$ntopics))
  })
  output$Topics = renderPlot(topics())
  
  issues = reactive({
    IssuePlot(values$subset)
  })
  output$Issues = renderPlot(issues())
  
  #add ntopics option conditional
  observeEvent(input$tabset, {
    toggle(id="ntopics", condition=input$tabset==3)
  })
  
  
})

# Run the app ----
shinyApp(ui=ui, server=server)
