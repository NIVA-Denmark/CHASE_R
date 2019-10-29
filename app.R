rm(list = ls())
library(shiny)
library(ggplot2)
source('CHASE.R')

script <- "$('#QEtable tbody tr td:nth-last-child(2)').each(function() {
var cellValue = $(this).text().trim();
if (cellValue > 10) {
$(this).css('background-color', 'rgb(255, 102, 0)');
}
else if (cellValue > 5) {
$(this).css('background-color', 'rgb(255, 153, 51)');
}
else if (cellValue > 1) {
$(this).css('background-color', 'rgb(255,255,102)');
}
else if (cellValue > 0.5) {
$(this).css('background-color', 'rgb(102,255,102');
}
else if (cellValue > 0) {
$(this).css('background-color', 'rgb(51,153,255)');
}
});
$('#QEtable tbody tr td:last-child').each(function() {
var cellValue = $(this).text().trim();
if (cellValue == 'Bad') {
$(this).css('background-color', 'rgb(255, 102, 0)');
}
else if (cellValue == 'Poor') {
$(this).css('background-color', 'rgb(255, 153, 51)');
}
else if (cellValue == 'Moderate') {
$(this).css('background-color', 'rgb(255,255,102)');
}
else if (cellValue == 'Good') {
$(this).css('background-color', 'rgb(102,255,102');
}
else if (cellValue == 'High') {
$(this).css('background-color', 'rgb(51,153,255)');
}
});
"

#== 'Moderate'
ui <- fluidPage(
  
  titlePanel("CHASE Tool"),
  sidebarLayout(
    sidebarPanel(
      fileInput('datafile', 'Choose input file'),
      withTags({
        div(class="header", checked=NA,
            h4("Instructions"),
            p("Select the file containing input data for the CHASE assessment.
              The file must be in text format with columns separated by semi-colons.
              Column headers must be included. The required columns are:"),
            ul(
              li("Matrix"),
              li("Substance"),
              li("Threshold"),
              li("Status")
            ),
            p("The following columns are optional:"),
            ul(
              li("Waterbody"),
              li("Response (1 or -1)")
            ),
            p("The assesssment is made per waterbody. If no waterbody is specified, all indicators are combined in a single assessment."),
            p("Response=1 (default): status worsens with increasing indicator value. Response=-1: status improves with increasing indicator value"),
            p("Example data can be found here:", HTML("<a href='data/CHASE_example_DK.txt' target='_blank'>CHASE_example_DK.txt</a>"))
        )
        
      }),
      
      
      withTags({
        div(class="header", checked=NA,
            h4("Options"),
            p("More assessment options..."),br(),br()
        )
      }),
      withTags({
        div(class="header", checked=NA,
            h4("More information"),
            p("To find out more, contact ",
            a(href="https://niva-denmark.dk/heat/", "NIVA Denmark")),
            a(href="https://niva-denmark.dk/heat/",img(src="NIVA-Denmark-150.png"))
        )
      })
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Data", tableOutput("InDatatable")),
        tabPanel("Results", 
                 uiOutput("Test1"),plotOutput("plot"))   
      ) # tabset panel
    )
  )  
  
) #fluid page

server <- function(input, output, session) {
  session$onFlushed(function() {
    session$sendCustomMessage(type='jsCode', list(value = script))
  }, FALSE)
  
  output$caption <- renderText(input$num)
  
  
  addResourcePath("data","./data/")
  #examplefile<-'CHASE_example.txt'
  #output$downloadData <- downloadHandler(
  #  filename = c(examplefile),
  #  content = function(file) {
  #    examplefile
  #  }
  #)
  
  
  #This function is repsonsible for loading in the selected file
  filedata <- reactive({
    infile <- input$datafile
    if (is.null(infile)) {
      # User has not uploaded a file yet
      return(NULL)
    }
    #filedata<-read.csv(infile$datapath,  sep=";", encoding = "UTF-8", fileEncoding = 'ISO8859-1')
    filedata<-read.csv(infile$datapath,  sep=";")
    return(filedata)
  })
  
  InData <- reactive({
    df<-filedata()
    if (is.null(df)){return(NULL)} 
    out<-Assessment(df)     #Individual indicator results
    return(out)
  })
  QEdata <- reactive({
    df<-filedata()
    if (is.null(df)){return(NULL)} 
    out<-Assessment(df,3)     #Quality Element results
    return(out)
  })
  QEspr <- reactive({
    df<-filedata()
    if (is.null(df)){return(NULL)} 
    out<-Assessment(df,2)     #QE Results transposed
    return(out)
  })
  
  CHASEplot<- reactive({
    QE<-QEdata()
    
    ymax=max(QE$ConSum,na.rm=TRUE)
    ymax=ceiling(ymax)
    if(ymax>5 & ymax<10){ymax=10}
    if(ymax>1 & ymax<5){ymax=5}
    
    if (is.null(QE)){return(NULL)}
    
    levels<-data_frame(factor(c("High","Good","Moderate","Poor","Bad"),
                              levels=c("High","Good","Moderate","Poor","Bad")),
                       c(0.0,0.5,1,5,10),
                       c(0.5,1,5,10,ymax))
    names(levels)[1] <- 'Status'
    names(levels)[2] <- 'ymin'
    names(levels)[3] <- 'ymax'
    
    levels2<-levels
    levels$x<-0.5
    levels2$x<-0.5+max(as.numeric(QE$Waterbody))
    
    levels<-rbind(levels, levels2)
    
    levels<-levels[levels$ymin<=ymax,]
    ymax2=max(levels$ymax,na.rm=TRUE)
    levels[levels$ymax==ymax2,]$ymax<-ymax    
    Palette1=c("#3399FF", "#66FF66", "#FFFF66","#FF9933","#FF6600" )
    
    p<-ggplot(data=QE,x=Waterbody,y=ConSum) + theme_bw() +
      geom_point(size=5,data=QE, aes(x=factor(Waterbody), y=ConSum,shape=Matrix, ymin=0)) +
      geom_ribbon(data=levels,aes(x=x,ymin=ymin,ymax=ymax,fill=Status),alpha=0.5) +
      geom_point(size=5,data=QE, aes(x=factor(Waterbody), y=ConSum,shape=Matrix, ymin=0)) +
      scale_fill_manual(name="Status", values=Palette1)+
      xlab('Waterbody')+ylab('Contamination Sum')
      return(p)
  })
  
  output$InDatatable <- renderTable({return(InData())})
  output$plot <- renderPlot({return(CHASEplot())})
  output$QEtable <- renderTable({return(QEspr())})
  output$Test1 <- renderUI({
    list(
      tags$head(tags$script(HTML('Shiny.addCustomMessageHandler("jsCode", function(message) { eval(message.value); });')))
      , tableOutput("QEtable")
    )})
}

shinyApp(ui=ui, server=server)
