library(ggplot2)
library(shiny)
library(arules)
library(arulesViz)
shinyServer(function(input,output, session){
  observe({
   if(input$dataInput==1){
      if(input$sampleData==1){
        mojedata<-read.delim("diabetes.csv", sep=";", header=TRUE)
        updateSelectInput(session, "y", "Vyber prvý parameter:", colnames(mojedata) [1:ncol(mojedata)])
        #nums <- sapply(mojedata, is.numeric),
        #mojedata[ , nums]
        updateSelectInput(session, "x", "Vyber druhý parameter:", colnames(mojedata) [1:ncol(mojedata)], names(mojedata)[[1]])
       updateSelectInput(session, "a", "Vyber prvý parameter:", colnames(mojedata) [1:ncol(mojedata)])
       updateSelectInput(session, "b", "Vyber druhý parameter:", colnames(mojedata) [1:ncol(mojedata)], names(mojedata)[[1]])
     } else {
       mojedata<-read.delim("hepatitis.csv", sep=";", header=TRUE)
       updateSelectInput(session, "y", "Vyber prvý parameter:", colnames(mojedata) [1:ncol(mojedata)])
       updateSelectInput(session, "x", "Vyber druhý parameter:", colnames(mojedata) [1:ncol(mojedata)], names(mojedata)[[1]])
       updateSelectInput(session, "a", "Vyber prvý parameter:", colnames(mojedata) [1:ncol(mojedata)])
       updateSelectInput(session, "b", "Vyber druhý parameter:", colnames(mojedata) [1:ncol(mojedata)], names(mojedata)[[1]])
    }
    }
    else 
    if(input$dataInput==2){
      inFile <- input$file1
      if (is.null(inFile))  {return(NULL)
      }

    #NACITANIE TABULKY A UPDATOVANIE PARAMETROV#
    mojedata=read.delim(inFile$datapath, header=input$header, sep=input$sep)
    updateSelectInput(session, "y", "Vyber prvý parameter:", colnames(mojedata) [1:ncol(mojedata)])
    updateSelectInput(session, "x", "Vyber druhý parameter:", colnames(mojedata) [1:ncol(mojedata)], names(mojedata)[[1]])
    updateSelectInput(session, "a", "Vyber prvý parameter:", colnames(mojedata) [1:ncol(mojedata)])
    updateSelectInput(session, "b", "Vyber druhý parameter:", colnames(mojedata) [1:ncol(mojedata)], names(mojedata)[[1]])
    }
    #ZOBRAZENIE TABULKY#
    output$tabulka <- renderDataTable(mojedata)
  
  #SUMMARY#
  output$summary <- renderPrint({
    if(input$sumarVolba==1){
      summary(mojedata)}
    else{
       str(mojedata)}
  })

   #GRAFY#
  boxplotText <- reactive({
    paste(input$y, " ~", input$x)
  })
  
  #BOXPLOT#
  output$graf1 <- renderPlot({
  if(input$plotvolba==1){
    if(input$horizontal==FALSE){
      boxplot(as.formula(boxplotText()),data=mojedata, main=paste(input$y, " vs ", input$x),horizontal=FALSE, xlab=input$x, ylab=input$y)}
  else{
    boxplot(as.formula(boxplotText()),data=mojedata, main=paste(input$x, " vs ", input$y),horizontal=TRUE, xlab=input$y, ylab=input$x) 
  }
  }
  #BARPLOT#
  else if(input$plotvolba==2){
    ggplot(data=mojedata, aes_string(x=input$a, fill=input$b)) + geom_bar(position=position_dodge())}
  })
  
  #stiahnutie grafu
  output$down<- downloadHandler(
    filename=function(){
      paste(input$plotvolba,'.pdf', sep='')
    },
    content=function(file){
      pdf(file)
      if(input$plotvolba==1){
        if(input$horizontal==FALSE){
          boxplot(as.formula(boxplotText()),data=mojedata, main=paste(input$y, " vs ", input$x),horizontal=FALSE, xlab=input$x, ylab=input$y)}
        else{
          boxplot(as.formula(boxplotText()),data=mojedata, main=paste(input$x, " vs ", input$y),horizontal=TRUE, xlab=input$y, ylab=input$x) 
        }
      }
      else if(input$plotvolba==2){
        ggplot(data=mojedata, aes_string(x=input$a, fill=input$b)) + geom_bar(position=position_dodge())
    }
    dev.off()}
    )
  
  #NAPOVEDA PRE BOXPLOT#
  values1 <- reactiveValues(shouldShow = FALSE)
  observe({
    if (input$button1 == 0) return()
    
    values1$shouldShow = TRUE
  })
  observe({
    if (is.null(input$button2) || input$button2 == 0)
      return()
    values1$shouldShow = FALSE
  })
  
  output$textToPrint1<- renderText({ 
    if (values1$shouldShow)
      ('Používa sa pre grafickú vizualizáciu dát pomocou ich kvartilov.
          Stredná časť diagramu je zhora ohraničená 3.kvartilom, zospodu 1.kvartilom
    a medzi nimi sa nachádza medián.')
  })
  
  #NAPOVEDA PRE BARPLOT#
  values2<- reactiveValues(shouldShow = FALSE)
  observe({
    if (input$button3 == 0) return()
    
    values2$shouldShow = TRUE
  })
  observe({
    if (is.null(input$button2) || input$button2 == 0)
      return()
    values2$shouldShow = FALSE
  })
  
  output$textToPrint2<- renderText({ 
    if (values2$shouldShow)
      ('Stĺpcový graf(ang. barplot) alebo histogram je tvorený obdĺžnikmi, 
          ktorých základne (os "x") majú dĺžku zvolených intervalov, 
         a ktorých výšky (os "y") majú veľkosť príslušných absolútnych 
         alebo relatívnych početností zvolených tried.')
  })
  
  output$uiButton2 <- renderUI({
    if (values1$shouldShow)
      actionButton("button2","Zatvoriť nápovedu")
    else
      if (values2$shouldShow)
        actionButton("button2","Zatvoriť nápovedu")
  })
 
 #SCATTERPLOT#
 
 #funkcia na asociacne pravidla
asociacne<-function(support, confidence){
   #nacitanie typu stlpca#
  typeCols <- sapply(mojedata, class)
   #nacitania stplcov ktore su factor#
  factCols <- grep('factor', typeCols)
  subM <- mojedata[,factCols]
  mojedata <- as(subM, "transactions")
  asociacne <- apriori(mojedata, parameter=list(support=input$s, confidence=input$c))
}
 
 output$graf3<- renderPlot({
   plot(asociacne(support, confidence))
 })
 
 #VYPISANIE PRAVIDIEL#
 output$pravidla <- renderPrint({
   inspect(head(sort(asociacne(support, confidence), by ="lift"),input$z))
 })

})
})
