library(ggplot2)
library(shiny)
library(arules)
library(arulesViz)


#mojedata<-read.csv("diabetes.csv", sep=";", header=TRUE,fileEncoding = "UTF-8")

shinyServer(function(input,output, session){
  observe({
   if(input$dataInput==1){
      if(input$sampleData==1){
        mojedata<-read.delim("diabetes.csv", sep=";", header=TRUE, fileEncoding = "UTF-8")
        typeCols <- sapply(mojedata, class)
        factCols <- grep('factor', typeCols)
        subM <- mojedata[,factCols]
        subM2 <- mojedata[,-factCols]
        updateSelectInput(session, "y", "Vyber prvý parameter:", colnames(subM2) [1:ncol(subM2)])
        updateSelectInput(session, "x", "Vyber druhý parameter:", colnames(mojedata) [1:ncol(mojedata)])
        updateSelectInput(session, "a", "Vyber prvý parameter:", colnames(mojedata) [1:ncol(mojedata)])
        updateSelectInput(session, "b", "Vyber druhý parameter:", colnames(subM) [1:ncol(subM)])
      }
     
     else if(input$sampleData==2) {
       mojedata<-read.delim("hepatitis.csv", sep=";", header=TRUE, fileEncoding = "UTF-8")
       typeCols <- sapply(mojedata, class)
       factCols <- grep('factor', typeCols)
       subM <- mojedata[,factCols]
       subM2 <- mojedata[,-factCols]
       updateSelectInput(session, "y", "Vyber prvý parameter:", colnames(subM2) [1:ncol(subM2)])
       updateSelectInput(session, "x", "Vyber druhý parameter:", colnames(mojedata) [1:ncol(mojedata)])
       updateSelectInput(session, "a", "Vyber prvý parameter:", colnames(mojedata) [1:ncol(mojedata)])
       updateSelectInput(session, "b", "Vyber druhý parameter:", colnames(subM) [1:ncol(subM)])
    }
    }
    else if(input$dataInput==2){
      inFile <- input$file1
      if (is.null(inFile))  
        return(NULL)
    #NACITANIE TABULKY A UPDATOVANIE PARAMETROV#
    mojedata=read.delim(inFile$datapath, header=input$header, sep=input$sep, fileEncoding = "UTF-8")
    typeCols <- sapply(mojedata, class)
    factCols <- grep('factor', typeCols)
   subM <- mojedata[,factCols]
   subM2 <- mojedata[,-factCols]
    updateSelectInput(session, "y", "Vyber prvý parameter:", colnames(subM2) [1:ncol(subM2)])
    updateSelectInput(session, "x", "Vyber druhý parameter:", colnames(mojedata) [1:ncol(mojedata)])
    updateSelectInput(session, "a", "Vyber prvý parameter:", colnames(mojedata) [1:ncol(mojedata)])
    updateSelectInput(session, "b", "Vyber druhý parameter:", colnames(subM) [1:ncol(subM)])
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
      paste(input$plotvolba,'.pdf', sep='',fileEncoding = "utf8")
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
      ('Boxplot alebo aj krabicový graf sa používa pre vizualizáciu dát pomocou ich kvartilov.
          Stredná časť diagramu(krabička) je zhora(zprava) ohraničená 3.kvartilom, zospodu(zľava) 1.kvartilom
    a medzi nimi sa nachádza medián. Ďalšia čiara nachádzajúca sa pod krabicou znázorňuje minimálnu hodnotu a nad krabicou maximálnu hodnotu pre zvolený parameter. 
       Viac v záložke INFO.')
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
         alebo relatívnych početností zvolených tried.
       Viac v záložke INFO.')
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
  asociacne <- apriori(mojedata, parameter=list(support=input$s, confidence=input$c, maxlen=4))
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
