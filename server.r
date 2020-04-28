load('Resultados_indicadores_textos.Rdata')
require(quanteda)
require(data.table)
library(rdrop2)

# Define the fields we want to save from the form
fields <- c("textos2","indicador_quali1", "indicador_quali2",
            'indicador_quali2','indicador_quali2','indicador_quali5')

outputDir <- "responses"

saveData <- function(data) {
  data <- t(data)
  # Create a unique file name
  fileName <- sprintf("%s_%s.csv", as.integer(Sys.time()), digest::digest(data))
  # Write the data to a temporary file locally
  filePath <- file.path(tempdir(), fileName)
  write.csv(data, filePath, row.names = FALSE, quote = TRUE)
  # Upload the file to Dropbox
  drop_upload(filePath, path = outputDir)
}

loadData <- function() {
  # Read all the files into a list
  filesInfo <- drop_dir(outputDir)
  filePaths <- filesInfo$path
  data <- lapply(filePaths, drop_read_csv, stringsAsFactors = FALSE)
  # Concatenate all data together into one data.frame
  data <- do.call(rbind, data)
  data
}



function(input, output, session) {

  
  # Define a reactive variable named `input_file`

  


  # Define a reactive variable named `input_file`
  
  input_file <- renderText({
    
    index<-match(input$textos,titulos_processados)
    
      return(mycorpus[[index]])
    
  })
  
  
  output$categoria <- renderText({
    
    index<-match(input$textos,titulos_processados)
    
    return(indicadores_texto$Categoria[index])
    
  })
  
  output$text <- renderText({ 
    input_file()
      
    })
  
  input_file2 <- renderText({
    
    index<-match(input$textos2,titulos_processados)
    
    return(mycorpus[[index]])
    
  })
  
  output$text3 <- renderText({ 
    input_file2()
    
  })
  input_text <- renderText({
    if (is.null(input$file)) {
      return(  input_file())
    }
    # Read the text in the uploaded file
    else{
      doc<-readtext(input$file$datapath)
      return(doc$text)
      }
  })
  

  
  output$text <- renderText({ 
    if (is.null(input$file)) {
      return(  input_file())
    }
    else
      return(input_text())
  })
  
  
######################################
# Mineracao do texto
#######################################

palavras_tokenizadas<-reactive({
  if(!is.null(input$file)){
    
    doc<-readtext(input$file$datapath)
    
    texto<-corpus(doc$text[1])
    
    # Tokenisation por palavra
    tok2 <- tokens(texto, what = "word",
                  remove_punct = TRUE,
                  remove_symbols = TRUE,
                  remove_numbers = TRUE,
                  remove_url = TRUE,
                  remove_hyphens = FALSE,
                  verbose = TRUE, 
                  include_docvars = TRUE)
    
    # Coloca todos como minuscula
    tok2 <- tokens_tolower(tok2)
    
    # Remove stop words
    tok2 <- tokens_select(tok2, stopwords("portuguese"), selection = "remove", padding = FALSE)
    
  }
  
  return(tok2)
})

  
my_corpus<-reactive({
    if(!is.null(input$file)){
      
      doc<-readtext(input$file$datapath)
      
      titulo<-strsplit(doc$tex,'\n')[[1]][1]
      metadados<-strsplit(doc$tex,'\n')[[1]][length(strsplit(doc$tex,'\n')[[1]])]
      texto<-toString(strsplit(doc$tex,'\n')[[1]][-c(1,length(strsplit(doc$tex,'\n')[[1]]))])
  
      texto<-corpus(texto)
      
    }
    return(texto)
})
  

  


  output$palavras <- renderValueBox({
    if(is.null(input$file)){
      index<-match(input$textos,titulos_processados)
     valueBox(
      value =ntoken(tok)[index],
      subtitle = "Numero de palavras",
      icon = icon("list"),
      color ="yellow" 
    )
    
    }
    else {
      
      valueBox(
        
        value =   ntoken(palavras_tokenizadas()),
        subtitle = "Numero de palavras",
        icon = icon("list"),
        color ="yellow" 
      )
      }
    
  })
  
  
  
  output$types <- renderValueBox({
    
    if(is.null(input$file)){
      index<-match(input$textos,titulos_processados)
    valueBox(
      
      value = ntype(tok)[index],
      subtitle = "Numero de palavras distintas",
      icon = icon("list"),
      color ="blue" 
    )
    }
    
    else{
      
      valueBox(
        
        value =  ntype(palavras_tokenizadas()),
        subtitle = "Numero de palavras distintas",
        icon = icon("list"),
        color ="blue"  
      )
    }
      
    
  })
  
  
  df_subset<- reactive({
    
    if(is.null(input$file)){
      index<-match(input$textos,titulos_processados)
      a <- readability[index,]
    
    
    }
    
    else{
      a<- textstat_readability(my_corpus(),'all', remove_hyphens = TRUE,
                                          min_sentence_length = 1, max_sentence_length = 10000,
                                          intermediate = FALSE)
      
    }
    return(a)
  })
  

  
  output$readability <- renderValueBox({
    
    
    
    valueBox(
      
      value =  round(df_subset()[[input$readability]],3) ,
      subtitle = input$readability,
      icon = icon("list"),
      color ="red" 
    )
    
    
  })
  
  df_subset_lexical<- reactive({
    
    if(is.null(input$file)){
      index<-match(input$textos,titulos_processados)
      a <- lexical_diversity[index,]
      
      
    }
    
    else{
      # Tokenisation por palavra
      tok <- tokens(mycorpus(), what = "word",
                    remove_punct = TRUE,
                    remove_symbols = TRUE,
                    remove_numbers = TRUE,
                    remove_url = TRUE,
                    remove_hyphens = FALSE,
                    verbose = TRUE, 
                    include_docvars = TRUE)
      
      # Calcula a matriz de frequencia das palavras
      word_matrix<-dfm(tok)
      
      # Calcula medidas de lexical diversity
      a<-textstat_lexdiv(word_matrix,measure = "all")
      
    }
    return(a)
  })
  
  output$lexical <- renderValueBox({
    
    
    
    valueBox(
      
      value =  round(df_subset_lexical()[[input$lexical]],3) ,
      subtitle = input$lexical,
      icon = icon("list"),
      color ="green" 
    )
    
    
  })
  
 
  output$prop_cont_words <- renderValueBox({
    if(is.null(input$file)){
      index<-match(input$textos,titulos_processados)
      valueBox(
        value =round(prop_cont_word[[index]],3),
        subtitle = "Proporcao de palavras de conteudo",
        icon = icon("list"),
        color ="yellow" 
      )
      
    }
    else {
      
      valueBox(
        
        value =   "Nao e possivel calcular",
        subtitle = "Proporcao de palavras de conteudo",
        icon = icon("list"),
        color ="yellow" 
      )
    }
    
  })
  
   output$downloadData <- downloadHandler(
     filename = function() {
       paste('data-', Sys.Date(), '.csv', sep='')
     },
     content = function(con) {
       fwrite(indicadores_texto[,c(1,63,2:62,64)],con,sep=';',dec=',')
     }
   )
   
   # Whenever a field is filled, aggregate all form data
   formData <- reactive({
     data <- sapply(fields, function(x) input[[x]])
     data
   })
   
   # When the Submit button is clicked, save the form data
   observeEvent(input$submit, {
     saveData(formData())
   })
   
   # Show the previous responses
   # (update with current response when Submit is clicked)
   output$responses <- DT::renderDataTable({
     input$submit
     loadData()
   })     
   
}  
