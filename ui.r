# Carrega os pacotes
require(shiny)
require(shinydashboard)
require(readtext)
require(quanteda)


dashboardPage(
  dashboardHeader(title="Complexidade  "
  ),
  
  dashboardSidebar(id="", sidebarMenu(
    
    fileInput("file", "Select a file"),
    
    
    
    menuItem("Indicadores quantitativos", tabName = "quantitativos", icon = icon("th")),
    
    
    
    menuItem("Indicadores qualitativos", tabName = "qualitativos", icon = icon("th")),
    
    downloadLink('downloadData', 'Download')
    
    
  )),
  
  
  dashboardBody(
    
    
    tabItems(
      # First tab content
      tabItem(tabName = "quantitativos",
              fluidRow(box(selectInput('textos', "Textos",choice= titulos_processados)),
                       box(selectInput('readability', "Escolha a medida de Readability", 
                          choices=colnames(readability)[-1])
              ,
              selectInput('lexical', "Escolha a medida de Lexical Diversity", 
                          choices=c("TTR", "C", "R", "CTTR", "U", "S", "K", "I", "D", "Vm", "Maas", "MATTR",
                                    "MSTTR")))),
              
              
              box( status = "primary",textOutput("text")),
              textOutput("text2"),
              box(title='Categoria',textOutput('categoria')),
              fluidRow(
                valueBoxOutput("palavras"),
                valueBoxOutput("types"),
                valueBoxOutput("readability"),
                valueBoxOutput("lexical"),
                valueBoxOutput("prop_cont_words")
              )
      ),
      
      # Second tab content
      tabItem(tabName = "qualitativos",
              selectInput('textos2', "Textos",choice= titulos_processados),
              box( status = "primary",textOutput("text3")),
              
              
              fluidRow(
              box(textInput('indicador_quali1','Digite a medida 1 de complexidade do texto :',width='100px'),width = 3),
              box(textInput('indicador_quali2','Digite a medida 2 de complexidade do texto :',width='100px'),width = 3),
              box(textInput('indicador_quali3','Digite a medida 3 de complexidade do texto :',width='100px'),width = 3),
              box(textInput('indicador_quali4','Digite a medida 4 de complexidade do texto :',width='100px'),width = 3),
              box(textInput('indicador_quali5','Digite a medida 5 de complexidade do texto :',width='100px'),width = 3)),
              actionButton("submit", "Submit"),
              DT::dataTableOutput("responses", width = 300)
      )
    )
  )
    
    
    
   
)



