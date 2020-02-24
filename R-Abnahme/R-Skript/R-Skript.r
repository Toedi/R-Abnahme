#Initialisieren der benötigten Lybraries!
library(edgeR)
library(shiny)
library(pheatmap)
#Vorbereiten und Filtern des Datensatzes auf die 50 am stärksten differenziell exprimierten Gene!
data <- read.table("data/Mov10_full_counts.txt",header=T, row.names = 1)
logcounts <- cpm(data)
var_genes <- apply(logcounts,1,var)
select_var <- names(sort(var_genes, decreasing=T))[1:50]
finalselect <- logcounts[select_var,]
#ShinyUi erstellen!
ui <- pageWithSidebar(
  headerPanel('Counts-Matrix Heatmap'),
  sidebarPanel(
    selectInput('distMethod', 'Distanz-Methode', c('euclidean', 'maximum', 'manhattan', 'canberra', 'binary','minkowski')),
    selectInput('clustMethod', 'Cluster-Methode', c('ward.D','ward.D2','single','complete','average'))
  ),
  mainPanel(
    plotOutput('heatplot', height = 600)
  )
)
#Serverfunktion erstellen!
server <- function(input, output, session){
  selectedMethods <- reactive({
    c(input$distMethod, input$clustMethod)
  })
  output$heatplot <- renderPlot({
    #heatmap(finalselect, distfun = function(x) dist(x, method = selectedMethods()[1]), hclustfun = function(x) hclust(x, method = selectedMethods()[2]))
    pheatmap(finalselect, clustering_distance_cols = selectedMethods()[1], clustering_distance_rows = selectedMethods()[1], clustering_method = selectedMethods()[2])
  })
}
#Shinyapp starten
shinyApp(ui,server)
