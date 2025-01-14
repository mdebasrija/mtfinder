#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(seqinr)
library(ape)
library(dplyr)
library(tidyr)

#reading in fasta files - not used for now
mtDNA <- read.fasta(file = "~/Downloads/MT_annotate_viz/mt_refsequence.fasta", as.string = TRUE, seqtype = "DNA")
mtDNA01 = read.GenBank("NC_012920", seq.names = "NC_012920", species.names = TRUE,
                     as.character = FALSE, chunk.size = 400, quiet = TRUE,
                     type = "DNA")

#list of mitochondrial genes with start and end positions
mito_genes = read.table("~/Downloads/MT_annotate_viz/MtFinderMito_genes.csv", sep = "\t", header = T, stringsAsFactors = F, fill = T)
mito_genes$Positions.in.the.mitogenome = gsub(",", "", mito_genes$Positions.in.the.mitogenome)

mito_genes <- mito_genes %>% separate(Positions.in.the.mitogenome, into = c('Start', 'End'), sep = "–")

mito_genes$Start = as.numeric(mito_genes$Start)
mito_genes$End = as.numeric(mito_genes$End)


#defining the UI
ui <- fluidPage(
  titlePanel("Mitochondrial Genome Base Locator"),
  sidebarLayout(
    sidebarPanel(
      numericInput("position", "Enter a base position (1–16569):", value = 1, min = 1, max = 16569),
      actionButton("find_gene", "Find Gene")
    ),
    mainPanel(
      textOutput("gene_output")
    )
  )
)

# Define the server
server <- function(input, output) {
  observeEvent(input$find_gene, {
    base_position <- input$position
    genes <- mito_genes[mito_genes$Start <= base_position & mito_genes$End >= base_position, "Genes"]
    
    if (length(genes) == 0) {
      # Case when the position is intergenic
      output$gene_output <- renderText(paste("The base position", base_position, "is intergenic."))
    } else if (length(genes) == 1) {
      # Case when the position lies in a single gene
      output$gene_output <- renderText(paste("The base position", base_position, "lies in:", genes))
    } else {
      # Case when the position lies in multiple genes
      gene_list <- paste(genes, collapse = " and ")
      output$gene_output <- renderText(paste("The base position", base_position, "lies in:", gene_list))
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)