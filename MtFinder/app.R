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
library(rentrez)

# Fetch mitochondrial genome sequence from NCBI GenBank
mtDNA01 <- entrez_fetch(db = "nucleotide", id = "NC_012920", rettype = "fasta")
mtDNA_vector <- gsub("\n", "", mtDNA01)
mtDNA_vector <- gsub(">NC_012920.1 Homo sapiens mitochondrion, complete genome", "", mtDNA_vector)

# Read the mitochondrial genes with start and end positions
mito_genes <- read.table("~/Downloads/MT_annotate_viz/MtFinder/Mito_genes.csv", 
                         sep = "\t", header = TRUE, stringsAsFactors = FALSE, fill = TRUE)
mito_genes$Positions.in.the.mitogenome <- gsub(",", "", mito_genes$Positions.in.the.mitogenome)
mito_genes <- mito_genes %>% 
  separate(Positions.in.the.mitogenome, into = c('Start', 'End'), sep = "–")
mito_genes$Start <- as.numeric(mito_genes$Start)
mito_genes$End <- as.numeric(mito_genes$End)

# Define the UI
ui <- fluidPage(
  titlePanel("Mitochondrial Genome Base Locator"),
  sidebarLayout(
    sidebarPanel(
      numericInput("position", "Enter a base position (1–16569):", value = 1, min = 1, max = 16569),
      actionButton("find_gene", "Find Gene")
    ),
    mainPanel(
      uiOutput("gene_output") 
    )
  )
)

# Define the server
server <- function(input, output) {
  observeEvent(input$find_gene, {
    base_position <- input$position
    genes <- mito_genes[mito_genes$Start <= base_position & mito_genes$End >= base_position, "Genes"]
    base <- substring(mtDNA_vector, first = base_position, last = base_position)
    
    if (length(genes) == 0) {
      # Case when the position is intergenic
      output$gene_output <- renderUI(HTML(paste(
        "The base position", base_position, "is intergenic.", "<br>",
        "The base at that position is:", base
      )))
    } else if (length(genes) == 1) {
      # Case when the position lies in a single gene
      output$gene_output <- renderUI(HTML(paste(
        "The base position", base_position, "lies in:", genes, ".", "<br>",
        "The base at that position is:", base
      )))
    } else {
      # Case when the position lies in multiple genes
      gene_list <- paste(genes, collapse = " and ")
      output$gene_output <- renderUI(HTML(paste(
        "The base position", base_position, "lies in:", gene_list, ".", "<br>",
        "The base at that position is:", base
      )))
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)
