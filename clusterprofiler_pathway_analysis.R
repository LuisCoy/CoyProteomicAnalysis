#Function to run Cluster profiler on a Peaks output 
#gseGO, gseKEGG and Pathway visualisation with pathview
#libraries required in the r include:
#library(clusterProfiler)
#library(enrichplot)
#library(pathview)
#library(tidyverse)
#input peaks file needs a column called log2fold, Accession and Gene_name
#pAdjustMethod one of “holm”, “hochberg”, “hommel”, “bonferroni”, “BH”, “BY”, “fdr”, “none”
require("org.Hs.eg.db", character.only = T)

pathway_analysis <- function(x, GO = TRUE, KEGG = TRUE, Pathview = TRUE, p_value_cutoff = 0.05, p_ajust_method = "none"){
#accession codes and log3fold list for kegg enrichment analysis
  kegg_list <- x$log2fold
  names(kegg_list) <- x$Accession
  kegg_list <- na.omit(kegg_list)
  kegg_list <- sort(kegg_list, decreasing = T)
#gene symbols and log3fold list for pathway visualisation
  gene_list <- x$log2fold
  names(gene_list) <- x$Gene_name
  gene_list <- na.omit(gene_list)
  gene_list <- sort(gene_list, decreasing = T)

#GO enrichment analysis
  if (GO){
    print("GO analysis started")
    gse_go <- gseGO(geneList = kegg_list,
                    ont = "ALL",
                    keyType = "UNIPROT",
                    nPerm = 10000,
                    minGSSize = 3,
                    maxGSSize = 1000,
                    pvalueCutoff =  p_value_cutoff,
                    verbose = T, 
                    OrgDb = "org.Hs.eg.db",
                    pAdjustMethod = p_ajust_method)
    
    gse_go_results <- gse_go@result
    write_csv(gse_go_results, file = "./gse_go_analysis.csv")
  }  
#KEGG enrichment analysis
  if (KEGG | Pathview){
    print("KEGG analysis started")
    gse_kegg <- gseKEGG(geneList = kegg_list,
                        organism = "hsa",
                        nPerm = 10000,
                        minGSSize = 3,
                        maxGSSize = 800,
                        pvalueCutoff =  p_value_cutoff,
                        pAdjustMethod = p_ajust_method,
                        keyType = "uniprot")
    
    gse_kegg_results <- gse_kegg@result
    write_csv(gse_kegg_results, file = "./gse_kegg_analysis.csv")
  }
#pathway visualisation
  if (Pathview){  
    print("pathway visualisation started")
    fun_pathways <- function(pathway){
      pathview(gene.data = gene_list,
               pathway.id = pathway,
               species = "hsa",
               gene.idtype = "symbol",
               kegg.native = T)
    }
    #extract pathways to visualise
    kegg_pathways <-
      gse_kegg_results %>%
      dplyr::filter(ID != "hsa01100")
    
    pathways_list = as.list(kegg_pathways[1])
    
    base::mapply(FUN = fun_pathways, pathways_list)
  }
}
