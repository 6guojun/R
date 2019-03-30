##################################################
## Project:
## Script purpose: Phylogenetics and Evolution
## Date: 19, Dec, 2018
## Author: GUOjun
##################################################
setwd("/Users/liu/Documents/02To_do/01CVID/files")
library(ape)
library(readxl)
library(reshape2)
m <- read.csv("candidate_plus_known.csv",header = T)
m <- m[,1:3]
m <- dcast(m,formula = Core_Gene~Target, value.var = "Distance")
m[is.na(m)] <- 0
rownames(m) <- m[,1]; m <- m[,-1]
arbol <- nj(as.dist(m))
m2 <- read.csv("CVID_for_phylogeneticstree.csv",header = T)
co <- def(m2$CVID_gene_symbol,regexp = TRUE, 
          "^A" = "blue", 
          default = "red")
arbol$edge.length <- arbol$edge.length + abs(min(arbol$edge.length))+1
pdf('FGA_tree.pdf',width=10, height=10,pointsize=0.1)
plot(arbol,cex=1.3,edge.width=1, font=15, 
     edge.color = "black", #edge.color=rainbow(5)
     tip.color=co, 
     type="fan") 
dev.off()

