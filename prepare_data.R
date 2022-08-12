library(phyloseq)
library(gsubfn)
library(stringr)
library(tibble)
library(prodlim)
library(data.table)
library(kableExtra)
library(aplot)
source('draw-functions.R')

prepare_data <- function(psObject, csvFile, outputPrefix) {
  
  tree <- phy_tree(psObject)
  table_tree <- tax_table(psObject)
  
  # build table_tree_df$name
  table_tree_df<-data.frame(table_tree)
  table_tree_df$name <-sprintf("k__%s|p__%s|c__%s|o__%s|f__%s|g__%s|s__%s",
                               table_tree_df$Kingdom,table_tree_df$Phylum,
                               table_tree_df$Class, table_tree_df$Order,
                               table_tree_df$Family, table_tree_df$Genus,
                               table_tree_df$Species)
  table_tree_df$name = sub(pattern = "*NA", replacement = "", table_tree_df$name)
  table_tree_df$name = sub(pattern = "*NA", replacement = "", table_tree_df$name)
  table_tree_df$name = sub(pattern = "([|][kpcofgs]__)*$", replacement = "", table_tree_df$name)
  
  # build bacteria_sorted$feature
  bacteria_sorted<-csvFile
  bacteria_sorted$feature = sub(pattern = "([kpcofgs]__)*$", replacement = "", bacteria_sorted$feature)
  bacteria_sorted$feature = sub(pattern = "_$", replacement = "", bacteria_sorted$feature)
  bacteria_sorted$feature = sub(pattern = "_[kpcofgs]__", replacement = "", bacteria_sorted$feature)
  bacteria_sorted$feature = sub(pattern = "_[kpcofgs]__", replacement = "", bacteria_sorted$feature)
  
  # prepare bar.csv
  index.bac <- match(bacteria_sorted$feature[bacteria_sorted[['feature']] %in% table_tree_df$name], bacteria_sorted$feature)
  index.tree <- match(bacteria_sorted$feature[bacteria_sorted[['feature']] %in% table_tree_df$name], table_tree_df$name)
  
  bar_prepared<-bacteria_sorted[index.bac,]
  bar_prepared$name<-table_tree_df[index.tree,][['name']]
  bar_prepared$base<-rownames(table_tree_df[index.tree,])
  
  tip.to.remove <- tree[['tip.label']] [ (! table_tree_df[['name']] %in% bacteria_sorted$feature)
                                         | duplicated(table_tree_df[['name']])]
  tree_prepared <- drop.tip(tree, tip.to.remove)
  tree_prepared$type="tree"
  
  index_bar<-match(bar_prepared$base, taxa_names(tree_prepared))
  bar_prepared<-bar_prepared[index_bar,]
  bar_prepared$pvalue<--log10(bar_prepared$pvalue)
  bar_prepared$Pvalue<-bar_prepared$pvalue
  bar_prepared$Group<-bar_prepared$enrich_group
  bar_prepared$Label<-sub(pattern = "^.*\\|", replacement = "", bar_prepared$name)
  
  # prepare tree.nwk

  species<-sub(pattern = "^.*\\|", replacement = "", bar_prepared$name)
  tree_prepared$tip.label<-species
  
  # prepare heatmap.csv
  OTU<-otu_table(psObject, taxa_are_rows = F)
  index_heatmap<-match(bar_prepared$base, taxa_names(OTU))
  OTU_cut<-OTU[,index_heatmap]
  colnames(OTU_cut)<-sub(pattern = "^.*\\|", replacement = "", bar_prepared$name)

  table_tree <- tax_table(psObject)
  table_tree_cut<-table_tree[index_heatmap,]
  rownames(table_tree_cut)<-sub(pattern = "^.*\\|", replacement = "", bar_prepared$name)
  
  sample<-sample_data(psObject)

  phyloseq_prepared<-phyloseq(OTU_cut, table_tree_cut, sample, tree_prepared)
  heatmap_prepared<-psmelt(phyloseq_prepared)
  heatmap_prepared$Category<-heatmap_prepared$Sample
  heatmap_prepared$Patient<-heatmap_prepared$Category
  heatmap_prepared$Label<-heatmap_prepared$OTU
  heatmap_prepared$Value<-heatmap_prepared$Abundance
  
  # save files
  write.csv(bar_prepared, file = paste(outputPrefix, ".bar.csv"), row.names= TRUE)
  write.tree(tree_prepared, file = paste(outputPrefix, ".nwk"))
  write.csv(heatmap_prepared, file = paste(outputPrefix, ".heatmap.csv"))
}



