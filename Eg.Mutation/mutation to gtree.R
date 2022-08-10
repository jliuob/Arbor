mutation<-read.csv("mutation_rate.csv")
#length(mutation$Sample.1) 
#82944

mutation$SNPs<-as.numeric(gsub(",","",mutation$SNPs))
dist<-mutation$Mutation.rate

hier_tree <- matrix(ncol=400, nrow=400)
hier_tree[lower.tri(hier_tree)] <- dist[1:79800]
diag(hier_tree) <- 0
hier_tree <- as.dist(hier_tree, diag = TRUE)
htree<-hclust(hier_tree)
plot(htree)

gtree<-as.phylo(htree)
ggtree(gtree)
write.tree(gtree, file = paste("mutation_tree.nwk"))



name<-unique(substr(mutation$Sample.1,1,4))
name

hm<-data.frame(Label = unique(mutation$Sample.1))
hm$Category = substr(hm$Label, 1, 4)
hm$Value = as.numeric(factor(hm$Category))
ggplot(hm, aes(x = Category, y = Label)) +
  geom_tile(aes(fill = Value)) + scale_fill_viridis_c() +
  theme(axis.ticks.y = element_blank(), axis.text.y = element_blank(),
        axis.text.x = element_text(
          angle = 90,
          hjust = 1,
          vjust = 0.5
        ))

