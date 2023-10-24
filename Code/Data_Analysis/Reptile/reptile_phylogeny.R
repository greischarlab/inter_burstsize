
library(phylobase)

reptile_tree <- read.tree(here(file='Data','reptile_phylogeny.txt'))
reptile_tree$tip.label<-str_replace_all(reptile_tree$tip.label, "_", " ")
reptile_tree_2 <- drop.tip(reptile_tree, Non_Species )

Found_Species <- subset(Reptile_dat,Reptile_dat$Species %in% reptile_tree$tip.label)
Non_Species <- subset(reptile_tree$tip.label, !(reptile_tree$tip.label %in% Found_Species$Species))
Not_Found_Species <- subset(Reptile_dat,!(Reptile_dat$Species %in% reptile_tree$tip.label))

###Create a dataframe that lets me do a one to one relationship with host/parasites


Found_Species_Shorter<- data.frame(Lower = do.call(rbind, by(Found_Species, 
   Found_Species$Species, function (x) min(x$Lower), simplify = FALSE)))

Found_Species_Shorter$Species <- row.names(Found_Species_Shorter)
Found_Species_Shorter$Upper <- do.call(rbind, by(Found_Species, 
                                                 Found_Species$Species, function 
                                                 (x) max(x$Upper), simplify = FALSE))

rownames(
  Found_Species_Shorter) <- 
  Found_Species_Shorter $Species


reptile_phylo4 <- as(
  reptile_tree_2 , "phylo4")

tree4d <- phylo4d(reptile_phylo4, tip.data=Found_Species_Shorter[,-2])
          
      
plot(tree4d)

W <- proxTips(tree4d , met="Abouheif")
 moran.idx(tdata(tree4d , type="tip")$Upper, W)



Lower <- tdata(tree4d , type="tip")$Upper
sim <- replicate(1000, moran.idx(sample(Upper), W))
sim <- c(moran.idx(Upper , W), sim)

cat("\n=== p-value (right-tail) === \n")
pval <- mean(sim>=sim[1])

 ung.abTests <- abouheif.moran(tree4d)

 
 myTree.withBrLe <- compute.brlen(reptile_phylo4)
 
plot(density(sim), main="Moran's I Monte Carlo test for 'bif'") # plot
mtext("Density of permutations, and observation (in red)")
abline(v=sim[1], col="red", lwd=3)


reptile_phylogeny_GG <- 
  ggtree::ggtree(reptile_tree_2) + geom_tiplab() + 
  theme(text = element_text(size = 7))
  
reptile_phylogeny_order <- get_taxa_name(reptile_phylogeny_GG)

Found_Species$Species <- factor(Found_Species$Species, levels=reptile_phylogeny_order )
Found_Species_Shorter$Species <- factor(Found_Species_Shorter$Species, levels=reptile_phylogeny_order )

burst_size_GG <- 
  ggplot(Found_Species) + 
  geom_segment(data =Found_Species_Shorter,
                   aes( x= Lower, 
                    xend= Upper, y = Species, yend = Species)) + 
  geom_point(data = Found_Species_Shorter,
               aes( x= Lower,  y = Species), shape ='|') + 
  geom_point(data = Found_Species_Shorter,
             aes( x= Upper,  y = Species), shape ='|') + 
  geom_point(data = Found_Species, aes( x= Average, y= Species,
                                       color = Plasmodium.species), size = 2.5) + 
  geom_vline(xintercept = 0) + 
  scale_y_discrete(limits=rev) + 
  scale_color_viridis(discrete = TRUE, option = 'turbo') + 
  theme_bw() + 
  theme(axis.text.y = element_blank(), 
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.text.x = element_text(size = 14, color = 'black'),
        legend.position = 'none')
reptile_phylogeny_GG  + burst_size_GG

ggsave(here("Figure","Phylogeny_Tree","Reptile_Tree_Burst.pdf"), width = 13, height = 14, units = 'in')

       