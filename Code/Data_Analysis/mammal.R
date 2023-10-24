mammal_tree <- read.nexus(here(file='Data','mammal.tre'))
mammal_tree$tip.label  

tip.label2 <- (strsplit(mammal_tree$tip.label , '_'))
# just the capitalized ones
# assemble back to a single string, if you want
sub("(_[^_]*)_", "\\1", mammal_tree$tip.label)

mammal_tree_newlabel <- NULL
for(k in seq(1,length(mammal_tree$tip.label))){
   a <- unlist(strsplit(mammal_tree$tip.label[[k]], "_"))

   if(a[[1]] ==  ""){
     new_word <- paste(a[[2]], a[[3]])
   }
   else{
     new_word <- paste(a[[1]],a[[2]])
   
   }
   mammal_tree_newlabel[[k]] <- new_word 
}
mammal_tree_newlabel<- do.call(rbind, mammal_tree_newlabel)

mammal_tree$tip.label<- mammal_tree_newlabel



mammal_tree_2 <- keep.tip(mammal_tree, 
                          unique(Found_Species$Species))

write.csv(mammal_tree$tip.label, 'mammal_name.csv')


Found_Species <- subset(
                           Mammal_dat,
                        Mammal_dat$Species %in%mammal_tree$tip.label)

NOT_FOUND_SPECIES <-subset(
    Mammal_dat,!(
    Mammal_dat$Species %in% mammal_tree$tip.label))



Found_Species_Shorter<- data.frame(Lower = do.call(rbind, by(Found_Species, 
                                                             Found_Species$Species, 
                                                             function (x) 
                                                               min(x$Lower, na.rm = TRUE), simplify = FALSE)))

Found_Species_Shorter$Species <- row.names(Found_Species_Shorter)
Found_Species_Shorter$Upper <- do.call(rbind, by(Found_Species, 
                                                 Found_Species$Species, function 
                                                 (x) max(x$Upper,na.rm = TRUE), simplify = FALSE))

Found_Species_Shorter <- do.call(data.frame,                      # Replace Inf in data by NA
                   lapply(Found_Species_Shorter,
                          function(x) replace(x, is.infinite(x), NA)))

Found_Species_Shorter[is.na(Found_Species_Shorter$Lower),]$Lower <- c(34,8,18,14,18)
Found_Species_Shorter[is.na(Found_Species_Shorter$Upper),]$Upper <- 8

mammal_phylo4 <- as(
  
  mammal_tree_2  , "phylo4")

tree4d <- phylo4d(mammal_phylo4 , tip.data=Found_Species_Shorter[,-2])


plot(tree4d)

W <- proxTips(tree4d , met="Abouheif")
moran.idx(tdata(tree4d , type="tip")$Upper, W)


Upper<- tdata(tree4d , type="tip")$Upper
sim <- replicate(1000, moran.idx(sample(Upper), W))
sim <- c(moran.idx(Upper , W), sim)

cat("\n=== p-value (right-tail) === \n")
pval <- mean(sim>=sim[1])


ung.abTests <- abouheif.moran(tree4d)


mammal_phylogeny_GG <- 
  ggtree::ggtree(mammal_tree_2 ) + geom_tiplab() + 
  theme(text = element_text(size = 7))
