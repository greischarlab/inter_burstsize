###REPTILE PHYLOGENY- the reptiles that appear 

reptile_tree <-  read.tree(here(file='Data','reptile_phylogeny.txt'))
Tree_Label_Reptiles <- reptile_tree$tip.label

reptile_tree$tip.label<- str_replace_all( reptile_tree$tip.label, "_", " ")

###malaria_dat <- 
found_rep_species <- subset(Reptile_dat,Reptile_dat$Species %in% reptile_tree$tip.label)
notfound_rep_species <- subset(Reptile_dat,!(Reptile_dat$Species %in% reptile_tree$tip.label))$species

nrow(found_rep_species)
nrow(Reptile_dat) #213 - 182

reptile_names_phylogeny <- write.csv(
  reptile_tree$tip.label, file = 'reptile_name.csv')
