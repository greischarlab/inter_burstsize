library(ape)
library(here)


###malaria
parasite_dat <- read.csv(here("Data", "MALARIA_PAK_SPECIES.csv"))

rename_labels <- function(tree, vec) {
  tree$tip.label <- vec
  return(tree)
}

bigtree <-  read.nexus(here(file='Data','phylogeny.tre'))

###1-20, 37-38,54-59
Non_Plas_species <- bigtree$tip.label[c(1:20,28:29,37:39,54:59)]
Plas_species <-  bigtree$tip.label[-c(1:20,28:29,37:39,54:59)]

new_label <- c("azurophilum", "chiricahuae", "coatneyi","cyclopsi","floridense","fragile","gaboni",
   "lacertiliae","leucocytica", "mackerrasae","malariae", 'mexicanum','minuoviridae',
  'ovale', 'berghei', 'chabaudi','cynomolgi','falciparum','gallinaceum','giganteum',
  'inui', 'juxtanucleare','knowlesi','reichenowi', 'relictum','vinckei','vivax','yoelii')


parasite_sub <- subset(parasite_dat, parasite_dat$Plasmodium.species %in% new_label)

ae<- drop.tip(bigtree, Non_Plas_species)

renamed_tree <- rename_labels(tree = ae, vec = new_label)


parasite_sub$Plasmodium.species

bm <-corBrownian(1, renamed_tree)

modelo1<-gls(
Upper~ mass, data=
  na.omit(try_mass) , correlation=bm)
summary(modelo1)
