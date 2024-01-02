###This cleans up the phylogenetic trees and makes it 
###ready for data-analysis - run this entire script
###
library(here)
library(ape)
library(phylobase)
library(adephylo)
library(stringr)
library(TreeTools)

source(here("Code","Functions","Parasite_Host_Unifier.R"))

###Host data necessary
Malaria_Host_Dat <- read.csv(here("Data", "MALARIA_PAK_HOSTS.csv"))
Malaria_Parasite_Dat <- read.csv(here("Data", "MALARIA_PAK_SPECIES.csv"))

#############
###Reptile###
#############
REP_Host_Dat <- subset(Malaria_Host_Dat,
                          Malaria_Host_Dat$Group == 'reptile')
REP_Phylo <- read.nexus(here('Data',"Reptile_Dat","reptile_phylo.nex"))

#############
###Avian####
#############
AVE_Host_Dat <- subset(Malaria_Host_Dat,
                         Malaria_Host_Dat$Group == 'avian')
AVE_Phylo <- read.nexus(here("Data", "Avian_dat", 'avian_phylo.nex'))

#############
###Mammal###
#############
MAM_Host_Dat <- subset(Malaria_Host_Dat,
                         Malaria_Host_Dat$Group == 'mammal')
MAM_Phylo <- read.nexus(here("Data", "Mammal_dat", 'mammal_phylo.nex'))

REP_Host_Dat$Species <- sub(" ", "_", REP_Host_Dat$Species)
AVE_Host_Dat$Species <- sub(" ", "_", AVE_Host_Dat$Species)
AVE_Host_Dat$Species[94] <- "Pulsatrix_koeniswaldiana"
MAM_Host_Dat$Species <- sub(" ", "_", MAM_Host_Dat$Species)


###Note that I already subsetted the information necessary for the 
###mammalian and avian phylogeny trees

AVE_Merged <- Parasite_Data_Collector(AVE_Host_Dat,
                                       Malaria_Parasite_Dat)

MAM_Merged <- Parasite_Data_Collector(MAM_Host_Dat,
                                       Malaria_Parasite_Dat)

REP_Merged <- Parasite_Data_Collector(REP_Host_Dat,
                                      Malaria_Parasite_Dat)

###As there can be multiple parasite species that infect a single host,
###this function choose the greater upper burst size across all species 
###There is also a chance there is no upper limit, so I have decided to 
###omit the data
AVE_Merged_F <- na.omit(data.frame(Upper = do.call(rbind, by(AVE_Merged, 
                                                  AVE_Merged$Species, 
                                                  function (x) max(na.omit(x$Upper)), 
                                                  simplify = FALSE))))
AVE_Merged_F$Species <- rownames(AVE_Merged_F)
AVE_Merged_F <- AVE_Merged_F[is.finite(AVE_Merged_F$Upper),]

###Mammal
MAM_Merged_F<- na.omit(data.frame(Upper = do.call(rbind, by(MAM_Merged, 
                                                  MAM_Merged$Species, 
                                                  function (x) max(na.omit(x$Upper)), 
                                                  simplify = FALSE))))

MAM_Merged_F$Species <- rownames(MAM_Merged_F)
MAM_Merged_F <- MAM_Merged_F[is.finite(MAM_Merged_F$Upper),]

REP_Merged_F <- na.omit(data.frame(Upper = do.call(rbind, by(REP_Merged, 
                                                            REP_Merged$Species, 
                                                            function (x) 
                                                            max(na.omit(x$Upper)), 
                                                            simplify = FALSE))))
REP_Merged_F$Species <- rownames(REP_Merged_F)
REP_Merged_F <- REP_Merged_F[is.finite(REP_Merged_F$Upper),]

###Find species not included
AVE_NOT_FOUND_SPECIES <- subset(AVE_Merged_F, !(rownames(AVE_Merged_F)
         %in% AVE_Phylo [[1]]$tip.label))

MAM_NOT_FOUND_SPECIES <- subset(MAM_Merged_F, !(rownames(MAM_Merged_F)
                                                %in% MAM_Phylo [[1]]$tip.label))

REP_NOT_FOUND_SPECIES <- subset(REP_Merged_F, !(rownames(REP_Merged_F)
                                                %in% REP_Phylo[[1]]$tip.label))

###Avian and mammal have distribution of trees
consensus_Avian_Tree <- consensus(AVE_Phylo, p = 1, check.labels = TRUE, rooted = TRUE)
consensus_Mammal_Tree <- consensus(MAM_Phylo, p = 1, check.labels = TRUE, rooted = TRUE)
consensus_Reptile_Tree <-   consensus(REP_Phylo, p = 1, check.labels = TRUE, rooted = TRUE)

###Adding missing species###
###########
###Avian###
###########
consensus_Avian_Tree<- AddTip(
  consensus_Avian_Tree,
  where = 53, "Acrocephalus_scirpaceus")

consensus_Avian_Tree<- AddTip(
  consensus_Avian_Tree,
  where = 112, "Accipiter_brevipes")

############
###Mammal###
############
consensus_Mammal_Tree<- AddTip(
  consensus_Mammal_Tree,
  where = 35, "Cebus_imitator")

#############
###Reptile###
#############

consensus_Reptile_Tree<- AddTip(
  consensus_Reptile_Tree,
  where = 48, "Agama_mossambica")


Avian_MERGED_F <- subset(AVE_Merged_F, 
                         AVE_Merged_F$Species %in% consensus_Avian_Tree$tip.label)

consensus_Avian_Tree_FINAL <- keep.tip(consensus_Avian_Tree,
                                       Avian_MERGED_F$Species)

Mammal_MERGED_F <- subset(MAM_Merged_F, 
                         MAM_Merged_F$Species %in% consensus_Mammal_Tree$tip.label)

consensus_Mammalian_Tree_FINAL <- keep.tip(consensus_Mammal_Tree,
                                           Mammal_MERGED_F$Species)

Reptile_MERGED_F <- subset(REP_Merged_F, 
                          REP_Merged_F$Species %in% consensus_Reptile_Tree$tip.label)

consensus_Reptile_Tree_FINAL <- keep.tip(consensus_Reptile_Tree,
                                           Reptile_MERGED_F$Species)

plot(consensus_Reptile_Tree_FINAL , "f", FALSE)
#########################
###I NEED A SUPER TREE###
#########################

tip.labels <- c("mam", "birds", "squam")

edge <- matrix(c(4,1,
                 4,5,
                 5,2,
                 5,3), 
               byrow=TRUE, ncol=2)
edge.length=edge.length<- c(1,1,1,1)

Nnode <-2
ordertree <- list(edge=edge, Nnode=Nnode, tip.label=tip.labels )
class(ordertree) <- 'phylo'
plot(ordertree)
tiplabels()
nodelabels()
edgelabels(ordertree$edge.length, bg="black", col="white", font=2)

###
tree_list <- list(squam=consensus_Reptile_Tree, 
                  birds=consensus_Avian_Tree, 
                  mam=consensus_Mammal_Tree)

class(tree_list) <- "multiPhylo"




rep_1 <- bind.tree(x=ordertree, y=consensus_Reptile_Tree, 
                   where = 3, interactive = FALSE)

ave_2 <- bind.tree(x=rep_1, y=consensus_Avian_Tree, 
                   where = 2, interactive = FALSE)

mam_3 <- bind.tree(x=ave_2 , y=consensus_Mammal_Tree, 
                   where = 1, interactive = FALSE)

FULL_DAT <- rbind(Avian_MERGED_F,
                  Mammal_MERGED_F,
                  Reptile_MERGED_F)

FULL_ORDER_FINAL <- keep.tip(mam_3 ,FULL_DAT$Species)



FULL_Merged_Phylogeny<- phylo4d(
  FULL_ORDER_FINAL , 
  tip.data=FULL_DAT[,1],
  match.data=TRUE)

Reptile_Data_Merged_Phylogeny <- phylo4d(
  keep.tip(consensus_Reptile_Tree, Reptile_MERGED_F$Species),
  tip.data =  Reptile_MERGED_F[,1],
  match.data = TRUE
)

Mammal_Data_Merged_Phylogeny <- phylo4d(
  keep.tip(consensus_Mammal_Tree, Mammal_MERGED_F$Species),
  tip.data =  Mammal_MERGED_F[,1],
  match.data = TRUE
)

Avian_Data_Merged_Phylogeny <- phylo4d(
  keep.tip(consensus_Avian_Tree, Avian_MERGED_F$Species),
  tip.data =  Avian_MERGED_F[,1],
  match.data = TRUE
)


plot(Reptile_Data_Merged_Phylogeny)

plot(FULL_Merged_Phylogeny)
