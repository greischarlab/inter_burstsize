
library(here)
library(ape)
source(here("Code","Data_Analysis","05_phylogeny_full_combiner.R"))


###This is the mammal trait data that we are interested in as it has the 
###mass data.
Mammal_dat <- read.csv(here("Data","Mammal_trait.csv"))
Mammal_Tree <- keep.tip(consensus_Mammal_Tree, Mammal_MERGED_F$Species)
Mammal_Tree_Species <- sub( "_"," ",Mammal_Tree$tip.label,)

###This combines the trait data as well as the phylogenetic data
Mammal_Trait_Data <- subset(mammal_dat, mammal_dat$phylacine_binomial %in%
                              Mammal_Tree_Species)

###MISSING DATA
Mammal_Trait_Data_MISSING <- subset(Mammal_Tree_Species, !(Mammal_Tree_Species %in%
                                                             Mammal_dat$phylacine_binomial))


Mammal_Trait_Data_2 <- Mammal_Trait_Data[!duplicated(Mammal_Trait_Data$phylacine_binomial), ]

###We want the adult_mass_g
Mammal_Trait_Data_2_MASS <- Mammal_Trait_Data_2[,c("order","family","genus","phylacine_binomial",
                                                   "adult_mass_g")]

###We calculate the reticulocyte based on the mammal mass
Mammal_Trait_Data_2_MASS$reticulocyte <-((0.96 - 0.288 * log(Mammal_Trait_Data_2_MASS $adult_mass_g)))
Mammal_Trait_Data_2_MASS$phylacine_binomial <- sub(" ","_", Mammal_Trait_Data_2_MASS$phylacine_binomial)




ggplot(Mammal_Trait_Data_2_MASS, aes(x = log(adult_mass_g), y = exp(reticulocyte)))+
  geom_point(size =2)+
  xlab("Adult mass (g) (log)")+
  ylab("Reticulocytes per 100 RBC")+
  ggtitle("Reticulocytes per 100 RBC depending on mass")+
  theme_classic()+
  theme(axis.text = element_text(size = 14, color = 'black'),
        axis.title = element_text(size = 15, color = 'black'))

ggsave(here("Figure", "Supp_Retic_Mass_Scatterplot.pdf"), width = 5, height = 4,
       units = 'in')