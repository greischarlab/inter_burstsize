library(here)
mammal_dat <- read.csv(here("Data","Mammal_trait.csv"))
                       
Mammal_Tree <- keep.tip(consensus_Mammal_Tree, Mammal_MERGED_F$Species)

Mammal_Tree_Species <- sub( "_"," ",Mammal_Tree$tip.label,)

Mammal_Trait_Data <- subset(mammal_dat, mammal_dat$phylacine_binomial %in%
         Mammal_Tree_Species)

###MISSING DATA
Mammal_Trait_Data_MISSING<- subset(Mammal_Tree_Species, !(Mammal_Tree_Species %in%
                                     mammal_dat$phylacine_binomial))


Mammal_Trait_Data_2 <- Mammal_Trait_Data[!duplicated(Mammal_Trait_Data$phylacine_binomial), ]

#Myoncteris angolensis is synonymous with Lissonycteris angolensis
#Cebus imitator is synonymous with Cebus capucinus
Mammal_Trait_Data_2<- 
  #rbind(Mammal_Trait_Data_2,
  #          subset(mammal_dat,mammal_dat$phylacine_binomial))
  #                 %in%
   #                                       c("Lissonycteris angolensis",
    #                                                 "Cebus capucinus")))
Mammal_Trait_Data_2 <- Mammal_Trait_Data[!duplicated(Mammal_Trait_Data$phylacine_binomial), ]

Mammal_Trait_Data_2_MASS <- Mammal_Trait_Data_2[,c("order","family","genus","phylacine_binomial",
                              "adult_mass_g")]
  

Mammal_Trait_Data_2_MASS$reticulocyte = ((.96 - .288 * log(Mammal_Trait_Data_2_MASS $adult_mass_g)))

Mammal_Trait_Data_2_MASS$phylacine_binomial <- sub(" ","_", Mammal_Trait_Data_2_MASS$phylacine_binomial)

plot(log(Mammal_Trait_Data_2_MASS$adult_mass_g ),
     exp(Mammal_Trait_Data_2_MASS$reticulocyte),
     xlab = "Adult Mass (g) (log)",
     ylab = "Reticulocytes per 100 RBC",
     type = 'p',
     main = 'Increasing mass leads to less reticulocytes')


Try <- merge(Mammal_MERGED_F,
          Mammal_Trait_Data_2_MASS, by.x = "Species", by.y = "phylacine_binomial")
rownames(Try)<- Try $Species
rownames(Try)[]

eep <- keep.tip(consensus_Mammal_Tree, Try $Species)
Mammal_Data_Merged_Phylogeny_Trait <- phylo4d(
  keep.tip(consensus_Mammal_Tree, Try $Species),
  tip.data = Try[,c("Upper","reticulocyte")],
  match.data = TRUE
)

pglsModel <- gls(Upper ~ reticulocyte,
                 data = Try, method = "ML")
summary(pglsModel)
