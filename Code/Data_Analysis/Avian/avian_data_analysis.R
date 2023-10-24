###Data Analysis of the reptilian burst size###
library(here)
library(stringr)
library(elevatr)

###bird trait data
bird_traits <- read.csv(here("Data", "bird_traits.csv"))
###host_dat 
malaria_hosts_dat <- read.csv(here("Data","MALARIA_PAK_HOSTS.csv"), encoding = "utf-8")
###parasite_dat
malaria_parasites_dat <- read.csv(here("Data", "MALARIA_PAK_SPECIES.csv"))



Avian_Parasite_Data <- Parasite_Data_Collector(malaria_hosts_dat, malaria_parasites_dat, 'avian')

Avian_Parasite_Data_Trait <- left_join(Avian_Parasite_Data,bird_traits, by = c("Species" = "Species1"))
          
subset(Avian_Parasite_Data_Trait, is.na(Avian_Parasite_Data_Trait$Sequence) == TRUE)$Species

ggplot(
  Avian_Parasite_Data, aes(y = Species, x = Average)) + 
  geom_point() +  
  geom_segment(aes(x = Lower, y= Species, yend = Species, xend = Upper)) + 
  facet_wrap(~Genus, scales = 'free_y')


