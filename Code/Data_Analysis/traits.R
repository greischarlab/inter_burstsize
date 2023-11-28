library(dplyr)
library(readr)
library(here)
mammal_trait <- as.data.frame(list.files(here("Data","Mammal_dat"), full.names = TRUE) %>% 
  lapply(read_csv) %>% 
  bind_rows )

reptile_weight <- read.csv(here("Data", "reptileweight_dat.csv"))
bird_traits <- read.csv(here("Data", "bird_traits.csv"))


###host_dat 
malaria_hosts_dat <- read.csv(here("Data","MALARIA_PAK_HOSTS.csv"), encoding = "utf-8")
###parasite_dat
malaria_parasites_dat <- read.csv(here("Data", "MALARIA_PAK_SPECIES.csv"))

Full_Merged_Parasite_Data <- Parasite_Data_Collector(malaria_hosts_dat,malaria_parasites_dat )

Mammal_Full_Merged_Parasite_Data <- subset(Full_Merged_Parasite_Data, Full_Merged_Parasite_Data$Group == 'mammal')
Reptile_Full_Merged_Parasite_Data <- subset(Full_Merged_Parasite_Data, Full_Merged_Parasite_Data$Group == 'reptile')
Avian_Full_Merged_Parasite_Data <- subset(Full_Merged_Parasite_Data, Full_Merged_Parasite_Data$Group == 'avian')

Mammal_dat <- left_join(Mammal_Full_Merged_Parasite_Data, 
                    mammal_trait,
                  by = c('Species' = 'iucn2020_binomial'))

Reptile_dat <- left_join(Reptile_Full_Merged_Parasite_Data ,
                         reptile_weight,
                        by = c('Species' = 'Binomial'))
                         
Avian_dat <- left_join(Avian_Full_Merged_Parasite_Data ,
                        bird_traits ,
                         by = c('Species' = 'Species1'))



Mammal_dat_mass <- unique(Mammal_dat[,c(1:10,13)])
colnames(Mammal_dat_mass)[11] <- 'mass'
Reptile_dat_mass <- Reptile_dat[,c(1:10,14)]
colnames(Reptile_dat_mass)[11] <- 'mass'
Reptile_dat_mass$mass <- 10^Reptile_dat_mass$mass

Avian_dat_mass <- Avian_dat[,c(1:10,30)]
colnames(Avian_dat_mass)[11] <- 'mass'

Total_Mass <- rbind(Mammal_dat_mass, Reptile_dat_mass, Avian_dat_mass)

ggplot(Total_Mass, aes(x = log10(mass), y = Average, color = Group
                         )) + geom_point(size =3) + 
  geom_segment(aes( x =  log10(mass), xend = log10(mass), y = Lower, yend = Upper)) + 
  facet_wrap(~Group,ncol = 1) + theme_classic() + 
  scale_color_viridis(discrete = TRUE, option = 'turbo')



try_mass <- subset(Total_Mass, Total_Mass$Plasmodium.species %in%
         parasite_sub$Plasmodium.species)
