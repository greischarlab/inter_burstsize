###Data Analysis of the reptilian burst size###
library(here)
library(stringr)
library(elevatr)


###reptile trait_dat
reptile_traits_dat <- read.csv(here("Data","reptile_traits.csv"))
###host_dat 
malaria_hosts_dat <- read.csv(here("Data","MALARIA_PAK_HOSTS.csv"))
###parasite_dat
malaria_parasites_dat <- read.csv(here("Data", "MALARIA_PAK_SPECIES.csv"))
###weight data
reptile_weight <- read.csv(here("Data", "reptileweight_dat.csv"))
###RBC area
reptile_RBC <- read.csv(here("Data", "reptile_blood.csv"))



##reptiles
malaria_reptiles_dat <- subset(malaria_hosts_dat, malaria_hosts_dat$Group == 'reptile')
malaria_parasites_rep_dat <- subset(malaria_parasites_dat, malaria_parasites_dat$OrderHost == 'reptile')

malaria_parasites_rep_dat$Plasmodium.species<- str_replace_all(malaria_parasites_rep_dat$Plasmodium.species, " ", "")
malaria_parasites_dat$Plasmodium.species <-  str_replace_all(malaria_parasites_dat$Plasmodium.species, " ", "")
malaria_reptiles_dat$Parasite <- str_replace_all(malaria_reptiles_dat$Parasite, " ", "")
malaria_reptiles_dat$Parasite <- str_replace_all(malaria_reptiles_dat$Parasite, " ", "")

reptile_RBC$animal <- str_replace_all(reptile_RBC$animal, "_", " ")

###Splitting up the species name
malaria_reptiles_dat$species <- str_split_fixed(malaria_reptiles_dat$Species, " ", 2)[,2]
malaria_reptiles_dat$Genus <-str_split_fixed(malaria_reptiles_dat$Species, " ", 2)[,1]


###Getting parasite species...
Parasite_Data <- NULL

for (k in seq(1,nrow(malaria_reptiles_dat))){

   par_species <- unlist(str_split(malaria_reptiles_dat[k,]$Parasite,";"))

   parasite_species_dat <- NULL
   
   for (species in seq(1,length(par_species))){
     
     dat_interest <- subset(malaria_parasites_dat, 
                      malaria_parasites_dat$Plasmodium.species ==  par_species[[species]])[,c("Subgenus",
                                                                                              "Plasmodium.species",
                                                                                               "Average",  "Lower", "Upper",
                                                                                              "Duration")]
    
     dat_interest$Genus <- malaria_reptiles_dat[k,]$Genus
     dat_interest$species <- malaria_reptiles_dat[k,]$species
     
     parasite_species_dat[[species]] <- dat_interest
   }
     
     

   
Parasite_Data[[k]] <- do.call(rbind,parasite_species_dat)
   }
  

Parasite_Data_F <- do.call(rbind,(Parasite_Data))


###Getting data.... (missing 10 species...)
reptile_found_genus <- subset(reptile_traits_dat, reptile_traits_dat$Genus %in% 
                      malaria_reptiles_dat$Genus )

reptile_found_Species <- subset(reptile_found_genus, reptile_found_genus $epithet %in%
                                  malaria_reptiles_dat$species )


a<- left_join(Parasite_Data_F, reptile_found_Species, by= c('Genus' = 'Genus', 
                                                        'species' = 'epithet'))


a$Binomial <- paste0(a$Genus," ", a$species)
a <- left_join(a,reptile_weight, by = c("Binomial" ="Binomial"))
ab <- left_join(a, reptile_RBC, by = c("Binomial" = "animal"), multiple = 'all')
eef <- a[,c("Maximum.mass..log10.g..","Binomial")]
subset(eef, is.na(eef$Maximum.mass..log10.g..)==TRUE)

ggplot(a,
       aes(x = Maximum.mass..log10.g.., y= Lower, color = Subgenus)) +
  geom_jitter(size = 5) +
  scale_color_viridis(discrete = TRUE) + 
  facet_wrap(~Subgenus,scales = 'free') + 
  xlab("Log10 Mass (grams)") + 
  ylab("Lower limit of  burst size") + 
  theme_classic() + 
  theme(axis.text = element_text(color = 'black', size = 16),
        axis.title = element_text(color = 'black', size = 17))



ggplot(a,
       aes(x =(maximum.mean.Tb) , 
           y= Lower, color = Subgenus)) +
  geom_jitter(size = 5) +
  scale_color_viridis(discrete = TRUE) + 
  xlab("Average Temperature") + 
  ylab("Lower limit of  burst size") + 
  theme_classic() + 
  theme(axis.text = element_text(color = 'black', size = 16),
        axis.title = element_text(color = 'black', size = 17))



ggplot(ab,
       aes(x =as.factor(foraging_mode) , 
           y= Average, color = Subgenus)) +
  geom_jitter(size = 5) +
  scale_color_viridis(discrete = TRUE) + 
  xlab("RBC Area") + 
  ylab("Average") + 
  theme_classic() + 
  theme(axis.text = element_text(color = 'black', size = 16),
        axis.title = element_text(color = 'black', size = 17))

