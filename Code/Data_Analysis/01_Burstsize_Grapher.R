library(ggplot2)
library(here)
###making the large figure of species and average/low/upper
###burst size

mal_dat <- read.csv(here("Data","MALARIA_PAK_SPECIES.csv"))

###circumflexum_1 get rid of (which is row 50)

mal_dat[mal_dat$Plasmodium.species=='circumflexum_2',] <- NA
###Groupped_Data
mal_avian <- subset(mal_dat, mal_dat$OrderHost == 'avian')
mal_mammal<- subset(mal_dat, mal_dat$OrderHost == 'mammal')
mal_reptile <- subset(mal_dat, mal_dat$OrderHost == 'reptile')
###Reordering

###Avian###
avian_species_order <- mal_avian$Plasmodium.species[order(mal_avian$Order)]
mal_avian$Plasmodium.species <- factor(mal_avian$Plasmodium.species,
                                       levels = avian_species_order)

###Mammal###
mam_species_order <- mal_mammal$Plasmodium.species[order(mal_mammal$Order)]
mal_mammal$Plasmodium.species <- factor(mal_mammal$Plasmodium.species,
                                       levels = mam_species_order)

###Reptile###
reptile_species_order <- mal_reptile$Plasmodium.species[order(mal_reptile$Order)]
mal_reptile$Plasmodium.species <- factor(mal_reptile$Plasmodium.species,
                                        levels = reptile_species_order )


###AVIAN PLOT###
mal_avian_GG <-
  ggplot(mal_avian, 
        aes(x = Average, 
            y = Plasmodium.species, 
            fill=Forms)) +
   geom_segment(data=mal_avian,
         aes(x = Lower, 
             xend = Upper, 
             y = Plasmodium.species,
             yend = Plasmodium.species))+
   geom_point(shape = 21, 
              size = 3) + 
   xlab("Burst size")+
   ylab("Plasmodium species")+
   scale_x_continuous(breaks=seq(0,40,5))+
   scale_y_discrete(limits = rev) + 
   scale_fill_viridis(discrete = TRUE)+
   theme_classic()+
   theme(legend.position="none",
         axis.text = element_text(size = 11.5, 
                                  face = 'italic'),
         axis.title = element_text(size = 14))

ggsave(here("Figure", "Data_Analysis", 
            "Raw", "01_Avian_burstsize_host_order.pdf"),
       width = 5, height = 11, units = 'in')
 
###MAMMAL PLOT###
mal_mammal_GG <-
  ggplot(mal_mammal, 
         aes(x = Average, 
             y = Plasmodium.species, 
             fill=Forms)) +
  geom_segment(data=mal_mammal,
               aes(x = Lower, 
                   xend = Upper, 
                   y = Plasmodium.species,
                   yend = Plasmodium.species))+
  geom_point(shape = 21, 
             size = 3) + 
  xlab("Burst size")+
  ylab("Plasmodium species")+
  scale_x_continuous(breaks=seq(0,40,5))+
  scale_y_discrete(limits = rev) + 
  scale_fill_viridis(discrete = TRUE)+
  theme_classic()+
  theme(legend.position="none",
        axis.text = element_text(size = 11.5, 
                                 face = 'italic'),
        axis.title = element_text(size = 14))

ggsave(here("Figure", "Data_Analysis", 
            "Raw", "02_Mammal_burstsize_host_order.pdf"),
       width = 5, height = 11, units = 'in')

###Reptile###
###Too long so going to split into two graphs (cut in half)

mal_reptile$facet <- 2
mal_reptile$facet[1:70] <- 1
###Reptile PLOT###
mal_reptile_GG <-
  ggplot(mal_reptile, 
         aes(x = Average, 
             y = Plasmodium.species, 
             fill = Forms)) +
  geom_segment(data=mal_reptile,
               aes(x = Lower, 
                   xend = Upper, 
                   y = Plasmodium.species,
                   yend = Plasmodium.species))+
  geom_point(shape = 21, 
             size = 3) + 
  facet_wrap(~facet, scales = 'free_y') + 
  xlab("Burst size") +
  ylab("Plasmodium species")+
  scale_x_continuous(breaks=seq(0,300,10))+
  scale_y_discrete(limits = rev) + 
  scale_fill_viridis(discrete = TRUE)+
  theme_classic()+
  theme(legend.position="none",
        axis.text = element_text(size = 11.5, 
                                 face = 'italic'),
        axis.title = element_text(size = 14),
        strip.text = element_blank())

ggsave(here("Figure", "Data_Analysis", 
            "Raw", "03_Reptile_burstsize_host_order.pdf"),
       width = 13.5, height = 11, units = 'in')
