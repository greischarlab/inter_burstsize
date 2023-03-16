###Creating a figure for the burst size comparison

library(here)
library(ggplot2)
library(grid)

mal_dat <- read.csv(here("Data","MALARIA_PAK_SPECIES.csv"))

#split data by orderhost
mal_dat_split <- split(mal_dat, mal_dat$OrderHost)

###Main burst size distribution 
mal_dat_gg <- ggplot(mal_dat, 
                    aes(x = Average,
                        fill = OrderHost))+ 
              geom_dotplot(
                    binwidth = 1,
                    size = 1)+
              facet_wrap(~OrderHost,ncol = 1)+
              scale_fill_manual(
                    guide = 'none',
                    values = c('avian' = '#59e3d5',
                             'mammal' = '#FFDE7D',
                             'reptile' = '#F6416C'),
                    labels = c('avian' = 'Avian',
                             'mammal' =' Mammal',
                             'reptile' = 'Reptile')) +
             scale_x_discrete(limits = c(seq(0,100,10))) +
             scale_y_continuous(limits = c(0,0.60), 
                                breaks = seq(0,0.6,0.2),
                                labels = seq(0,15,length=4))+
             xlab("Average burst size") +
             ylab("Species count") +
             coord_fixed(ratio = 25)+
             theme_classic() +
             theme(axis.text = element_text(size = 14,color = 'black'),
                   axis.title = element_text(size = 15,color = 'black' ),
                   legend.text = element_text(size = 12,color = 'black'),
                   strip.text = element_blank(),
                   panel.border = element_rect(fill = NA))

mal_dat_gg

ggsave(here("Figure","Data_Analysis","Raw","avg_burstsize_dist.pdf"),
       width = 5.5, height= 11, units='in')
