
ung.abTests <- abouheif.moran(FULL_Merged_Phylogeny, nrepet = 5000)
W1 <- proxTips(FULL_Merged_Phylogeny,method="oriAbouheif")
abouheif.moran(dom,W1)
FULL_Merged_Phylogeny$data$Upper
myTests <- abouheif.moran(FULL_Merged_Phylogeny)

W1 <- proxTips(FULL_Merged_Phylogeny,method="oriAbouheif")
ung.abTests_2<- abouheif.moran(FULL_Merged_Phylogeny, W = W1)

plot(W1)
W_melted <- melt(W1)
W_melted$Var1 <- factor(W_melted$Var1 , levels = c(FULL_ORDER_FINAL$tip.label ))
W_melted$Var2 <- factor(W_melted$Var2 , levels = c(FULL_ORDER_FINAL$tip.label ))

ggplot(W_melted, aes(x = Var1, y = Var2, fill = log10(value)))+ geom_tile() + 
  scale_fill_viridis(option = 'turbo')+
  theme(axis.text.x = element_text(angle= 90))

