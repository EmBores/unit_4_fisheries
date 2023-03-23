##EmB
##2023-03-23
##class notes

library(tidyverse)
library(palmerpenguins)
head(penguins)

pen_drop_na= penguins%>%
  drop_na()
head(pen_drop_na)

pen_num= pen_drop_na %>%
  select(ends_with("mm"), body_mass_g)
head(pen_num)
pen_meta= pen_drop_na %>%
  select(species, sex, island, year)
head(pen_meta)

#split into 2 different data frames- one w/ numbers, one w/ non numerics

#run the PCA
pen_pca= prcomp(pen_num, scale.=TRUE, center=TRUE) #scale normalizes all numeric numbers, center= pcs centered over 0

pen_pca
summary(pen_pca)
str(pen_pca) #structure
pen_pca$sdev

dim(pen_num)
head(pen_pca$x)
dim(pen_pca$x)

str(summary(pen_pca))
summary(pen_pca)$importance[2,]

#calculate proportion of variance from SDevs
(pen_pca$sdev)^2/sum(pen_pca$sdev^2)

#scree plot
plot(pen_pca)

pca_scree= data.frame(pc=c(1:4),
                      var=summary(pen_pca)$importance[2,])

ggplot(data=pca_scree, aes(x=pc, y=var))+
  geom_col()+
  geom_point()+
  geom_line()+
  theme_bw()+
  ggtitle("Scree plot")+
  ylab("proportion of variance explained")

# create biplot
head(pen_pca$x)
pen_pca_meta=cbind(pen_pca$x, pen_meta)
head(pen_pca_meta)

ggplot()+
  geom_point(aes(x=PC1, y=PC2, color=species, shape=sex), data=pen_pca_meta)+
  coord_fixed(ratio=1)

library(devtools)
install_github("vqv/ggbiplot")
library(ggbiplot)
ggbiplot(pen_pca, scale=1, groups=pen_meta$species, ellipse=TRUE, alpha=0)+
  geom_point(aes(color=pen_meta$species, shape=pen_meta$sex))+
  xlim(-3,3)+
  theme_bw()

pen_pca

#look at PC 3 and 4
ggbiplot(pen_pca, scale=1, group=pen_meta$species, ellipse=TRUE, choices=(3,4))










