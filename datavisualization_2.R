library(ggplot2)
library(ggrepel)
library(ggpubr)


datum=read.csv("MycotoxinData.csv",na.strings="na")
datum$Treatment=as.factor(datum$Treatment)


##set the color palette
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", 
                "#F0E442", "#0072B2", "#D55E00", "#CC79A7")


 
#1 Boxplot of DON by treatment
plot_DON<-ggplot(datum, aes(x = Treatment, y = DON, fill = Cultivar)) +
  geom_boxplot(alpha=0.6) + 
  geom_jitter(aes(color = Cultivar), width = 0.2, alpha = 0.6) + #b transparency of jitter point
  scale_fill_manual(values = c(cbbPalette[[6]], cbbPalette[[4]])) +
  scale_color_manual(values = c(cbbPalette[[6]], cbbPalette[[4]])) +
  labs(y = "DON (ppm)", x = "") +#c
  theme_classic() +#d
  facet_wrap(~Cultivar)#e
plot_DON

#2 Ensure Treatment is a factor with specified order
datum$TreatmentOrder <- factor(datum$Treatment, levels = c("NTC", "Fg", "Fg + 37", "Fg + 40", "Fg + 70"))

plot_order<- ggplot(datum, aes(x = TreatmentOrder, y = DON, fill = Cultivar)) +
  geom_boxplot(alpha=0.6) + 
  geom_jitter(aes(color = Cultivar), width = 0.2, alpha = 0.6) + #b transparency of jitter point
  scale_fill_manual(values = c(cbbPalette[[6]], cbbPalette[[4]])) +
  scale_color_manual(values = c(cbbPalette[[6]], cbbPalette[[4]])) +
  labs(y = "DON (ppm)", x = "") +#c
  theme_classic() +#d
  facet_wrap(~Cultivar)#e
plot_order

#3 3.	5pts. Change the y-variable to plot X15ADON and MassperSeed_mg. 
#The y-axis label should now be “15ADON” and “Seed Mass (mg)”. 
#Save plots made in questions 1 and 3 into three separate R objects.

plot_15ADON <- ggplot(datum, aes(x = Treatment, y = X15ADON, fill = Cultivar)) +
  geom_boxplot(alpha = 0.6) +
  geom_jitter(aes(color = Cultivar), width = 0.2, alpha = 0.6) +
  scale_fill_manual(values = c(cbbPalette[[6]], cbbPalette[[4]])) +
  scale_color_manual(values = c(cbbPalette[[6]], cbbPalette[[4]])) +
  labs(y = "15ADON", x = "") +
  theme_classic() +
  facet_wrap(~Cultivar)

plot_15ADON

plot_SeedMass <- ggplot(datum, aes(x = Treatment, y = MassperSeed_mg, fill = Cultivar)) +
  geom_boxplot(alpha = 0.6) +
  geom_jitter(aes(color = Cultivar), width = 0.2, alpha = 0.6) +
  scale_fill_manual(values = cbbPalette) +
  scale_color_manual(values = cbbPalette) +
  labs(y = "Seed Mass (mg)", x = "") +
  theme_classic() +
  facet_wrap(~Cultivar)


plot_SeedMass

# 4. Arrange the three plots in a single figure
combined_plot <- ggarrange(plot_DON, plot_15ADON, plot_SeedMass, 
                           labels = c("A", "B", "C"),
                           ncol = 3, nrow = 1, common.legend = TRUE,legend="right")
combined_plot

# 5. Add pairwise comparisons using t-tests
plot_DON_ttest <- plot_DON + geom_pwc(aes(group=Treatment),method = "t.test")
plot_15ADON_ttest <- plot_15ADON + geom_pwc(aes(group=Treatment),method = "t.test")
plot_SeedMass_ttest <- plot_SeedMass + geom_pwc(aes(group=Treatment),method = "t.test")
combined_plot_ttest <- ggarrange(plot_DON_ttest, plot_15ADON_ttest, plot_SeedMass_ttest, 
                                 labels = c("A", "B", "C"),
                                 ncol = 3, nrow = 1, common.legend = TRUE, legend = "right")


combined_plot_ttest

