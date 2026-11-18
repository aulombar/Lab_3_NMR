u<-read.csv("C:\\Users\\Austin\\Desktop\\Lab3\\Unknown\\Unknown_peak_table.csv", row.names=NULL)
a<-read.csv("C:\\Users\\Austin\\Desktop\\Lab3\\Arabica\\Arabica_peak_table.csv", row.names=NULL)
r<-read.csv("C:\\Users\\Austin\\Desktop\\Lab3\\Robusta\\Robusta_peak_table.csv", row.names=NULL)
c<-rbind(u, a, r)
c

library(stats)
library(ggplot2)

Species <- c[[1]]
num <- c[, -1]
num <- num[, sapply(num, is.numeric)]

ok <- complete.cases(num)
num <- num[ok, ]
Species <- Species[ok]

pca <- prcomp(num, scale. = TRUE)

var_exp <- round(100 * pca$sdev^2 / sum(pca$sdev^2), 1)
scores <- data.frame(pca$x, Species = Species)

ggplot(scores, aes(PC1, PC2, color = Species)) +
  geom_point(size = 3) +
  stat_ellipse(level = 0.95) +        # needs at least ~4 points per group
  labs(x = paste0("PC1 (", var_exp[1], "%)"),
       y = paste0("PC2 (", var_exp[2], "%)")) +
  theme_minimal()



# Extract loadings for PC1 and PC2
loadings <- pca$rotation[, 1:2]

# Display numerical values
print(loadings)

# Plot loadings
dev.new()
plot(
  loadings[, 1],
  loadings[, 2],
  xlim = c(-1.1, 1.1),
  ylim = c(-1.2, 1.2),
  xlab = "PC1 loading",
  ylab = "PC2 loading",
  pch = 19,
  main = "PCA Loading Plot"
)

abline(h = 0, v = 0, lty = 2)

text(
  loadings[, 1],
  loadings[, 2],
  labels = rownames(loadings),
  pos = 3
)

  