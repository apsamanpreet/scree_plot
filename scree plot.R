library(ggplot2)

pcdata <- data [,-1]
pca_result <- prcomp(pcdata, scale = TRUE)

variance_explained <- pca_result$sdev^2 / sum(pca_result$sdev^2)

#scree plot
scree_data <- data.frame(
  PC = 1:length(variance_explained),
  Variance = variance_explained)

ggplot(scree_data, aes(x = PC, y = Variance)) +
  geom_point(shape = 17, color = "blue", size = 5) +
  geom_line(color = "red", size = 1) +
  geom_text(aes(label = round(Variance, 2)), vjust = +1.5, color = "black", size = 3.5) +
  labs(title = "", x = "Principal Component Number", y = "Eigen Values") +
  theme_minimal() +
  theme(
    text = element_text(family = "Times New Roman", size = 12),
    axis.line = element_line(color = "black", size = 0.5),
    axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.5),
    legend.position = "none",
    panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
    plot.background = element_rect(fill = "white"), 
    panel.background = element_rect(fill = "white")) +
labs(
  title = "",
  x = "Principal Component (PC)",
  y = "Eigen Value"
)
