library(tidyverse)
library(patchwork)
library(ggrepel)

generate_dataset <- function() {
  a <- rnorm(1, mean=1)
  k <- rnorm(1, mean=1, sd=2)
  d <- k*a
  dataset <<- tibble(genotype       = c("B[1]*B[1]",
                                        "B[1]*B[2]",
                                        "B[2]*B[2]"),
                     gene_content   = c(0, 1, 2),
                     LynchWalsh     = c(0, (1+k)*a, 2*a),
                     FalconerMackay = c(-a, d, a))
}

generate_dataset()
(dataset %>%
    pivot_longer(c(LynchWalsh, FalconerMackay)) %>%
    ggplot(aes(x=genotype, y=value, color=name)) +
    geom_line(aes(group=name)) +
    geom_line(aes(group=genotype), color="black", linetype="dashed") +
    geom_point() +
    scale_color_brewer(type="qual", palette=6) +
    scale_x_discrete(labels=parse(text=dataset$genotype))+
    labs(x="Genotype", y="Genotipic value")) +
  (dataset %>%
     ggplot(aes(x=LynchWalsh, y=FalconerMackay)) +
     geom_line() +
     geom_point() +
     geom_text_repel(aes(label=genotype), size=3,
                     parse = T, segment.color = NA)) &
  plot_layout(guides = "collect") &
  theme_bw() &
  theme(legend.position = "bottom",
        legend.direction = "vertical",
        legend.title=element_blank())
