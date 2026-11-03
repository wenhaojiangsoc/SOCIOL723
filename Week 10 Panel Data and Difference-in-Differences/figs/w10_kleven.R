## Week 10 -- Kleven, Landais, and Sogaard (2019) Figure 1 A-B, redrawn in the deck's style.
## The estimates are not tabulated in the paper: values below are READ OFF their Figure 1
## (accepted manuscript, 2018; figs/kleven_fig1.png), accurate to about +/- 0.01.
## Long-run penalties printed in the paper: earnings 0.194, hours 0.097.
##   Rscript figs/w10_kleven.R   (from the week folder; writes figs/kleven_es.pdf)
library(ggplot2)
k <- -5:10
v <- rbind(
  data.frame(panel = "A: Earnings", parent = "mothers", k = k,
             est = c(-.068, -.047, -.022, .004, 0, -.258, -.308, -.223, -.258, -.261, -.243, -.239, -.238, -.236, -.234, -.230)),
  data.frame(panel = "A: Earnings", parent = "fathers", k = k,
             est = c(-.047, -.033, -.020, -.008, 0, -.013, -.009, -.005, -.008, -.012, -.015, -.020, -.024, -.027, -.029, -.032)),
  data.frame(panel = "B: Hours worked", parent = "mothers", k = k,
             est = c(-.052, -.028, -.010, .002, 0, -.120, -.182, -.088, -.115, -.116, -.100, -.095, -.093, -.090, -.088, -.083)),
  data.frame(panel = "B: Hours worked", parent = "fathers", k = k,
             est = c(-.028, -.022, -.012, -.004, 0, .003, .008, .013, .013, .013, .013, .012, .011, .011, .012, .013)))
lr <- data.frame(panel = c("A: Earnings", "B: Hours worked"),
                 lab = c("long-run penalty 0.194", "long-run penalty 0.097"))
blue <- rgb(1, 33, 105, maxColorValue = 255); orange <- rgb(200, 78, 0, maxColorValue = 255)
p <- ggplot(v, aes(k, est, colour = parent)) +
  geom_hline(yintercept = 0, colour = "gray55", linewidth = 0.3) +
  geom_vline(xintercept = -0.5, colour = "gray55", linetype = 2, linewidth = 0.3) +
  geom_line(linewidth = 0.7) + geom_point(size = 1.6) +
  geom_text(data = lr, aes(x = 10, y = -0.42, label = lab), inherit.aes = FALSE,
            hjust = 1, family = "Times", size = 4.3) +
  annotate("text", x = -0.3, y = 0.08, label = "first birth", hjust = 0, family = "Times", size = 4.3,
           colour = "gray35") +
  scale_colour_manual(values = c(mothers = orange, fathers = blue), name = NULL) +
  scale_x_continuous(breaks = seq(-5, 10, 5)) +
  coord_cartesian(ylim = c(-0.45, 0.1)) +
  facet_wrap(~ panel) +
  labs(x = "years since first birth (k)", y = "relative to k = -1") +
  theme_minimal(base_family = "Times", base_size = 13) +
  theme(panel.grid.minor = element_blank(), strip.text = element_text(size = 13, hjust = 0),
        legend.position = "right", legend.text = element_text(size = 13))
ggsave("figs/kleven_es.pdf", p, width = 8.6, height = 3.2, device = pdf, family = "Times")
