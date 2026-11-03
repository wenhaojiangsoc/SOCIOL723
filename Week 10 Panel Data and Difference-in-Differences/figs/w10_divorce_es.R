## Week 10 -- five event studies on the divorce data, one panel each (slide 'The Event Studies, Side by Side').
## Same sample as figs/w10_divorce.R section 4: drop the 9 always-treated states; k = year - adoption year,
## window k = -10..15; never-treated states (divyear 2000) are controls. SEs clustered by state.
##   Rscript figs/w10_divorce_es.R   (from the week folder; writes figs/divorce_es5.pdf)
suppressMessages({library(fixest); library(did); library(didimputation); library(ggplot2)})
data(divorce, package = "bacondecomp")
d <- subset(divorce, sex == 2 & divyear >= 1964)[, c("st", "year", "divyear", "suiciderate_jag")]
d$y <- d$suiciderate_jag * 1e6
d$D <- as.integer(d$year >= d$divyear)
d$id <- as.integer(factor(d$st))
d$g0 <- ifelse(d$divyear == 2000, 0, d$divyear)                    # 0 = never treated (did, didimputation)
d$kb <- pmin(pmax(d$year - d$divyear, -10), 15); d$kb[d$divyear == 2000] <- -1000
d$coh <- ifelse(d$divyear == 2000, 10000, d$divyear)

## 1. TWFE event study
m1 <- feols(y ~ i(kb, ref = c(-1, -1000)) | st + year, d, cluster = ~st)
tw <- data.frame(k = as.numeric(sub("kb::", "", names(coef(m1)))), est = coef(m1), se = se(m1))
## 2. Sun-Abraham (fixest::sunab)
m2 <- feols(y ~ sunab(coh, year, ref.p = c(-1, -10000)) | st + year, d, cluster = ~st)
sa <- data.frame(k = as.numeric(sub("year::", "", names(coef(m2)))), est = coef(m2), se = se(m2))
## 3. Callaway-Sant'Anna (did::att_gt + aggte), never-treated controls, universal base
cs <- att_gt(yname = "y", tname = "year", idname = "id", gname = "g0", data = d,
             control_group = "nevertreated", base_period = "universal", bstrap = FALSE, cband = FALSE)
ce <- aggte(cs, type = "dynamic", min_e = -10, max_e = 15, bstrap = FALSE, cband = FALSE)
csd <- data.frame(k = ce$egt, est = ce$att.egt, se = ce$se.egt)
## 3b. Callaway-Sant'Anna, not-yet-treated controls (never treated + states adopting after t), universal base
cs2 <- att_gt(yname = "y", tname = "year", idname = "id", gname = "g0", data = d,
              control_group = "notyettreated", base_period = "universal", bstrap = FALSE, cband = FALSE)
ce2 <- aggte(cs2, type = "dynamic", min_e = -10, max_e = 15, bstrap = FALSE, cband = FALSE)
csn <- data.frame(k = ce2$egt, est = ce2$att.egt, se = ce2$se.egt)
## 4. imputation (didimputation::did_imputation)
im <- did_imputation(d, yname = "y", gname = "g0", tname = "year", idname = "id",
                     horizon = 0:15, pretrends = -10:-1, cluster_var = "id")
imp <- data.frame(k = as.numeric(im$term), est = im$estimate, se = im$std.error)

lab <- c("TWFE event study\nfixest::feols + i()", "Sun-Abraham\nfixest::sunab()",
         "CS, never treated\ndid::att_gt()",
         "CS, not yet treated\ndid::att_gt()", "Imputation (BJS)\ndidimputation")
all <- rbind(cbind(rbind(tw, data.frame(k = -1, est = 0, se = 0)), m = lab[1]),
             cbind(sa, m = lab[2]), cbind(csd, m = lab[3]), cbind(csn, m = lab[4]),
             cbind(imp, m = lab[5]))
all <- subset(all, k >= -10 & k <= 15)
all$m <- factor(all$m, levels = lab)
all$post <- all$k >= 0
ghost <- rbind(tw, data.frame(k = -1, est = 0, se = 0))
ghost <- ghost[rep(seq_len(nrow(ghost)), 4), ]; ghost$m <- factor(rep(lab[2:5], each = nrow(tw) + 1), levels = lab)

cat("average over k = 0..10, by method:\n")
print(round(tapply(all$est[all$k >= 0 & all$k <= 10], droplevels(all$m[all$k >= 0 & all$k <= 10]), mean), 2))
cat("k = 15:\n"); print(round(setNames(all$est[all$k == 15], all$m[all$k == 15]), 2))

blue <- rgb(1, 33, 105, maxColorValue = 255); orange <- rgb(200, 78, 0, maxColorValue = 255)
all$phase <- ifelse(all$post, "after adoption", "before adoption")
ghost$phase <- "TWFE, for comparison"
cols <- c(`before adoption` = blue, `after adoption` = orange, `TWFE, for comparison` = "gray65")
p <- ggplot(all, aes(k, est)) +
  geom_hline(yintercept = 0, colour = "gray55", linewidth = 0.3) +
  geom_vline(xintercept = -0.5, colour = "gray55", linetype = 2, linewidth = 0.3) +
  geom_line(data = ghost, aes(k, est, colour = phase), linewidth = 0.5) +
  geom_ribbon(aes(ymin = est - 1.96 * se, ymax = est + 1.96 * se, fill = phase), alpha = 0.18,
              show.legend = FALSE) +
  geom_line(aes(colour = phase), linewidth = 0.6) +
  geom_point(aes(colour = phase), size = 1.1) +
  scale_colour_manual(values = cols, breaks = names(cols), name = NULL) +
  scale_fill_manual(values = cols) +
  facet_wrap(~ m, nrow = 1) +
  coord_cartesian(ylim = c(-35, 20)) +
  labs(x = "years since unilateral divorce (k)", y = "female suicides per million") +
  theme_minimal(base_family = "Times", base_size = 15) +
  theme(panel.grid.minor = element_blank(), strip.text = element_text(size = 14, hjust = 0),
        panel.spacing = unit(0.8, "lines"), legend.position = "none",
        legend.text = element_text(size = 13), legend.key.width = unit(1.4, "lines"))
ggsave("figs/divorce_es5.pdf", p, width = 11, height = 3, device = pdf, family = "Times")
