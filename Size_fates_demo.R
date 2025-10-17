# DIGITIFERA DEMOGRAPHY - SIZE-DEPENDENT FATES
# Load only the packages this script actually uses
packages <- c("readxl","ggplot2","dplyr","ggpubr","RColorBrewer","mgcv","quantreg")
lapply(packages, require, character.only = TRUE)

# ---- Load & prepare data ----
# Filter to colonies >5 cm^2, bin by initial radius, flag "alive" fates, and ensure Fate is a factor.
Fates_all <- read_excel("Size_fates_demo.xlsx") %>%
  filter(Area_cm > 5) %>%
  mutate(
    bin   = cut(Radius,
                breaks = c(1, 2.5, 5, 7.5, 10, 20),
                labels = c("1.25 - 2.5","2.5 - 5","5 - 7.5","7.5 - 10","10+")),
    alive = ifelse(Fate %in% c("Grow", "Loss"), "yes", "no"),
    Fate  = as.factor(Fate)
  )

# ---- Quick radius vs area plot ----
ggplot(Fates_all, aes(x = Area_cm, y = Radius)) +
  geom_point() +
  geom_smooth() +
  theme_classic()

# ---- Fate counts by size bin and site ----
# Count outcomes per fate × radius bin for each site to build proportional bars
counts1 <- data.frame(
  Fates_all[which(Fates_all$id == "Site 1"), ] %>%
    dplyr::count(Fate, bin)
)
counts1$id <- "Site 1"

counts2 <- data.frame(
  Fates_all[which(Fates_all$id == "Site 2"), ] %>%
    dplyr::count(Fate, bin)
)
counts2$id <- "Site 2"

# Convert counts to within-bin proportions for each site
df1 <- xtabs(n ~ bin + Fate + id, data = counts1)
df1 <- data.frame(prop.table(df1, 1))

df2 <- xtabs(n ~ bin + Fate + id, data = counts2)
df2 <- data.frame(prop.table(df2, 1))

# Combine site tables and add helper columns
df_both <- rbind.fill(df1, df2) %>%    # NOTE: rbind.fill() is from {plyr}. If you don't load plyr, use base rbind() instead (columns should match).
  mutate(
    alive = ifelse(Fate %in% c("Grow", "Loss"), "yes", "no"),
    Freq  = as.numeric(Freq),
    bin   = as.factor(bin)
  )

# ---- Diverging stacked bar: proportion of fates by size bin ----
# Positive bars for "alive" fates (Grow/Loss), negative for mortality/transition categories
ggplot(
  df_both,
  aes(
    x = bin,
    y = ifelse(alive == "yes", Freq, -Freq),
    fill = factor(Fate, levels = c("Grow", "Loss", "H to TMP", "H to TMV", "PM to TMP", "PM to TMV"))
  )
) +
  geom_bar(stat = "identity", position = "stack", colour = "black") +
  theme_classic() +
  xlab("Radius (cm)") +
  ylab("Proportion") +
  scale_y_continuous(labels = abs) +
  geom_hline(yintercept = 0, size = 0.7) +
  scale_fill_brewer(
    palette = "Set3", name = "Fate",
    breaks = c("Grow", "Loss", "H to TMP", "H to TMV", "PM to TMP", "PM to TMV"),
    labels = c(
      "Grow" = "Net growth", "Loss" = "Net shrinkage",
      "H to TMP" = "Healthy to TM (present)",
      "H to TMV" = "Healthy to TM (absent)",
      "PM to TMP" = "PM to TM (present)",
      "PM to TMV" = "PM to TM (absent)"
    )
  ) +
  theme(
    axis.text.y = element_text(size = 10),
    legend.box.just = "right",
    legend.margin = margin(6, 6, 6, 6),
    legend.text = element_text(size = 10),
    legend.title = element_text(face = "bold"),
    legend.background = element_blank(),
    legend.box.background = element_rect(colour = "black"),
    strip.background = element_blank(),
    panel.spacing = unit(2, "lines"),
    strip.text = element_blank(),
    axis.text.x = element_text(angle = 45, vjust = 0.6, hjust = 0.5)
  ) +
  facet_wrap(~ id)

# Quick totals by site (handy checks that counts look sensible)
sum(Fates_all$id == "Site 1")
sum(Fates_all$id == "Site 2")

# ---- Size-dependent probability curves ----
# Survival: points with jitter (to reduce overplot), dashed GAM fit (flexible),
# and per-site logistic (GLM) fits for interpretability.
survive <- ggplot(Fates_all, aes(x = Radius, y = Survive)) +
  geom_point(aes(colour = id), alpha = 0.5,
             position = position_jitter(w = 0.2, h = 0.01)) +
  geom_smooth(
    method = "gam", formula = y ~ s(x, bs = "cr"),
    method.args = list(family = "binomial"),
    fullrange = TRUE, level = 0.95, se = FALSE, aes(colour = id),
    linetype = "dashed"
  ) +
  geom_smooth(
    data = Fates_all[which(Fates_all$id == "Site 2"), ],
    method = "glm", formula = y ~ x,
    method.args = list(family = "binomial"),
    fullrange = TRUE, level = 0.95, se = FALSE, aes(colour = id)
  ) +
  geom_smooth(
    data = Fates_all[which(Fates_all$id == "Site 1"), ],
    method = "glm", formula = y ~ x,
    method.args = list(family = "binomial"),
    fullrange = TRUE, level = 0.95, se = FALSE, aes(colour = id)
  ) +
  theme_classic() +
  scale_colour_manual(values = c("#FF776C", "#71B3D7")) +
  xlab("Initial Radius (cm)") +
  ylab("Probability of Survival") +
  scale_y_continuous(
    breaks = c(0, 0.25, 0.5, 0.75, 1),
    name = "Probability of Survival\n",
    sec.axis = sec_axis(~ 1 - .*1,
                        breaks = c(0, 0.25, 0.5, 0.75, 1),
                        name   = "Probability of Death\n")
  ) +
  scale_x_continuous(
    breaks  = c(0, 1.25, 2.5, 5, 7.5, 10, 12.5, 15),
    sec.axis = sec_axis(trans = ~(.*.) * 3.14,
                        name   = bquote('Initial Area'~(cm^2)),
                        breaks = c(5, 10, 25, 50, 100, 250, 500))
  )

# Net growth: analogous curves for probability of growth vs. initial radius
grow <- ggplot(Fates_all, aes(x = Radius, y = Grow)) +
  geom_point(aes(colour = id), alpha = 0.5,
             position = position_jitter(w = 0.2, h = 0.01)) +
  geom_smooth(
    data = Fates_all[which(Fates_all$id == "Site 1"), ],
    method = "glm", formula = y ~ x,
    method.args = list(family = "binomial"),
    fullrange = TRUE, level = 0.95, se = FALSE, aes(colour = id)
  ) +
  geom_smooth(
    data = Fates_all[which(Fates_all$id == "Site 2"), ],
    method = "glm", formula = y ~ x,
    method.args = list(family = "binomial"),
    fullrange = TRUE, level = 0.95, se = FALSE, aes(colour = id)
  ) +
  theme_classic() +
  scale_colour_manual(values = c("#FF776C", "#71B3D7")) +
  xlab("Initial Radius (cm)") +
  ylab("Probability of Net Growth") +
  scale_x_continuous(
    breaks  = c(0, 1.25, 2.5, 5, 7.5, 10, 12.5, 15),
    sec.axis = sec_axis(trans = ~(.*.) * 3.14,
                        name   = bquote('Initial Area'~(cm^2)),
                        breaks = c(5, 10, 25, 50, 100, 250, 500))
  ) +
  scale_y_continuous(
    name    = "Probability of Net Growth\n",
    sec.axis = sec_axis(~ 1 - .*1,
                        breaks = c(0, 0.25, 0.5, 0.75, 1),
                        name   = "Probability of Net Shrinkage\n")
  )

# Arrange the two probability plots with a small spacer and a shared legend
ggarrange(survive, NULL, grow,
          ncol = 3, widths = c(1, 0.05, 1),
          common.legend = TRUE, legend = "right")

# Extra sanity checks
sum(Fates_all$id == "Site 1")
sum(Fates_all$id == "Site 2")
sum(Fates_all$id == "Site 1" & Fates_all$Survive == 1)
sum(Fates_all$id == "Site 2" & Fates_all$Survive == 1)

# ---- Logistic regression summaries ----
# Quadratic + linear binomial GLMs by site for Survival and Growth
summary(glm(formula = Survive ~ Radius + I(Radius^2),
            family = "binomial",
            data   = Fates_all[which(Fates_all$id == "Site 1"), ]))
glms1 <- glm(formula = Survive ~ Radius,
             family  = "binomial",
             na.action = na.exclude,
             data    = Fates_all[which(Fates_all$id == "Site 1"), ])

summary(glm(formula = Survive ~ Radius + I(Radius^2),
            family = "binomial",
            data   = Fates_all[which(Fates_all$id == "Site 2"), ]))
glms2 <- glm(formula = Survive ~ Radius,
             family  = "binomial",
             na.action = na.exclude,
             data    = Fates_all[which(Fates_all$id == "Site 2"), ])

summary(glm(formula = Grow ~ Radius + I(Radius^2),
            family = "binomial",
            data   = Fates_all[which(Fates_all$id == "Site 1"), ]))
glmg1 <- summary(glm(formula = Grow ~ Radius,
                     family  = "binomial",
                     data    = Fates_all[which(Fates_all$id == "Site 1"), ]))

summary(glm(formula = Grow ~ Radius + I(Radius^2),
            family = "binomial",
            data   = Fates_all[which(Fates_all$id == "Site 2"), ]))
glmg2 <- glm(formula = Grow ~ Radius,
             family  = "binomial",
             data    = Fates_all[which(Fates_all$id == "Site 2"), ])

# ---- Site × outcome association tests ----
chisq.test(Fates_all$Survive, Fates_all$id, correct = FALSE)
chisq.test(Fates_all$Grow,    Fates_all$id, correct = FALSE)

