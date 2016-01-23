

cor.mtest <- function(mat, conf.level = 0.95) {
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat <- lowCI.mat <- uppCI.mat <- matrix(NA, n, n)
  diag(p.mat) <- 0
  diag(lowCI.mat) <- diag(uppCI.mat) <- 1
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      tmp <- cor.test(mat[, i], mat[, j], conf.level = conf.level)
      p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
      lowCI.mat[i, j] <- lowCI.mat[j, i] <- tmp$conf.int[1]
      uppCI.mat[i, j] <- uppCI.mat[j, i] <- tmp$conf.int[2]
    }
  }
  return(list(p.mat, lowCI.mat, uppCI.mat))
}

## Parallels between language ontogeny and evolution
get measures of developmental change in vocabulary

## Learnability pressures predicts AOA across languages
Predict AOA by social variables, by social variable
```{r, fig.width=10, fig.height = 10, eval = F}
aoa_central = aoa_data %>% 
  mutate(language = tolower(language)) %>%
  group_by(language, measure, lexical_category) %>%
  summarise(mean_aoa = mean(aoa))

d3 = inner_join(d, aoa_central, by = "language") %>%
  select(language, log.RatioL2, log.pop2,
         log.numNeighbors, log.area, 
         mean.temp, mean_aoa, measure, lexical_category) %>%
  mutate(language = as.factor(language)) %>%
  gather(pop_measure, pop_value, 2:6) 

ggplot(d3, aes(y = mean_aoa, 
               x = pop_value,
               label = language,
               group = measure,
               color = measure)) +
  geom_text(size = 3, aes(group = measure)) +
  facet_grid(lexical_category ~ pop_measure, scales = "free") +
  geom_smooth(method = "lm") +
  theme_bw() 
````

Fits controlling for language family (mean.fam.lang ~ mean.fam.pop)
Aggregating across families
```{r, fig.width=10, fig.height = 14, eval = F}
d.scatter.fam = d %>% 
  select(log.n.consonants, log.n.vow, son, log.asjp.mean.length, 
         H.LDT, lun.mean.aoa, p.complexity.bias, m_obs, 
         log.RatioL2, log.pop2, growingSeason, mean.temp, 
         log.sum.precip, log.area, langFamily) %>%
  rename(log.length = log.asjp.mean.length, lexical.diversity = H.LDT, 
         aoa = lun.mean.aoa, complexity.bias = p.complexity.bias,
         dependency.length = m_obs, log.n.vowels = log.n.vow) %>%
  group_by(langFamily) %>% 
  summarise_each(funs(mean(., na.rm = TRUE))) %>%
  gather(lang_measure, lang_value, 
         which(is.element(names(.), lang_vars))) %>%
  group_by(lang_measure) %>%
  gather(pop_measure, pop_value,
         which(is.element(names(.), demo_vars)))

is.na(d.scatter.fam) <- sapply(d.scatter.fam, is.infinite)

# get correlations
d.scatter.corrs = d.scatter.fam %>%
  group_by(lang_measure, pop_measure) %>%
  do(tidy(cor.test(~ .$pop_value + .$lang_value))) %>%
  mutate(sig = ifelse(p.value < .05, "*", "")) %>%
  mutate(sig.col = ifelse(p.value < .05 & estimate > 0, "pos",
                          ifelse(p.value < .05 & estimate < 0, "neg",
                                 "none"))) %>%
  mutate(lab = paste("r = ", round(estimate,2),sig, sep = "")) %>%
  mutate(pop_value = .1, lang_value = .1) %>% # this is a hack
  full_join(text.pos) %>%
  ungroup

ggplot(d.scatter.fam, aes(x = pop_value, y = lang_value)) +
  geom_rect(data = d.scatter.corrs, aes(fill = sig.col), 
            xmin = -Inf, xmax = Inf,
            ymin = -Inf, ymax = Inf, alpha = 0.2) +
  geom_point(size = .3) +
  geom_smooth(method = "lm", color = "green") +
  facet_grid(lang_measure~pop_measure, scale = "free") +
  scale_fill_manual(name = "grp", 
                    values = c( "mediumblue", "grey99","red1")) +
  theme_bw() +
  xlab("Demographic variables") +
  ylab("Language variables") +
  theme(legend.position = "none") 
```

Fits controling for family -- controling for family in a linear model
(lm(lang ~ pop + lang.family))
```{r, fig.width=10, fig.height = 14, eval = F}
d.scatter.fam2 = d %>% 
  filter(log.pop2>2) %>%
  select(log.n.consonants, log.n.vow, son,  log.asjp.mean.length,
         H.LDT, lun.mean.aoa, p.complexity.bias, m_obs, 
         log.RatioL2, log.pop2, growingSeason, mean.temp, 
         log.sum.precip, log.area,langFamily) %>%
  rename(log.length = log.asjp.mean.length, lexical.diversity = H.LDT, 
         aoa = lun.mean.aoa, complexity.bias = p.complexity.bias,
         dependency.length = m_obs, log.n.vowels = log.n.vow) %>%
  gather(lang_measure, lang_value, 
         which(is.element(names(.), lang_vars))) %>%
  group_by(lang_measure) %>%
  gather(pop_measure, pop_value,
         which(is.element(names(.), demo_vars)))

is.na(d.scatter.fam2) <- sapply(d.scatter.fam2, is.infinite) 
d.scatter.fam2 <- filter(d.scatter.fam2, !is.na(pop_value), !is.na(lang_value))

# get correlations
d.scatter.corrs = d.scatter.fam2 %>%
  group_by(lang_measure, pop_measure) %>%
  do(tidy(lm(lang_value ~ pop_value * langFamily, data = .))) %>%
  filter(term == "pop_value") %>%
  mutate(sig.col = ifelse(p.value < .05 & estimate > 0, "pos",
                          ifelse(p.value < .05 & estimate < 0, "neg",
                                 "none"))) %>%
  mutate(pop_value = .1, lang_value = .1) %>% # this is a hack
  ungroup

ggplot(d.scatter.fam2, aes(x = pop_value, y = lang_value)) +
  geom_rect(data = d.scatter.corrs, aes(fill = sig.col), 
            xmin = -Inf, xmax = Inf,
            ymin = -Inf, ymax = Inf, alpha = 0.2) +
  geom_point(size = .3, aes(col = langFamily)) +
  geom_smooth(method = "lm", color = "green") +
  facet_grid(lang_measure~pop_measure, scales = "free") +
  scale_fill_manual(name = "grp", 
                    values = c( "mediumblue", "grey99","red1")) +
  theme_bw() +
  xlab("Demographic variables") +
  ylab("Language variables") +
  theme(legend.position = "none")
```
```{r, fig.width=10, fig.height = 14, eval = F}
# these are the variables we care about for the paper
demo_vars_crit = c("n.neighbors_log",
                   "mean.temp", "sum.precip", "sd.temp",
                   "pop_log", "area_log", "n.neighbors_log",
                   "sd.precip_log", "ratio.L2.L1_log",
                   "distance.from.origin")

lang_vars_crit = c("p.complexity.bias","scaled.LDT.TTR",
                   "mean.dependency.length",
                   "mean.length", "mean.aoa",
                   "n.consonants_log", "n.vowels_log", "morphological.complexity")

d.scatter = d.clean %>% 
  gather(lang_measure, lang_value, 
         which(is.element(names(.), lang_vars_crit))) %>%
  group_by(lang_measure) %>%
  gather(pop_measure, pop_value,
         which(is.element(names(.), demo_vars_crit))) %>%
  select(pop_measure, lang_measure, pop_value, lang_value)
is.na(d.scatter) <- sapply(d.scatter, is.infinite)

# get correlations
d.scatter.corrs = d.scatter %>%
  group_by(lang_measure, pop_measure) %>%
  do(tidy(cor.test(~ .$pop_value + .$lang_value))) %>%
  mutate(sig.col = ifelse(p.value < .05 & estimate > 0, "pos",
                          ifelse(p.value < .05 & estimate < 0, "neg",
                                 "none"))) %>%
  mutate(pop_value = .1, lang_value = .1) %>% # this is a hack
  ungroup

ggplot(d.scatter, aes(x = pop_value, y = lang_value)) +
  geom_rect(data = d.scatter.corrs, aes(fill = sig.col), 
            xmin = -Inf, xmax = Inf,
            ymin = -Inf, ymax = Inf, alpha = 0.2) +
  geom_point(size = .3) +
  geom_smooth(method = "lm", color = "green") +
  facet_grid(lang_measure~pop_measure, scale = "free") +
  scale_fill_manual(values = c( "mediumblue", "grey99","red1")) +
  theme_bw() +
  xlab("Demographic variables") +
  ylab("Language variables") +
  theme(legend.position = "none") 

```{r,  fig.height = 10, fig.width = 10, eval = F}
Correlation matrix (stuff in the upper right quadrant matters)
is.na(d.clean) <- sapply(d.clean, is.infinite)
corrs = cor(d.clean[,c(8:22, 25:32, 34:49)], use = "pairwise.complete.obs")
corrplot(corrs)
```

Number of variables we have data for, for each language
```{r, fig.height = 6, eval = F}
# as is this plot is too hard to read to be useful
d.clean %>%
  gather(variable, num, c(7:21, 23:48))  %>%
  group_by(language) %>%
  summarize(counts = length(which(is.na(num) == F))) %>%
  filter(!is.na(counts), counts > 10, !is.na(language)) %>%
  ggplot(aes(y = counts, x = language)) + 
  geom_bar(stat = "identity") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ylab("Number of variables (minimum 10)") +
  ggtitle("Number variables per language")
```



"p.complexity.bias","scaled.LDT.TTR",
"mean.dependency.length",
"mean.length", "mean.aoa",
"n.consonants_log", "n.vowels_log"








# these are the variables we care about for the paper
demo_vars_crit = c("n.neighbors_log",
                   "mean.temp", "sum.precip", "sd.temp",
                   "pop_log", "area_log", "n.neighbors_log",
                   "sd.precip_log", "ratio.L2.L1_log",
                   "distance.from.origin")

lang_vars_crit = c("p.complexity.bias","scaled.LDT.TTR",
                   "mean.dependency.length",
                   "mean.length", "mean.aoa",
                   "n.consonants_log", "n.vowels_log", "morphological.complexity",
                   "information.density", "information.rate")

d.scatter.fam = d.clean %>% 
  select(n.consonants_log, n.vowels_log, mean.length,
         p.complexity.bias, scaled.LDT.TTR, morphological.complexity,
         information.density, information.rate,
         mean.aoa, lang.family, country, ISO) %>%
  gather(lang_measure, lang_value, 
         which(is.element(names(.), lang_vars_crit))) %>%
  select(lang_measure, lang_value, lang.family, country, ISO) 

all_measures <- unique(d.scatter.fam$lang_measure)

d.wide <- d.scatter.fam %>%
  spread(lang_measure, lang_value) 




my_measure = all_measures[1]

model.fits <- function(all_measures, mymeasure, df) {
  df = na.omit(df)
  lapply(all_measures, function(measure) {tidy(lm(mymeasure ~ measure 
                                                  , df))})
  bind_rows()
}

model.fits(all_measures, all_measures[1], d.wide)



%>%
  sperad(lan)

is.na(d.scatter.fam) <- sapply(d.scatter.fam, is.infinite) 
d.scatter.fam <- filter(d.scatter.fam, !is.na(lang_value2),
                        !is.na(lang_value))

# get model fits
d.model.fits = d.scatter.fam %>%
  group_by(lang_measure, lang_measure2) %>%
  do(tidy(lmer(lang_value ~ lang_value2 + (lang_value2|lang.family) +
                 (1|country), data=.))) %>%
  filter(term == "lang_value2") %>%
  mutate(sig = ifelse(abs(statistic) > 1.96, "*", "")) %>%
  mutate(sig.col = ifelse(statistic > 1.96, "pos",
                          ifelse(statistic < -1.96, "neg",
                                 "none"))) %>%
  mutate(lang_value2 = .1, lang_value = .1) %>% # this is a hack
  ungroup
d.model.fits[d.model.fits == "NaN"] = "NA"

# plot
ggplot(d.scatter.fam, aes(x = lang_value2, y = lang_value)) +
  geom_rect(data = d.model.fits, aes(fill = sig.col), 
            xmin = -Inf, xmax = Inf,
            ymin = -Inf, ymax = Inf, alpha = 0.2) +
  geom_point(size = .3) +
  geom_smooth(method = "lm", color = "green") +
  facet_grid(lang_measure ~ lang_measure2, scales = "free") +
  scale_fill_manual(values = c( "mediumblue", "grey99","red1")) +
  theme_bw() +
  xlab("Demographic variables") +
  ylab("Language variables") +
  theme(legend.position = "none")



mutate(lang_measure2 = lang_measure, 
       lang_value2 = lang_value) %>%
  select(lang_measure, lang_value, lang_measure2, lang_value2, lang.family, country, ISO) %>%
  group_by(ISO) %>%
  mutate(lang_measure2 = rev(lang_measure2), 
         lang_value2 = rev(lang_value2)) 