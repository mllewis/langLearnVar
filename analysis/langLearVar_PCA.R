## Relationship between language and demographic variables
##Fits controling for family -- controlling for family in a mixed effect model
d.clean = read.csv("../data/langLearnVar_data_clean.csv")[-1]

# these are the variables we care about for the paper
demo_vars_crit = c("n.neighbors_log",
                   "mean.temp", "sum.precip", "sd.temp",
                   "pop_log", "area_log", "n.neighbors_log",
                   "sd.precip_log", "ratio.L2.L1_log",
                   "distance.from.origin")

lang_vars_crit = c("p.complexity.bias","scaled.LDT.TTR",
                   "mean.dependency.length", "mean.length",
                   "mean.aoa", "n.consonants_log", "n.vowels_log",
                   "morphological.complexity", "morphological.complexity.ld")

############### DEMOGRAPHICS PCA ####################
# get demographic variables only (excluding L2 because intersections contains only 40 languages)
d.demo = d.clean %>%
  select(lat, mean.temp, sd.temp, sum.precip, sd.precip_log, 
         area_log, pop_log, n.neighbors_log) %>% 
  mutate(lat = abs(lat))
is.na(d.demo) <- sapply(d.demo, is.infinite) 

# figure out what variables to exclude, based on correlation matrix
demo.corr = cor(d.demo, use = "pairwise.complete")
abs(demo.corr)>.7
#exclude lat (correlated with mean.temp, sd.precip_log) [threshold: > .8]
d.demo = select(d.demo, -lat, -sd.precip_log)
pca.demo = prcomp(na.omit(d.demo), scale = TRUE, tol = .6)

# merge PCAs with full dataset for model fits
d.demo[!is.na(d.demo)] = 0
d.demo[is.na(d.demo)] = 1
good_is = which(rowSums(d.demo) == 0)
d.pca.demo = d.clean[good_is,] %>% cbind(pca.demo$x)

d.scatter.fam = d.pca.demo %>% 
  select(n.consonants_log, n.vowels_log, 
         mean.length, p.complexity.bias, scaled.LDT.TTR,
         morphological.complexity, mean.aoa,
         lang.family, native.country, PC1, PC2) %>%
  gather(lang_measure, lang_value, 
         which(is.element(names(.), lang_vars_crit))) %>%
  group_by(lang_measure) %>%
  gather(pop_measure, pop_value,
         which(is.element(names(.), demo_vars_crit)))

is.na(d.scatter.fam) <- sapply(d.scatter.fam, is.infinite) 
d.scatter.fam <- filter(d.scatter.fam, !is.na(pop_value),
                        !is.na(lang_value))

# get model fits
d.model.fits = d.scatter.fam %>%
  group_by(lang_measure, pop_measure) %>%
  do(tidy(lmer(lang_value ~ pop_value + (pop_value|lang.family) + (1|native.country),
               data=.))) %>%
  filter(term == "pop_value") %>%
  mutate(sig.col = ifelse(statistic > 1.96, "pos",
                          ifelse(statistic < -1.96, "neg",
                                 "none"))) %>%
  mutate(pop_value = .1, lang_value = .1) %>% # this is a hack
  ungroup

d.model.fits[d.model.fits == "NaN"] = "NA"
ggplot(d.scatter.fam, aes(x = pop_value, y = lang_value)) +
  geom_rect(data = d.model.fits, aes(fill = sig.col), 
            xmin = -Inf, xmax = Inf,
            ymin = -Inf, ymax = Inf, alpha = 0.2) +
  #geom_point(size = .3) +
  #geom_smooth(method = "lm", color = "green") +
  facet_grid(lang_measure~pop_measure, scales = "free") +
  scale_fill_manual(values = c( "mediumblue", "grey99","red1")) +
  theme_bw() +
  xlab("Demographic variables") +
  ylab("Language variables") +
  theme(legend.position = "none")


############### LANGUAGE PCA ####################
d.lang =  d.clean %>%
  select(morphological.complexity.ld, scaled.LDT.H, 
         mean.length, n.consonants_log, n.vowels_log) 
is.na(d.lang) <- sapply(d.lang, is.infinite) 

#lang.corr = cor(d.lang, use = "pairwise.complete")
#abs(lang.corr)>.8

# do pca
#pca.lang = prcomp(na.omit(d.lang), scale = TRUE)
#barplot(pca.lang$sdev/pca.lang$sdev[1])

# look at variance explained
#fviz_eig(pca.lang, addlabels = TRUE, 
#        linecolor ="red", geom = "line") +
# theme_bw()
pca.lang = prcomp(na.omit(d.lang), scale = TRUE, tol = .7)


# plot loadings
pcs.lang <- cbind(names(d.lang),
                  gather(as.data.frame(pca.lang$rotation), 
                         pc, value))
names(pcs.lang)[1] = "variable"

pcs.lang$variable =  factor(pcs.lang$variable , levels = pcs.lang$variable[1:8]) # order variables
pcs.lang$magnitude = ifelse(abs(pcs.lang$value)>.2, TRUE, FALSE)
ggplot(pcs.lang) +
  geom_bar(aes(x = variable, y = value,
               fill = magnitude), stat = "identity") +
  facet_wrap(~pc) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))  + 
  scale_fill_manual(name = "Magnitude", 
                    values = c("grey","red1")) +
  ggtitle("PCA loadings")

# merge PCAs with full dataset for model fits
d.lang[!is.na(d.lang)] = 0
d.lang[is.na(d.lang)] = 1
good_is = which(rowSums(d.lang) == 0)
d.pca.lang = d.clean[good_is,] %>% cbind(pca.lang$x)

names(d.pca.lang)[names(d.pca.lang) == "PC1"] = "PC1_L"
names(d.pca.lang)[names(d.pca.lang) == "PC2"] = "PC2_L"

lang_vars_crit2 = c("PC1_L", "PC2_L")

demo_vars_crit2 = c("n.neighbors_log",
                    "mean.temp", "sum.precip", "sd.temp",
                    "pop_log", "area_log", "n.neighbors_log",
                    "sd.precip_log", "ratio.L2.L1_log",
                    "distance.from.origin", "mean.aoa")

d.scatter.fam = d.pca.lang %>% 
  select(ratio.L2.L1_log, pop_log, distance.from.origin,
         n.neighbors_log, area_log, mean.temp, sd.temp, sum.precip, mean.aoa, 
         PC1_L, PC2_L, lang.family, native.country) %>%
  gather(lang_measure, lang_value, 
         which(is.element(names(.), lang_vars_crit2))) %>%
  group_by(lang_measure) %>%
  gather(pop_measure, pop_value,
         which(is.element(names(.), demo_vars_crit2)))

is.na(d.scatter.fam) <- sapply(d.scatter.fam, is.infinite) 
d.scatter.fam <- filter(d.scatter.fam, !is.na(pop_value),
                        !is.na(lang_value))

# get model fits
d.model.fits = d.scatter.fam %>%
  group_by(lang_measure, pop_measure) %>%
  do(tidy(lmer(lang_value ~ pop_value + (1|native.country), data=.))) %>%
  filter(term == "pop_value") %>%
  mutate(sig.col = ifelse(statistic > 1.96, "pos",
                          ifelse(statistic < -1.96, "neg",
                                 "none"))) %>%
  mutate(pop_value = .1, lang_value = .1) %>% # this is a hack
  ungroup

d.model.fits[d.model.fits == "NaN"] = "NA"
ggplot(d.scatter.fam, aes(x = pop_value, y = lang_value)) +
  geom_rect(data = d.model.fits, aes(fill = sig.col), 
            xmin = -Inf, xmax = Inf,
            ymin = -Inf, ymax = Inf, alpha = 0.2) +
  geom_point(size = .3) +
  geom_smooth(method = "lm", color = "green") +
  facet_grid(lang_measure~pop_measure, scales = "free") +
  scale_fill_manual(values = c( "mediumblue", "grey99","red1")) +
  theme_bw() +
  xlab("Demographic variables") +
  ylab("Language variables") +
  theme(legend.position = "none")

############### ALL PCA ####################
all.pca = left_join(d.pca.demo, d.pca.lang[,c("PC1_L", "PC2_L", "ISO")],
                    by = "ISO")

demo_vars_crit2 = c("big_and_cold", "big_and_hot")
lang_vars_crit2 = c("PC1_L", "PC2_L")

d.scatter.fam = all.pca %>% 
  select(PC1_L, PC2_L, big_and_cold, big_and_hot, 
         lang.family, native.country, ISO) %>%
  gather(lang_measure, lang_value, 
         which(is.element(names(.), lang_vars_crit2))) %>%
  group_by(lang_measure) %>%
  gather(pop_measure, pop_value,
         which(is.element(names(.), demo_vars_crit2)))

is.na(d.scatter.fam) <- sapply(d.scatter.fam, is.infinite) 
d.scatter.fam <- filter(d.scatter.fam, !is.na(pop_value),
                        !is.na(lang_value))

# get model fits
d.model.fits = d.scatter.fam %>%
  group_by(lang_measure, pop_measure) %>%
  do(tidy(lmer(lang_value ~ pop_value + (pop_value|lang.family) +
                 (1|native.country), data=.))) %>%
  filter(term == "pop_value") %>%
  mutate(sig.col = ifelse(statistic > 1.96, "pos",
                          ifelse(statistic < -1.96, "neg",
                                 "none"))) %>%
  mutate(pop_value = .1, lang_value = .1) %>% # this is a hack
  ungroup

as.data.frame(d.model.fits)

d.model.fits[d.model.fits == "NaN"] = "NA"
ggplot(d.scatter.fam, aes(x = pop_value, y = lang_value)) +
  geom_rect(data = d.model.fits, aes(fill = sig.col),
            xmin = -Inf, xmax = Inf,
            ymin = -Inf, ymax = Inf, alpha = 0.2) +
  geom_point(size = .3) +
  geom_smooth(method = "lm", color = "green") +
  facet_grid(lang_measure~pop_measure, scales = "free") +
  scale_fill_manual(values = c( "mediumblue", "grey99","red1")) +
  theme_bw() +
  xlab("Demographic variables") +
  ylab("Language variables") +
  theme(legend.position = "none")


#####
d = read.csv("../data/langLearnVar_data.csv")[,-1]


