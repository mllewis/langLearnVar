library(ggplot2);
library(grid);
library(gridExtra)

vplayout <- function(x, y)
  viewport(layout.pos.row = x, layout.pos.col = y)

a.d = filter(d.scatter.fam, lang_measure == "PC1_L" & pop_measure == "hot")
b.d = filter(d.scatter.fam, lang_measure == "PC1_L" & pop_measure == "big")
c.d = filter(d.scatter.fam, lang_measure == "PC2_L" & pop_measure == "hot")
d.d = filter(d.scatter.fam, lang_measure == "PC2_L" & pop_measure == "big")
  
a.line.fits = filter(line.fits, lang_measure == "PC1_L" & pop_measure == "hot")
b.line.fits = filter(line.fits, lang_measure == "PC1_L" & pop_measure == "big")
c.line.fits = filter(line.fits, lang_measure == "PC2_L" & pop_measure == "hot")
d.line.fits = filter(line.fits, lang_measure == "PC2_L" & pop_measure == "big")

pcs.lang.1 = filter(pcs.lang, pc == "PC1")
pcs.lang.2 = filter(pcs.lang, pc == "PC2")
pcs.demo.1 = filter(pcs.demo, pc == "hot")
pcs.demo.2 = filter(pcs.demo, pc == "big")

p.a = ggplot(a.d, aes(x = pop_value, y = lang_value)) +
 annotate("rect", xmin=-Inf, xmax=Inf, ymin=-Inf,  ymax=Inf, fill="mediumblue", 
           alpha=0.2)  +
  geom_point() +
  geom_abline(a.line.fits, mapping = aes(slope = s, intercept = i), 
              size = 1, color = "green") +
  theme_bw() +
  theme(legend.position = "none") +
  ylab('') +
  xlab('') 

p.b = ggplot(b.d, aes(x = pop_value, y = lang_value)) +
  annotate("rect", xmin=-Inf, xmax=Inf, ymin=-Inf,  ymax=Inf, fill="mediumblue", 
           alpha=0.2)  +
  geom_point() +
  geom_abline(b.line.fits, mapping = aes(slope = s, intercept = i), 
              size = 1, color = "green") +
  theme_bw() +
  theme(legend.position = "none") +
  ylab('') +
  xlab('') 

p.c = ggplot(c.d, aes(x = pop_value, y = lang_value)) +
  geom_point() +
  geom_abline(a.line.fits, mapping = aes(slope = s, intercept = i), 
              size = 1, color = "green") +
  theme_bw() +
  theme(legend.position = "none") +
  ylab('') +
  xlab('') 

p.d = ggplot(d.d, aes(x = pop_value, y = lang_value)) +
  geom_point() +
  geom_abline(d.line.fits, mapping = aes(slope = s, intercept = i), 
              size = 1, color = "green") +
  theme_bw() +
  theme(legend.position = "none") +
  ylab('') +
  xlab('') 

pca.lang1 = ggplot(pcs.lang.1) +
  geom_bar(aes(x = variable, y = value), stat = "identity") +
  theme_bw() +
  theme(axis.text.y = element_blank(), 
        strip.background = element_blank(),
        strip.text = element_blank())  + 
  coord_flip() +
  xlab('')

pca.lang2 = ggplot(pcs.lang.2) +
  geom_bar(aes(x = variable, y = value), stat = "identity") +
  theme_bw() +
  theme(axis.text.y = element_blank(), 
        strip.background = element_blank(),
        strip.text = element_blank())  + 
  coord_flip() +
  xlab('')

pca.demo1 = ggplot(pcs.demo.1) +
  geom_bar(aes(x = variable, y = value), stat = "identity") +
  theme_bw() +
  theme(axis.text.x = element_blank(), 
        strip.background = element_blank(),
        strip.text = element_blank()) +
  xlab('')

pca.demo2 = ggplot(pcs.demo.2) +
  geom_bar(aes(x = variable, y = value), stat = "identity") +
  theme_bw() +
  theme(axis.text.x = element_blank(), 
        strip.background = element_blank(),
        strip.text = element_blank()) +
  xlab('') +
  ylab("component loading")+

  facet_env_names <- c(PC1="Hot and rainy", PC2="Large")
facet_lang_names <- c(PC1="Complex",PC2="Short words")

pc.demo = ggplot(pcs.demo) +
  geom_bar(aes(x = variable, y = value, fill = variable), stat="identity") +
  facet_wrap(~pc, labeller = labeller(pc = facet_env_names)) +
  ylab("Component loading")+
  theme(axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        strip.text= element_text(size = 17)) +
  scale_x_discrete(breaks=NULL) +
  scale_fill_brewer(palette="YlOrRd") +
  #theme(legend.margin=unit(-5,"cm")) 
  theme(legend.position = "none")

pc.lang = ggplot(pcs.lang) +
  geom_bar(aes(x = variable, y = value, fill = variable), stat="identity") +
  theme(axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        strip.text= element_text(size = 17))  +
  coord_flip() +
  facet_grid(pc ~ ., scales="free_x", 
             labeller = labeller(pc = facet_lang_names)) +
  scale_x_discrete(breaks=NULL) +
  ylab("Component loading") +
  theme(legend.position = "none")  +
  scale_fill_brewer(palette="BuGn") 

p1 <- ggplot(d.scatter.fam, aes(x = pop_value, y = lang_value)) +
  geom_rect(data = sigs, aes(fill = sig.col),
            xmin = -Inf, xmax = Inf,
            ymin = -Inf, ymax = Inf, alpha = 0.2) +
  geom_point() +
  geom_abline(line.fits, mapping = aes(slope = s, intercept = i), 
              size = 1) +
  facet_grid(lang_measure~pop_measure, scales = "free") +
  scale_fill_manual(values = c( "mediumblue", "grey99","red1")) +
  theme(strip.background = element_blank(),
        strip.text = element_blank())+
  ylab("Rotated language values")+
  xlab("Rotated environmental values")+
  theme(legend.position = "none") 

grid.newpage()
pushViewport(viewport(layout = grid.layout(6, 6))) # a 5 by 5 grid
print(pc.demo, vp=vplayout(1:2.3,1:4)) # the first density plot will occupy the top of the grid
print(pc.lang, vp=vplayout(3:6,5:6)) # the main x/y plot will instead spread across most of the grid
print(p1, vp=vplayout(3:6,1:4)) # with the second density plot occupying a narrow vertical strip at the right




print(p.a, vp=vplayout(3:4,1:2)) # with the second density plot occupying a narrow vertical strip at the right
print(p.a, vp=vplayout(3:4,3:4)) # with the second density plot occupying a narrow vertical strip at the right
print(p.c, vp=vplayout(5:6,1:2)) # with the second density plot occupying a narrow vertical strip at the right
print(p.d, vp=vplayout(5:6,3:4)) # with the second density plot occupying a narrow vertical strip at the right


grid.arrange(pca.demo1, pca.demo2, blankPlot, p.a, p.b, 
             pca.lang1, p.c, p.d, pca.lang2 ,
             ncol=3, nrow=3, widths=c(1, 1,1), heights=c(1, 1,1))


grid.arrange(arrangeGrob(pp2, ncol = 2,widths=c(2,1)),
             arrangeGrob(p1,ncol = 2,widths = c(2,1)),
             arrangeGrob(pp1,nrow=1,heights=c(2)))



pca.lang1 = ggplot(pcs.lang.1) +
  geom_bar(aes(x = variable, y = value), stat = "identity") +
  theme_bw() +
  theme(axis.text.y = element_blank(), 
        strip.background = element_blank(),
        strip.text = element_blank())  + 
  coord_flip() +
  xlab('')

pca.lang2 = ggplot(pcs.lang.2) +
  geom_bar(aes(x = variable, y = value), stat = "identity") +
  theme_bw() +
  theme(axis.text.y = element_blank(), 
        strip.background = element_blank(),
        strip.text = element_blank())  + 
  coord_flip() +
  xlab('')

pca.demo1 = ggplot(pcs.demo.1) +
  geom_bar(aes(x = variable, y = value), stat = "identity") +
  theme_bw() +
  theme(axis.text.x = element_blank(), 
        strip.background = element_blank(),
        strip.text = element_blank()) +
  xlab('')

pca.demo2 = ggplot(pcs.demo.2) +
  geom_bar(aes(x = variable, y = value), stat = "identity") +
  theme_bw() +
  theme(axis.text.x = element_blank(), 
        strip.background = element_blank(),
        strip.text = element_blank()) +
  xlab('')

pca.lang1_grob = ggplotGrob(pca.lang1)
pca.lang2_grob = ggplotGrob(pca.lang2)
pca.demo1_grob = ggplotGrob(pca.demo1)
pca.demo2_grob = ggplotGrob(pca.demo2)

p1 + annotation_custom(grob = pca.lang1_grob, xmin = 3,xmax= 7,
                       ymin = 5, ymax = 3) +
  annotation_custom(grob = pca.lang1_grob, xmin = 3,xmax= 7,
                    ymin = 5, ymax = 3) 
  

library(gtable)
g2 <- ggplotGrob(p2)
g3 <- ggplotGrob(p3)
g <- rbind(g2, g3, size="first")
g$widths <- unit.pmax(g2$widths, g3$widths)
grid.newpage()
grid.draw(g)
  

