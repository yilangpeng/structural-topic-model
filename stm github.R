library(stm)  # package for sturctural topic modeling
library(psych) #decribe
library(ggplot2) # ggsave, ggplot
library(MASS) #write.matrix
library(tidystm)  # function extract.estimateEffect

rm(list = ls())
setwd("~/Dropbox/Research/Letter to self/Text/") # replace with the file path

# read the text file
data = read.table('cond-message-letter-length.csv',
                  header = TRUE, sep = ",", quote = "\"")
nrow(data)
head(data)

# remove letters that are too short (fewer than 10 words)
d = subset(data, data$nword > 10)
nrow(d)
describe(d$nword) # get average word length

# only keep columns that are needed (covariates and documents)
l = d[c('letter','cond')]
l$letter <- as.character(l$letter)  
table(l$cond)
l$cond <- factor(l$cond)
l$cond <- factor(l$cond, levels = c("c2", "c1", "c3"))
table(l$cond)

# preprocess
processed <- textProcessor(l$letter, metadata=l, removestopwords = TRUE,
                           customstopwords=c("concussion","concussions"))
str(processed)  

# create a folder to save results
cwd = "~/Dropbox/Research/Letter to self/R/Graph-R/STM/"
resultpath = paste(cwd,nrow(d),sep="")
resultpath; dir.create(resultpath); 

# prepare documents
out <- prepDocuments(processed$documents, processed$vocab, processed$meta)
docs <- out$documents
vocab <- out$vocab
meta <- out$meta
vocab

#select models (decide the number of topics)
findingk <- searchK(out$documents, out$vocab, K = c(3:12), # topic range
                    prevalence = ~cond, data = out$meta, verbose=FALSE)
findingk$results
dfindingK = data.frame(findingk$results)
dfindingK

resultpath = paste(cwd,nrow(d),'/diagnosis',sep="")
resultpath; dir.create(resultpath); 
setwd(resultpath)

quartz(width=10, height=10)
plot(findingk)
dev.copy2pdf(file = paste("diagosis",".pdf", sep=""))
dev.off()

dfindingK

ggplot(data = dfindingK, aes(x=semcoh, y=exclus, label = K)) +
  geom_point(size = 2, shape=1) + 
  geom_text(hjust=-0.5) + theme_classic(base_size = 18)+
  labs(x = "Semantic coherence",
       y = "Exclusivity")
ggsave("semantic-exclusivity.png",w=6,h=4)

ggplot(data = dfindingK, aes(x=K , y=heldout)) +
  geom_point(size = 2, shape=1) + 
  theme_classic(base_size = 18)+geom_path()+
  scale_x_continuous(breaks = seq(0, 20, by = 1)) +
  labs(x = "Number of topics",
       y = "Held-out likelihood")
ggsave("held-out likelihood.png",w=6,h=4)

# build model
tfit4 <- stm(out$documents, out$vocab, K=4, prevalence=~cond, max.em.its=75, data=out$meta, init.type="Spectral",seed=1)
tfit5 <- stm(out$documents, out$vocab, K=5, prevalence=~cond, max.em.its=75, data=out$meta, init.type="Spectral",seed=1)
tfit6 <- stm(out$documents, out$vocab, K=6, prevalence=~cond, max.em.its=75, data=out$meta, init.type="Spectral",seed=1)
tfit7 <- stm(out$documents, out$vocab, K=7, prevalence=~cond, max.em.its=75, data=out$meta, init.type="Spectral",seed=1)

# save all the results
for (nk in 4:7){  
# interpret results
# get example words
nk = 6
if (nk == 4){tfit = tfit4}
if (nk == 5){tfit = tfit5}
if (nk == 6){tfit = tfit6}
if (nk == 7){tfit = tfit7}

resultpath = paste("~/Dropbox/Research/Letter to self/R/Graph-R/STM/",nrow(d),nk,sep="/")
resultpath; dir.create(resultpath); setwd(resultpath)

# doc topic matrix
doctopic = tfit$theta
write.matrix(doctopic, file =paste(nk, "doctopic",".txt", sep=""), sep = "\t")

# words for each topic
label = labelTopics(tfit, n = 20)
capture.output(label, file = paste(nk, "label",".txt", sep=""), append = FALSE)

# topic proportion
quartz(width=10, height=10)
plot(tfit, type="summary", xlim=c(0,.7), n = 8)
dev.copy2pdf(file = paste(nk, "proportion",".pdf", sep=""))
dev.off()

plot(tfit, type="labels")
dev.copy2pdf(file = paste(nk, "keyword", ".pdf", sep=""))

# get example documents
thought = findThoughts(tfit, texts = l$letter, n = 10)
thought
plotQuote(thought, width = 30, main = "Topic 3")
capture.output(thought, file = paste(nk, "document",".txt", sep=""), append = FALSE)

# estimate effects
est = estimateEffect(formula = ~ cond, stmobj = tfit, metadata = out$meta, uncertainty = "Global")
est = estimateEffect(formula = ~ cond, stmobj = tfit, metadata = out$meta, uncertainty = "Global")
sest = summary(est)
sest
capture.output(sest, file = paste(nk, "effects",".txt", sep=""), append = FALSE)

effect <- extract.estimateEffect(est, "cond")
effect

exnames = c("Traditional symptom","Revised symptom","Consequence-based")
p = ggplot(data=effect, aes(x=covariate.value, y=estimate, ymin=ci.lower, ymax=ci.upper))+
  facet_wrap(.~topic, ncol = 1) + 
  geom_point(size=1.2) +
  geom_errorbar(position=position_dodge(width=0.3),width=0,size=0.3,alpha=1)+
  scale_x_discrete(labels=exnames)+
  coord_flip()+
  theme_bw(base_size=12)+ xlab("Experimental condition") + ylab("Topic prevalence") +
  theme(text=element_text(size=12,family="Arial Narrow"),
        strip.background =element_blank(),panel.border = element_blank())
p
ggsave(paste(nk, "condition",".png", sep=""),w=7,h=7)
}