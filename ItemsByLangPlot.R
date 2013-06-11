require(rjson)
require(reshape)
require(ggplot2)
library(scales)
library(nlstools)
library(stringr)
library(gridExtra)
setwd("~/workspace/venn-analysis")
filter1 <- fromJSON(file='filter1.json')
filter2 <- fromJSON(file='filter2.json')
filter3 <- fromJSON(file='filter3.json')
alln <- fromJSON(file='alln.json')
langtot <- fromJSON(file='langtotals_1.json')

filter1m <- melt(filter1)
filter2m <- melt(filter2)
filter3m <- melt(filter3)
allnm <- melt(alln)


langtotm <-melt(langtot)
langtotm <- langtotm[order(langtotm$value),]


a <- data.frame(matrix(ncol = 0, nrow = length(filter1m$value)))
a["langs"] <- filter1m[c(order(filter1m$L1,filter1m$value)),"L1"]
a["uniqs"] <- filter1m[c(order(filter1m$L1,filter1m$value)),"value"]
a["tots"] <- langtotm[c(order(langtotm$L1,langtotm$value)),"value"]
b <- subset(a, tots > 1000)

c <- b[-63,]
#linear scale
p1 <- ggplot(b, aes(x=tots, y=uniqs), geom="line")+ geom_point(stat="identity") + 
  geom_text(data=b, aes(x=tots,y=uniqs,label=langs,colour=factor(langs),alpha=.9), 
            position=position_dodge(width=0.8,height=0.8),vjust=0, angle = 0, size = 8, guide = FALSE) +
  scale_x_continuous(labels=comma) + scale_y_continuous(labels=comma) +
  geom_smooth(method="loess", se=FALSE) +
  labs(x="Total Language Links ", y="Unique Language Links", title="Comparison of Languages in Wikidata, by Language Links \n Minimum 1,000 Items \n Linear Scale") +
  theme(legend.position="none")
p1

#log-log plot
p2 <- ggplot(b, aes(x=tots, y=uniqs), geom="line")+ geom_point(stat="identity") + 
  geom_text(data=b, aes(x=tots,y=uniqs,label=langs,colour=factor(langs),alpha=.9), position=position_dodge(width=0.8,height=0.8),vjust=0, angle = 0, size = 5, guide = FALSE) +
  scale_x_log10(labels=comma) + scale_y_log10(labels=comma) +
  geom_smooth(method="auto", se=FALSE) +
  labs(x="Total Language Links ", y="Unique Language Links", title="Comparison of Languages in Wikidata, by Language Links \n Minimum 1,000 Items \n Log-Log Scale") +
  theme(legend.position="none")
p2


#lets try this with a insstead of filter1m
q <- data.frame(matrix(ncol = 0, nrow = length(a$tots)))
q["langs"] <- a[c(order(a$tots,a$langs)),"langs"]
q["uniqs"] <- a[c(order(a$tots,a$langs)),"uniqs"]
q["tots"] <- a[c(order(a$tots,a$langs)),"tots"]
#q["resid"] <- a[c(order(a$uniqs,a$langs)),"resid"]
q["rat"] <- q[,"uniqs"] / q[,"tots"]

q.1 <- subset(q, q[,"tots"]>=100000)
q.2 <- subset(q, q[,"tots"]>=10000 & q[,"tots"]<100000)

totordplot1 <- ggplot(q.1, aes(x=langs, y=rat, fill=tots/max(tots)*100))+ 
  geom_bar(stat="identity", width = .9) + 
  #geom_bar(stat="identity", width = .5, alpha = .5, colour='red', aes(x=langs, y=resid), data=q.1)+
  scale_x_discrete(limits=q.1$langs) +
  scale_y_continuous(labels=percent)+
  scale_fill_gradient(low=muted("darkgreen"), high=muted("purple"), 
                      limits=c(0,40), "Total Language Links \n as % of English")+
  geom_text(aes(x=langs,y=0,label=paste(round(100*rat,2), '%', sep='')),hjust=0, angle = 90, colour='grey', data=q.1) +
  labs(title="More than 100,000 Language Links")+
  theme_bw()+
  theme(axis.title.x=element_blank(), axis.title.y=element_blank())
totordplot1

totordplot2 <- ggplot(q.2, aes(x=langs, y=rat, fill=tots/max(q.1$tots)*100))+ 
  geom_bar(stat="identity", width = .95) + 
  #geom_bar(stat="identity", width = .5, alpha = .5, colour='red', aes(x=langs, y=resid), data=q.1)+
  scale_x_discrete(limits=q.2$langs) +
  scale_y_continuous(labels=percent)+
  scale_fill_gradient(low=muted("lightblue"), high=muted("lightgreen"), "Total Language Links \n as % of English")+
  geom_text(aes(x=langs,y=0,
                label=paste(round(100*rat,1), '%', sep='')),
            hjust=0, angle = 90, colour='grey')+
  labs(title="10,000 to 100,000 Language Links")+
  theme_bw()+
  theme(axis.title.x=element_blank(), axis.title.y=element_blank())
totordplot2

grid.arrange(totordplot2, totordplot1,
             left="Percentage of Unique Items", main="Wikipedia Unique Article Percentage by Wikipedia Size (size as % of English)" )

oq <- data.frame(matrix(ncol = 0, nrow = length(filter1m$value)))
oq["langs"] <- q[c(order(q$rat,q$langs)),"langs]"
oq["rat"] <- q[c(order(q$rat,q$langs)),"rat"]
oq["tots"] <- q[c(order(q$rat,q$langs)),"tots"]
#oq["resid"] <- q[c(order(q$rat,q$langs)),"resid"]

oq.1 <- subset(oq, oq[,"tots"]>=100000)
oq.2 <- subset(oq, oq[,"tots"]>=10000 & oq[,"tots"]<100000)
p6 <- ggplot(oq.1, aes(x=langs, y=rat))+ 
  geom_bar(stat="identity", width = .75, alpha = .5, colour='blue') + 
  #geom_bar(stat="identity", width = .5, alpha = .5, colour='red', aes(x=langs, y=resid), data=oq.1)+
  scale_x_discrete(limits=oq.1$langs) +
  scale_y_continuous(labels=comma)+
  geom_text(aes(x=langs,y=0,label=paste(round(100*rat,2), '%', sep='')),hjust=0, angle = 90, colour='black', data=oq.1) +
  labs(y="Red difference from expected Unique articles. Blue percentage of articles Unique", title="Wikipedia article uniqueness percentage and difference in expected Uniqueness \n Minimum 100,000 articles")
p6


p7 <- ggplot(oq.2, aes(x=langs, y=rat))+ 
  geom_bar(stat="identity", width = .75, alpha = oq.2$tots/max(oq.2$tots), colour='blue') + 
  scale_x_discrete(limits=oq.2$langs) +
  scale_y_continuous(labels=comma)+
  geom_text(aes(x=langs,y=0,label=paste(round(100*rat,2), '%', sep='')),hjust=0, angle = 90, colour='black', data=oq.2) +
  labs(y="Red difference from expected Unique articles. Blue percentage of articles Unique", title="Wikipedia article uniqueness percentage and difference in expected Uniqueness \n Maximum 100,000 articles")
p7


#on to the pairs
#i'd like to make an upper triangular heatmap
filter2m.5k <- subset(filter2m, value > 5000)
pairs <- data.frame(matrix(ncol = 0, nrow = length(filter2m.5k$value)))
pairs["langs"] <- filter2m.5k[order(-filter2m.5k$value),"L1"]
pairs["tots"] <- filter2m.5k[order(-filter2m.5k$value),"value"]
pairs["hasEN"] <- grepl('en', pairs[,"langs"])
pairs["isP"] <- rep(TRUE, nrow(pairs))
#triples

filter3m.1k <- subset(filter3m, value > 1000)
trips <- data.frame(matrix(ncol = 0, nrow = length(filter3m.1k$value)))
trips["langs"] <- filter3m.1k[order(-filter3m.1k$value),"L1"]
                 trips["tots"] <- filter3m.1k[order(-filter3m.1k$value),"value"]
                 trips["hasEN"] <- grepl('en', trips[,"langs"])
                 trips["isP"] <- rep(FALSE, nrow(trips))
                 
pairsplot <- ggplot(pairs[1:20,], aes(x=langs, y=tots, fill=factor(! hasEN)))+
                   geom_bar(stat="identity", width=.75, size=3, alpha=1)+
                   scale_x_discrete(limits=pairs[1:20,]$langs,labels=rep('',20))+
                   scale_y_continuous(label=comma)+
                   scale_fill_brewer("English in cluster?", labels=c('Yes', 'No'), palette="Set2")+
                   geom_text(aes(x=langs,y=0,
                                 label=paste(langs, ' ', prettyNum(tots, big.mark=","))),
                             hjust=0, angle = 90, colour=muted('pink'))+
                   theme(axis.text.x = element_text(angle = 90, hjust = 1))+
                   labs(x="Pair",y='')
pairsplot


                 
tripsplot <- ggplot(trips[1:20,], aes(x=langs, y=tots, fill=factor(! hasEN)))+
                   geom_bar(stat="identity", width=.75, size=3, alpha=1)+
                   scale_x_discrete(limits=trips[1:20,]$langs,labels=rep('',20))+
                   scale_y_continuous(label=comma)+
                   scale_fill_brewer("English in cluster?", labels=c('Yes', 'No'), palette="Set2")+
                   geom_text(aes(x=langs,y=0,
                                 label=paste(langs, ' ', prettyNum(tots, big.mark=","))),
                             hjust=0, angle = 90, colour=muted('pink'))+
                   theme(axis.text.x = element_text(angle = 90, hjust = 1))+
                   labs(x="Triple",y='')
tripsplot
                 
                 
                 
tmp <- ggplot_gtable(ggplot_build(tripsplot))
leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
legend <- tmp$grobs[[leg]]
                 
grid.arrange(arrangeGrob(pairsplot + theme(legend.position="none"),
                                          tripsplot + theme(legend.position="none"),
                                          main ="Comparison of Highest Co-occurring Languge Clusters \n Pairs and Triples",
                                          left ="Number of Wikidata Items in Cluster"), legend, 
                              widths=unit.c(unit(.85, "npc"), legend$width), nrow=1)

tap <- rbind(pairs[1:20,], trips[1:20,])
tapo <-data.frame(matrix(ncol = 0, nrow = length(tap$tots)))
tapo["ordr"] <- order(tap$tots)
tapo["tots"]<- tap[order(tap$tots),"tots"]
tapo["langs"]<- tap[order(tap$tots),"langs"]
tapo["isP"]<- tap[order(tap$tots),"isP"]
tapo["hasEN"]<- tap[order(tap$tots),"hasEN"]

tapplot <- ggplot(tapo, aes(x=langs, y=tots, fill=factor(! isP), colour=factor(! hasEN)))+
geom_bar(stat="identity", width=.5, size=3, alpha=.7)+
  scale_x_discrete(limits=tapo$langs)+
  scale_y_continuous(label=comma)+
  scale_colour_brewer("English in cluster?", labels=c('Yes', 'No'), palette="Set1")+
  scale_fill_brewer("Cluster type", labels=c('Pair', 'Triple'), palette="Set2")+
  #scale_fill_hue("Cluster type", labels=c('Pair', 'Triple'), l=50, h=c(315,135), c=80)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  labs(y="Number of Wikidata Items in Cluster", x="Cluster", 
       title="Comparison of Highest Co-occurring Languges Clusters")



tapplot

#alln



allnm["univgroup"] <- rep(1,length(allnm[,0]))
allnm["ninecol"] <- as.numeric(allnm[,"L1"]) %% 8
allnm["rat"] <- allnm[,"value"] / sum(allnm[,"value"])  
allnm["sumsupto"] <- cumsum(allnm$rat)



allnplot1 <- ggplot(allnm, aes(x=factor(univgroup), y=rat, fill=factor(ninecol))) +
  scale_y_continuous(breaks=subset(allnm$sumsupto, allnm$sumsupto <0.971), 
                     labels=paste(round(100*subset(allnm$sumsupto, allnm$sumsupto <0.971),2), '%', sep=''))+
  scale_x_discrete(breaks="none",label="none")+
  geom_bar(stat ="identity" ,position = "stack") + 
  geom_text(aes(y=sumsupto-(rat/2),
                label=allnm$L1),
            size=(1/as.numeric(allnm$L1)) *15 ) +
  scale_fill_brewer(type = "seq", palette = 3,guide='none')+
  coord_cartesian(ylim=c(0, 1))+
  theme(title=element_blank())

allnplot2 <- ggplot(allnm, aes(x=factor(univgroup), y=rat, fill=factor(ninecol))) +
  scale_y_continuous(breaks=subset(allnm$sumsupto, allnm$sumsupto <0.992), 
                     labels=paste(round(100*subset(allnm$sumsupto, allnm$sumsupto <0.992),2), '%', sep=''))+
  scale_x_discrete(breaks="none",label="none")+
  geom_bar(stat ="identity" ,position = "stack") + 
  geom_text(aes(y=sumsupto-(rat/2),
                label=allnm$L1),
            size=(1/as.numeric(allnm$L1)) *60 ) +
  scale_fill_brewer(type = "seq", palette = 3,guide='none')+
  coord_cartesian(ylim=c(0.9442275, 1))+
  theme(title=element_blank())

allnplot3 <- ggplot(allnm, aes(x=factor(univgroup), y=rat, fill=factor(ninecol))) +
  scale_y_continuous(breaks=subset(allnm$sumsupto, allnm$sumsupto <0.9987), 
                     labels=paste(round(100*subset(allnm$sumsupto, allnm$sumsupto <0.9987),2), '%', sep=''))+
  scale_x_discrete(breaks="none",label="none")+
  geom_bar(stat ="identity" ,position = "stack") + 
  geom_text(aes(y=sumsupto-(rat/2),
                label=allnm$L1),
            size=(1/as.numeric(allnm$L1)) *120 ) +
  scale_fill_brewer(type = "seq", palette = 3,guide='none')+
  coord_cartesian(ylim=c(0.9951513, 1))+
  theme(title=element_blank())

allnplot4 <- ggplot(allnm, aes(x=factor(univgroup), y=rat, fill=factor(ninecol))) +
  scale_y_continuous(breaks=subset(allnm$sumsupto, allnm$sumsupto <0.99995), 
                     labels=paste(round(100*subset(allnm$sumsupto, allnm$sumsupto <0.99995),5), '%', sep=''))+
  scale_x_discrete(breaks="none",label="none")+
  geom_bar(stat ="identity" ,position = "stack", label="none") + 
  geom_text(aes(y=sumsupto-(rat/2),
                label=allnm$L1),
            size=(1/as.numeric(allnm$L1)) *360 ) +
  scale_fill_brewer(type = "seq", palette = 3,guide='none')+
  coord_cartesian(ylim=c(0.9998727, 1))+
  theme(title=element_blank())

grid.arrange(allnplot1, allnplot2, allnplot3, allnplot4, ncol=4, 
             left="Composition of Wikidata Items", sub="Number of Language Links in Wikidata Item", main="Composition of Wikidata Items by Language Link Count")