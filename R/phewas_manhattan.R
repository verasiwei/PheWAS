#' This is an update of PheWAS Manhattan plot
#'
library(ggrepel)
library(ggplot2)
phewas_manhattan <- function(dat,pvalue){
  #add the groups and phecode description
  d=addPhecodeInfo(dat,groupnums = T,groupcolors = T)
  d$logp=-log10(d$p)
  d$color=as.factor(d$color)
  num=nlevels(as.factor(d$group))
  color=c("#FF0000FF","#FF4600FF", "#FF8B00FF" ,"#FFD100FF","#824acd",
          "#2ac075","#3b5998","#b58096","#a958a5","#d1a258",
          "#00FFB9FF", "#f06261" ,"#00B9FFFF" ,"#0074FFFF" ,"#002EFFFF",
          "#1700FFFF" ,"#5D00FFFF" ,"#A200FFFF" ,"#E800FFFF" ,"#FF00D1FF",
          "#FF008BFF", "#FF0046FF")
  levels(d$color)=color[1:num]
  ggplot(d,aes(x=group,y=logp,col=color))+
    geom_jitter()+
    geom_label_repel(aes(label=ifelse(logp>as.numeric(-log10(pvalue)),as.character(description),"")),hjust=0,vjust=1,size=3,col="black")+
    xlab("Phenotype")+ylab("-log10(p)")+ggtitle("PheWAS Plot")+
    theme(axis.text.x = element_text(angle = 45, hjust = 1,size=10),
          axis.title.x =element_text(size=10),
          axis.title.y =element_text(size=10),
          axis.title = element_text(size = 8,face = "bold"),
          panel.background = element_blank(),axis.line = element_line(colour = "black"),
          legend.position = "none")
  
}
