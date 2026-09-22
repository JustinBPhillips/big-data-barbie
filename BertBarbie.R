rm(list=ls())

library(data.table)
library(lubridate)
library(scales)
library(hrbrthemes)
library(ggplot2)
library(dplyr)

setwd('/home/justin/OneDrive/manuscripts/Barbie/bert/tmp')


### Load up BERTopic

### 1a) BERT addition
#### OK, we've now run everything through Python, Colab, etc.  Let's graph some results
library(data.table)
library(stringr)
results_bert = fread("./results_hdbscan_barbie0.005_mcp.tsv",header=TRUE,sep="\t",quote=FALSE)
results_all_sentences = fread("./all_sentences_barbie.tsv",header=TRUE,sep="\t",quote=FALSE)
unique_sentences = fread("./uniques_barbie.tsv",header=FALSE,sep="\t",quote=FALSE)
all_posts = fread("./all_barbie_posts_combo.tsv",header=FALSE,sep="\t",quote=FALSE)
all_posts$V1 = as.Date(as.POSIXct(all_posts$V1, origin="1970-01-01", tz="UTC"))
hdbscan_results = fread("./hdbscan_barbie0.005_mcp.tsv",header=TRUE,sep="\t",quote=FALSE)



#index is zero based for python, populate all sentences with topics and dates
results_all_sentences$Date = all_posts[results_all_sentences$index+1]$V1
results_all_sentences$Topic = hdbscan_results[match(results_all_sentences$sentence,unique_sentences$V1)]
results_all_sentences$Platform = all_posts[results_all_sentences$index+1]$V3
results_all_sentences$TopicName = results_bert[match(results_all_sentences$Topic,results_bert$Topic)]$Name


#reconstruct audience and engagement data
facebook = fread("../facebook.csv",na.strings = c("", "NA","<NA>","N/A"))
facebook$'Post Created' = as.POSIXct(facebook$'Post Created', tz = "NZST")
insta = fread("../instagram.csv",na.strings = c("", "NA","<NA>","N/A"))
insta$'Post Created' = as.POSIXct(insta$'Post Created' , tz = "NZST")

audience = as.numeric(pmax(
  c(facebook$`Likes at Posting`,rep(NA,length(insta$Account))),
  c(facebook$`Followers at Posting`,insta$`Followers at Posting`),
  na.rm = TRUE))
totalInteractions = as.numeric(c(facebook$`Total Interactions`,insta$`Total Interactions`))

all_posts$Audience = audience
all_posts$totalInteractions = totalInteractions

results_all_sentences$Audience = all_posts[results_all_sentences$index+1]$Audience
results_all_sentences$totalInteractions = all_posts[results_all_sentences$index+1]$totalInteractions

#print proportions
meta_proportions_table = data.table(hdbscan=c(),audience=c(),engagement=c())
english_classified = filter(results_all_sentences,Topic!="-1",Topic!="14",Topic!="15")



### draw up proportions table and create thumbnail graphs
library(lubridate)
library(dplyr)
library(ggplot2)
library(hrbrthemes)
library(scales)
library(data.table)
customPalette = c("#0866ff","#c72a52")
my_theme <- function(){
  list(
    theme_ipsum_rc(),
    scale_color_manual(values = customPalette),
    scale_linetype_manual(values = c(1,2))
  )
}

topics = unique(english_classified$Topic)
for(cluster in topics) {
  cluster_sentences = filter(results_all_sentences,Topic==cluster)
  
  theGraphData <- cluster_sentences %>% group_by(Date,Platform) %>% summarize(Posts = n())
  
  ragg::agg_png(paste("./thumbnails/timeseries_",cluster,".png",sep=""), width = 176, height = 99, units = "px", res = 60, scaling=1)
  tmp = ggplot(data=theGraphData,aes(x=Date,y=Posts,color=Platform)) +
    stat_smooth(method="loess",size=1.4,se=FALSE,span=0.2,aes(linetype=Platform))+
    my_theme()+theme(legend.position = "none")+
    scale_y_continuous(label=comma,name = "Daily sentences")+scale_y_log10(labels = label_number(accuracy = 1),limits = c(1,3000),breaks=c(3, 30, 300, 3000))+
    scale_x_date(breaks = "2 week", date_labels =  "%d-%b",name="Day")+
    theme(legend.position = "none",axis.text.x=element_text(size=12),axis.title.y = element_blank(),axis.text.y = element_text(size=12),axis.title.x= element_blank(),plot.margin = margin(t = 0.5,r = 0.2,b = 1,l = 0.2,unit = "mm"))
  print(tmp)
  dev.off()
  
  topic_percentOfReach = sum(cluster_sentences$Audience,na.rm=TRUE)/sum(results_all_sentences$Audience,na.rm=TRUE)
  topic_percentOfSocialInteractions = sum(cluster_sentences$totalInteractions,na.rm=TRUE)/sum(results_all_sentences$totalInteractions,na.rm=TRUE)
  meta_proportions_table = rbind(meta_proportions_table,data.table(hdbscan=c(cluster),audience=c(topic_percentOfReach),engagement=c(topic_percentOfSocialInteractions)))
  
  library(RColorBrewer)
  # get rid of green
  set1mod = brewer.pal(n = 6, name = "Set1")[3:4]
  my_bartheme <- function(){
    list(
      theme_ipsum_rc(),
      scale_fill_manual(values =  set1mod),
      scale_color_manual(values = set1mod)
    )
  }
  
  bardata = data.table(type=c("audience","social media engagements"),percent=round(c(topic_percentOfReach,topic_percentOfSocialInteractions)*100,2))
  tmp_barplot = ggplot(data=bardata,aes(x=type,y=percent,fill=type,color=type)) +
    geom_col(show.legend = FALSE)+
    my_bartheme()+coord_flip()+geom_text(aes(label = paste(percent,"%",sep="")),size = 9,hjust = -0.1, show.legend=FALSE)+ylim(0,13)+
    theme(legend.position = "none",axis.title.x = element_blank(),axis.text.x=element_blank(),axis.title.y = element_blank(),axis.text.y = element_blank(),plot.margin = margin(t = 0.1,r = 0.1,b = 0.1,l = 0.1,unit = "mm"))
  ragg::agg_png(paste("./thumbnails/barplot_",cluster,".png",sep=""), width = 176, height = 99, units = "px", res = 50, scaling=1)
  print(tmp_barplot)
  dev.off()
  
}


# do legends
library(grid)
library(gridExtra)
library(cowplot)
bardata = data.table(type=c("audience","social media engagements"),percent=round(c(topic_percentOfReach,topic_percentOfSocialInteractions)*100,2),levels=levels(as.factor(c("social media engagements","audience"))))

tmp_barplot = ggplot(data=bardata,aes(x=type,y=percent,fill=type,color=type)) +
  geom_col(show.legend = FALSE)+
  my_bartheme()+coord_flip()+geom_text(aes(label = paste(percent,"%",sep="")),size = 13,hjust = -0.1, show.legend=FALSE)+ylim(0,13)+
  theme(legend.position = "none",axis.title.x = element_blank(),axis.text.x=element_blank(),axis.title.y = element_blank(),axis.text.y = element_blank(),legend.margin=margin(0, 35, 0, 0))
tmp_barplotLEGEND = tmp_barplot + theme(legend.position = "right", legend.title = element_blank(), text = element_text(family='serif',size=19))+geom_col(show.legend = TRUE)+scale_fill_manual(breaks=c("social media engagements","audience"),values =  rev(set1mod))+scale_color_manual(breaks=c("social media engagements","audience"),values =  rev(set1mod))
legend <- cowplot::get_plot_component(tmp_barplotLEGEND, 'guide-box-right', return_all = TRUE)
tmplabel = grid.newpage()
ragg::agg_png(paste("./thumbnails/barplot_legend.png"), width = 176, height = 30, units = "px", res = 60, scaling=1)
grid.draw(legend)
dev.off()

library(grid)
library(gridExtra) 
tmp_timeseriesLEGEND = tmp + theme(legend.position = "top",legend.title=element_blank(),legend.text = element_text(family='serif',size=20),legend.key.spacing.x = unit(7, "mm"),legend.margin=margin(0, 18, 0, 0))
legend <- cowplot::get_plot_component(tmp_timeseriesLEGEND, 'guide-box-top', return_all = TRUE)
tmplabel = grid.newpage()
ragg::agg_png(paste("./thumbnails/timeseries_legend.png"), width = 176, height = 30, units = "px", res = 50, scaling=1)
grid.draw(legend)
dev.off()



# export results and images to html
library(xtable)
tmp_results_bert = results_bert
tmp_results_bert$Timeline = paste("<img src=\"/home/justin/OneDrive/manuscripts/Barbie/bert/tmp/thumbnails/timeseries_",results_bert$Topic,".png\" /img>",sep="")
tmp_results_bert$Socials = paste("<img src=\"/home/justin/OneDrive/manuscripts/Barbie/bert/tmp/thumbnails/barplot_",results_bert$Topic,".png\" /img>",sep="")
tmp_html = gsub(pattern="/img&gt;", replacement="/>",gsub(pattern="&lt;img",replacement="<img",print(xtable(tmp_results_bert), include.rownames=FALSE,type="html")))
tmp_html = gsub(pattern="<th> Timeline </th>", replacement="<th> Timeline <br><img src='/home/justin/OneDrive/manuscripts/Barbie/bert/tmp/thumbnails/timeseries_legend.png' /img></th>",tmp_html)
tmp_html = gsub(pattern="<th> Socials </th>", replacement="<th> Socials <br><img src='/home/justin/OneDrive/manuscripts/Barbie/bert/tmp/thumbnails/barplot_legend.png' /img></th>",tmp_html)
cat(tmp_html,file="./htmlTables/htmlTable.html")



#investigate top Meta topics
audiences = meta_proportions_table[order(meta_proportions_table$audience,decreasing=TRUE)][1:50]
engagements = meta_proportions_table[order(meta_proportions_table$engagement,decreasing=TRUE)][1:50]
both = filter(meta_proportions_table,hdbscan %in% audiences | hdbscan %in% engagements$hdbscan)
both = both[order(both$engagement,decreasing = TRUE)]

library(xtable)
abridged_results_bert = results_bert %>% filter(Topic %in% both$hdbscan)
abridged_results_bert = abridged_results_bert[match(both$hdbscan, abridged_results_bert$Topic),]
abridged_results_bert <- subset(abridged_results_bert, select = c("Topic","Name", "Representation"))
abridged_results_bert$Timeline = paste("<img src=\"/home/justin/OneDrive/manuscripts/Barbie/bert/tmp/thumbnails/timeseries_",abridged_results_bert$Topic,".png\" /img>",sep="")
abridged_results_bert$Socials = paste("<img src=\"/home/justin/OneDrive/manuscripts/Barbie/bert/tmp/thumbnails/barplot_",abridged_results_bert$Topic,".png\" /img>",sep="")
abridged_results_bert = abridged_results_bert[,!c("Topic")]
abridged_results_html = gsub(pattern="/img&gt;", replacement="/>",gsub(pattern="&lt;img",replacement="<img",print(xtable(abridged_results_bert), include.rownames=FALSE,type="html")))
abridged_results_html = gsub(pattern="<th> Timeline </th>", replacement="<th> Timeline <br><img src='/home/justin/OneDrive/manuscripts/Barbie/bert/tmp/thumbnails/timeseries_legend.png' /img></th>",abridged_results_html)
abridged_results_html = gsub(pattern="<th> Socials </th>", replacement="<th> Socials <br><img src='/home/justin/OneDrive/manuscripts/Barbie/bert/tmp/thumbnails/barplot_legend.png' /img></th>",abridged_results_html)
cat(abridged_results_html,file="./htmlTables/htmlTable_abridged.html")



