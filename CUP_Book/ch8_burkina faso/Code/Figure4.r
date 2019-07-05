### MAP OF STUDY MUNICIPALITIES

#Read in Shape Files for Map
communes<-readRDS("Shape Files/BFA_adm3.rds")
names(communes)
unique(communes$NAME_1)
unique(communes$TYPE_3)
unique(communes$VARNAME_3)
length(unique(communes$NAME_3))

provinces<-readRDS("Shape Files/BFA_adm2.rds")
regions<-readRDS("Shape Files/BFA_adm1.rds")
country<-readRDS("Shape Files/BFA_adm0.rds")

concordance<-read.csv("Shape Files/Commune Concordance for Shape Files.csv",header=TRUE)

## Collapse to commune-level data set

#Read in list of communes covered by the study
commune.data<-unique(survey[,c("region","commune")])

#Set variable "study" to "Included" for all municipalities covered by the study
commune.data$study<-"Included"

#Merge with concordance list of municipality names
commune.data<-merge(concordance,commune.data,by=c("commune"),all.x=TRUE)

#Set variable "study" to "Not included" for all other municipalities
commune.data$study[is.na(commune.data$study)]<-"Not included"

#Merge with data slot of shape files
temp<-merge(communes@data,commune.data,by=c("NAME_1","NAME_3"),all.x=TRUE)
communes@data<-temp[order(temp$ID_3),]

#Tidy up shape files and merge information from data slot into tidied-up dataset
require(broom)
communes@data$id<-communes@data$ID_3
communes.2<-tidy(communes)
communes.2<-merge(communes.2,communes@data,by="id",all.x=TRUE)

#Make map with ggplot
burkina.communes<-ggplot(communes.2)+
  geom_polygon(aes(x=long,y=lat,group=group),fill="white",color="gray80",alpha=1,data=communes.2)+
  geom_polygon(aes(x=long,y=lat,group=group,fill=factor(study)),alpha=0.8,color="gray20",lwd=0.2)+
  geom_polygon(aes(x=long,y=lat,group=id),color="black",fill="NA",lwd=0.8,data=regions)+
  labs(fill="Study areas (municipalities)")+
  scale_fill_manual(values=c("Gray50","White"),labels=c("Included","Not Included"))+
  theme_few()+xlab("")+ylab("")+coord_equal()+
  theme(legend.position=c(0.75,0.15))+
  theme(axis.text.x=element_blank(),axis.text.y=element_blank(),axis.ticks.x=element_blank(),axis.ticks.y=element_blank())
#scale_fill_gradient2(name=scale.name,limits=c(scale.min,scale.max),midpoint=scale.mid,low=scale.colors[1],mid=scale.colors[2],high=scale.colors[3])
burkina.communes 

#Save map
ggsave("Figures/BF_StudyAreas.pdf",width=8,height=5,units="in",dpi=150)

