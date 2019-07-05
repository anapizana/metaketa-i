
### FIGURE 6: DEVIATION OF PRIORS VS. CERTAINTY

survey$uncertainty.quantile<-NULL
survey$uncertainty.quantile[survey$ranking_estimate_certainty=="No idea"]<-1
survey$uncertainty.quantile[survey$ranking_estimate_certainty=="Pretty uncertain"]<-0.75
survey$uncertainty.quantile[survey$ranking_estimate_certainty=="Uncertain but confident"]<-0.5
survey$uncertainty.quantile[survey$ranking_estimate_certainty=="Pretty certain"]<-0.25
survey$uncertainty.quantile[survey$ranking_estimate_certainty=="Completely certain"]<-0

survey$uncertainty.quantile.ordered<-ordered(survey$uncertainty.quantile,levels=c(0,0.25,0.5,0.75,1),labels=c("Completely certain","Pretty certain","Uncertain but confident","Pretty uncertain","No idea"))

bias.quantile.density<-ggplot(aes(x=bias.quantile),data=survey)+theme_few()+
  geom_density(aes(x=bias.quantile,..count..,group=uncertainty.quantile.ordered,fill=uncertainty.quantile.ordered),position="fill",color="black",alpha=0.8)+
  xlab("Deviation of Priors From Actual Performance")+ylab("Proportion")+
  labs(fill="Certainty")+scale_fill_grey(start=0.1,end=0.9)+
  geom_vline(aes(xintercept=0),color="white",lty=2,alpha=0.5)
bias.quantile.density
ggsave("Figures/BF_FigBiasCertainty.pdf",width=6,height=4)
