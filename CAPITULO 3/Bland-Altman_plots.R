#Bland-Altman plots, also called difference plots

#The Bland-Altman plot determines differences between the two estimations against the observer estimations, or reference sensu (Krouwer JS (2008) Why Bland-Altman plots should use X, not (Y + X)/2 when X is a reference method. Stat Med 27:778–780) 
library(ggplot2)
library(cowplot)
library(plotly)
library(ggpubr) 
library(gridExtra)
library(gridGraphics)
library(doBy)
library(reshape)
library(dplyr)
library(plotly)
library(ggpubr)




library(dplyr)
#combine the 2 dataframes 
blandaltman <- merge(dfRobot, dfHuman, 
                  by = c("Name", "Date", "DataSet", "site", "reef name", "reef area"), 
                  all = TRUE,
                  suffixes = c(".robot", ".human"))


#"Dictyota dichotoma"                           
#"Macroalgae: Filamentous / filiform : red"     
#"Macroalgae:Encrusting:Red:Calcareous:Crustose"
#"Corynactis carnea"                            
#"Substrate: Consolidated (hard)"               
#"Macroalgae: Sheet-like / membraneous: red"    
#"Ulva"                                         
#"Aulacomya atra"


SC <- blandaltman %>%
  select(Name, Date, DataSet, site, `reef name`, `reef area`, 
         `Corynactis carnea.human`, `Corynactis carnea.robot`)%>%
  mutate(diff = `Corynactis carnea.human` - `Corynactis carnea.robot`) %>%
  mutate(avg = (`Corynactis carnea.robot`+`Corynactis carnea.human`)/2)

#by reef area
SC2 <- SC %>% 
  group_by(`reef area`) %>% 
  summarise_at(vars(`Corynactis carnea.robot`,`Corynactis carnea.human`,diff,avg), list(mean, sd)) 
colnames(SC2) <- c("reef area","Corynactis carnea.R.mean","Corynactis carnea.H.mean","diff","avg","Corynactis carnea.R.SD","Corynactis carnea.H.SD","diff.SD","avg.SD")



g1 <- ggplot(SC, aes(x=avg, y=diff,colour=`reef area`,shape=site))+geom_hline(yintercept=0) + geom_point()+ theme_bw()+labs(y="Differencia en % de cobertura entre humano e IA", x="% de cobertura promedio por humano",title = "Corynactis carnea")+theme(legend.position="none",plot.margin=unit(c(0,0,1,1), "cm"))

g2 <- ggplot(SC2, aes(x=avg, y=diff,colour=`reef area`)) + geom_point()+theme_bw() + labs(y="", x="% de cobertura promedio por humano (± DS)") + theme(axis.title.y =element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank())+geom_hline(yintercept=0)+theme(legend.position="none",plot.margin=unit(c(0,1,1,-0.1), "cm"))+geom_errorbar(aes(ymin =diff-diff.SD,ymax =diff+diff.SD),width = 0.2) + geom_errorbarh(aes(xmin =avg-avg.SD ,xmax = avg+avg.SD),height =-.2)

#set y limits and brecks
limits <- c(-40, 40)#see plot to set 
breaks <- seq(limits[1], limits[2], by=8)

# assign common axis to both plots
p1.common.y <- g1 + scale_y_continuous(limits=limits, breaks=breaks)
p2.common.y <- g2 + scale_y_continuous(limits=limits, breaks=breaks)

# At this point, they have the same axis, but the axis lengths are unequal, so ...
# build the plots 
p1.common.y <- ggplot_gtable(ggplot_build(p1.common.y))
p2.common.y <- ggplot_gtable(ggplot_build(p2.common.y))

# copy the plot height from p1 to p2
p2.common.y$heights <- p1.common.y$heights
#plot grid
grid1 <- grid.arrange(p1.common.y,p2.common.y,ncol=2,widths=c(1,1))

#add legend
legend <- ggplot(SC, aes(x=avg, y=diff,colour=`reef area`,shape=site))+geom_hline(yintercept=0) + geom_point()+ theme_bw()+labs(y="Differencia en % de cobertura entre humano e IA", x="",title = "Dictyota dichotoma")+theme(legend.position="bottom",legend.title = element_blank(),legend.box="vertical",plot.margin=unit(c(0,0,1,1), "cm"))
legend_plot <- get_legend(legend)#take legend

plot.Dictyotadichotoma <- plot_grid(grid1,legend_plot, ncol = 1, rel_heights = c(.25,1))


plot_grid(
plot.Dictyotadichotoma,
plot.MacroalgasFilamentosasrojas,
plot.Macroalgascalcareasinscrustantes,
plot.Corynactiscarnea,
plot.SUBSTRATE,
plot.Macroalgaslaminaresrojas,
plot.ulva,
plot.Aulacomyaatra, ncol=2)




