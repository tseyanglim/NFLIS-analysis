################################################################################
###                           FIGURES                                        ###
################################################################################
library(geofacet)
library(grid)
library(formattable)

# Read in the state label file for plotting 
state_label <- read_csv("./data-public/State labels.csv", show_col_types = FALSE)


##########################################
###     Monthly trend line graph       ###
##########################################

colorlist <- c("#006ddb", "#009e73", "#984ea3", "#CC0033","#004949", "#E69F00", '#fa8072',"#7b6500",'#56B4E9')

alldrugs_month_v2 <- alldrugs_month %>% 
  group_by(Drug.sample) %>%
  mutate(Max_fent = max(P100_fent)) %>%
  mutate(Drug.sample = as_factor(Drug.sample)) %>% 
  mutate(Drug.sample = fct_reorder(Drug.sample, .x=P100_fent, .fun=max, .desc=TRUE))

years <- seq(as.Date("2013-01-01"), as.Date("2023-01-01"), by = "1 year")

total_samples <- alldrugs_year %>% filter(State == "0.US") %>% group_by(Drug.sample) %>% summarise(count=sum(Total)) %>% deframe()

### Panel A
### Heroin and Club drugs
data.A <- alldrugs_month_v2[alldrugs_month_v2$Drug.sample %in% c("Heroin","Club drugs"),]

max(data.A$P100_fent)
# 50.8

sum(data.A$Total[data.A$Drug.sample == "Heroin"]) # 1,325,203
sum(data.A$Total[data.A$Drug.sample == "Club drugs"]) # 97,025

Figure1.panelA <-  
  ggplot(data = data.A, aes(x= Time, group = Drug.sample, color = Drug.sample))+
  scale_colour_manual(name = "", values = colorlist[c(1,2)],
                      breaks = c("Heroin", "Club drugs"),
                      labels = c(str_glue("Heroin ({comma(total_samples[['Heroin']], format='d')})"),
                                 str_glue("Club drugs ({comma(total_samples[['Club drugs']], format='d')})\ne.g., ketamine, MDMA"))) +
  scale_x_date(name='',breaks=years, date_labels="%Y",
               limits=c(as.Date("2013-01-01"), as.Date("2023-12-31"))) +
  scale_y_continuous(name='', limits=c(0, 52), breaks=seq(0,52,5)) +
  geom_point(size=1, aes(y=P100_fent), alpha=0.3) +
  geom_line(linewidth=0.8, aes(y=P100_fent),alpha=0.3) +
  geom_line(linewidth=2, aes(y=P100_fent_roll12mon.V2)) +
  ggtitle("(a)")+
  guides(color = guide_legend(title = "Substance (total samples)",
                              override.aes = list(alpha = c(1, 1)))) +
  theme(plot.margin = margin(1,13,1,1, "cm"),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.title = element_text(size=20, face="bold"),
        axis.title.x = element_text(vjust=-2),
        axis.text.y = element_text(size=18),
        axis.text.x = element_text(size = 18, hjust = -0.4),
        axis.line = element_line(colour = "black"),
        legend.text = element_text(size=18),
        legend.title = element_text(size=22, face="bold"),
        plot.title = element_text(size = 18, face="bold"),
        legend.key.width = unit(2,"cm"),
        legend.key.height = unit(1.2,"cm"),
        legend.key=element_blank(),
        # legend.title.align=0.5,
        legend.position = c(1.22, 0.5))

### Panel B
### Cocaine, Methamphetamine , Prescription opioids

data.B <- alldrugs_month_v2[alldrugs_month_v2$Drug.sample %in% c("Cocaine","Methamphetamine","Prescription opioids"),]

max(data.B$P100_fent)

sum(data.B$Total[data.B$Drug.sample == "Cocaine"]) # 2,012,480
sum(data.B$Total[data.B$Drug.sample == "Methamphetamine"]) # 3,850,672
sum(data.B$Total[data.B$Drug.sample == "Prescription opioids"]) # 795,166

Figure1.panelB <- 
  ggplot(data = data.B, 
         aes(x= Time, group = Drug.sample, color = Drug.sample))+
  scale_colour_manual(name = "", values = colorlist[c(3, 5, 6)],
                      breaks = c("Cocaine", "Methamphetamine", "Prescription opioids"),
                      labels = c(str_glue("Cocaine ({comma(total_samples[['Cocaine']], format='d')})"), 
                                 str_glue("Methamphetamine ({comma(total_samples[['Methamphetamine']], format='d')})"), 
                                 str_glue("Prescription opioids ({comma(total_samples[['Prescription opioids']], format='d')})")))+
  scale_x_date( name='',breaks=years, date_labels="%Y") +
  scale_y_continuous( name='',limits=c(0,4.1), breaks=seq(0,4.1,0.5)) +
  geom_point(size=1, aes(y=P100_fent), alpha=0.3) +
  geom_line(linewidth=0.8, aes(y=P100_fent), alpha=0.3) +
  geom_line(linewidth=2, aes(y=P100_fent_roll12mon.V2))+
  ggtitle("(b)")+
  theme(plot.margin = margin(1,13,1,1, "cm"),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.title = element_text(size=20, face="bold"),
        axis.title.x = element_text(vjust=-2),
        axis.text.y = element_text(size=18),
        axis.text.x = element_text(size = 18, hjust = -0.4),
        axis.line = element_line(colour = "black"),
        legend.text = element_text(size=18),
        legend.title = element_text(size=18),
        legend.title.align=0.5,
        plot.title = element_text(size = 18, face="bold"),
        legend.key.width = unit(2,"cm"),
        legend.key.height = unit(1.2,"cm"),
        legend.key=element_blank(),
        legend.position = c(1.22, 0.5))


### Panel C
### Hallucinogens, Prescription benzodiazepines, Prescription stimulants, Cannabinoids

data.C <- alldrugs_month_v2[alldrugs_month_v2$Drug.sample %in% c("Hallucinogens","Cannabinoids","Prescription benzodiazepines","Prescription stimulants"),]

max(data.C$P100_fent)
sum(data.C$Total[data.C$Drug.sample == "Hallucinogens"]) # 144,235
sum(data.C$Total[data.C$Drug.sample == "Cannabinoids"]) # 3,177,205
sum(data.C$Total[data.C$Drug.sample == "Prescription benzodiazepines"]) # 482,977
sum(data.C$Total[data.C$Drug.sample == "Prescription stimulants"]) # 142,107

Figure1.panelC <- 
  ggplot(data = data.C, 
         aes(x= Time, group = Drug.sample, color = Drug.sample))+
  scale_colour_manual(name = "", values = colorlist[c(4,7:9)],
                      breaks = c("Hallucinogens", "Cannabinoids", "Prescription benzodiazepines", "Prescription stimulants"),
                      labels = c(str_glue("Hallucinogens ({comma(total_samples[['Hallucinogens']], format='d')})"),
                                 str_glue("Cannabinoids ({comma(total_samples[['Cannabinoids']], format='d')})"),
                                 str_glue("Prescription\nbenzodiazepines ({comma(total_samples[['Prescription benzodiazepines']], format='d')})"),
                                 str_glue("Prescription\nstimulants ({comma(total_samples[['Prescription stimulants']], format='d')})"))) +
  scale_x_date(name='',breaks=years, date_labels="%Y") +
  scale_y_continuous( name='',limits=c(0,1.3), breaks=seq(0,1.3,0.2)) +
  geom_point(size=1, aes(y=P100_fent), alpha=0.3) +
  geom_line(linewidth=0.8, aes(y=P100_fent), alpha=0.3) +
  geom_line(linewidth=2, aes(y=P100_fent_roll12mon.V2))+
  ggtitle("(c)") +
  theme(plot.margin = margin(1,13,1,1, "cm"),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.title = element_text(size=20, face="bold"),
        axis.title.x = element_text(vjust=-2),
        axis.text.y = element_text(size=18),
        axis.text.x = element_text(size = 18, hjust = -0.4),
        axis.line = element_line(colour = "black"),
        legend.text = element_text(size=18),
        legend.title = element_text(size=18),
        legend.title.align=0.5,
        plot.title = element_text(size = 18, face="bold"),
        legend.key.width = unit(2,"cm"),
        legend.key.height = unit(1.2,"cm"),
        legend.key=element_blank(),
        legend.position = c(1.22, 0.5))


plotcombine <- ggarrange(Figure1.panelA, Figure1.panelB,Figure1.panelC, ncol = 1, nrow = 3)
plotcombine <- annotate_figure(plotcombine,
                               left = text_grob("% of samples of different substances with co-occurrence of fentanyl", 
                                                size=20, rot=90, face="bold",vjust=2.5), 
                               bottom = text_grob("Year", size=20, face="bold",vjust=-2))
plotcombine
ggsave(file=str_glue("./figures/Fig1_{datestring}.{fig_ext}"),
       plot=plotcombine, width = 15, height = 15)


# Save data for Figure 1
data1 <- alldrugs_month_v2 %>%
  mutate(Time = substr(Time,1,7),
         Total_samples = Total,
         Fent_present_samples=Count_fent,
         Percent_fent_cooccurrence_monthly=round(P100_fent,3),
         Percent_fent_12month_average=round(P100_fent_roll12mon.V2,3)) %>%
  arrange(Drug.sample, Time) %>%
  select(Drug.sample, Time, Total_samples, Fent_present_samples,
         Percent_fent_cooccurrence_monthly, Percent_fent_12month_average)

data1 %>% write_csv("./outputs/Figure 1 data.csv")


##########################################
###  Samples & co-occurrence by state  ###
##########################################

fent_samples_long <- fent_samples %>% 
  pivot_longer(c("Listed_only", "Count_fent_listed", "Count_fent_only", "Count_fent_other", "Other_only")) %>% 
  mutate(`Sample type` = as_factor(name), 
         `Sample type` = fct_relevel(`Sample type`, c("Other_only", "Count_fent_other", "Count_fent_only", "Count_fent_listed", "Listed_only")))

fent_samples_long2 <- merge(fent_samples_long, state_label, by=c("State"), all.x=TRUE)

fent_bar_percs <- fent_samples_long %>% 
  filter(State == '0.US') %>% 
  group_by(`Sample type`) %>% 
  summarise(Total = sum(value)) %>% 
  mutate(P100 = Total / sum(Total) * 100) %>% 
  pull(P100)

fent_bar_labels <- c(str_glue("Substances examined, w/o fentanyl ({round(fent_bar_percs[5], digits=1)}%)"),
                     str_glue("Co-occurring fentanyl & substances examined ({round(fent_bar_percs[4], digits=1)}%)"),
                     str_glue("Fentanyl only ({format(round(fent_bar_percs[3], digits=1),nsmall=1)}%)"),
                     str_glue("Co-occurring fentanyl & substances not examined ({round(fent_bar_percs[2], digits=1)}%)"),
                     str_glue("Neither fentanyl nor substances examined ({round(fent_bar_percs[1], digits=1)}%)"))


# Create a named vector for the State labels
state_labels <- setNames(fent_samples_long2$State_name, fent_samples_long2$State)

p_fent_samples <- ggplot(fent_samples_long2) + 
  geom_col(aes(x=Time, y=value, fill=`Sample type`)) + 
  geom_line(data=(fent_samples %>% 
                    filter(State != "0.US") %>% 
                    mutate(scaled_P100 = P100_fent * 4000)),
            aes(x=Time, y=scaled_P100, colour="% of substances examined \nw/ co-occurring fent."), 
            linewidth=1) +
  geom_line(data=(fent_samples %>% 
                    filter(State != "0.US") %>% 
                    mutate(scaled_P100_fa = P100_fent_all * 4000)),
            aes(x=Time, y=scaled_P100_fa, colour="% of all samples w/ any fentanyl"),
            linewidth=1) +
  scale_y_continuous(sec.axis = sec_axis(~. / 4000),labels = scales::comma_format()) +
  coord_cartesian(ylim=c(0,200000)) +
  facet_geo(~ State, labeller = as_labeller(state_labels)) +
  theme_bw() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.text.x = element_blank(),
    axis.line.y = element_line(colour = "black"),
    # axis.ticks.x = element_blank(),
    axis.title = element_text(size=15, face="bold"),
    strip.text.x = element_text(size = 12),
    plot.tag = element_text(size = 9, hjust=0.5, vjust=0.5), 
    plot.tag.position = c(0.5157, 0.248)
  ) +
  labs(tag = "2013       2018       2023") +
  scale_color_manual(values = c("% of all samples w/ any fentanyl"="blue", "% of substances examined \nw/ co-occurring fent."="black"),
                     guide = guide_legend(title = "", position = "bottom", nrow=2, 
                                          theme = theme(legend.text=element_text(size=12)))) +
  scale_fill_manual(values = c('lightgray','green','red','purple','lightblue'), 
                    guide = guide_legend(title = "Samples", position = "bottom", direction = "horizontal", nrow=3, 
                                         theme = theme(legend.title=element_text(size=14, face="bold"), legend.text=element_text(size=12))),
                    labels = c(fent_bar_labels[5], fent_bar_labels[4], fent_bar_labels[3], fent_bar_labels[2], fent_bar_labels[1])) +
  scale_x_continuous(breaks=c(2013, 2018, 2023)) +
  xlab("Year") + 
  ylab("Samples reported")
p_fent_samples

ggsave(file=str_glue("./figures/Fig2_{datestring}.{fig_ext}"),
       plot=p_fent_samples, width = 16, height = 11, dpi=400)



# Save data for Figure 2
data2 <- fent_samples_long2 %>% 
  select(State, Time, P100_fent, P100_fent_all, name, value) %>% 
  pivot_wider(names_from=name, values_from=value) %>% 
  select(State, Time, Listed_only:Other_only, P100_fent, P100_fent_all) %>% 
  rename(Listed_no_fent = Listed_only, 
         Listed_fent = Count_fent_listed,
         Fent_only = Count_fent_only, 
         Nonlisted_and_fent = Count_fent_other,
         Nonlisted_only = Other_only, 
         Perc_listed_cooccurring_fent = P100_fent, 
         Perc_total_samples_fent = P100_fent_all)
  
data2 %>% write_csv("./outputs/Figure 2 data.csv")

##########################################
###  Yearly trend by state map graph   ###
##########################################

dat.state0 <- alldrugs_year %>% filter(State!="0.US")

dat.state <- merge(dat.state0,state_label, by=c("State"), all.x=TRUE)

# Create a named vector for the State labels
state_labels <- setNames(dat.state$State_name, dat.state$State)

# Save data for Figure 3
data3 <- dat.state %>%
  mutate(State.name = State_name,
         Year = Time,
         Total_samples = Total,
         Fent_present_samples = Count_fent,
         Percent_fent_cooccurrence = round(P100_fent,3)) %>%
  arrange(State, Drug.sample, Year) %>%
  select(State, Drug.sample, Year, Total_samples,
         Fent_present_samples, Percent_fent_cooccurrence)

data3 %>% write_csv("./outputs/Figure 3 data.csv")

##### Figure for heroin #####

check.lowcount.heroin <- dat.state[dat.state$Drug.sample=="Heroin"& 
                                     (dat.state$Total<=5 & dat.state$Count_fent!=0),]
# State Time Drug.sample Total Count_fent P100_fent    Region           Division   State_name
# DE 2021      Heroin     2          1  50.00000     South     South Atlantic     Delaware
# ME 2023      Heroin     2          1  50.00000 Northeast        New England        Maine
# SD 2023      Heroin     3          1  33.33333   Midwest West North Central South Dakota

dat.heroin0 <- dat.state[dat.state$Drug.sample=="Heroin",]
dat.heroin <- anti_join(dat.heroin0, check.lowcount.heroin,
                        by = c("State", "Time", "Drug.sample", "Total", "Count_fent", "P100_fent"))

map.year.heroin.bars <- 
  ggplot(data=dat.heroin) +
  facet_geo(~ State, labeller = as_labeller(state_labels))+
  geom_col(aes(x=Time, y=Total, fill=Drug.sample), alpha=0.4) +
  geom_point(aes(x = Time, y = P100_fent * 250, color=Drug.sample), size=1.5) +
  geom_line(aes(x = Time, y = P100_fent * 250, color=Drug.sample), linewidth=1) +
  scale_y_continuous(name="Total samples reported", labels = scales::comma_format(), sec.axis = sec_axis(~. / 250, name="% fentanyl co-occurrence")) +
  scale_colour_manual(name = "", values = "#006ddb")+
  scale_fill_manual(name = "", values = "#006ddb")+
  scale_x_continuous(breaks=c(2013, 2018, 2023)) +
  xlab("Year") + 
  ggtitle("(a)")+
  theme_bw() +
  theme(plot.margin = margin(1,1,1,1, "cm"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.title = element_text(size=15, face="bold"),
        axis.title.x = element_text(vjust=-2),
        axis.title.y = element_text(vjust=4),
        axis.title.y.right = element_text(vjust=4),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size=12),
        axis.line.x = element_blank(),
        axis.line.y = element_line(colour = "black"),
        # axis.ticks.x = element_blank(),
        strip.text.x = element_text(size = 12),
        plot.title = element_text(size = 15, face="bold"),
        legend.position="top",
        legend.justification='left',
        legend.text = element_text(size=15),
        legend.title = element_text(size=15),
        legend.key.width = unit(2,"cm"),
        legend.key=element_blank(),
        plot.tag = element_text(size = 9, hjust=0.5, vjust=0.5), 
        plot.tag.position = c(0.5075, 0.146)
  ) +
  labs(tag = "2013      2018       2023")

ggsave(file=str_glue("./figures/Fig3A_heroin_{datestring}.{fig_ext}"),
       plot=map.year.heroin.bars,width = 16, height = 10, dpi=400)


##### Figures combining cocaine and meth  #####

check.lowcount <- dat.state[dat.state$Drug.sample %in% c("Cocaine","Methamphetamine") & 
                              (dat.state$Total<=5 & dat.state$Count_fent!=0),]
# State  Time Drug.sample                  Total Count_fent P100_fent
# <chr> <dbl> <chr>                        <dbl>      <dbl>     <dbl>
# 1 VT     2023 Cocaine                          5          2        40

dat.2drugs0 <- dat.state[dat.state$Drug.sample %in% c("Cocaine","Methamphetamine"),]
dat.2drugs <- anti_join(dat.2drugs0, check.lowcount,
                        by = c("State", "Time", "Drug.sample", "Total", "Count_fent", "P100_fent"))

map.year.stims.bars <-  
  ggplot(data=dat.2drugs) +
  facet_geo(~ State,labeller = as_labeller(state_labels))+
  geom_col(aes(x=Time, y=Total, fill=Drug.sample), alpha=0.4) +
  geom_point(aes(x = Time, y = P100_fent * 3280, color = Drug.sample ), size=1.5) +
  geom_line(aes(x = Time, y = P100_fent * 3280, color = Drug.sample ), linewidth=1) +
  scale_y_continuous(name="Total samples reported", labels = scales::comma_format(), sec.axis = sec_axis(~. / 3280, name="% fentanyl co-occurrence")) +
  scale_fill_manual(name = "", values = c("#984ea3","#004949"))+
  scale_colour_manual(name = "", values = c("#984ea3","#004949"))+
  scale_x_continuous(breaks=c(2013, 2018, 2023)) +
  xlab("Year") + 
  ggtitle("(b)")+
  theme_bw() +
  theme(plot.margin = margin(1,1,1,1, "cm"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.title = element_text(size=15, face="bold"),
        axis.title.x = element_text(vjust=-2),
        axis.title.y = element_text(vjust=4),
        axis.title.y.right = element_text(vjust=4),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size=12),
        axis.line.x = element_blank(),
        axis.line.y = element_line(colour = "black"),
        # axis.ticks.x = element_blank(),
        strip.text.x = element_text(size = 12),
        plot.title = element_text(size = 15, face="bold"),
        legend.position="top",
        legend.justification='left',
        legend.text = element_text(size=15),
        legend.title = element_text(size=15),
        legend.key.width = unit(2,"cm"),
        legend.key=element_blank(),
        plot.tag = element_text(size = 9, hjust=0.5, vjust=0.5), 
        plot.tag.position = c(0.51, 0.146)
  ) +
  labs(tag = "2013      2018       2023")

ggsave(file=str_glue("./figures/Fig3B_stims_{datestring}.{fig_ext}"),
       plot=map.year.stims.bars, width = 16, height = 10, dpi=400)



##### Figures Club drugs #####
colorlist <- c("#006ddb", "#009e73", "#984ea3", "#CC0033","#004949", "#E69F00", '#fa8072',"#7b6500",'#56B4E9')


check.lowcount.club <- dat.state[dat.state$Drug.sample %in% c("Club drugs") & 
                              (dat.state$Total<=5 & dat.state$Count_fent!=0),]
# State  Time Drug.sample Total Count_fent P100_fent
# <chr> <dbl> <chr>       <dbl>      <dbl>     <dbl>
# 1 DE     2019 Club drugs      2          1        50
# 2 CT     2023 Club drugs      1          1       100

dat.club0 <- dat.state[dat.state$Drug.sample %in% c("Club drugs"),]
dat.club <- anti_join(dat.club0, check.lowcount.club,
                        by = c("State", "Time", "Drug.sample", "Total", "Count_fent", "P100_fent"))

map.year.club.bars <-  
  ggplot(data=dat.club) +
  facet_geo(~ State,labeller = as_labeller(state_labels))+
  geom_col(aes(x=Time, y=Total, fill=Drug.sample), alpha=0.4) +
  geom_point(aes(x = Time, y = P100_fent * 40, color = Drug.sample ), size=1.5) +
  geom_line(aes(x = Time, y = P100_fent * 40, color = Drug.sample ), linewidth=1) +
  scale_y_continuous(name="Total samples reported", labels = scales::comma_format(), sec.axis = sec_axis(~. / 40, name="% fentanyl co-occurrence")) +
  scale_fill_manual(name = "", values = c('#009e73'))+
  scale_colour_manual(name = "", values = c('#009e73'))+
  scale_x_continuous(breaks=c(2013, 2018, 2023)) +
  xlab("Year") + 
  ggtitle("(a)")+
  theme_bw() +
  theme(plot.margin = margin(1,1,1,1, "cm"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.title = element_text(size=15, face="bold"),
        axis.title.x = element_text(vjust=-2),
        axis.title.y = element_text(vjust=4),
        axis.title.y.right = element_text(vjust=4),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size=12),
        axis.line.x = element_blank(),
        axis.line.y = element_line(colour = "black"),
        # axis.ticks.x = element_blank(),
        strip.text.x = element_text(size = 12),
        plot.title = element_text(size = 15, face="bold"),
        legend.position="top",
        legend.justification='left',
        legend.text = element_text(size=15),
        legend.title = element_text(size=15),
        legend.key.width = unit(2,"cm"),
        legend.key=element_blank())

ggsave(file=str_glue("./figures/FigS2A_club_{datestring}.{fig_ext}"),
       plot=map.year.club.bars, width = 16, height = 10, dpi=400)



##### Figures Cannabinoids #####

check.lowcount.cannab <- dat.state[dat.state$Drug.sample %in% c("Cannabinoids") & 
                                   (dat.state$Total<=5 & dat.state$Count_fent!=0),]

dat.cannab <- dat.state[dat.state$Drug.sample %in% c("Cannabinoids"),]

map.year.cannab.bars <-  
  ggplot(data=dat.cannab) +
  facet_geo(~ State,labeller = as_labeller(state_labels))+
  geom_col(aes(x=Time, y=Total, fill=Drug.sample), alpha=0.4) +
  geom_point(aes(x = Time, y = P100_fent * 12801, color = Drug.sample ), size=1.5) +
  geom_line(aes(x = Time, y = P100_fent * 12801, color = Drug.sample ), linewidth=1) +
  scale_y_continuous(name="Total samples reported", labels = scales::comma_format(), sec.axis = sec_axis(~. / 12801, name="% fentanyl co-occurrence")) +
  scale_fill_manual(name = "", values = c('#fa8072'))+
  scale_colour_manual(name = "", values = c('#fa8072'))+
  scale_x_continuous(breaks=c(2013, 2018, 2023)) +
  xlab("Year") + 
  ggtitle("(b)")+
  theme_bw() +
  theme(plot.margin = margin(1,1,1,1, "cm"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.title = element_text(size=15, face="bold"),
        axis.title.x = element_text(vjust=-2),
        axis.title.y = element_text(vjust=4),
        axis.title.y.right = element_text(vjust=4),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size=12),
        axis.line.x = element_blank(),
        axis.line.y = element_line(colour = "black"),
        # axis.ticks.x = element_blank(),
        strip.text.x = element_text(size = 12),
        plot.title = element_text(size = 15, face="bold"),
        legend.position="top",
        legend.justification='left',
        legend.text = element_text(size=15),
        legend.title = element_text(size=15),
        legend.key.width = unit(2,"cm"),
        legend.key=element_blank())

ggsave(file=str_glue("./figures/FigS2B_cannab_{datestring}.{fig_ext}"),
       plot=map.year.cannab.bars, width = 16, height = 10, dpi=400)




##### Figures combining Hallucinogens,Prescription benzodiazepines, Prescription stimulants #####

check.lowcount.pbps <- dat.state[dat.state$Drug.sample %in% c("Hallucinogens","Prescription benzodiazepines","Prescription stimulants") & 
                                   dat.state$P100_fent>=25,]
# State  Time Drug.sample                  Total Count_fent P100_fent
# <chr> <dbl> <chr>                        <dbl>      <dbl>     <dbl>
# 1 CT     2022 Prescription benzodiazepines    20          5        25
# 2 ME     2022 Prescription benzodiazepines     4          1        25

dat.pbps0 <- dat.state[dat.state$Drug.sample %in% c("Hallucinogens","Prescription benzodiazepines","Prescription stimulants"),]
dat.pbps <- anti_join(dat.pbps0, check.lowcount.pbps,
                      by = c("State", "Time", "Drug.sample", "Total", "Count_fent", "P100_fent"))
dat.pbps$Drug.sample2 = ifelse(dat.pbps$Drug.sample=="Hallucinogens", "0.Hallucinogens", 
                               ifelse(dat.pbps$Drug.sample=="Prescription stimulants","1.Prescription stimulants","2.Prescription benzodiazepines"))
table(dat.pbps$Drug.sample, dat.pbps$Drug.sample2)

map.year.pbps.bars <-  
  ggplot(data=dat.pbps) +
  facet_geo(~ State,labeller = as_labeller(state_labels))+
  geom_col(aes(x=Time, y=Total, fill=Drug.sample2), alpha=0.4) +
  geom_point(aes(x = Time, y = P100_fent * 455, color = Drug.sample2 ), size=1.5) +
  geom_line(aes(x = Time, y = P100_fent * 455, color = Drug.sample2 ), linewidth=1) +
  scale_y_continuous(name="Total samples reported", labels = scales::comma_format(), sec.axis = sec_axis(~. / 455, name="% fentanyl co-occurrence")) +
  scale_fill_manual(name = "", values = c("#CC0033",'#56B4E9',"#7b6500"),labels=c("Hallucinogens","Prescription stimulants","Prescription benzodiazepines"))+
  scale_colour_manual(name = "", values = c("#CC0033",'#56B4E9',"#7b6500"), labels=c("Hallucinogens","Prescription stimulants","Prescription benzodiazepines"))+
  scale_x_continuous(breaks=c(2013, 2018, 2023)) +
  xlab("Year") + 
  ggtitle("(c)")+
  theme_bw() +
  theme(plot.margin = margin(1,1,1,1, "cm"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.title = element_text(size=15, face="bold"),
        axis.title.x = element_text(vjust=-2),
        axis.title.y = element_text(vjust=4),
        axis.title.y.right = element_text(vjust=4),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size=12),
        axis.line.x = element_blank(),
        axis.line.y = element_line(colour = "black"),
        # axis.ticks.x = element_blank(),
        strip.text.x = element_text(size = 12),
        plot.title = element_text(size = 15, face="bold"),
        legend.position="top",
        legend.justification='left',
        legend.text = element_text(size=15),
        legend.title = element_text(size=15),
        legend.key.width = unit(2,"cm"),
        legend.key=element_blank())

ggsave(file=str_glue("./figures/FigS2C_pbps_{datestring}.{fig_ext}"),
       plot=map.year.pbps.bars, width = 16, height = 10, dpi=400)
