.libPaths(tempdir())

install.packages(c("dplyr","stringr","ggplot2","patchwork","ragg","pharmaverseadam","forcats"))

library(dplyr)
library(ggplot2)
library(ragg)

# 1 - Lab spaghetti plot exercise
##Read in lab data
adlb2 <- pharmaverseadam::adlb %>%
  filter(PARAMCD=="CHOLES") %>%
  select(PARAM, PARAMCD, SUBJID, AVAL, ADY)

##ggplot and aesthetics
ggplot(data = adlb2, aes(x=ADY,y=AVAL,group=SUBJID)) +
  geom_line() +
  scale_x_continuous(name="Study Day", expand=c(0.01,0.01)) +
  scale_y_continuous(name=adlb2$PARAM, expand=c(0.01,0.01)) 



adlb3 <- pharmaverseadam::adlb %>%
filter(PARAMCD=="CHOLES" & AVISITN<=24 & SUBJID<1081 & SUBJID>1074) %>%
  select(PARAM, PARAMCD, SUBJID, AVAL, ADY)

##ggplot and aesthetics
f1 <- ggplot(data = adlb3, aes(x=ADY,y=AVAL,linetype=SUBJID,colour=SUBJID)) +
  geom_line(linewidth=1.3) +
  scale_x_continuous(limits=c(-25,200), breaks=seq(-25,200,25), name="Study Day", expand=c(0.01,0.01)) +
  scale_y_continuous(limits=c(4,9), breaks=seq(4,9,1), name=adlb3$PARAM, expand=c(0.01,0.01)) +
  scale_colour_manual(values=c("#FF0000","#0000FF","#FFA500","#800080"), name="Subject") +
  scale_linetype_manual(values=c("solid","11","twodash","longdash"), name="Subject") +
  theme_bw() +
  theme(legend.justification = c("left", "top"), legend.position.inside = c(.53, .98),legend.key.width = unit(2.05, 'cm'),
        legend.background = element_blank(), legend.box.background = element_rect(colour = "black")) +
  guides(color = guide_legend(nrow=1, position="inside"))

#Save figure
ggsave("f1.png", f1, device = agg_png, width=9.00, height=4.75, dpi=300, scaling=0.8)





# 2 - Vital signs box plot exercise
##Read in vital signs data
advs2 <- pharmaverseadam::advs %>%
  filter(PARAMCD=="DIABP" & AVISITN<=16 & !is.na(AVAL)) %>%
  select(PARAM, PARAMCD, USUBJID, AVISITN, AVISIT, AVAL, TRTA)


ggplot(data = advs2, aes(x=AVISIT,y=AVAL,group=interaction(AVISIT,TRTA),colour=TRTA)) +
  geom_boxplot(width=0.5, linewidth=0.5) +
  stat_summary(fun=mean, geom="point", shape="\u2606", size=4, position=position_dodge(0.5)) +
  geom_hline(yintercept=c(60,120), colour="red", linewidth=0.5, linetype="dashed") +
  scale_x_discrete(name="Study Visit") +
  scale_y_continuous(limits=c(30,130), breaks=seq(30,130,20), name=advs2$PARAM, expand=c(0.01,0.01)) +
  scale_colour_manual(values=c("#0000FF","#FFA500","#800080"), name="Treatment") +
  theme_bw() +
  theme(legend.position = "bottom")



library(forcats)

# IMPORTANT: enforce correct ordering via TRTAN and AVISITN (not alphabetical)
advs3 <- advs2 %>%
  mutate(
    TRTAN = case_when(
      TRTA == "Placebo" ~ 1,
      TRTA == "Xanomeline Low Dose" ~ 2,
      TRTA == "Xanomeline High Dose" ~ 3,
      TRUE ~ NA_real_
    )
  ) %>%
  mutate(
    TRTA = fct_reorder(TRTA, TRTAN),
    AVISIT = fct_reorder(AVISIT, AVISITN)
  )


##ggplot and aesthetics
f2 <- ggplot(data = advs3, aes(x=AVISIT,y=AVAL,group=interaction(AVISIT,TRTA),colour=TRTA)) +
  geom_boxplot(width=0.5, linewidth=0.5) +
  stat_summary(fun=mean, geom="point", shape="\u2606", size=4, position=position_dodge(0.5)) +
  geom_hline(yintercept=c(60,120), colour="red", linewidth=0.5, linetype="dashed") +
  scale_x_discrete(name="Study Visit") +
  scale_y_continuous(limits=c(30,130), breaks=seq(30,130,20), name=advs3$PARAM, expand=c(0.01,0.01)) +
  scale_colour_manual(values=c("#0000FF","#FFA500","#800080"), name="Treatment") +
  theme_bw() +
  theme(legend.position = "bottom")

#Save figure
ggsave("f2.png", f2, device = agg_png, width=9.00, height=4.75, dpi=300, scaling=0.8)





# 3 Adverse event bar chart exercise
##Read in AE data


adae <- pharmaverseadam::adae

adae2 <- pharmaverseadam::adae %>%
  filter(AESOC=="EYE DISORDERS") %>%
  mutate(
    TRTAN = case_when(
      TRT01A == "Placebo" ~ 1,
      TRT01A == "Xanomeline Low Dose" ~ 2,
      TRT01A == "Xanomeline High Dose" ~ 3,
      TRUE ~ NA_real_
    )
  ) %>%
  mutate(TRT01A = fct_reorder(TRT01A,TRTAN)) %>% 
  distinct(TRT01A, AEDECOD, USUBJID) %>% 
  group_by(AEDECOD,TRT01A,.drop=FALSE) %>% 
  count() 

##ggplot and aesthetics
f3 <- ggplot(data = adae2, aes(x=n,y=AEDECOD,group=interaction(AEDECOD,TRT01A),fill=TRT01A)) +
  geom_bar(stat="identity", position=position_dodge(0.9), width=0.7) +
  geom_text(aes(label=n), position=position_dodge(0.9), hjust=-0.25) +
  scale_y_discrete(labels = function(x) stringr::str_wrap(x, width = 10), name="Eye Disorder Preferred Terms") +
  scale_x_continuous(limits=c(0,2), breaks=seq(0,2,1), name="Number of Patients with at least 1 AE", expand=c(0.01,0.01)) +
  scale_fill_manual(values=c("black","red","light blue"), name="Treatment") +
  theme_bw() +
  theme(legend.position = "bottom")

#Save figure
ggsave("f3.png", f3, device = agg_png, width=9.00, height=4.75, dpi=300, scaling=0.5)



library(patchwork)

f1 + f2 + f3

f1 + f2 + f3 + plot_layout(widths=c(6,2,2))



