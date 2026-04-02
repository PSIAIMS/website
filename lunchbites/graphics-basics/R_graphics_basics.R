.libPaths(tempdir())

install.packages(c("haven","dplyr","ggplot2","patchwork","ragg"))


#############################################################
###### Data   ############################################
#############################################################


set.seed(123)

## ---- dADSL: Subject-level dataset ----

n_subj <- 20

dadsl <- data.frame(
  SUBJID   = sprintf("SUBJ%03d", 1:n_subj),
  WEIGHTBL = round(rnorm(n_subj, mean = 75, sd = 12), 1),   # kg
  HEIGHTBL = round(rnorm(n_subj, mean = 170, sd = 10), 1),  # cm
  TRT01P   = sample(c("Placebo", "Drug A"), n_subj, replace = TRUE),
  TRTDURD  = sample(30:180, n_subj, replace = TRUE)
)

## ---- dADLB: Lab dataset (ALT only) ----

# number of lab records per subject
n_visits <- 5

dadlb <- do.call(
  rbind,
  lapply(dadsl$SUBJID, function(id) {
    data.frame(
      SUBJID  = id,
      PARAMCD = "ALT",
      ADY     = c(1, 7, 14, 28, 56),
      AVAL    = round(rlnorm(n_visits, meanlog = log(30), sdlog = 0.4), 1)
    )
  })
)

## ---- Check structure ----
str(dadsl)
str(dadlb)


library(haven)
#Read in adsl
adsl <- read_sas("C:/Users/Martin.Brown1/Desktop/OldCBTI/WorkBTI/RDemo/adsl.sas7bdat")

#############################################################
###### Easy start   ##########################################
#############################################################



#Read in the required packages
library(dplyr)
library(ggplot2)


#Pick your data and your x and y variables as first aesthetics
ggplot(data = dadsl, aes(x=WEIGHTBL,y=HEIGHTBL))

#Then choose your geom, ‘+’ is used to combine ggplot2 elements
ggplot(data = dadsl, aes(x=WEIGHTBL,y=HEIGHTBL)) +
  geom_point()


#############################################################
######  Layers  ############################################
#############################################################

#Choosing different geometric object layers – can change geom to area
ggplot(data = dadsl, aes(x=WEIGHTBL,y=HEIGHTBL)) +
  geom_line()

ggplot(data = dadsl, aes(x=TRT01P,y=TRTDURD)) +
  geom_boxplot()

#Can add more than 1 layer (the later code lines go on top of the earlier code lines)
ggplot(data = dadsl, aes(x=TRT01P,y=TRTDURD)) +
  geom_boxplot() +
  geom_point()

ggplot(data = dadsl, aes(x=WEIGHTBL,y=HEIGHTBL)) +
  geom_point() +
  geom_hline(yintercept=200)


#############################################################
######  geoms  ###########################################
#############################################################

#Can bring the data and aesthetics part into geom
ggplot() +
  geom_point(data=dadsl, aes(x=TRT01P,y=TRTDURD))

#Can bring different data into different geoms (run with and without extra point)
ggplot() +
  geom_boxplot(data = dadsl, aes(x=TRT01P,y=TRTDURD)) +
  geom_point(data = dadsl %>% filter(SUBJID=="SUBJ007"), aes(x=TRT01P,y=150))


#############################################################
###### Make changes at data level   ##########################
#############################################################

dadsl2 <- dadsl %>% 
          mutate(WEIGHTBL = ifelse(HEIGHTBL > 180, NA, HEIGHTBL))

#Back to initial example
ggplot(data = dadsl2, aes(x=WEIGHTBL,y=HEIGHTBL)) +
  geom_point()

#Find the missing value
View(dadsl2)

#Let’s filter it out before to remove warning message
dadsl3 <- dadsl2 %>% 
  select(WEIGHTBL, HEIGHTBL) %>%
  filter(!is.na(WEIGHTBL))

ggplot(data = dadsl3, aes(x=WEIGHTBL,y=HEIGHTBL)) +
  geom_point()


#############################################################
###### Colours, linetypes, fills, alpha   #############
#############################################################

#Back to earlier examples and add more aesthetics
ggplot(data = dadsl2, aes(x=WEIGHTBL,y=HEIGHTBL)) +
  geom_point(colour="red", shape="square plus", size=4)

ggplot(data = dadsl2, aes(x=WEIGHTBL,y=HEIGHTBL)) +
  geom_line(colour="#CD6090", linetype="longdash", linewidth=1.5) +
  geom_hline(yintercept=200,colour="#96CDCD", linetype="dotted", linewidth=4)

ggplot(data = dadsl, aes(x=TRT01P,y=TRTDURD)) +
  geom_boxplot(fill="purple", alpha=0.3)


#############################################################
###### Grouping  ###########################################
#############################################################


#Fill by treatment group
ggplot(data = dadsl, aes(x=TRT01P,y=TRTDURD,fill=TRT01P)) +
  geom_boxplot()

#Line by patient using lab data
##Read in lab data
dadlb2 <- dadlb %>%
  filter(SUBJID %in% c("SUBJ001","SUBJ004","SUBJ007"))

##Just all points joined up
ggplot(data = dadlb2, aes(x=ADY,y=AVAL)) +
  geom_line()

##Add group aspect but nothing to distinguish (and therefore no legend)
ggplot(data = dadlb2, aes(x=ADY,y=AVAL,group=SUBJID)) +
  geom_line()

##Add group aspect and linetype to distinguish
ggplot(data = dadlb2, aes(x=ADY,y=AVAL,linetype=SUBJID)) +
  geom_line()


#############################################################
###### Grouping and colour  ###########################################
#############################################################

#Continue with last example and choose colours for each line
ggplot(data = dadlb2, aes(x=ADY,y=AVAL,colour=SUBJID)) +
  geom_line(linewidth=1.5) +
  scale_colour_manual(values = c("dark green", "black", "grey60"), name="Subject")
#scale_colour_manual(values = c("#0072B2", "#D55E00", "#CC79A7"), name="")


#############################################################
###### Axes  ###########################################
#############################################################

#Continuous data example
ggplot(data = dadlb2, aes(x=ADY,y=AVAL,colour=SUBJID)) +
  geom_line(linewidth=1.5) +
  scale_colour_manual(values = c("#0072B2", "#D55E00", "#CC79A7"), name="Subject") +
  scale_x_continuous(limits=c(0,60), breaks=seq(0,60,10), expand=c(0.01,0.01), name="Study Day") +
  scale_y_continuous(limits=c(10,70), breaks=seq(10,70,10), expand=c(0.01,0.01), name="ALT Result")

#Discrete data example - can update the data instead
ggplot(data = dadsl, aes(x=TRT01P,y=TRTDURD)) +
  geom_boxplot() +
  scale_x_discrete(labels=c("Placebo"="P","Xanomeline High Dose"="X2","Xanomeline Low Dose"="X1"), name="Treatment")


#############################################################
###### Themes  ###########################################
#############################################################

theme_bw() 
theme_linedraw()
theme_light()
theme_dark()
theme_minimal()
theme_classic()
theme_void()
theme_test()



#############################################################
###### Theme modification   ###########################################
#############################################################

#Example
ggplot(data = dadlb2, aes(x=ADY,y=AVAL,colour=SUBJID)) +
  geom_line(linewidth=1.5) +
  scale_colour_manual(values = c("#0072B2", "#D55E00", "#CC79A7"), name="Subject") +
  scale_x_continuous(limits=c(0,60), breaks=seq(0,60,10), expand=c(0.01,0.01), name="Study Day") +
  scale_y_continuous(limits=c(10,70), breaks=seq(10,70,10), expand=c(0.01,0.01), name="ALT Result") +
  theme(
    text = element_text(size = 10),
    legend.position="bottom",
    legend.background = element_blank(), 
    legend.box.background = element_rect(colour = "black"),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    plot.margin = unit(c(0.25, 0.75, 0.25, 0.75),"inches")
  )


#############################################################
###### Patchwork   ###########################################
#############################################################

#Example
f1 <- ggplot(data = dadlb2, aes(x=ADY,y=AVAL,colour=SUBJID)) +
  geom_line(linewidth=1.5) +
  scale_colour_manual(values = c("#0072B2", "#D55E00", "#CC79A7"), name="Subject") +
  scale_x_continuous(limits=c(0,60), breaks=seq(0,60,10), expand=c(0.01,0.01), name="Study Day") +
  scale_y_continuous(limits=c(10,70), breaks=seq(10,70,10), expand=c(0.01,0.01), name="ALT Result") +
  theme(
    text = element_text(size = 10),
    legend.position="bottom",
    legend.background = element_blank(), 
    legend.box.background = element_rect(colour = "black"),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank()
  )
f2 <- ggplot() +
  geom_boxplot(data = dadsl, aes(x=TRT01P,y=TRTDURD))

library(patchwork)
f1 + f2 


f1 + f2 + plot_layout(widths=c(1:2))


#############################################################
###### Image saving  ###########################################
#############################################################

#Set figure from last slide to the name f3
f3 <- f1 + f2

#Standard ggsave call – save png to working directory – device can be jpeg, tiff, eps, pdf etc.

ggsave("f3.png", f3, device = png, width=9.00, height=4.75, dpi=300)

#Improvements with ragg package

library(ragg)
ggsave("f3r.png", f3, device = agg_png, width=9.00, height=4.75, dpi=300, scaling=0.7)




#############################################################
###### Example with treatment and visit ordering driven from data  ####
#############################################################

set.seed(123)

# Define subjects and visits
n_subj <- 120
visits <- 1:16

# Assign treatment per subject (numeric drives correct order)
subj_trt <- data.frame(
  USUBJID = paste0("SUBJ", sprintf("%03d", 1:n_subj)),
  TRTAN = sample(1:3, n_subj, replace = TRUE)
)

dadvs <- expand.grid(
  USUBJID = subj_trt$USUBJID,
  AVISITN = visits
) %>%
  arrange(USUBJID, AVISITN) %>%
  left_join(subj_trt, by = "USUBJID") %>%
  mutate(
    # Treatment labels (NOT alphabetically ordered correctly)
    TRTA = case_when(
      TRTAN == 1 ~ "Placebo",      
      TRTAN == 2 ~ "Xanomeline Low Dose",
      TRTAN == 3 ~ "Xanomeline High Dose"
    ),
    
    # Visit labels
    AVISIT = paste0("Visit ", AVISITN),
    
    # Parameter info
    PARAMCD = "DIABP",
    PARAM = "Diastolic Blood Pressure (mmHg)",
    
    # Simulated values with treatment effect
    AVAL = round(
      rnorm(
        n(),
        mean = 80 +
          case_when(
            TRTAN == 1 ~ -3,
            TRTAN == 2 ~ -6,
            TRTAN == 3 ~ 0
          ) +
          AVISITN * 0.2,
        sd = 10
      ),
      1
    )
  ) %>%
  select(PARAM, PARAMCD, USUBJID, AVISITN, AVISIT, AVAL, TRTA, TRTAN)


ggplot(data = dadvs, aes(x=AVISIT,y=AVAL,group=interaction(AVISIT,TRTA),colour=TRTA)) +
  geom_boxplot(width=0.5, linewidth=0.5) +
  stat_summary_bin(fun=mean, geom="point", shape="\u2606", size=4, position=position_dodge(0.5)) +
  geom_hline(yintercept=c(60,120), colour="red", linewidth=0.5, linetype="dashed") +
  scale_x_discrete(name="Study Visit") +
  scale_y_continuous(limits=c(30,130), breaks=seq(30,130,20), name=dadvs$PARAM, expand=c(0.01,0.01)) +
  scale_colour_manual(values=c("#0000FF","#FFA500","#800080"), name="Treatment") +
  theme_bw() +
  theme(legend.position = "bottom")



library(forcats)

# IMPORTANT: enforce correct ordering via TRTAN and AVISITN (not alphabetical)
dadvs2 <- dadvs %>%
  mutate(
    TRTA = fct_reorder(TRTA, TRTAN),
    AVISIT = fct_reorder(AVISIT, AVISITN)
  )


##ggplot and aesthetics
f4 <- ggplot(data = dadvs2, aes(x=AVISIT,y=AVAL,group=interaction(AVISIT,TRTA),colour=TRTA)) +
  geom_boxplot(width=0.5, linewidth=0.5) +
  stat_summary_bin(fun=mean, geom="point", shape="\u2606", size=4, position=position_dodge(0.5)) +
  geom_hline(yintercept=c(60,120), colour="red", linewidth=0.5, linetype="dashed") +
  scale_x_discrete(name="Study Visit") +
  scale_y_continuous(limits=c(30,130), breaks=seq(30,130,20), name=dadvs$PARAM, expand=c(0.01,0.01)) +
  scale_colour_manual(values=c("#0000FF","#FFA500","#800080"), name="Treatment") +
  theme_bw() +
  theme(legend.position = "bottom")

#Save figure
ggsave("f4.png", f4, device = agg_png, width=9.00, height=4.75, dpi=300, scaling=0.8)
