### BASICS ###
Dataframe <- read.csv("LAB C_Poster Survey Data.csv", header = T)[-1, ]
CurrentAttitude <- Dataframe$C_g7_Q1
FutureViews  <- Dataframe$C_g7_Q2
Personality <- Dataframe$C_g7_Q3
Students.data <-
  data.frame(CurrentAttitude, FutureViews, Personality)[-1, ]

#split students into introverts (1) and others (2)
Students.data$PersonalityNum <- as.numeric(
  factor(Students.data$Personality,
    levels = c('Introvert',
               'Ambivert',
               'Extrovert'),
    labels = c('Introverts',
               'Others',
               'Others')
  ))

### GRAPH 1 ###

#turn responses into easier titles
Students.data$CurrentAttitude <-
  factor(Students.data$CurrentAttitude,
    levels = c('It disgusts me',
               'Hate it',
               'Don\'t mind',
               'Like it',
               'Love it'),
    labels = c('Detest',
               'Hate',
               'Neutral',
               'Like',
               'Love')
  )

#orders data labels
Students.data$FutureViews <-
  factor(Students.data$FutureViews,
    levels = c(
      "Strongly agree",
      "Agree",
      "Neutral",
      "Disagree",
      "Strongly disagree"
    )
  )

Students.data$CurrentAttitude <-
  factor(Students.data$CurrentAttitude,
         levels = c("Detest",
                    "Hate",
                    "Neutral",
                    "Like",
                    "Love")
         )

#run the below lines the first time
#install.packages('extrafont')
#install.packages('scales')
#font_import()
library(ggplot2)
library(extrafont)
library(scales)

viewcolors <-
  c(
    'Agree' = "chartreuse4",
    'Strongly agree' = "chartreuse",
    'Neutral' = "cornsilk",
    'Disagree' = "firebrick3",
    'Strongly disagree' = "red1"
  )

stacked_graph <-
  ggplot(data = Students.data,
         aes(x = CurrentAttitude, y = PersonalityNum, fill = FutureViews)) +
  geom_col(position = "fill") +
  scale_y_continuous(labels = percent)

p.labels <- c("Introverts", "Others")
names(p.labels) <- c("1", "2")

stacked_graph + facet_wrap(~ PersonalityNum, labeller = labeller(PersonalityNum = p.labels)) +
  labs(x = "Views On Telework", y = "Desire To Continue Telework (%)", title =
         " Present vs Future Views On Telework/School") +
  theme_bw() +
  theme(
    axis.title = element_text(
      size = 12,
      lineheight = .8,
      vjust = 1,
      family = "Times New Roman"
    ),
    axis.text.x = element_text(
      angle = 50,
      size = 12,
      vjust = 0.75,
      hjust = 0.45,
      family = "Schoolbell"
    ),
    axis.ticks = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    legend.title = element_blank(),
    #allows us to choose font and font size
    #use 'fonts()' to see options
    plot.title = element_text(
      size = 24,
      lineheight = .8,
      vjust = 1,
      family = "Rock Salt",
      margin = margin(10, 0, 15, 0)
    )
  ) +
  scale_fill_manual(values = viewcolors)

### GRAPH 2 ###
#install.packages('ggpubr')
#install.packages('rstatix')

Students.data$FutureViewsNum <-
  as.numeric(factor(
    Students.data$FutureViews,
    levels = c(
      "Strongly agree",
      "Agree",
      "Neutral",
      "Disagree",
      "Strongly disagree"
    )
  ))

Students.data$CurrentAttitudeNum <-
  as.numeric(factor(
    Students.data$CurrentAttitude,
    levels = c("Detest",
               "Hate",
               "Neutral",
               "Like",
               "Love")
  ))

Students.data$Personality <-
  factor(Students.data$Personality,
         levels = c('Introvert',
                    'Ambivert',
                    'Extrovert'),
         labels = c('Introverts',
                    'Others',
                    'Others')
  )

Students.data$Telework <-
  ((6 - Students.data$CurrentAttitudeNum) + Students.data$FutureViewsNum) / 2

ggplot(data = Students.data, aes(x = Personality, y = Telework, fill=Personality)) +
  labs(x = "Personality Type", y = "Desire To Continue Telework", title =
         "Introversion and Telework") +
  scale_fill_manual(values=c("#cc0000", "#e69f00")) +
  geom_boxplot(color="darkblue") +
  geom_jitter(width=0.13, height=0.3, color = "#99ccff") + 
  theme_dark() +
  theme(
    axis.title = element_text(
      size = 12,
      lineheight = .8,
      vjust = 1,
      family = "Times New Roman"
    ),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    legend.position = "none"
  )

library('rstatix')
#outlier check
Students.data %>%
  identify_outliers(Telework)
#there are no extreme outliers

#normality check
library('ggpubr')

ggdensity(Students.data$Telework)
ggqqplot(Students.data$Telework)
#graphs seem approx. Normal
shapiro_test(Students.data$Telework)
#p-value below 0.05 indicates non-normality, but sample size
#is greater than 50 so visual inspection is superior

#variance check

library(psych)
library(car)
describe(Students.data$Telework)
#mean of 3.44, sd of 0.9
describeBy(Students.data$Telework, Students.data$Personality)
#introverts: mean of  2.96, sd of 0.96
#others: mean of 3.59, sd of 0.83
cor.test(Students.data$Telework, Students.data$Personality, use = "complete.obs")
#p-value 0.001445, cor 0.302816, [0.121, 0.465]
t.test(Telework ~ Personality, data = Students.data, var.equal = TRUE)
#t = -3.271, df = 106, p-value = 0.001445
summary(aov(Telework ~ Personality, data = Students.data))
#f-value = 10.7, p = 0.00145
Anova(aov(Telework ~ Personality, data = Students.data), type="III")

library(Hmisc)
ggplot(Students.data, aes(Personality, Telework)) + 
  stat_summary(fun = mean, geom = "bar", fill = c("red", "blue"), color = "black") + 
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.2) +
  labs (x = "Group", y = "Continued Telework Desire") + ylim(0, 5) + theme_bw()
