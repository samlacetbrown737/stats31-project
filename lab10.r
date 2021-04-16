### BASICS ###
Dataframe <- read.csv("LAB C_Poster Survey Data.csv", header = T)[-1, ]
CurrentAttitude <- Dataframe$C_g7_Q1
FutureViews  <- Dataframe$C_g7_Q2
Personality <- Dataframe$C_g7_Q3
Students.data <-
  data.frame(CurrentAttitude, FutureViews, Personality)[-1, ]

#split students into introverts (1) and others (2)
Students.data$Personality <- as.numeric(
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
         aes(x = CurrentAttitude, y = Personality, fill = FutureViews)) +
  geom_col(position = "fill") +
  scale_y_continuous(labels = percent)

p.labels <- c("Introverts", "Others")
names(p.labels) <- c("1", "2")

stacked_graph + facet_wrap(~ Personality, labeller = labeller(Personality = p.labels)) +
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
      size = 12,
      lineheight = .8,
      vjust = 1,
      family = "Rock Salt",
      margin = margin(10, 0, 15, 0)
    )
  ) +
  scale_fill_manual(values = viewcolors)

### GRAPH 2 ###

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
Students.data$Telework <-
  ((6 - Students.data$CurrentAttitudeNum) + Students.data$FutureViewsNum) / 2
