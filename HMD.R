library(tidyverse)
options(scipen = 999)
death <- read.csv("data/CANdeath.txt")
pop <- read.csv("data/CANpop.txt")
birth <- read.csv("data/CANbirth.txt")

# Group pop age into groups of 5
pop$AgeGroup <- cut(
  pop$Age,
  breaks = seq(0, max(pop$Age, na.rm = TRUE) + 5, by = 5),
  labels = paste(seq(0, max(pop$Age, na.rm = TRUE), by = 5),
                 seq(4, max(pop$Age, na.rm = TRUE) + 4, by = 5), sep = "-"),
  right = FALSE
)

pop <- pop %>%
  mutate(Sex = ifelse(Sex == "m", "Male",
                      ifelse(Sex == "f", "Female", Sex)))

pop_pyr = pop %>%
  mutate(Population = ifelse(test = Sex=="Female",
                             yes = -Population,
                             no = Population))


# Create pyramid for 2023 (latest)

pyramid_2023 <- ggplot(data = filter(pop_pyr, Year==2023),
                          aes(x = AgeGroup, y = Population/1000, fill = Sex)) +
  geom_col() +
  scale_y_continuous(labels=function(x) abs(x),
                     name="Population in 1,000s",
                     limits=c(-1800,1800)) +
  theme_minimal() +
  theme(plot.title = element_text(lineheight=.8, face="bold"),
        axis.text.x = element_text(angle=0, vjust=0.5),
        legend.position = "right") +
  labs(title = "Population of Canada, 2023") +
  coord_flip()


