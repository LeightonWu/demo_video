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
                 seq(4, max(pop$Age, na.rm = TRUE) + 4, by = 5), sep = " - "),
  right = FALSE
)

pop_pyr <-  pop %>%
  mutate(Sex = ifelse(Sex == "m", "Male",
                      ifelse(Sex == "f", "Female", Sex)),
         Population = ifelse(test = Sex=="Female",
                             yes = -Population,
                             no = Population))


# Create pyramid for 2023 (latest)
gg_pyramid_2023 <- ggplot(data = filter(pop_pyr, Year==2023),
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


##################################DEPENDENCY
pop_dep_abs <-  pop_pyr %>%
  mutate(AgeCat= case_when(
    as.numeric(sub("^\\D*(\\d+).*", "\\1", AgeGroup)) <= 10 ~ "Child",
    as.numeric(sub("^\\D*(\\d+).*", "\\1", AgeGroup)) >= 15 &
      as.numeric(sub("^\\D*(\\d+).*", "\\1", AgeGroup)) <= 60 ~ "Working",
    TRUE ~ "Old"
  )) %>%
  group_by(Year,AgeCat) %>% summarise(Population=sum(abs(Population))) %>%
  ungroup()

pop_dep <- pop_dep_abs %>%
  pivot_wider(names_from="AgeCat", values_from="Population") %>%
  mutate(Total = Child+Old,
         Child = Child/Working*100,
         Old = Old/Working*100,
         Total = Total/Working*100) %>%
  select(-Working) %>%
  pivot_longer(cols = Child:Total,
               names_to = "AgeCat",
               values_to = "Dependency ratio") %>%
  droplevels()


gg_agecat_2023 <- ggplot(filter(pop_dep_abs, Year=="2023"),
                      aes(x = AgeCat, y = Population,
                          fill = AgeCat)) +
  geom_col() +
  xlab("Age category") +
  labs(title = "Dependency , Canada, 2023")

gg_dep_2023 <- ggplot(filter(pop_dep, Year=="2023"),
       aes(x = AgeCat, y = `Dependency ratio`,
           fill = AgeCat)) +
  geom_col() +
  xlab("Age category") +
  labs(title = "Dependency Ratios, Canada, 2023")

gg_dep <- ggplot(pop_dep,
                      aes(x = Year,
                          y = `Dependency ratio`, colour = AgeCat)) +
  geom_line(linewidth=1.5) +
  xlab("Year") +
  labs(title = "Dependency ratios, Poland, 1990-2024")






