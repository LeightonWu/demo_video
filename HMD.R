library(tidyverse)
library("HMDHFDplus")

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
  labs(title = "Population by Age Category, Canada, 2023")

gg_agecat <- ggplot(pop_dep_abs,
                 aes(x = Year,
                     y = Population, colour = AgeCat)) +
  geom_line(linewidth=1.5) +
  xlab("Year") +
  labs(title = "Population by Age Category, Canada, 1925-2023")



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
  labs(title = "Dependency Ratios, Canada, 1925-2023")


##################################LIFE TABLE#######################

#   n is the number of years within each age interval.
# qx shows the probability of dying in each specific-age interval. Notice, that for the last, open-ended,
# age group, this probability is 1. Why?
#   px indicates the probability of surviving in each specific-age interval.
# lx corresponds to the number of people alive after x years.
# dx denotes the number of deaths between x and x+n.
# Lx is the number of person-years lived between x and x+n.
# Tx refers to the total number of person-years left to live after age x.
# ex indicates the life expectancy at age x.



# just use gg_grow_out for mortality pattern over the years
# deaths <- readHMDweb(CNTRY = "CAN",
#                       item = "Deaths_5x5",
#                       username = "leighton.d@live.com",
#                       password = "mFAT^L9^es34",
#                       fixup = FALSE)



#female lifetable per 5x5
ltf2020 <- readHMDweb(CNTRY = "CAN",
                   item = "fltper_5x5",
                   username = "leighton.d@live.com",
                   password = "mFAT^L9^es34",
                   fixup = FALSE) %>%
  filter(Year=="2020") %>%
  select(Year, Age, mx, ax) %>%
  mutate(Age = as_factor(Age),
         Gender = "Female")

#male lifetable per 5x5
ltm2020 <- readHMDweb(CNTRY = "CAN",
                      item = "mltper_5x5",
                      username = "leighton.d@live.com",
                      password = "mFAT^L9^es34",
                      fixup = FALSE) %>%
  filter(Year=="2020") %>%
  select(Year, Age, mx, ax) %>%
  mutate(Age = as_factor(Age),
         Gender = "Male")


ltm2020_calc <- ltm2020 %>%
  mutate(n = case_when(
    Age=="0" ~ 1,
    Age=="1-4" ~ 4,
    TRUE ~ 5),
    qx = n*mx/(1+(n-ax)*mx),
    qx = ifelse(test = Age=="110+",yes = 1,no = qx),
    px = 1 - qx,
    lx = lag(cumprod(px), default=1),
    dx = lx - lead(lx, default = 0),
    Lx = n * lead(lx, default = 0) + (ax* dx),
    Lx = ifelse(Age=="110+",lx/mx,Lx),
    Tx = rev(cumsum(rev(Lx))),
    ex = Tx / lx
  )


ltf2020_calc <- ltf2020 %>%
  mutate(n = case_when(
    Age=="0" ~ 1,
    Age=="1-4" ~ 4,
    TRUE ~ 5),
    qx = n*mx/(1+(n-ax)*mx),
    qx = ifelse(test = Age=="110+",yes = 1,no = qx),
    px = 1 - qx,
    lx = lag(cumprod(px), default=1),
    dx = lx - lead(lx, default = 0),
    Lx = n * lead(lx, default = 0) + (ax* dx),
    Lx = ifelse(Age=="110+",lx/mx,Lx),
    Tx = rev(cumsum(rev(Lx))),
    ex = Tx / lx
  )


lt2020 <- tibble(rbind(ltf2020_calc, ltm2020_calc))


# probability of survival px
# probability of dying qx (age specific mortality rate asmr)
ASMR2020 <- lt2020 %>%
  gather(Probability, Value, qx:px) %>%
  ggplot(aes(x = Age, y = Value,
             group = interaction(Gender, Probability),
             color = Gender,
             linetype  = Probability)) +
  geom_line(linewidth = 2) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))

ASMR2020

# number of deaths dx
dx2020 <- lt2020 %>%
  ggplot(aes(x = Age, y = dx, group = Gender, color = Gender)) +
  geom_line(linewidth = 2) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))
dx2020

# life expectancy ex
ex2020 <- lt2020 %>%
  ggplot(aes(x = Age, y = ex, group = Gender, color=Gender)) +
  geom_line(linewidth = 2) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))
ex2020