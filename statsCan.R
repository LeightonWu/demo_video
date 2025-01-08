library(tidyverse)
options(scipen = 999)

fert <- read.csv("data/fertility.csv")
grow <- read.csv("data/growth.csv")
imm <- read.csv("data/immigration_22to24.csv")


####################################ASFR
ASFR <- fert %>% filter(REF_DATE==2023,
                GEO=="Canada, place of residence of mother") %>%
        select(Characteristics, VALUE)
# remove totals
ASFR0 <- ASFR[-c((nrow(ASFR)-1):nrow(ASFR)), ]
ASFR0$AgeGroup <- sub(".*(\\d{2,2} to \\d{2,2}).*", "\\1", ASFR0$Characteristics)
ASFR0$AgeGroup <- sub(" to ", "-", ASFR0$AgeGroup)
ASFR0

gg_ASFR_2023 <- ggplot(ASFR0, aes(x=AgeGroup, y=VALUE))+
  geom_col(fill="tomato1")+
  theme_minimal() +
  theme(plot.title = element_text(lineheight=.8, face="bold"),
        axis.text.x = element_text(angle=0, vjust=0.5),
        legend.position = "right") +
  labs(title = "Age-Specific Fertility Rate of Canada, 2023",
       x = "Age Group", y = "Live Births per 1000 Women")

######################################################################


glimpse(grow)

grow0 <- grow %>% filter(GEO=="Canada",
                            Components.of.population.growth %in%
                              c("Immigrants", "Emigrants",
                                "Births", "Deaths")) %>%
  select(REF_DATE, TYPE = Components.of.population.growth,
                        VALUE)

grow_in <- grow0 %>% filter(TYPE %in% c("Immigrants", "Births")) %>%
                        mutate(VALUE = ifelse(test = TYPE=="Immigrants",
                                              yes = -VALUE,
                                              no = VALUE))

grow_out <- grow0 %>% filter(TYPE %in% c("Deaths", "Emigrants")) %>%
                        mutate(VALUE = ifelse(test = TYPE=="Emigrants",
                                              yes = -VALUE,
                                              no = VALUE))


gg_grow_in <- ggplot(grow_in, aes(x = REF_DATE, y = VALUE, fill = TYPE)) +
  geom_col()+
  scale_y_continuous(labels=function(x) abs(x))+
  labs(title = "Counts of Births and Immigrants of Canada",
       y="Population Growths",
       x="Year") +
  theme_minimal() +
  coord_flip()

gg_grow_out <- ggplot(grow_out, aes(x = REF_DATE, y = VALUE, fill = TYPE)) +
  geom_col()+
  scale_y_continuous(labels=function(x) abs(x))+
  labs(title = "Counts of Deaths and Emigrants of Canada",
       y="Population Growths",
       x="Year") +
  theme_minimal() +
  coord_flip()


#########################################immigration dep
imm0 <- imm %>% filter(REF_DATE=="2022/2023",
                       !(Age.group %in% c("All ages", "-1 year"))) %>%
  select(Age.group, VALUE) %>%
  mutate(AgeCat = case_when(
    as.numeric(sub("^\\D*(\\d+).*", "\\1", Age.group)) <= 10 ~ "Child",
    as.numeric(sub("^\\D*(\\d+).*", "\\1", Age.group)) >= 15 &
      as.numeric(sub("^\\D*(\\d+).*", "\\1", Age.group)) <= 60 ~ "Working",
    TRUE ~ "Old"
  )
  ) %>%
  group_by(AgeCat) %>% summarise(VALUE=sum(abs(VALUE))) %>%
  ungroup()

imm1 <- imm0 %>%
  pivot_wider(names_from="AgeCat", values_from="VALUE") %>%
  mutate(Total = Child+Old,
         Child = Child/Working*100,
         Old = Old/Working*100,
         Total = Total/Working*100) %>%
  select(-Working) %>%
  pivot_longer(cols = Child:Total,
               names_to = "AgeCat",
               values_to = "VALUE") %>%
  droplevels()


gg_imm_dep_2023 <- ggplot(imm1,
                              aes(x = AgeCat, y = VALUE, fill=AgeCat))+
  geom_col()+
  xlab("Age Category") +
  labs(title = "Dependency Ratio of Immigrants, Canada, 2023")

gg_imm_agecat_2023 <- ggplot(imm0,
                      aes(x = AgeCat, y = VALUE, fill=AgeCat))+
  geom_col()+
  xlab("Age Category") +
  labs(title = "Age Category of Immigrants, Canada, 2023")
