library(tidyverse)
options(scipen = 999)

fert <- read.csv("data/fertility.csv")
grow <- read.csv("data/growth.csv")

ASFR <- fert %>% filter(REF_DATE==2023,
                GEO=="Canada, place of residence of mother") %>%
        select(Characteristics, VALUE)
# remove totals
ASFR0 <- ASFR[-c((nrow(ASFR)-1):nrow(ASFR)), ]
ASFR0$AgeGroup <- sub(".*(\\d{2,2} to \\d{2,2}).*", "\\1", ASFR0$Characteristics)
ASFR0$AgeGroup <- sub(" to ", "-", ASFR0$AgeGroup)
ASFR0

ASFR_2023 <- ggplot(ASFR0, aes(x=AgeGroup, y=VALUE))+
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
