### Venn diagrams in R
### By Jehan Gonsal
### This is quick and dirty, but should be reproducible
### All data is simulated from basic assumptions

### Let's make a quick and dirty Venn Diagram

library(tidyverse)
library(VennDiagram)

### Let's simulate our data

set.seed(23)

nrows = 10000

### Probabilities

prob_acu <- .3

prob_programming <- .3

prob_stats <- .3

prob_stats_programming <- .5

df <- data.frame(matrix(0, ncol = 0, nrow = nrows))

df <- df %>% mutate(business_acumen = 
                rbinom(n = nrows, size = 1, prob = prob_acu),
              programming = 
                rbinom(n = nrows, size = 1, prob = prob_stats),
              maths_statistics = 
                rbinom(n = nrows, size = 1, prob = ifelse(programming == 1, prob_stats_programming, prob_stats)),
              business_acumen = 
                rbinom(n = nrows, size = 1, prob = ifelse(business_acumen == 0 & programming == 0 & maths_statistics == 0, 
                                                          1, business_acumen)))


df <- df %>% mutate(business_acumen = ifelse(business_acumen == 1, "Business Acumen", "No Business Acumen") %>%
                as.factor(),
              programming = ifelse(programming == 1, "Programming Expertise", "No Programming Expertise") %>%
                as.factor(),
              maths_statistics = ifelse(maths_statistics == 1, "Applied Mathematics and Statistics", "No Maths nor Stats") %>%
                as.factor())


venn.plot <- draw.triple.venn(
  area1 = df %>% 
    filter(business_acumen == "Business Acumen") %>%
    summarise(n()) %>%
    as.numeric(),
  area2 = df %>% 
    filter(programming == "Programming Expertise") %>%
    summarise(n()) %>%
    as.numeric(),
  area3 = df %>% 
    filter(maths_statistics == "Applied Mathematics and Statistics") %>%
    summarise(n()) %>%
    as.numeric(),
  n12 = df %>% 
    filter(business_acumen == "Business Acumen" & 
             programming == "Programming Expertise") %>%
    summarise(n()) %>%
    as.numeric(),
  n23 = df %>% 
    filter(programming == "Programming Expertise" &
             maths_statistics == "Applied Mathematics and Statistics") %>%
    summarise(n()) %>%
    as.numeric(),
  n13 = df %>% 
    filter(business_acumen == "Business Acumen" &
             maths_statistics == "Applied Mathematics and Statistics") %>%
    summarise(n()) %>%
    as.numeric(),
  n123 =df %>% 
    filter(business_acumen == "Business Acumen" & 
             programming == "Programming Expertise" & 
             maths_statistics == "Applied Mathematics and Statistics") %>%
    summarise(n()) %>%
    as.numeric(),
  category = c(
"Business Acumen", 
"Programming \ 
Expertise", 
"Applied Mathematics \
and Statistics"),
euler.d = TRUE,
scaled = TRUE,
area.vector = TRUE,
print.mode = "percent",
  fill = c("blue", "red", "green"),
  lty = "blank",
  cex = 2,
  cat.cex = 2,
  cat.col = c("dark blue", "dark red", "dark green")
)

grid.draw(venn.plot)
grid.newpage()

### This is not great as we don't have proportionality
### Next doesn't show percentages, but we will need to compute this

library(eulerr)

v <- eulerr::euler(c(
  `Business Acumen` = df %>% 
    filter(business_acumen == "Business Acumen"&  
             programming != "Programming Expertise" & 
             maths_statistics != "Applied Mathematics and Statistics") %>%
    summarise(n()) %>%
    as.numeric(), 
  `Programming Expertise` = df %>% 
    filter(business_acumen != "Business Acumen"&  
             programming == "Programming Expertise" & 
             maths_statistics != "Applied Mathematics and Statistics") %>%
    summarise(n()) %>%
    as.numeric(), 
  `Mathematics and Statistics` = df %>% 
    filter(business_acumen != "Business Acumen"&  
             programming != "Programming Expertise" & 
             maths_statistics == "Applied Mathematics and Statistics") %>%
    summarise(n()) %>%
    as.numeric(),
  "Business Acumen&Programming Expertise" = df %>% 
    filter(business_acumen == "Business Acumen" & 
             programming == "Programming Expertise" & maths_statistics != "Applied Mathematics and Statistics") %>%
    summarise(n()) %>%
    as.numeric(), 
  "Business Acumen&Mathematics and Statistics" = df %>% 
    filter(business_acumen == "Business Acumen" & 
             programming != "Programming Expertise" & maths_statistics == "Applied Mathematics and Statistics") %>%
    summarise(n()) %>%
    as.numeric(), 
  "Programming Expertise&Mathematics and Statistics" = df %>% 
    filter(programming == "Programming Expertise" &
             maths_statistics == "Applied Mathematics and Statistics" & business_acumen != "Business Acumen") %>%
    summarise(n()) %>%
    as.numeric(), 
  "Business Acumen&Programming Expertise&Mathematics and Statistics" = df %>% 
    filter(business_acumen == "Business Acumen" & 
             programming == "Programming Expertise" & 
             maths_statistics == "Applied Mathematics and Statistics") %>%
    summarise(n()) %>%
    as.numeric()),
  shape = "ellipse")

plot(v, quantities = TRUE, main = "Naive Venn Diagram of Analytics Skills in Industry")

### Run a quick check we have accounted for everythong
print(sum(v$original)) == nrows

### Create df as proportions

prop_df <- df %>% filter(business_acumen == "Business Acumen" | 
                                    programming == "Programming Expertise" | 
                                    maths_statistics == "Applied Mathematics and Statistics") %>%
  group_by(business_acumen,maths_statistics, programming) %>%
  summarise(count = n()) %>%
  ungroup() %>%
  mutate(proportions = count / sum(count)*100)

df %>% filter(business_acumen == "Business Acumen" | 
                         programming == "Programming Expertise" | 
                         maths_statistics == "Applied Mathematics and Statistics") %>%
  group_by(business_acumen,maths_statistics, programming) %>%
  summarise(count = n()) %>%
  ungroup() %>%
  mutate(proportions = count / sum(count)*100) %>%
  summarise(sum(proportions))

### Recreate plot

v <- eulerr::euler(c(
  `Business Acumen` = prop_df %>% 
    filter(business_acumen == "Business Acumen"&  
             programming != "Programming Expertise" & 
             maths_statistics != "Applied Mathematics and Statistics") %>%
    summarise(sum(proportions)) %>% 
    round(2) %>%
    as.numeric(), 
  `Programming Expertise` = prop_df %>% 
    filter(business_acumen != "Business Acumen"&  
             programming == "Programming Expertise" & 
             maths_statistics != "Applied Mathematics and Statistics") %>%
    summarise(sum(proportions)) %>%
    round(2) %>%
    as.numeric(), 
  `Mathematics and Statistics` = prop_df %>% 
    filter(business_acumen != "Business Acumen"&  
             programming != "Programming Expertise" & 
             maths_statistics == "Applied Mathematics and Statistics") %>%
    summarise(sum(proportions)) %>%
    round(2) %>%
    as.numeric(),
  "Business Acumen&Programming Expertise" = prop_df %>% 
    filter(business_acumen == "Business Acumen" & 
             programming == "Programming Expertise" & maths_statistics != "Applied Mathematics and Statistics") %>%
    summarise(sum(proportions)) %>%
    round(2) %>%
    as.numeric(), 
  "Business Acumen&Mathematics and Statistics" = prop_df %>% 
    filter(business_acumen == "Business Acumen" &
             maths_statistics == "Applied Mathematics and Statistics" & programming != "Programming Expertise") %>%
    summarise(sum(proportions)) %>%
    round(2) %>%
    as.numeric(), 
  "Programming Expertise&Mathematics and Statistics" = prop_df %>% 
    filter(programming == "Programming Expertise" &
             maths_statistics == "Applied Mathematics and Statistics" & business_acumen != "Business Acumen") %>%
    summarise(sum(proportions)) %>%
    round(2) %>%
    as.numeric(), 
  "Business Acumen&Programming Expertise&Mathematics and Statistics" = prop_df %>% 
    filter(business_acumen == "Business Acumen" & 
             programming == "Programming Expertise" & 
             maths_statistics == "Applied Mathematics and Statistics") %>%
    summarise(sum(proportions)) %>%
    round(2) %>%
    as.numeric()),
  shape = "ellipse")

plot(v, quantities = TRUE, main = "Naive Venn Diagram of Analytics Skills in Industry")

### Let's run some checks
print(sum(v$original)) == 100

print(v$original)

