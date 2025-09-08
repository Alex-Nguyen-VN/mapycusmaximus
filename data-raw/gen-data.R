# generate simple data sets for testing
library(geozoo)
library(tidyverse)

n <- 100

set.seed(305)
c1 <- as_tibble(sphere.hollow(p=2, n)$points)
c2 <- as_tibble(sphere.hollow(p=2, n)$points*0.4)
c3 <- as_tibble(sphere.hollow(p=2, n)$points*0.8)
c4 <- as_tibble(sphere.hollow(p=2, n)$points*1.2)
c5 <- as_tibble(sphere.hollow(p=2, n)$points*1.6)
c6 <- as_tibble(sphere.hollow(p=2, n)$points*2)
c7 <- as_tibble(sphere.hollow(p=2, n)$points*2.4)
circles <- bind_rows(c1, c2, c3, c4, c5, c6, c7)
circles <- circles |>
  mutate(V1 = (((V1 - min(V1))/(max(V1)-min(V1)))-0.5)*2,
         V2 = (((V2 - min(V2))/(max(V2)-min(V2)))-0.5)*2)


ggplot(circles, aes(V1, V2)) + geom_point() + theme(aspect.ratio=1)
