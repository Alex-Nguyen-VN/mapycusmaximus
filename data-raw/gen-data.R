# generate simple data sets for testing
library(geozoo)
library(tidyverse)

n <- 100

# circles
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

write_csv(circles, file="data-raw/circles.csv")

# squares
set.seed(315)
c1 <- as_tibble(cube.dotline(p=2)$points)
c2 <- as_tibble(cube.dotline(p=2)$points)
c2$Var1 = c2$Var1 + 1
c3 <- as_tibble(cube.dotline(p=2)$points)
c3$Var1 = c3$Var1 + 2
c4 <- as_tibble(cube.dotline(p=2)$points)
c4$Var2 = c4$Var2 + 1
c5 <- as_tibble(cube.dotline(p=2)$points)
c5$Var1 = c5$Var1 + 1
c5$Var2 = c5$Var2 + 1
c6 <- as_tibble(cube.dotline(p=2)$points)
c6$Var1 = c6$Var1 + 2
c6$Var2 = c6$Var2 + 1
c7 <- as_tibble(cube.dotline(p=2)$points)
c7$Var2 = c7$Var2 + 2
c8 <- as_tibble(cube.dotline(p=2)$points)
c8$Var1 = c8$Var1 + 1
c8$Var2 = c8$Var2 + 2
c9 <- as_tibble(cube.dotline(p=2)$points)
c9$Var1 = c9$Var1 + 2
c9$Var2 = c9$Var2 + 2
squares <- bind_rows(c1, c2, c3, c4, c5, c6, c7, c8, c9)
squares <- squares |>
  rename(V1 = Var1, V2 = Var2) |>
  mutate(V1 = (((V1 - min(V1))/(max(V1)-min(V1)))-0.5)*2,
         V2 = (((V2 - min(V2))/(max(V2)-min(V2)))-0.5)*2)


ggplot(circles, aes(V1, V2)) + geom_point() + theme(aspect.ratio=1)

write_csv(circles, file="data-raw/circles.csv")
