library(readr)
library(tidyverse)
library(plyr)
library(dplyr)
library(ggplot2)

ingredients <- read_csv("data/srep00196-s3.csv")
head(ingredients)

ingredients <- as.data.frame(ingredients)

df_regions <- ingredients %>%
  nest(data = !Region)

regions <- ingredients %>% 
  distinct(Region)

regions

missing_all <- apply(X=is.na(ingredients) , MARGIN = 1, FUN = sum) 

ingredients <- ingredients %>%
  mutate(Num_of_Ing = 23 - missing_all)

df <- ingredients %>% 
  select(Region, Num_of_Ing)
  
p1 <- ggplot(df, aes(x=Num_of_Ing, fill= Region)) + 
  geom_histogram() +
  xlab("Number of ingredients") +
  ylab("Number of recipes") +
  ggtitle("Recipes by regions")

p1


p2 <- ggplot(df, aes(x=Num_of_Ing, fill = Region)) + 
  geom_histogram() +
  facet_wrap(~Region, scales = "free") +
  xlab("Number of ingredients") +
  ylab("Number of recipes") +
  ggtitle("Recipes by regions")

p2

LatinA <- ingredients %>%
  filter(Region == "LatinAmerican")


p3 <- ggplot(LatinA, aes(x=Num_of_Ing)) + 
  geom_histogram(fill="pink") +
  xlab("Number of ingredients") +
  ylab("Number of recipes") +
  ggtitle("Recipes in Latin America")

p3

num_of_ing <- 23 - missing_LA
summary(num_of_ing)


df_ing <- LatinA[,2:24]

ing_LA <- as.character(unique(unlist(df_ing)))


i=0
out <- list()
for (x in ing_LA) {
  i = i + 1
  out[i] <- sum(str_count(df_ing, x))
}

count_ing_LA <- unlist(out)

df_LA <- data.frame(ing_LA, count_ing_LA)  

names(df_LA)= c("Ingredient", "Count")

df_LA 




unique_ing <- as.character(unique(unlist(ingredients[,2:24])))
is.na(unique_ing)
unique_ing <- unique_ing[-295]
is.na(unique_ing)

df_regions <- ingredients %>%
  nest(data = !Region) 

ing_by_reg <- function(x){
  name <- df_regions$Region[x]
  i=1
  counts <- list()
  for (j in unique_ing) {
    counts[i] <- sum(str_count(df_regions$data[x], j))
    i = i + 1
  }
  
  count_ing <- unlist(counts)
  df_x <- data.frame(rep(name, length(unique_ing)), unique_ing, count_ing)
  return(df_x)
}

ing_African <- ing_by_reg(1)
ing_EastAsian <- ing_by_reg(2)
ing_EasternEuropean <- ing_by_reg(3)
ing_LatinAmerican <- ing_by_reg(4)
ing_MiddleEastern <- ing_by_reg(5)
ing_NorthernEuropean <- ing_by_reg(7)
ing_SouthAsian <- ing_by_reg(8)
ing_SoutheastAsian <- ing_by_reg(9)
ing_westernEuropean <- ing_by_reg(11)
ing_SouthernEuropean <- ing_by_reg(10)
ing_NorthAmerican <- ing_by_reg(6)



counts_ingd <- rbind.data.frame(ing_African, ing_EastAsian)
names(counts_ingd) <- c("Region", "Ingredients", "Quantity")

p4 <- counts_ingd %>%
  group_by(Region) %>%
  top_n(n = 15, Quantity) %>%
  ggplot(., aes(x=Ingredients, y= Quantity, fill= Region)) + 
    geom_histogram(stat="identity") + facet_wrap(~Region, scales="free")
  
p4

ggplot(counts_ingd, aes(y=Quantity, fill=Ingredients))+
  geom_histogram() +
  facet_wrap(~Region)

i=1
counts <- list()
for (x in unique_ing) {
  counts[i] <- sum(str_count(df_regions$data[4], x))
  i = i + 1
}

count_ing <- unlist(out2)

df_LA_2 <- data.frame(unique_ing, count_ing)

cat("aaa","name", sep="") <- 1+1

