library(tidytext)
library(RColorBrewer)
library(wordcloud)


animal_txt <-
  animal_dt %>%
  mutate(id = row_number()) %>% 
  unnest_tokens(word, Incident.Detail) %>% 
  count(id, word, sort = TRUE)

# remove stop words and numerics
data(stop_words)

animal_txt  <- 
  animal_txt %>%
  filter(!str_detect(word, "^[0-9]")) %>%
  anti_join(stop_words)


word_ct <-
animal_txt  %>%
  count(word, sort = TRUE) 


animal_txt  %>%
  count(word, sort = TRUE) %>% 
  filter(n > 20) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word)) +
  geom_col() +
  labs(y = NULL)

tokens <- 
  animal_txt %>% 
  count(word, sort = TRUE)

pal <- RColorBrewer::brewer.pal(10,"Set3")

pal <- fBasics::qualiPalette(30, name = "Dark2")

# plot the 80 most common words
set.seed(1224)
CairoPNG("./outputs/animal_words.png")
tokens  %>% 
  with(wordcloud(word, n, random.order = FALSE, max.words = 80, colors=pal))
dev.off()

# blank space around it is a pain.  Use magick to load again and crop

# install.packages("magick")
library(magick)

# Reading a PNG
image <- image_read('./outputs/animal_words.png')

# Printing the image
print(image, info = FALSE)

image_info(image)

r<-image_crop(image = image, geometry = "2300x250")
r<-image_crop(image = image, geometry = "280x280+100+100")
print(r, info = FALSE)

image_write(r, path = './outputs/animal_words2.png')

animal_txt  %>%
  count(word, sort = TRUE) %>% 
  filter(n < 20, n >5) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word)) +
  geom_col() +
  labs(y = NULL)


# 'Cat stuck' is most common by a long-shot
animal_txt %>% 
  bind_tf_idf(word, id, n) %>% 
  arrange(tf_idf) %>% 
  print(n = 2000)


# dog, pigeon, bird, cat, puppy, kitten, horse, deer, jack russell, fox

animal_dt <-
  animal_dt %>% 
  mutate(Incident.Detail = str_to_lower(Incident.Detail)) %>% 
  mutate(
    animal = 
        case_when(
        str_detect(Incident.Detail, "dog") ~ "Dog",
        str_detect(Incident.Detail, "greyhound") ~ "Dog",
        str_detect(Incident.Detail, "huskey") ~ "Dog",
        str_detect(Incident.Detail, "german shepherd") ~ "Dog",
        str_detect(Incident.Detail, "pupp") ~ "Dog",
        str_detect(Incident.Detail, "jack rus") ~ "Dog",
        str_detect(Incident.Detail, "terrier") ~ "Dog",
        str_detect(Incident.Detail, "labrador") ~ "Dog",
        str_detect(Incident.Detail, "chihuahua") ~ "Dog",
        str_detect(Incident.Detail, "cat") ~ "Cat",
        str_detect(Incident.Detail, "itten") ~ "Cat",
        str_detect(Incident.Detail, "bird") ~ "Bird",
        str_detect(Incident.Detail, "chick") ~ "Bird",
        str_detect(Incident.Detail, "pigeon") ~ "Bird",
        str_detect(Incident.Detail, "pidgeon") ~ "Bird",
        str_detect(Incident.Detail, "swan") ~ "Bird",
        str_detect(Incident.Detail, "cygnet") ~ "Bird",
        str_detect(Incident.Detail, "crow") ~ "Bird",
        str_detect(Incident.Detail, "kestrel") ~ "Bird",
        str_detect(Incident.Detail, "kesterl") ~ "Bird",
        str_detect(Incident.Detail, "swift") ~ "Bird",
        str_detect(Incident.Detail, "swirft") ~ "Bird",
        str_detect(Incident.Detail, "magpie") ~ "Bird",
        str_detect(Incident.Detail, "gull") ~ "Bird",
        str_detect(Incident.Detail, "hawk") ~ "Bird",
        str_detect(Incident.Detail, "falcon") ~ "Bird",
        str_detect(Incident.Detail, "heron") ~ "Bird",
        str_detect(Incident.Detail, "duck") ~ "Bird",
        str_detect(Incident.Detail, "duick") ~ "Bird",
        str_detect(Incident.Detail, "goose") ~ "Bird",
        str_detect(Incident.Detail, "geese") ~ "Bird",
        str_detect(Incident.Detail, "gosling") ~ "Bird",
        str_detect(Incident.Detail, "eagle") ~ "Bird",
        str_detect(Incident.Detail, "mallard") ~ "Bird",
        str_detect(Incident.Detail, "parrot") ~ "Bird",
        str_detect(Incident.Detail, "starling") ~ "Bird",
        str_detect(Incident.Detail, "budgie") ~ "Bird",
        str_detect(Incident.Detail, "owl") ~ "Bird",
        str_detect(Incident.Detail, "sparrow") ~ "Bird",
        str_detect(Incident.Detail, "horse") ~ "Horse",
        str_detect(Incident.Detail, "dorse") ~ "Horse",
        str_detect(Incident.Detail, "foal") ~ "Horse",
        str_detect(Incident.Detail, "pony") ~ "Horse",
        str_detect(Incident.Detail, "deer") ~ "Deer",
        str_detect(Incident.Detail, "dear") ~ "Deer",
        str_detect(Incident.Detail, "fox") ~ "Fox",
        str_detect(Incident.Detail, "badger") ~ "Badger",
        str_detect(Incident.Detail, "squirrel") ~ "Squirrel",
        str_detect(Incident.Detail, "cow") ~ "Cow",
        str_detect(Incident.Detail, "pig") ~ "Pig",
        str_detect(Incident.Detail, "sheep") ~ "Sheep",
        str_detect(Incident.Detail, "lamb") ~ "Sheep",
        str_detect(Incident.Detail, "ewe") ~ "Sheep",
        str_detect(Incident.Detail, "snake") ~ "Snake",
        str_detect(Incident.Detail, "boa") ~ "Snake",
        str_detect(Incident.Detail, "python") ~ "Snake",
        str_detect(Incident.Detail, "rabbit") ~ "Small pet",
        str_detect(Incident.Detail, "hamster") ~ "Small pet",
        str_detect(Incident.Detail, "guinea") ~ "Small pet",
        str_detect(Incident.Detail, "hedgehog") ~ "Small pet",
        str_detect(Incident.Detail, "chinchilla") ~ "Small pet",
        str_detect(Incident.Detail, "tortoise") ~ "Small pet",
        str_detect(Incident.Detail, "fish") ~ "Fish",
        .default = NA
        )
    )

animal_dt %>% 
  filter(is.na(animal))


library(colorRamps)


animal_label <-
  animal_dt %>% 
  group_by(animal) %>% 
  summarise(total=n()) %>%
  arrange(desc(total)) %>% 
  pull(animal)


animal_dt$animal <- factor(animal_dt$animal, animal_label)

#### Counts by animals
animal_dt %>% 
  group_by(animal) %>% 
  summarise(total=n()) %>%
  arrange(desc(total)) %>% 
  #mutate(animal = factor(animal, animal)) %>% 
  ggplot(aes(y=total, x=animal, fill=animal))+
  geom_col(alpha=0.5, col = "black")+
  scale_fill_manual(values = primary.colors(15, steps = 3, no.white = TRUE))+
  scale_x_discrete(guide = guide_axis(n.dodge = 2))+
  theme(legend.position = "none")


animal_dt %>% 
  filter(District != "Birmingham") %>% 
  group_by(District, animal) %>% 
  summarise(total=n()) %>%
  arrange(desc(total)) %>% 
  #mutate(animal = factor(animal, animal)) %>% 
  ggplot(aes(y=total, x=animal, fill=animal))+
  geom_col(alpha=0.5, col = "black")+
  scale_fill_manual(values = primary.colors(15, steps = 3, no.white = TRUE))+
  scale_x_discrete(guide = guide_axis(n.dodge = 2))+
  facet_wrap(~District)+
  theme(legend.position = "none")



# 
# animal_dt <- 
#   animal_dt %>% 
#   mutate(dt_year = as.character(fiscal_year(yearquarter(Incdate, fiscal_start = 4))))
# 
# animal_dt <- 
#   animal_dt %>% 
#   mutate(dt_year = paste0(as.character((as.numeric(dt_year) -1)),"/",substring(dt_year, 3,4)))
# 
# animal_dt <- 
#   animal_dt %>% 
#   mutate(dt_year = factor(dt_year, unique(animal_dt$dt_year)))



animal_dt %>% 
  group_by(fyear,animal) %>% 
  summarise(total=n()) %>%
  arrange(fyear, animal) %>% 
  ungroup() %>% 
  #mutate(animal = factor(animal, animal)) %>% 
  ggplot(aes(y=total, x=fyear, fill=animal))+
  geom_col(alpha=0.5, col = "black", position = position_stack(reverse = TRUE)) +
  scale_fill_manual(values = primary.colors(15, steps = 3, no.white = TRUE))+
  scale_x_discrete(guide = guide_axis(n.dodge = 2))

