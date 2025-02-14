##################################################################
##################################################################
##  Word co-occurrence for hot topic analysis                   ##
##  Karoline Azevedo, for PALOPs paper   09 de Janeiro de 2025  ##
##################################################################
##################################################################
##
# Install and load packages
if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")
library(pacman)

p_load(tidyverse, tidytext, widyr, igraph, ggraph, readxl, stopwords)


data <-read_excel("C:/Users/drive/Downloads/filteredDataV2.xlsx") #change path
colnames(data)
#TI = title
#DE = keywords
#Country = country


# Function to standardize text (lowercase and remove accents)
pattern_text <- function(text) {
  text %>%
    tolower() %>%  # convert to lowercase
    iconv(to = "ASCII//TRANSLIT")  # remove accents
}

# Merge title and keywords into a single text column
dados <- data %>%
  mutate(text = paste(TI, DE, sep = " ")) %>%
  mutate(text = pattern_text(text))

# Crie um vetor de stopwords em ingles
stopwords_en <- stopwords("en") %>%
  pattern_text()

# Add country names to the stopwords vector
country_names <- c("mozambique", "angola", "cape verde", "guinea-bissau", "sao tome e principe", "sao tome", "guinea bissau", "africa", "african", "cape", "cabo", "cabo verde") %>%
  pattern_text()
stopwords_en <- c(stopwords_en, country_names)

# Adjust the code to remove the stopwords
dados_limpos <- dados %>%
  unnest_tokens(word, text) %>%  # split text into words
  filter(!word %in% stopwords_en)  # Remove stopwords

# Check the remaining words
palavras_unicas <- unique(dados_limpos$word)
print(palavras_unicas)

# Word by country frequency
frequency_words <- dados_limpos %>%
  count(Country, word, sort = TRUE)

# Bar chart of most common words by country
frequency_words %>%
  group_by(Country) %>%
  top_n(5, n) %>%  # Seleciona as 5 palavras mais comuns por país
  ggplot(aes(x = reorder(word, n), y = n, fill = Country)) +
  geom_col() +
  facet_wrap(~ Country, scales = "free") +
  coord_flip() 

# Create a word co-occurrence matrix
co_occurrence <- dados_limpos %>%
  pairwise_count(word, Country, sort = TRUE) %>%  # Counts pairs of words that appear together
  filter(n > 1)  # Filters pairs that appear more than once

# Create a word co-occurrence network
rede <- co_occurrence %>%
  graph_from_data_frame()

# Network view
ggraph(rede, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_void() 

###############################################################################
#### Alternative approach: similar to VOSViewer

library(pacman)
p_load(tidyverse, tidytext, widyr, igraph, ggraph, readxl, stopwords, topicmodels)


# Import dataset (change path)
data <-read_excel("C:/Users/drive/Downloads/filteredDataV2.xlsx")
colnames(data)

# Function to standardize text (lowercase and remove accents)
pattern_text <- function(text) {
  text %>%
    tolower() %>%  # convert to lowercase
    iconv(to = "ASCII//TRANSLIT")  # remove accents
}

# Merge title and keywords into a single text column
dados <- data %>%
  mutate(text = paste(TI, DE, sep = " ")) %>%
  mutate(text = pattern_text(text))

# Crie um vetor de stopwords em ingles
stopwords_en <- stopwords("en") %>%
  pattern_text()

# Add country names to the stopwords vector
country_names <- c("mozambique", "angola", "cape verde", "guinea-bissau", "sao tome e principe", "sao tome", "guinea bissau", "africa", "african", "cape", "cabo", "cabo verde") %>%
  pattern_text()
stopwords_en <- c(stopwords_en, country_names)

# Adjust the code to remove the stopwords
dados_limpos <- dados %>%
  unnest_tokens(word, text) %>%  # split text into words
  filter(!word %in% stopwords_en)  # Remove stopwords


# Create a Document-Term Matrix
dtm <- dados_limpos %>%
  count(Country, word) %>%
  cast_dtm(Country, word, n)

# Apply LDA to identify topics
modelo_lda <- LDA(dtm, k = 5, control = list(seed = 1234))  # k = number of topics

# Extracting topics
topicos <- tidy(modelo_lda, matrix = "beta")

# View the most relevant words for each topic
topicos_principais <- topicos %>%
  group_by(topic) %>%
  top_n(5, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

print(topicos_principais)

# Extract topic distribution by country
topicos_paises <- tidy(modelo_lda, matrix = "gamma") %>%
  mutate(document = as.character(document))

# Merge with original data to get country
topicos_paises <- topicos_paises %>%
  left_join(dados %>% select(Country), by = c("document" = "Country"), relationship = "many-to-many")

# View most relevant topics by country
topicos_paises_principais <- topicos_paises %>%
  group_by(document) %>%
  top_n(1, gamma) %>%
  ungroup()

print(topicos_paises_principais)

# Create network
rede_topicos_paises <- topicos_paises_principais %>%
  rename(pais = document, topico = topic) %>%
  graph_from_data_frame(directed = FALSE)

# Network view
ggraph(rede_topicos_paises, layout = "fr") +
  geom_edge_link(aes(edge_alpha = gamma), show.legend = FALSE) +
  geom_node_point(aes(color = ifelse(name %in% unique(topicos_paises_principais$document), "País", "Tópico")), size = 5) +
  geom_node_text(aes(label = name, color = ifelse(name %in% unique(topicos_paises_principais$document), "País", "Tópico")), vjust = 1, hjust = 1) +
  theme_void() 

