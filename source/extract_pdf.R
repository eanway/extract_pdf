# Extract PDF
# Data pipeline to extract text from one or more pdfs in a directory
# One file abstract is post-processed and exported as an example

# require and, if needed, install package to easily load more packages
if(!require(pacman)) install.packages("pacman"); library(pacman)


# Import ####
# load packages needed in this section
p_load(
  here, 
  tibble, 
  dplyr, 
  tidyr, 
  pdftools, 
  purrr
)

df_pdfs <- 
  # get relative local path to pdfs folder
  ## the data folder is ignored in this repo and should be make locally
  here::here("data", "pdfs") %>%
  # convert the file path into a tibble (data frame) for easier looping
  enframe(name = NULL, value = "folder") %>%
  # get a list of files in the folder
  mutate(
    file = map(folder, list.files)
  ) %>%
  # expand the list to show all files as separate row in the tibble
  unnest_longer(file) %>%
  # create a file path and extract the text from each pdf at that path
  ## as a list, one page per element
  mutate(
    path = file.path(folder, file), 
    text_raw = map(path, pdf_text)
  ) %>%
  # expand the list to show each page on a separate row
  unnest_longer(
    text_raw, 
    indices_to = "page"
  )


# Remove header and footer ####
remove_header_footer <- function(text, header_lines, footer_lines) {
  text %>%
    # create a "connection" to the text for processing with readLines
    textConnection() %>%
    # splits the text into a vector, one element for each line
    readLines() %>%
    # remove the footer lines
    head(-footer_lines) %>%
    # remove the header lines
    tail(-header_lines) %>%
    # recombine the remaining text with a line break
    paste(collapse = "\n")
}

df_pdfs_body <- df_pdfs %>%
  # remove the first and last three lines from each page
  mutate(
    text_body = map_chr(text_raw, remove_header_footer, header_lines = 3, footer_lines = 3)
  )


# Combine text ####
df_pdfs_concatenate <- df_pdfs_body %>%
  # group by fields to ID unique combinations
  ## keeps these fields in the output
  group_by(folder, file, path) %>%
  # combine all of the pages into one row for each group
  summarise(
    text = paste(text_body, collapse = "\n")
  ) %>%
  # removes grouping to make future calculations faster
  ungroup()

# print current output to console as an example
cat(df_pdfs_concatenate$text[1])


# Vectorize paragraph ####
p_load(
  stringr
)
# https://rstudio.com/wp-content/uploads/2016/09/RegExCheatsheet.pdf

abstract <- 
  # look for text in the abstract
  ## returns just the second column, which is the first group (.+)
  ## (?s) causes . to include line breaks
  str_match(df_pdfs_concatenate$text[1], "ABSTRACT: (?s)(.+)Key Words")[,2] %>%
  # remove white space
  str_squish() %>%
  # convert to lowercase
  str_to_lower() %>%
  # remove punctuation
  str_remove_all("[[:punct:]]")


# Position of words in each document ####
# input list of words, output list of positions in the document
standardize_text <- function(text) {
  text %>%
    str_squish() %>%
    # convert to lowercase
    str_to_lower() %>%
    # remove punctuation
    str_remove_all("[[:punct:]]")
}

get_positions <- function(text, string_to_locate) {
  vec_words <- text %>%
    str_split(" ", simplify = TRUE)
  
  list(which(vec_words == string_to_locate))
}

df_pdfs_positions <- df_pdfs_concatenate %>%
  mutate(
    text_standard = standardize_text(text), 
    positions = get_positions(text_standard, "biomimicry")
  )

# goal is to get a list of every unique word and the positions of that word
# by page, paragraph, sentence


# Export ####
# writes text to a file
write_text_to_file <- function(path, text) {
  # creates and opens file connection
  con_file <- file(path)
  # writes text to the connection
  writeLines(text, con_file)
  # closes the connection
  close(con_file)
}

# output folder path
fol_output <- here::here("output")

# creates the output folder if needed
if(!dir.exists(fol_output)) {
  dir.create(fol_output)
}

# writes abstract to the output file
write_text_to_file(
  file.path(fol_output, "abstract.txt"), 
  abstract
)
