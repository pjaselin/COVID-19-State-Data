# library imports
library(rvest)
library(dplyr)
library(ggplot2)
library(readxl)

# 2019 Census data imports
nst_est2019 <- read_excel("nst-est2019.xlsx", skip = 3)

# function to scrape from wikipedia
get_and_process_states <- function(x){
  # get wikipedia site
  state_data <- xml2::read_html(paste0("https://en.wikipedia.org/wiki/2020_coronavirus_pandemic_in_", x))
  
  # get the state name
  state_name_cleaned <- stringr::str_trim(gsub("_", " ", gsub("\\s*\\([^\\)]+\\)\\s*$","", x)))
  
  # pull the table of interest out of the page
  try(state_data <- state_data %>%
        html_nodes(xpath = "/html/body/div[3]/div[3]/div[4]/div/div[3]/div/table") %>%
        html_table() %>%
        magrittr::extract2(1))
  try(state_data <- state_data %>%
        html_nodes(xpath = "/html/body/div[3]/div[3]/div[4]/div/div[2]/div/table") %>%
        html_table() %>%
        magrittr::extract2(1))
  try(state_data <- state_data %>%
        html_nodes(xpath = "/html/body/div[3]/div[3]/div[4]/div/div[1]/div/table") %>%
        html_table() %>%
        magrittr::extract2(1))
  try(state_data <- state_data %>%
        html_nodes(xpath = "/html/body/div[3]/div[3]/div[4]/div/div[5]/div/table") %>%
        html_table() %>%
        magrittr::extract2(1))
  try(state_data <- state_data %>%
        html_nodes(xpath = "/html/body/div[3]/div[3]/div[4]/div/div[4]/div/table") %>%
        html_table() %>%
        magrittr::extract2(1))
  try(state_data <- state_data %>%
        html_nodes(xpath =  "/html/body/div[3]/div[3]/div[4]/div/div[6]/div/table") %>%
        html_table() %>%
        magrittr::extract2(1))
  try(state_data <- state_data %>%
        html_nodes(xpath =  "/html/body/div[3]/div[3]/div[4]/div/div[4]/div/table") %>%
        html_table() %>%
        magrittr::extract2(1))
  try(state_data <- state_data %>%
        html_nodes(xpath =  "/html/body/div[3]/div[3]/div[4]/div/div[4]/div/table") %>%
        html_table() %>%
        magrittr::extract2(1))
  try(state_data <- state_data %>%
        html_nodes(xpath =  '//*[@id="mw-content-text"]/div/div[9]/div/table') %>%
        html_table() %>%
        magrittr::extract2(1))
  try(state_data <- state_data %>%
        html_nodes(xpath =  "/html/body/div[3]/div[3]/div[4]/div/div[5]/div/div/div/table") %>%
        html_table() %>%
        magrittr::extract2(1))
  try(state_data <- state_data %>%
        html_nodes(xpath =  "/html/body/div[3]/div[3]/div[4]/div/div[8]/div/table") %>%
        html_table() %>%
        magrittr::extract2(1))
  try(state_data <- state_data %>%
        html_nodes(xpath =  "/html/body/div[3]/div[3]/div[4]/div/table[4]/tbody/tr/td/div/div/table") %>%
        html_table() %>%
        magrittr::extract2(1))
  
  # choose the first item from the list 
  state_data <- state_data[-c(dim(state_data)[1]), -c(1,3)]
  colnames(state_data) <- unique(as.character(state_data[1,]))
  if (length(which(names(state_data) %in% c(NA))) > 0){
    state_data <- state_data[-c(1), -which(names(state_data) %in% c(NA))]
  } else {
    state_data <- state_data[-c(1), ]
  }
  
  state_data <- state_data %>%
    mutate(Date = as.Date(Date)) %>%
    filter(!is.na(Date))
  
  # first replace all commas with ""
  try(state_data$`# of cases` <- gsub(",","", as.character(state_data$`# of cases`)))
  try(state_data$`of cases` <- gsub(",","", as.character(state_data$`of cases`)))
  try(state_data$`# of deaths` <- gsub(",","", as.character(state_data$`# of deaths`)))
  
  # remove text after and including the first parenthesis
  try(state_data$`# of cases` <- as.numeric(gsub("\\(.*","", state_data$`# of cases`)))
  try(state_data$`of cases` <- as.numeric(gsub("\\(.*","", state_data$`of cases`)))
  try(state_data$`# of deaths` <- as.numeric(gsub("\\(.*","", state_data$`# of deaths`)))
  
  # drop rows that are all na 
  state_data <- state_data[rowSums(is.na(state_data)) != ncol(state_data), ]
  
  # set all NAs to 0
  state_data[is.na(state_data)] <- 0
  if (state_name_cleaned %in% state.name){
    state_data$State <- state.abb[grep(state_name_cleaned, state.name)][1]
  } else {
    state_data$State <- state_name_cleaned
  }
  
  # return data
  return(state_data)
}

# clean the census data
names(nst_est2019)[1] <- "State"
nst_est2019 <- nst_est2019[-c(1:5),]
nst_est2019$State <- gsub('^\\.', '', nst_est2019$State)
nst_est2019 <- nst_est2019[-c(52:58),]
nst_est2019 <- nst_est2019 %>%
  select("State", `2019`)

replace_w_abb <- function(x){
  if(x == "Washington,_D.C."){
    "Washington, D.C."
  } else {
    state.abb[grep(x, state.name)][1]
  }
}
nst_est2019$State <- sapply(nst_est2019$State, replace_w_abb, simplify = TRUE, USE.NAMES = FALSE)

# vector of available state tags for wikipedia urls
available_states = c('Alabama', 'Alaska', 'Arizona', 'Arkansas', 'California', 'Colorado', 'Connecticut', 'Delaware', 'Florida', 'Georgia_(U.S._state)', 'Hawaii', 'Idaho', 'Illinois', 'Indiana', 'Iowa', 'Kansas', 'Kentucky', 'Louisiana', 'Maine', 'Maryland', 'Massachusetts', 'Michigan', 'Minnesota', 'Mississippi', 'Missouri', 'Montana', 'Nebraska', 'Nevada', 'New_Hampshire', 'New_Jersey', 'New_Mexico', 'New_York_(state)', 'North_Carolina', 'North_Dakota', 'Ohio', 'Oklahoma', 'Oregon', 'Pennsylvania', 'Rhode_Island', 'South_Carolina', 'South_Dakota', 'Tennessee', 'Texas', 'Utah', 'Vermont', 'Virginia', "Washington_(state)", "Washington,_D.C.", 'West_Virginia', 'Wisconsin', 'Wyoming')

# loop through the above vector and generate a list of the results
master_state_data <- list()
for (state in available_states){
  print(state)
  master_state_data[[state]] <- get_and_process_states(state)
}

# stack the list of dataframes
master_data <- data.table::rbindlist(master_state_data, fill = TRUE)

# final cleaning steps
master_data$deaths <- as.numeric(master_data$deaths)
master_data$`of cases` <- as.numeric(master_data$`of cases`)
master_data$deaths[is.na(master_data$deaths)] <- 0
master_data$`of cases`[is.na(master_data$`of cases`)] <- 0
cleaned_data <- master_data %>%
  mutate(`# of deaths` = `# of deaths` + deaths) %>%
  mutate(`# of cases` = `# of cases` + `of cases`) %>%
  select(-deaths, -`of cases`)

# add the 2019 population models to the data
cleaned_data <- cleaned_data %>%
  inner_join(nst_est2019, by = "State")

# save the data as RDS and csv
saveRDS(cleaned_data, "cleaned_data.rds")
write.csv(cleaned_data, "cleaned_data.csv")
