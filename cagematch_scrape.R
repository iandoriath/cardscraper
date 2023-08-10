# Load the required packages
library(tidyverse)
library(rvest)
library(lubridate)
library(XML)
library(xml2)
library(caret)
library(lme4)

#### Function Definitions ####

# Define a function to fetch a page from the website
fetch_page = function(year=2023, page=0, promotion='WWE'){
  
  # Replace spaces in promotion with +
  promotion = promotion %>% str_replace_all(' ', '+')
  
  # Construct the URL
  main_url = 'https://www.cagematch.net/'
  search_url = paste0('?id=1&view=tvppv&search=', promotion)
  page_url = '&s='
  year_url = '&Year='
  url = paste0(main_url, search_url, year_url, year, page_url, page)
  
  # Parse the HTML from the URL
  Sys.sleep(0.02)
  doc = XML::htmlParse(paste(readLines(url), collapse='\n'))
  
  # Get the nodes of the tables and the navigation elements
  table_nodes <- getNodeSet(doc, "//table")
  nav_nodes <- XML::getNodeSet(doc, "//*[contains(@class, 'NavigationPart')]")
  old_format = FALSE
  
  # If no nodes were found, try 'TRowTVShow' instead
  if (length(nav_nodes) == 0) {
    old_format = TRUE
    nav_nodes <- XML::getNodeSet(doc, "//*[contains(@class, 'TRowTVShow')]")
  }
  
  # Extract the text from the links in the navigation nodes
  link_text <- lapply(nav_nodes, function(node) {
    link_nodes <- XML::getNodeSet(node, ".//a")
    link_text <- sapply(link_nodes, XML::xmlValue)
    return(link_text)
  })
  
  # Read the HTML table into a dataframe
  df <- readHTMLTable(table_nodes[[1]])
  
  # Get the nodes of the links in the table
  link_nodes <- getNodeSet(doc, "//table//a")
  
  # Extract the href attributes from the links
  hrefs <- sapply(link_nodes, xmlGetAttr, "href")
  
  # Find the header columns & remove from the hrefs
  hrefs = hrefs[which(grepl('page=2', hrefs))]
  
  # Reshape the hrefs into a dataframe
  hrefs = tibble(event = str_extract(hrefs, "(?<=&nr=)\\d+"), event_url=hrefs)
  
  # Read the HTML table into a dataframe again
  df = XML::readHTMLTable(doc, as.data.frame = TRUE)
  
  # Transform the date into a date object and unnest the dataframe
  df = df %>% tibble() %>% unnest(cols=c(.), names_repair = 'unique') %>% 
    mutate(Date = dmy(Date))
  
  # Bind the hrefs dataframe to the main dataframe
  df = bind_cols(df, hrefs) %>% select(Date, `Event Name`, event, event_url)
  
  # Unlist the nested list and convert to numeric, coercing non-numeric values to NA
  num_list <- as.numeric(unlist(link_text))
  
  # Remove NA values resulted from coercion
  num_list <- num_list[!is.na(num_list)]
  
  # Find the maximum number
  max_num <- max(num_list, 0)
  
  # Prepare the output as a list with the number of navigation pages and the main dataframe
  output = list(n=max_num, df=df)
  
  return(output)
}

get_matches = function(year=2023, scrape_delay = 0.03, promotion = 'WWE'){
  # Fetch the first page
  output = fetch_page(year, page=0, promotion=promotion)
  df = output$df
  
  # Get the number of navigation pages
  navpages = output$n
  
  # If there's only one page then we're done
  if(navpages<1) return(output)
  
  # Prepare a sequence of page numbers for the subsequent pages
  nextpages = 100*(2:navpages-1)
  
  # Loop over the subsequent pages, sleep for 527 seconds between each request, and append the data to the main dataframe
  for(p in (100*(2:navpages-1))){
    Sys.sleep(scrape_delay)
    output = fetch_page(2023, p)
    df = df %>% bind_rows(output$df)
  } 
  
  return(df)
}

fetch_card = function(event, scrape_delay=0.03){
  # delay to prevent getting your IP blocked.  Cagematch website owner says 2ms is OK.
  Sys.sleep(scrape_delay)
  
  # Construct the URL for fetching the card information
  url = paste0('https://www.cagematch.net/?id=1&nr=', event)
  doc = XML::htmlParse(paste(readLines(url), collapse='\n'))
  
  # Extract nodes related to matches and match results
  match_nodes <- XML::getNodeSet(doc, "//*[contains(@class, 'Match')]")
  match_results = XML::getNodeSet(doc, "//*[contains(@class, 'MatchResults')]")
  raw_text <- sapply(match_results, XML::xmlValue)
  
  # Extract information on match type
  match_type = XML::getNodeSet(doc, "//*[contains(@class, 'MatchType')]") %>% 
    sapply(xmlValue)
  
  # Initialize data frames to hold wrestler and match information
  all_wrestlers = tibble()
  df <- tibble()
  
  # Loop through match results to extract detailed information
  for(i in 1:length(match_results)) {
    wrestler_links <- xpathSApply(match_results[[i]], ".//a", xmlGetAttr, "href")
    wrestler_names <- xpathSApply(match_results[[i]], ".//a", xmlValue)
    wrestler_ids <- str_extract(wrestler_links, "(?<=&nr=)\\d+")
    
    # Bind the wrestler information to the all_wrestlers data frame
    all_wrestlers = all_wrestlers %>% bind_rows(tibble(wrestler_names, wrestler_ids, wrestler_links))
    
    # Perform text manipulation to extract other match details
    text_values = raw_text[[i]]
    
    for(w in wrestler_names) text_values = text_values %>% str_replace_all(w, '')
    
    match_time_seconds = text_values %>% str_extract_all("\\(\\d{1,2}:\\d{2}\\)") %>% as.character() %>% 
      str_replace_all("\\(", "") %>% 
      str_replace_all("\\)", "") %>% 
      strsplit(":") %>% 
      sapply(function(x) as.numeric(x[1]) * 60 + as.numeric(x[2]))
    
    title_holder <- str_extract(text_values, "\\(c\\)")
    title_holder_pos <- str_locate(text_values, "\\(c\\)")[1]
    text_values <- str_remove_all(text_values, "\\(c\\)")
    
    str = text_values %>% str_remove_all("\\(\\d{1,2}:\\d{2}\\)") %>% str_remove_all('w/')
    
    outcome = str_extract_all(str, "\\b[a-zA-Z]+\\b")[[1]][[1]]
    outcome_pos <- str_locate(raw_text[[i]], outcome)[1]
    
    wrestler_pos = integer(length(wrestler_names))
    for(j in 1:length(wrestler_pos)) wrestler_pos[[j]] = str_locate(raw_text[[i]], wrestler_names[[j]])[1]
    
    defeat_occured = as.logical(grepl('defeat', outcome))
    
    result_df = tibble(wrestler_ids, winner=as.numeric(wrestler_pos < outcome_pos))
    
    if(defeat_occured) {
      result_df = result_df %>% mutate(winner = if_else(winner==1, 1, -1))
    } else {      
      result_df = result_df %>% mutate(winner = 0)
    } 
    
    title_change = isTRUE(title_holder) & title_holder_pos > outcome_pos & defeat_occured
    result_df$title_change = title_change
    
    result_df = result_df %>% 
      pivot_wider(names_from=wrestler_ids, values_from=winner, names_prefix = 'w')
    
    # Bind the result to the main data frame
    df <- bind_rows(df, result_df)
  }

  # Include match type information in the data frame
  df$match_type = match_type
  
  # Prepare and return the output
  output = list()
  output$df = df
  output$wrestlers = all_wrestlers
  
  return(output)
}

#### Main Program ####
events = tibble()
for(y in c(1995:1996, 2006:2023)){
  print(y)
  Sys.sleep(0.03)
  new_events = get_matches(y, promotion='Monday Night RAW')
  new_events = new_events$df
  events = events %>% bind_rows(new_events)
} 
for(y in 1997:2001){
  print(y)
  Sys.sleep(0.03)
  new_events = get_matches(y, promotion='WWF RAW is WAR')
  new_events = new_events$df
  events = events %>% bind_rows(new_events)
}
for(y in 2002:2005){
  print(y)
  Sys.sleep(0.03)
  new_events = get_matches(y, promotion='WWE RAW')
  new_events = new_events$df
  events = events %>% bind_rows(new_events)
}

# SmackDown
for(y in 1999:2023){
  Sys.sleep(0.03)
  new_events = get_matches(y, promotion = 'SmackDown')
  new_events = new_events$df
  events = events %>% bind_rows(new_events)
}

fetch_card_safe = safely(fetch_card)

df = events %>%
  mutate(event_data = map(event, fetch_card_safe)) %>%
  mutate(
    result = map(event_data, "result"),
    error = map(event_data, "error")
  ) %>% 
  mutate(
    df = map(result, "df"),
    wrestlers = map(result, "wrestlers")
  ) %>% select(-event_data, -result)

# Write the data locally
# df %>% write_rds('fullwwe.Rds')
