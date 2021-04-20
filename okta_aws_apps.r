# Read list of active Okta AWS apps in the client's Okta tenant
# <okta_apps.rds generated via separate API call
# Get all users assigned to those AWS apps
# Push to db table and Google sheet
# Helps client manage their app inventory / assigment support and
# app role assignments

library(readr)
library(httr)
library(jsonlite)
library(dplyr)
library(tidyr)
library(purrr)
library(stringr)
library(googlesheets4)
library(DBI)
library(odbc)
library(janitor)

okta_apps_aws <- readRDS("c:/scripts/okta_apps.rds") %>%
  filter(str_detect(tolower(oa_name), pattern = "aws") |
           str_detect(tolower(oa_name), pattern = "aws"), oa_status == "ACTIVE") %>% 
  select(oa_id,
         oa_name,
         oa_label,
         oa_status,      
         oa_created,
         oa_signonmode,
         oa_credentials_usernametemplate_template)

aws_app_list <- unique(okta_apps_aws) %>% 
  select(oa_id, oa_label) %>% 
  mutate_all(tolower)

# Build URL
o_org <- "okta_org_name_here"
# CAUTION! protect your keys
# e.g., https://cran.r-project.org/web/packages/httr/vignettes/secrets.html
o_key <- Sys.getenv("okey")
o_content <- list()
o_get <- list()


# Set other API variables
apiLimit = as.character("200")

# Get app label and appid vectors
oappid <- aws_app_list$oa_id
x_oapplabel <-  aws_app_list$oa_label
oapplabel <- x_oapplabel[length(x_oapplabel):1]

# Need empty list to store each datata frame we get
okta_app_user_list <- list()

# Create the vector of API URL's we'll call
o_url <-  paste0("https://",o_org,".okta.com/api/v1/apps/",oappid,"/users?limit=",apiLimit)

# mystart <- Sys.time()

# GET the data for each item in the vector of API URLs
for (i in seq_along(o_url)){
  
  # Pass initial URL to get first batch
  o_get <- httr::GET(o_url[[i]],
                     config = (
                       add_headers(Authorization = o_key)
                     )
  )
  
  # Get the content into a data frame
  o_content <- fromJSON(httr::content(o_get, as = "text"), flatten = TRUE)
  
  # Check headers for "next" cursor URL - means we have more
  isNextHead <- o_get$headers[grepl('"next\"', o_get$headers)]
  
  # Extract 'next' URL assuming it exists
  getNextHead <- str_extract(isNextHead,"(?<=<).*(?=>)")
  
  # If there is a 'next' (paginated cursor) URL, repeat til there isn't
  if(length(isNextHead) > 0)
    repeat{
      # Pass next URL to get next batch
      o_get <- httr::GET(getNextHead,
                         config = (
                           add_headers(Authorization = o_key)
                         )
      )
      
      # Add to content data frame
      o_content <- bind_rows(o_content, fromJSON(httr::content(o_get, as = "text"), flatten = TRUE))
      
      # Check headers for "next" cursor URL - means we have more
      isNextHead <- o_get$headers[grepl('"next\"', o_get$headers)]
      
      # Extract 'next' URL assuming it exists
      getNextHead <- str_extract(isNextHead,"(?<=<).*(?=>)")
      
      # Check for break condition
      if(length(isNextHead) == 0){
        break
      }
    }

  # add next data frame to the list
  okta_app_user_list <- append(okta_app_user_list, list(o_content), 0)
  
  # Give the API a break   
  Sys.sleep(5)
  
}

# Apply list names for later use
names(okta_app_user_list) <- oapplabel

# Tidy data for easier use 
okta_app_users <- okta_app_user_list %>% bind_rows( .id = "okta_app_name")

okta_app_users <- okta_app_users %>% 
  select_if(~ !all(is.na(.)))

okta_app_users$exported_date <- format(Sys.time(), tz = "America/New_York")
okta_app_users <- okta_app_users %>% rename_at(names(.)[-1],function(x) paste0("au_",x))

# Fix col names
oau_names <- names(okta_app_users) %>% 
  str_to_lower() %>%
  str_replace_all("[.]","_")

names(okta_app_users) <- oau_names

aws_okta_apps_users <- okta_app_users %>% 
  select(okta_app_name,
         au_credentials_username,
         au_externalid,
         au_scope,
         au__links_group_name,
         au_profile_firstname,
         au_profile_lastname,
         au_profile_email,
         au_profile_role,
         au_profile_samlroles,
         au_status,
         au_created,
         au_id,
         au_exported_date
  )

aws_okta_apps_users <- aws_okta_apps_users %>% rename(up_id = au_id) %>%
  relocate(au_credentials_username, .after = okta_app_name)

# flatten and expand the 'au_profile_role' list col to get complete user / app assignment
aws_okta_apps_users <- aws_okta_apps_users %>%
  rowwise %>%
  summarise_all(~ list(unlist(.))) %>%
  unnest(cols = everything())

# just distinct apps and associated roles
aws_okta_group_roles <- aws_okta_apps_users %>%
  select(okta_app_name, au_profile_role, au_profile_samlroles) %>% 
  group_by(okta_app_name) %>% 
  distinct() %>% 
  arrange(okta_app_name)

# refresh db table
conpgdb <- dbConnect(odbc::odbc(), "odbc_system_connection_name_here")
odbc::dbSendQuery(conpgdb, "TRUNCATE aws_okta_app_users")
odbc::dbWriteTable(conpgdb, "aws_okta_app_users", aws_okta_apps_users, append = TRUE, row.names = FALSE)

odbc::dbSendQuery(conpgdb, "TRUNCATE aws_okta_group_roles")
odbc::dbWriteTable(conpgdb, "aws_okta_group_roles", aws_okta_group_roles, append = TRUE, row.names = FALSE)

# refresh Google Sheet
aws_user_sheet <- "<google_sheet_ID_here" 
write_sheet(aws_okta_apps_users, 
            ss = aws_user_sheet, 
            sheet = "aws_okta_apps_users")

write_sheet(aws_okta_group_roles, 
            ss = aws_user_sheet, 
            sheet = "aws_okta_group_roles")





