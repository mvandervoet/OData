
# Load packages -----------------------------------------------------------

packages <- c("httr", "jsonlite", "dplyr", "stringr", "ggplot2", "scales", "plotly", "shiny")
missing <- setdiff(packages, rownames(installed.packages()))
if (length(missing) > 0) {
  utils::install.packages(missing)
}
lapply(packages, library, character.only = TRUE)


# Function to get API data ------------------------------------------------------------

get_cbs_data <- function(endpoint_code, top = NULL, verbose = TRUE, save_csv = FALSE, save_dir = NULL) {
  # 📦 Zorg dat benodigde packages geladen zijn
  required_packages <- c("httr", "jsonlite", "utils")
  invisible(lapply(required_packages, function(pkg) {
    if (!require(pkg, character.only = TRUE)) install.packages(pkg)
    library(pkg, character.only = TRUE)
  }))
  
  # 📌 Bouw de twee endpoint-URLs
  # endpoint_code <- "83625NED"
  # url <- "https://opendata.cbs.nl/ODataApi/odata/83625NED/TypedDataSet"
  endpoint_url_data <- paste0("https://opendata.cbs.nl/ODataApi/odata/", endpoint_code, "/TypedDataSet")
  endpoint_url_meta <- paste0("https://opendata.cbs.nl/ODataApi/odata/", endpoint_code, "/DataProperties")
  
  # Voeg $top toe indien gewenst
  if (!is.null(top)) {
    endpoint_url_data <- paste0(endpoint_url_data, "?$top=", top)
  }
  
  # 🔁 Ophalen van beide endpoints
  endpoint_urls <- list(
    typed = endpoint_url_data,
    meta  = endpoint_url_meta
  )
  
  responses <- lapply(endpoint_urls, function(url) {
    response <- httr::GET(url)
    
    if (httr::status_code(response) == 200) {
      data_raw <- httr::content(response, as = "text", encoding = "UTF-8")
      data_json <- jsonlite::fromJSON(data_raw)
      
      if (!is.null(data_json$value)) {
        data <- data_json$value
        
        if (verbose) {
          cat("✅ Succesvol opgehaald:", nrow(data), "rijen van", url, "\n")
        }
        
        return(data)
      } else {
        stop("⚠️ Geen 'value'-element gevonden in de response van: ", url)
      }
    } else {
      stop("❌ Ophalen mislukt van: ", url, " (Status code: ", httr::status_code(response), ")")
    }
  })
  
  # 💾 Opslaan als CSV (optioneel, beide datasets)
  if (save_csv && !is.null(save_dir)) {
    dir.create(save_dir, showWarnings = FALSE)
    utils::write.csv(responses$typed, file = file.path(save_dir, paste0(endpoint_code, "_TypedDataSet.csv")), row.names = FALSE)
    utils::write.csv(responses$meta, file = file.path(save_dir, paste0(endpoint_code, "_DataProperties.csv")), row.names = FALSE)
    if (verbose) cat("💾 CSV-bestanden opgeslagen in:", save_dir, "\n")
  }
  
  return(responses)
}


# TESTing -----------------------------------------------------------------

get_cbs_data_all <- function(endpoint_code, top = NULL, verbose = TRUE, save_csv = FALSE, save_dir = NULL) {
  # 📦 Load required packages
  required_packages <- c("httr", "xml2", "utils", "jsonlite", "here")
  invisible(lapply(required_packages, function(pkg) {
    if (!require(pkg, character.only = TRUE)) install.packages(pkg)
    library(pkg, character.only = TRUE)
  }))
  
  # 🌐 Step 1: Get XML overview of all subendpoints
  overview_url <- paste0("https://opendata.cbs.nl/ODataApi/OData/", endpoint_code, "/$metadata")
  
  if (verbose) cat("🔍 Getting subendpoints from:\n", overview_url, "\n")
  
  overview_response <- httr::GET(overview_url)
  
  if (httr::status_code(overview_response) != 200) {
    stop("❌ Failed to get overview metadata. Status code: ", httr::status_code(overview_response))
  }
  
  overview_xml <- xml2::read_xml(httr::content(overview_response, as = "raw"))
  
  # 📥 Step 2: Extract all EntitySet names (subendpoints)
  ns <- xml2::xml_ns(overview_xml)
  entity_sets <- xml2::xml_find_all(overview_xml, ".//d1:EntitySet", ns)
  #entity_sets <- xml2::xml_find_all(overview_xml, ".//edmx:DataServices//EntitySet", 
  #                                  ns = xml2::xml_ns(overview_xml))
  names <- xml2::xml_attr(entity_sets, "Name")

  subendpoint_names <- xml2::xml_attr(entity_sets, "Name")
  if (length(subendpoint_names) == 0) stop("⚠️ No subendpoints found in metadata.")
  
  if (verbose) cat("📌 Subendpoints found:\n", paste(subendpoint_names, collapse = ", "), "\n")
  
  # 🌐 Step 3: Download all subendpoints
  base_url <- paste0("https://opendata.cbs.nl/ODataApi/odata/", endpoint_code, "/")
  urls <- setNames(paste0(base_url, subendpoint_names), subendpoint_names)
  
  if (!is.null(top)) {
    urls <- lapply(urls, function(u) paste0(u, "?$top=", top))
  }
  
  responses <- lapply(names(urls), function(name) {
    url <- urls[[name]]
    if (verbose) cat("⬇️ Downloading:", name, "\n")
    response <- httr::GET(url)
    content_type <- httr::headers(response)[["content-type"]]
    response_text <- httr::content(response, as = "text", encoding = "UTF-8")
    
    if (!grepl("application/json", content_type)) {
      warning("⚠️ ", name, " is not JSON. Skipping. Content type: ", content_type)
      return(NULL)
    }
    
    json <- jsonlite::fromJSON(response_text)
    if (!is.null(json$value)) {
      if (verbose) cat("✅ Retrieved", nrow(json$value), "rows from", name, "\n")
      return(json$value)
    } else {
      warning("⚠️ No 'value' field in JSON for ", name)
      return(NULL)
    }
  })
  names(responses) <- names(urls)
  
  # 💾 Step 4: Save to CSV if needed
  if (save_csv && !is.null(save_dir)) {
    dir.create(here::here(save_dir), showWarnings = FALSE, recursive = TRUE)
    for (name in names(responses)) {
      if (!is.null(responses[[name]])) {
        utils::write.csv(responses[[name]], file = here::here(save_dir, paste0(endpoint_code, "_", name, ".csv")), row.names = FALSE)
      }
    }
    if (verbose) cat("💾 CSV files saved to:", here::here(save_dir), "\n")
  }
  
  return(responses)
}



# Get the data ------------------------------------------------------------
koop_index_raw <- get_cbs_data_all(endpoint_code = "85773NED") # Bestaande koopwoningen; index Nederland
koop_type_raw <- get_cbs_data_all(endpoint_code = "85791NED") # Bestaande koopwoningen; type, 2020=100
koop_regio_raw <- get_cbs_data_all(endpoint_code = "85792NED") # Bestaande koopwoningen; 2020=100, regio
koop_regio_corop_raw <- get_cbs_data_all(endpoint_code = "85819NED") # Bestaande koopwoningen; regio (COROP)
koop_regio_prijzen_raw <- get_cbs_data_all(endpoint_code = "83625NED", top = 9999) # Bestaande koopwoningen; prijzen, regio
koop_woz_raw <- get_cbs_data_all(endpoint_code = "85036NED", top = 9999) # Gemiddelde WOZ-waarde van woningen;regio
koop_uitgavenbezit_raw <- get_cbs_data_all(endpoint_code = "85838NED") # Koopwoningen; uitgaven en bezit
koop_nieuwbestaand_raw <- get_cbs_data_all(endpoint_code = "85822NED") # Koopwoningen; nieuwe en bestaande
koop_onroerend_raw <- get_cbs_data_all(endpoint_code = "37610", top = 9999) # Waarde onroerende zaken 1997-2020

# Write -------------------------------------------------------------------

# Use mget() to fetch them from the environment dynamically
object_names <- ls(pattern = "^koop_.*_raw$")
datasets <- mget(object_names)
# saveRDS(datasets, file = here::here("data-raw", "cbs_datasets_raw.rds"))

# Load -------------------------------------------------------------------
readRDS(here::here("data-raw", "cbs_datasets_raw.rds")) %>% list2env(envir = .GlobalEnv)
object_names <- ls(pattern = "^koop_.*$")
datasets <- mget(object_names)

# Process -------------------------------------------------------------------

# Rename columns in each dataset using metadata
datasets_cleaned_titles <- lapply(datasets, function(x) {
  #x <- datasets$koop_type
  typed <- x$TypedDataSet
  meta  <- x$DataProperties
  
  meta <- meta %>%
    left_join(
      meta %>% select(ID, ParentTitle = Title),
      by = c("ParentID" = "ID")
    ) %>%
    mutate(
      FullTitle = ifelse(
        is.na(ParentTitle),
        Title,
        paste0(ParentTitle, "__", Title)
      ),
      FullTitle = gsub("  ", " ", FullTitle),  # ✅ Replace spaces with underscores
      FullTitle = gsub(" ", "_", FullTitle)  # ✅ Replace spaces with underscores
    ) %>%
    dplyr::select(Key, FullTitle)
  
  rename_vector <- setNames(meta$FullTitle, meta$Key)
  
  if (is.data.frame(typed) && ncol(typed) > 0) {
    colnames(typed) <- ifelse(
      colnames(typed) %in% names(rename_vector),
      rename_vector[colnames(typed)],
      colnames(typed)
    )
  }
  # ✅ Check if both 'RegioS' and 'TypedDataSet' exist and are non-empty
  if ("RegioS" %in% names(x)) {
    
    regio_lookup <- x$RegioS %>%
      select(Key, Title) %>%
      distinct()
    
    # ✅ Replace 'RegioS' codes with titles in typed dataframe
    if ("Regio's" %in% names(typed)) {
      typed <- typed %>%
        left_join(regio_lookup, by = c("Regio's" = "Key")) %>%
        mutate('Regio\'s' = Title) %>%
        select(-Title)  # Remove temp join column
    }
  }
  if ("SoortKoopwoning" %in% names(x)) {
    regio_lookup <- x$SoortKoopwoning %>%
      select(Key, Title) %>%
      distinct()
    
    # ✅ Replace 'SoortKoopwoning' codes with titles in typed dataframe
    if ("Soort_koopwoning" %in% names(typed)) {
      typed <- typed %>%
        left_join(regio_lookup, by = c("Soort_koopwoning" = "Key")) %>%
        mutate(Soort_koopwoning = Title) %>%
        select(-Title)  # Remove temp join column
    }
  }
  if ("TypeWoning" %in% names(x)) {
    regio_lookup <- x$TypeWoning %>%
      select(Key, Title) %>%
      distinct()
    
    # ✅ Replace 'TypeWoning' codes with titles in typed dataframe
    if ("Type_woning" %in% names(typed)) {
      typed <- typed %>%
        left_join(regio_lookup, by = c("Type_woning" = "Key")) %>%
        mutate(Type_woning = Title) %>%
        select(-Title)  # Remove temp join column
    }
  }
  if ("UitgaveVoorAanschafEnBezit" %in% names(x)) {
    regio_lookup <- x$UitgaveVoorAanschafEnBezit %>%
      select(Key, Title) %>%
      distinct()
    
    # ✅ Replace 'UitgaveVoorAanschafEnBezit' codes with titles in typed dataframe
    if ("Uitgave_voor_aanschaf_en_bezit" %in% names(typed)) {
      typed <- typed %>%
        left_join(regio_lookup, by = c("Uitgave_voor_aanschaf_en_bezit" = "Key")) %>%
        mutate(Uitgave_voor_aanschaf_en_bezit = Title) %>%
        select(-Title)  # Remove temp join column
    }
  }
  names(typed) <- gsub("'", "", names(typed), fixed = TRUE)
  names(typed) <- gsub("%", "pct", names(typed), fixed = TRUE)
  names(typed) <- gsub(".", "", names(typed), fixed = TRUE)
  names(typed) <- gsub("[^a-zA-Z0-9_]", "", names(typed), fixed = TRUE)
  typed
})
test <- datasets_cleaned_titles$koop_regio_corop_raw


# Clean columns -----------------------------------------------------------
datasets_cleaned_columns <- lapply(datasets_cleaned_titles, function(x) {
  # 🧹 Perioden opsplitsen
  output <- x %>%
    mutate(
      PeriodeJaar = stringr::str_sub(Perioden, 1, 4),
      PeriodeType = stringr::str_sub(Perioden, 5, 6),
      PeriodeCode = stringr::str_sub(Perioden, 7, 8),
      TijdLabel = case_when(
        PeriodeType == "JJ" ~ PeriodeJaar,
        PeriodeType == "KW" ~ paste0(PeriodeJaar, "-", "Q", PeriodeCode),
        PeriodeType == "MM" ~ paste0(PeriodeJaar, "-", "M", PeriodeCode),
        TRUE ~ Perioden
      ),
      TijdLabel = factor(TijdLabel, levels = unique(TijdLabel), ordered = TRUE)
    )
  output <- output %>%
    mutate(PeriodeType = recode(PeriodeType,
                                "KW" = "Quarter",
                                "JJ" = "Year",
                                "MM" = "Month"))
  output
})
test <- datasets_cleaned_columns$koop_type_raw
  
# Write -------------------------------------------------------------------

# Use mget() to fetch them from the environment dynamically
names(datasets_cleaned_columns) <- gsub("_raw$", "", names(datasets_cleaned_columns))
saveRDS(datasets_cleaned_columns, file = here::here("data", "cbs_datasets_clean.rds"))
saveRDS(datasets_cleaned_columns, file = here::here("ShinyOData/data", "cbs_datasets_clean.rds"))


readRDS(here::here("data", "cbs_datasets_clean.rds")) %>% list2env(envir = .GlobalEnv)
object_names <- ls(pattern = "^koop_.*_raw$")
datasets <- mget(object_names)

