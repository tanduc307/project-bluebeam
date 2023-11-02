library(FAOSTAT)
fao_metadata_1 <- FAOsearch()
fao_metadata_2 <- search_dataset()

search_dataset(dataset_code, dataset_label, latest = TRUE, reset_cache = FALSE)


search_dataset(dataset_code = "QCL",
               latest = TRUE, 
               reset_cache = FALSE)

search_dataset(dataset_label = "Crops",
               latest = TRUE, 
               reset_cache = FALSE)

data_folder <- "data_raw_1"
crop_production <- get_faostat_bulk(code = "QCL", 
                                    data_folder = data_folder)


crop_production <- get_faostat_bulk(code = "QCL", 
                                    data_folder = "./")




function (dataset_code, dataset_label, latest = TRUE, reset_cache = FALSE) 
{
  if (deparse(match.call()[[1]]) == "FAOsearch") {
    .Deprecated("search_fao", msg = "FAOsearch has deprecated been replaced by search_dataset as the old API doesn't work anymore. \n                search_dataset was called instead")
  }
  if (missing(dataset_code)) 
    dataset_code <- NULL
  if (missing(dataset_label)) 
    dataset <- NULL
  if (length(dataset) > 1) {
    warning("More than 1 values was supplied to dataset, only the first will be used")
    dataset <- dataset[1]
  }
  search_data <- cache_data("search_dataset", get_fao("/domains"), 
                            reset = reset_cache)
  data <- rbindlist(content(search_data)[["data"]], fill = TRUE)
  metadata <- content(search_data)[["metadata"]]
  function_env <- environment()
  if (!is.null(dataset_code)) {
    data <- data[code %chin% get("dataset_code", envir = function_env), 
    ]
  }
  if (!is.null(dataset)) {
    data <- data[grepl(get("dataset", envir = function_env), 
                       label), ]
  }
  if (latest) {
    data <- data[order(date_update)]
  }
  data <- as.data.frame(data)
  attr(data, "query_metadata") <- metadata
  return(data)
}