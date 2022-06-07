as.character.adjustmentSets <- function(adjustment_sets){
  if(length(adjustment_sets) == 0) return("No adjustment sets")
  if(length(adjustment_sets) == 1){
    if(length(adjustment_sets)[[1]] == 0){
      return("Empty set")
    }
  }
  return(return(paste0(sapply(adjustment_sets, function(x) 
    paste("{", paste0(x, collapse = ","), "}", sep = "")), collapse = ",")))
}

structure_df <- function(df, max_string_length = 60){
  tab <- lapply(df, function(column){
    if(class(column) == "numeric" | class(column) == "integer"){
      unique_values <- summary(column)[1:6]
      zeros <- str_extract(unique_values, "0\\.0+")
      zeros[is.na(zeros)] <- "00"
      zeros <- max(mean(nchar(zeros)-2), 2)
      sample_values <- paste0(paste(names(unique_values), round(unname(unique_values), zeros), sep = " = "), collapse = ", ")
      ans <- data.frame(class = class(column),
                        sample_values = sample_values,
                        missing = paste0(round(mean(is.na(column))*100, 2), "%"),
                        stringsAsFactors = F)
    } else if(class(column) == "factor"){
      unique_values <- unique(na.omit(column))
      n_unique_values <- length(unique_values)
      sample_values <- paste0(sample(unique_values, size = min(5, n_unique_values), replace = F), collapse = ", ")
      if(nchar(sample_values) > max_string_length){
        sample_values <- paste0(substr(sample_values, 1, max_string_length - 3), "...")
      }
      ans <- data.frame(class = paste0("factor with ", n_unique_values, " levels"),
                        sample_values = sample_values,
                        missing = paste0(round(mean(is.na(column))*100, 2), "%"),
                        stringsAsFactors = F)
    } else if(class(column) == "character") {
      unique_values <- unique(na.omit(column))
      n_unique_values <- length(unique_values)
      sample_values <- paste0(sample(unique_values, size = min(5, n_unique_values), replace = F), collapse = ", ")
      if(nchar(sample_values) > max_string_length){
        sample_values <- paste0(substr(sample_values, 1, max_string_length - 3), "...")
      }
      ans <- data.frame(class = paste0("char with ", n_unique_values, " unique values"),
                        sample_values = sample_values,
                        missing = paste0(round(mean(is.na(column)), 2)*100, "%"),
                        stringsAsFactors = F)
    } else if(class(column) == "logical"){
      ans <- data.frame(class = "logical",
                        sample_values = paste0("FALSE freq = ", round(sum(column == FALSE, na.rm = T)/length(column),3), ", ",
                                               "TRUE freq = ", round(sum(column == TRUE, na.rm = T)/length(column),3)),
                        missing = paste0(round(mean(is.na(column))*100, 2), "%"),
                        stringsAsFactors = F)
      
    }
    return(ans)
  })
  tab <- bind_rows(tab)
  tab <- tab %>% mutate(variable = names(df)) %>%
    select(variable, everything())
  return(tab)
}
