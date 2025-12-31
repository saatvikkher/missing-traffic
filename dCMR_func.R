library(acepack) # maximal correlation
library(Hmisc) # hoeffd
library(geohashTools)
theme_set(theme_minimal())

count_nas_dCMR = function(dataset_name, dataset, conditioning_var_str){ 
  ###
  
  # Aggregate dataset over levels of conditioning_variable, count the number of NAs
  # conditioning_variable in c("hour", "week", "g6")
  
  ###
  print(dataset_name)
  
  
  if ( sum(conditioning_var_str == "time") >= 1 ){
    
    dataset$row_NA = rowSums(is.na(dataset |> dplyr::select(-time)))
    m_minus_1 = dim(dataset |> dplyr::select(-time))[2]
    
    dcmr = dataset |>
      filter(!is.na(time)) |>
      mutate(`dataset_name` = dataset_name,
             hour_24 = substr(time, 1, 2),
             hour_24 = as.integer(hour_24),
             conditioning_variable = "hour") |>
      group_by(hour_24, dataset_name, conditioning_variable) |>
      dplyr::summarize(count = n(),
                       sum_row_NA = sum(row_NA),
                       dCMR = mean(row_NA)/m_minus_1, .groups="drop") |>
      mutate(m = m_minus_1 + 1)
    
    if (dim(dcmr)[1] > 4){ # greater than 4 observations
      
      dcmr$hoeffd_pval = hoeffd(dcmr$hour_24, dcmr$dCMR)$P[1,2] # p-value
      dcmr$hoeffd_D = hoeffd(dcmr$hour_24, dcmr$dCMR)$D[1,2] # D
      
      a = ace(dcmr$hour_24, dcmr$dCMR, circ=1)
      dcmr$max_cor = cor(a$tx, a$ty)
    } else {
      
      dcmr$hoeffd_pval = NA
      dcmr$hoeffd_D = NA
      dcmr$max_cor = NA
      
    }
    
  } else if ( sum(conditioning_var_str == "date") >= 1) {
    
    dataset$row_NA = rowSums(is.na(dataset |> dplyr::select(-date)))
    m_minus_1 = dim(dataset |> dplyr::select(-date))[2]
    
    dcmr = dataset |>
      filter(!is.na(date)) |>
      mutate(week = ymd(cut(ymd(date), "week")),
             # week_integer = (interval(min(week), week) %/% weeks(1)) + 1, 
             # the NP correlation functions manage character weeks correctly
             `dataset_name` = dataset_name,
             conditioning_variable = "week") |>
      group_by(week, dataset_name, conditioning_variable) |>
      dplyr::summarize(count = n(),
                       sum_row_NA = sum(row_NA),
                       dCMR = mean(row_NA)/m_minus_1, .groups="drop") |>
      mutate(m = m_minus_1 + 1)
    
    if (dim(dcmr)[1] > 4) {
      
      dcmr$hoeffd_pval = hoeffd(dcmr$week, dcmr$dCMR)$P[1,2]
      dcmr$hoeffd_D = hoeffd(dcmr$week, dcmr$dCMR)$D[1,2] # p-value
      
      a = ace(dcmr$week, dcmr$dCMR)
      dcmr$max_cor = cor(a$tx, a$ty)
      
    } else {
      
      dcmr$hoeffd_pval = NA
      dcmr$hoeffd_D = NA
      dcmr$max_cor = NA
      
    }
    
  } else if ( sum(conditioning_var_str == c("lat", "lng")) >= 1) {
    
    dataset$row_NA = rowSums(is.na(dataset |> dplyr::select(-lat, -lng)))
    m_minus_1 = dim(dataset |> dplyr::select(-lat, -lng))[2]
    
    dcmr = dataset |>
      filter(!is.na(lat) & !is.na(lng)) |>
      filter(lat < 90 & lat >= -90 ) |>
      mutate(g6 = geohashTools::gh_encode(lat, lng),
             `dataset_name` = dataset_name,
             conditioning_variable = "g6") |>
      group_by( g6, dataset_name, conditioning_variable) |>
      dplyr::summarize(count = n(),
                       mean_lat = mean(lat),
                       mean_lng = mean(lng), 
                       sum_row_NA = sum(row_NA),
                       dCMR = mean(row_NA)/m_minus_1, .groups="drop")|>
      mutate(m = m_minus_1 + 1)
    
    pval_hoeffding_d = hoeffd(cbind(dcmr$mean_lat, dcmr$mean_lng, dcmr$dCMR))$P
    print(pval_hoeffding_d)
    D_hoeffding_d = hoeffd(cbind(dcmr$mean_lat, dcmr$mean_lng, dcmr$dCMR))$D
    print(D_hoeffding_d)
    
    dcmr$hoeffd_pval_lat = pval_hoeffding_d[1, 3]
    dcmr$hoeffd_pval_lng = pval_hoeffding_d[2, 3]
    
    dcmr$hoeffd_D_lat = D_hoeffding_d[1, 3]
    dcmr$hoeffd_D_lng = D_hoeffding_d[2, 3]
    
    a = ace(dcmr$mean_lat, dcmr$dCMR)
    dcmr$max_cor_lat = cor(a$tx, a$ty)
    
    a = ace(dcmr$mean_lng, dcmr$dCMR)
    dcmr$max_cor_lng = cor(a$tx, a$ty)
    
  }
  else { print("typo in conditioning_variable") }
  
  
  return(dcmr)
  
}
