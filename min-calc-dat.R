rm(list=ls())

require(xml2)
require(rvest)
require(dplyr)
require(purrr)
require(here)
require(glue)
require(stringr)
require(lubridate)
require(readxl)
require(stringr)
require(lubridate)
require(tidyr)
require(future)
require(furrr)

setwd("S:/Finance/Shared Area/BNSSG - BI/8 Modelling and Analytics/projects/corona/rtt/ode model (2022)/england application/")


providers <- c(
  "BARTS HEALTH NHS TRUST", "LONDON NORTH WEST UNIVERSITY HEALTHCARE NHS TRUST",
  "ROYAL FREE LONDON NHS FOUNDATION TRUST", "ROYAL NATIONAL ORTHOPAEDIC HOSPITAL NHS TRUST",
  "NORTH MIDDLESEX UNIVERSITY HOSPITAL NHS TRUST", "THE HILLINGDON HOSPITALS NHS FOUNDATION TRUST",
  "NORTH EAST LONDON NHS FOUNDATION TRUST", "KINGSTON HOSPITAL NHS FOUNDATION TRUST",
  "BARKING, HAVERING AND REDBRIDGE UNIVERSITY HOSPITALS NHS TRUST",
  "GUY'S AND ST THOMAS' NHS FOUNDATION TRUST", "LEWISHAM AND GREENWICH NHS TRUST",
  "CROYDON HEALTH SERVICES NHS TRUST", "ST GEORGE'S UNIVERSITY HOSPITALS NHS FOUNDATION TRUST",
  "KING'S COLLEGE HOSPITAL NHS FOUNDATION TRUST", "WHITTINGTON HEALTH NHS TRUST",
  "WEST LONDON NHS TRUST", "GREAT ORMOND STREET HOSPITAL FOR CHILDREN NHS FOUNDATION TRUST",
  "MOORFIELDS EYE HOSPITAL NHS FOUNDATION TRUST", "OXLEAS NHS FOUNDATION TRUST",
  "THE ROYAL MARSDEN NHS FOUNDATION TRUST", "CHELSEA AND WESTMINSTER HOSPITAL NHS FOUNDATION TRUST",
  "HOMERTON UNIVERSITY HOSPITAL NHS FOUNDATION TRUST", "SOUTH WEST LONDON AND ST GEORGE'S MENTAL HEALTH NHS TRUST",
  "UNIVERSITY COLLEGE LONDON HOSPITALS NHS FOUNDATION TRUST", "CENTRAL AND NORTH WEST LONDON NHS FOUNDATION TRUST",
  "EPSOM AND ST HELIER UNIVERSITY HOSPITALS NHS TRUST", "HOUNSLOW AND RICHMOND COMMUNITY HEALTHCARE NHS TRUST",
  "IMPERIAL COLLEGE HEALTHCARE NHS TRUST", "CENTRAL LONDON COMMUNITY HEALTHCARE NHS TRUST",
  "UNIVERSITY HOSPITALS DORSET NHS FOUNDATION TRUST", "YEOVIL DISTRICT HOSPITAL NHS FOUNDATION TRUST",
  "UNIVERSITY HOSPITALS BRISTOL AND WESTON NHS FOUNDATION TRUST",
  "TORBAY AND SOUTH DEVON NHS FOUNDATION TRUST", "DORSET COUNTY HOSPITAL NHS FOUNDATION TRUST",
  "NORTHERN DEVON HEALTHCARE NHS TRUST", "ROYAL UNITED HOSPITALS BATH NHS FOUNDATION TRUST",
  "ROYAL CORNWALL HOSPITALS NHS TRUST", "SOMERSET NHS FOUNDATION TRUST",
  "ROYAL DEVON AND EXETER NHS FOUNDATION TRUST", "CORNWALL PARTNERSHIP NHS FOUNDATION TRUST",
  "UNIVERSITY HOSPITALS PLYMOUTH NHS TRUST", "GREAT WESTERN HOSPITALS NHS FOUNDATION TRUST",
  "SALISBURY NHS FOUNDATION TRUST", "GLOUCESTERSHIRE HOSPITALS NHS FOUNDATION TRUST",
  "NORTH BRISTOL NHS TRUST",
  "WESTON AREA HEALTH NHS TRUST",
  "SOLENT NHS TRUST", "ISLE OF WIGHT NHS TRUST",
  "ROYAL SURREY COUNTY HOSPITAL NHS FOUNDATION TRUST", "SUSSEX COMMUNITY NHS FOUNDATION TRUST",
  "FRIMLEY HEALTH NHS FOUNDATION TRUST", "UNIVERSITY HOSPITAL SOUTHAMPTON NHS FOUNDATION TRUST",
  "PORTSMOUTH HOSPITALS UNIVERSITY NATIONAL HEALTH SERVICE TRUST",
  "ROYAL BERKSHIRE NHS FOUNDATION TRUST", "HAMPSHIRE HOSPITALS NHS FOUNDATION TRUST",
  "DARTFORD AND GRAVESHAM NHS TRUST", "MEDWAY NHS FOUNDATION TRUST",
  "QUEEN VICTORIA HOSPITAL NHS FOUNDATION TRUST", "OXFORD UNIVERSITY HOSPITALS NHS FOUNDATION TRUST",
  "ASHFORD AND ST PETER'S HOSPITALS NHS FOUNDATION TRUST",
  "SURREY AND SUSSEX HEALTHCARE NHS TRUST",
  "EAST KENT HOSPITALS UNIVERSITY NHS FOUNDATION TRUST", "SOUTHERN HEALTH NHS FOUNDATION TRUST",
  "MAIDSTONE AND TUNBRIDGE WELLS NHS TRUST", "BERKSHIRE HEALTHCARE NHS FOUNDATION TRUST",
  "EAST SUSSEX HEALTHCARE NHS TRUST", "BUCKINGHAMSHIRE HEALTHCARE NHS TRUST",
  "UNIVERSITY HOSPITALS SUSSEX NHS FOUNDATION TRUST", "KENT COMMUNITY HEALTH NHS FOUNDATION TRUST",
  "HEREFORDSHIRE AND WORCESTERSHIRE HEALTH AND CARE NHS TRUST",
  "SHROPSHIRE COMMUNITY HEALTH NHS TRUST", "WALSALL HEALTHCARE NHS TRUST",
  "CHESTERFIELD ROYAL HOSPITAL NHS FOUNDATION TRUST", "SOUTH WARWICKSHIRE NHS FOUNDATION TRUST",
  "UNIVERSITY HOSPITALS OF NORTH MIDLANDS NHS TRUST", "SHERWOOD FOREST HOSPITALS NHS FOUNDATION TRUST",
  "UNIVERSITY HOSPITALS COVENTRY AND WARWICKSHIRE NHS TRUST", "THE ROBERT JONES AND AGNES HUNT ORTHOPAEDIC HOSPITAL NHS FOUNDATION TRUST",
  "THE ROYAL WOLVERHAMPTON NHS TRUST", "WYE VALLEY NHS TRUST",
  "GEORGE ELIOT HOSPITAL NHS TRUST", "THE DUDLEY GROUP NHS FOUNDATION TRUST",
  "KETTERING GENERAL HOSPITAL NHS FOUNDATION TRUST", "NORTHAMPTON GENERAL HOSPITAL NHS TRUST",
  "NORTHAMPTONSHIRE HEALTHCARE NHS FOUNDATION TRUST", "LINCOLNSHIRE PARTNERSHIP NHS FOUNDATION TRUST",
  "BIRMINGHAM WOMEN'S AND CHILDREN'S NHS FOUNDATION TRUST", "MIDLANDS PARTNERSHIP NHS FOUNDATION TRUST",
  "THE ROYAL ORTHOPAEDIC HOSPITAL NHS FOUNDATION TRUST", "UNIVERSITY HOSPITALS BIRMINGHAM NHS FOUNDATION TRUST",
  "UNIVERSITY HOSPITALS OF DERBY AND BURTON NHS FOUNDATION TRUST",
  "UNITED LINCOLNSHIRE HOSPITALS NHS TRUST", "UNIVERSITY HOSPITALS OF LEICESTER NHS TRUST",
  "WORCESTERSHIRE ACUTE HOSPITALS NHS TRUST", "NOTTINGHAM UNIVERSITY HOSPITALS NHS TRUST",
  "SANDWELL AND WEST BIRMINGHAM HOSPITALS NHS TRUST", "DERBYSHIRE HEALTHCARE NHS FOUNDATION TRUST",
  "THE SHREWSBURY AND TELFORD HOSPITAL NHS TRUST", "COVENTRY AND WARWICKSHIRE PARTNERSHIP NHS TRUST",
  "BIRMINGHAM COMMUNITY HEALTHCARE NHS FOUNDATION TRUST", "MID AND SOUTH ESSEX NHS FOUNDATION TRUST",
  "BEDFORDSHIRE HOSPITALS NHS FOUNDATION TRUST", "THE QUEEN ELIZABETH HOSPITAL, KING'S LYNN, NHS FOUNDATION TRUST",
  "MILTON KEYNES UNIVERSITY HOSPITAL NHS FOUNDATION TRUST", "EAST SUFFOLK AND NORTH ESSEX NHS FOUNDATION TRUST",
  "ROYAL PAPWORTH HOSPITAL NHS FOUNDATION TRUST", "NORTH WEST ANGLIA NHS FOUNDATION TRUST",
  "JAMES PAGET UNIVERSITY HOSPITALS NHS FOUNDATION TRUST", "WEST SUFFOLK NHS FOUNDATION TRUST",
  "CAMBRIDGE UNIVERSITY HOSPITALS NHS FOUNDATION TRUST", "NORFOLK AND NORWICH UNIVERSITY HOSPITALS NHS FOUNDATION TRUST",
  "THE PRINCESS ALEXANDRA HOSPITAL NHS TRUST", "CAMBRIDGESHIRE AND PETERBOROUGH NHS FOUNDATION TRUST",
  "WEST HERTFORDSHIRE HOSPITALS NHS TRUST", "EAST AND NORTH HERTFORDSHIRE NHS TRUST",
  "NORFOLK COMMUNITY HEALTH AND CARE NHS TRUST", "HERTFORDSHIRE COMMUNITY NHS TRUST",
  "CAMBRIDGESHIRE COMMUNITY SERVICES NHS TRUST", "MANCHESTER UNIVERSITY NHS FOUNDATION TRUST",
  "WIRRAL UNIVERSITY TEACHING HOSPITAL NHS FOUNDATION TRUST", "ST HELENS AND KNOWSLEY TEACHING HOSPITALS NHS TRUST",
  "LIVERPOOL HEART AND CHEST HOSPITAL NHS FOUNDATION TRUST", "ALDER HEY CHILDREN'S NHS FOUNDATION TRUST",
  "MID CHESHIRE HOSPITALS NHS FOUNDATION TRUST", "THE CHRISTIE NHS FOUNDATION TRUST",
  "LIVERPOOL UNIVERSITY HOSPITALS NHS FOUNDATION TRUST", "THE CLATTERBRIDGE CANCER CENTRE NHS FOUNDATION TRUST",
  "LIVERPOOL WOMEN'S NHS FOUNDATION TRUST", "THE WALTON CENTRE NHS FOUNDATION TRUST",
  "EAST CHESHIRE NHS TRUST", "SALFORD ROYAL NHS FOUNDATION TRUST",
  "BOLTON NHS FOUNDATION TRUST", "TAMESIDE AND GLOSSOP INTEGRATED CARE NHS FOUNDATION TRUST",
  "WRIGHTINGTON, WIGAN AND LEIGH NHS FOUNDATION TRUST", "UNIVERSITY HOSPITALS OF MORECAMBE BAY NHS FOUNDATION TRUST",
  "SOUTHPORT AND ORMSKIRK HOSPITAL NHS TRUST", "LANCASHIRE & SOUTH CUMBRIA NHS FOUNDATION TRUST",
  "PENNINE ACUTE HOSPITALS NHS TRUST", "STOCKPORT NHS FOUNDATION TRUST",
  "WARRINGTON AND HALTON TEACHING HOSPITALS NHS FOUNDATION TRUST",
  "BLACKPOOL TEACHING HOSPITALS NHS FOUNDATION TRUST", "LANCASHIRE TEACHING HOSPITALS NHS FOUNDATION TRUST",
  "EAST LANCASHIRE HOSPITALS NHS TRUST", "BRIDGEWATER COMMUNITY HEALTHCARE NHS FOUNDATION TRUST",
  "WIRRAL COMMUNITY HEALTH AND CARE NHS FOUNDATION TRUST", "SOUTH TYNESIDE AND SUNDERLAND NHS FOUNDATION TRUST",
  "BRADFORD TEACHING HOSPITALS NHS FOUNDATION TRUST", "YORK AND SCARBOROUGH TEACHING HOSPITALS NHS FOUNDATION TRUST",
  "HARROGATE AND DISTRICT NHS FOUNDATION TRUST", "AIREDALE NHS FOUNDATION TRUST",
  "SHEFFIELD CHILDREN'S NHS FOUNDATION TRUST", "BARNSLEY HOSPITAL NHS FOUNDATION TRUST",
  "THE ROTHERHAM NHS FOUNDATION TRUST", "SHEFFIELD TEACHING HOSPITALS NHS FOUNDATION TRUST",
  "NORTHERN LINCOLNSHIRE AND GOOLE NHS FOUNDATION TRUST", "NORTH CUMBRIA INTEGRATED CARE NHS FOUNDATION TRUST",
  "DONCASTER AND BASSETLAW TEACHING HOSPITALS NHS FOUNDATION TRUST",
  "GATESHEAD HEALTH NHS FOUNDATION TRUST", "LEEDS TEACHING HOSPITALS NHS TRUST",
  "THE NEWCASTLE UPON TYNE HOSPITALS NHS FOUNDATION TRUST", "NORTHUMBRIA HEALTHCARE NHS FOUNDATION TRUST",
  "SOUTH TEES HOSPITALS NHS FOUNDATION TRUST", "NORTH TEES AND HARTLEPOOL NHS FOUNDATION TRUST",
  "HULL UNIVERSITY TEACHING HOSPITALS NHS TRUST", "CALDERDALE AND HUDDERSFIELD NHS FOUNDATION TRUST",
  "CUMBRIA, NORTHUMBERLAND, TYNE AND WEAR NHS FOUNDATION TRUST",
  "MID YORKSHIRE HOSPITALS NHS TRUST", "SOUTH WEST YORKSHIRE PARTNERSHIP NHS FOUNDATION TRUST",
  "COUNTY DURHAM AND DARLINGTON NHS FOUNDATION TRUST", "LEEDS COMMUNITY HEALTHCARE NHS TRUST",
  "BRADFORD DISTRICT CARE NHS FOUNDATION TRUST"
)

providers <- providers %>%
  purrr::set_names()

year_range <- 2019:2022

url_stem <- glue::glue("https://www.england.nhs.uk/statistics/statistical-work-areas/rtt-waiting-times/rtt-data-20{(year_range[-length(year_range)] - 2000)}-{(year_range[-1] - 2000)}/")

pathways <- c("Incomplete", "NonAdmitted", "Admitted", "New-Periods") %>%
  purrr::set_names(., nm = .)

urls <- purrr::map(
  url_stem,
  .f = function(stem) purrr::imap(
    pathways,
    ~stringr::str_subset(html_attr(html_nodes(read_html(stem), "a"), "href"), glue::glue("/{.x}-Provider"))
  )
)


read_rtt_trust_data <- function(url,
                                pathway = "Incomplete",
                                provider = "NORTH BRISTOL NHS TRUST",
                                year_range = year_range
                                ) {
  per_pat <- glue::glue("-({glue::glue_collapse(month.abb, sep = '|')})({glue::glue_collapse((year_range - 2000), sep = '|')})-")
  period <- stringr::str_match(url, per_pat)
  period <- lubridate::ymd(glue::glue("20{period[3]}-{period[2]}-01"))
  period <- lubridate::rollforward(period)
  cols <- c(
    "Region Code",
    "CCG Code",
    "CCG Name",
    "Provider Name",
    "Provider Code",
    "Treatment Function Code",
    "Treatment Function",
    "Total within 18 weeks"
  )
  wait_cols <- c(">0-1", ">1-2", ">2-3", ">3-4",
                 ">4-5", ">5-6", ">6-7",
                 ">7-8", ">8-9", ">9-10",
                 ">10-11", ">11-12", ">12-13",
                 ">13-14", ">14-15", ">15-16", 
                 ">16-17", ">17-18", ">18-19",
                 ">19-20", ">20-21", ">21-22",
                 ">22-23", ">23-24", ">24-25",
                 ">25-26", ">26-27", ">27-28",
                 ">28-29", ">29-30", ">30-31",
                 ">31-32", ">32-33", ">33-34",
                 ">34-35", ">35-36", ">36-37",
                 ">37-38", ">38-39", ">39-40",
                 ">40-41", ">41-42", ">42-43",
                 ">43-44", ">44-45", ">45-46",
                 ">46-47", ">47-48", ">48-49",
                 ">49-50", ">50-51", ">51-52",
                 "52 plus", ">52-53", ">53-54",
                 ">54-55", ">55-56", ">56-57",
                 ">57-58", ">58-59", ">59-60",
                 ">60-61", ">61-62", ">62-63",
                 ">63-64", ">64-65", ">65-66",
                 ">66-67", ">67-68", ">68-69",
                 ">69-70", ">70-71", ">71-72",
                 ">72-73", ">73-74", ">74-75",
                 ">75-76", ">76-77", ">77-78",
                 ">78-79", ">79-80", ">80-81",
                 ">81-82", ">82-83", ">83-84",
                 ">84-85", ">85-86", ">86-87",
                 ">87-88", ">88-89", ">89-90",
                 ">90-91", ">91-92", ">92-93", 
                 ">93-94", ">94-95", ">95-96",
                 ">96-97", ">97-98", ">98-99",
                 ">99-100", ">100-101", ">101-102",
                 ">102-103", ">103-104", "104 plus")
  w_i <- seq(from = 0.5, by = 1, length.out = 105)
  cols_out <- switch(pathway,
                     "Incomplete" = c("Treatment Function",
                                      "% within 18 weeks",
                                      "Total number of incomplete pathways",
                                      "Total 52 plus weeks",
                                      wait_cols,
                                      "Average (median) waiting time (in weeks)"),
                     "NonAdmitted" = c("Treatment Function",
                                       wait_cols,
                                       ">100-101", ">101-102", ">102-103", ">103-104", "104 plus",
                                       "Total number of completed pathways (all)"),
                     "Admitted"    = c("Treatment Function",
                                       wait_cols,
                                       "Total number of completed pathways (all)"),
                     "New-Periods" = c("Treatment Function",
                                       "Number of new RTT clock starts during the month")
  )
  
  cols <- switch(pathway,
                 "Incomplete" = c(cols, cols_out),
                 "NonAdmitted" = c(cols, cols_out),
                 "Admitted" = c(cols, cols_out),
                 "New-Periods" = c(cols, cols_out)
  )
  # set the clockstop varname later (as the columns are equally named in the
  # excel files for non-admitted and admitted and we want to keep them separate)
  clckstp_var <- switch(pathway,
                        "Incomplete" = "",
                        "NonAdmitted" = "nonad",
                        "Admitted" = "ad",
                        "New-Periods" = ""
  )
  
  incpl_pthwy_cols_gt18 <- c(">18-19", ">19-20", ">20-21", ">21-22", ">22-23", 
                             ">23-24", ">24-25", ">25-26", ">26-27", ">27-28", ">28-29", ">29-30", 
                             ">30-31", ">31-32", ">32-33", ">33-34", ">34-35", ">35-36", ">36-37", 
                             ">37-38", ">38-39", ">39-40", ">40-41", ">41-42", ">42-43", ">43-44", 
                             ">44-45", ">45-46", ">46-47", ">47-48", ">48-49", ">49-50", ">50-51", 
                             ">51-52", ">52-53", ">53-54", ">54-55", ">55-56", ">56-57", ">57-58", 
                             ">58-59", ">59-60", ">60-61", ">61-62", ">62-63", ">63-64", ">64-65", 
                             ">65-66", ">66-67", ">67-68", ">68-69", ">69-70", ">70-71", ">71-72", 
                             ">72-73", ">73-74", ">74-75", ">75-76", ">76-77", ">77-78", ">78-79", 
                             ">79-80", ">80-81", ">81-82", ">82-83", ">83-84", ">84-85", ">85-86", 
                             ">86-87", ">87-88", ">88-89", ">89-90", ">90-91", ">91-92", ">92-93", 
                             ">93-94", ">94-95", ">95-96", ">96-97", ">97-98", ">98-99", ">99-100", 
                             ">100-101", ">101-102", ">102-103", ">103-104", "104 plus", "52 plus")
  
  tfc_codes <- c("General Surgery" = "(:?C_|[INA]P)?100",
                 "Urology" = "(:?C_|[INA]P)?101",
                 "Trauma and Orthopaedic"= "(:?C_|[INA]P)?110",
                 "Ear Nose and Throat" = "(:?C_|[INA]P)?120",
                 "Ophthalmology" = "(:?C_|[INA]P)?130",
                 "Oral Surgery" = "(:?C_|[INA]P)?140",
                 "Neurosurgical" = "(:?C_|[INA]P)?150",
                 "Plastic Surgery" = "(:?C_|[INA]P)?160",
                 "Cardiothoracic Surgery" = "(:?C_|[INA]P)?170",
                 "General Internal Medicine" ="300",
                 "Gastroenterology" = "(:?C_|[INA]P)?301",
                 "Cardiology" = "(:?C_|[INA]P)?320",
                 "Dermatology" = "(:?C_|[INA]P)?330",
                 "Respiratory Medicine" = "(:?C_|[INA]P)?340",
                 "Neurology" = "(:?C_|[INA]P)?400",
                 "Rheumatology" = "(:?C_|[INA]P)?410",
                 "Elderly Medicine" = "(:?C_|[INA]P)?430",
                 "Gynaecology" = "(:?C_|[INA]P)?502",
                 "Other" = "X0[1-6]",
                 "Total" = "999")
  
  tmp <- tempfile(fileext = ".xls")
  on.exit(unlink(tmp))
  download.file(url,
                destfile = tmp,
                quiet = TRUE,
                mode = "wb")
  
  
  
  read_xls_safe <- purrr::safely(readxl::read_xls)
  dat <- read_xls_safe(path.expand(tmp), sheet = "Provider", skip = 13)
  if(is.null(dat$error)){
    dat <- dat$result
  } else {
    dat <- readxl::read_xlsx(path.expand(tmp), sheet = "Provider", skip = 13)
  }
  
  # if(pathway == "Admitted") browser()
  
  
  dat <- dat %>%
    dplyr::select(dplyr::any_of(cols)) %>%
    dplyr::filter(`Provider Name` %in% provider) %>%
    dplyr::mutate(dplyr::across(-c("Region Code", 
                                   "Provider Name",
                                   "Provider Code",
                                   "Treatment Function Code",
                                   "Treatment Function"),
                                .fns = as.numeric)) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(specialty = names(tfc_codes[stringr::str_detect(`Treatment Function Code`, pattern = tfc_codes)]),
                  `Treatment Function Code` = stringr::str_remove_all(`Treatment Function Code`, pattern = "[^0-9]")
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(tidyselect::contains(cols_out), `Provider Name`, `Treatment Function`, specialty) %>%
    dplyr::rename_with(.cols = dplyr::matches("% within 18 weeks"), .fn = ~"rtt_18") %>%
    dplyr::rename_with(.cols = dplyr::matches("Total number of incomplete pathways"), .fn = ~"n_wl") %>%
    dplyr::rename_with(.cols = dplyr::matches("Number of new RTT clock starts during the month"), .fn = ~"n_clckstrt") %>%
    dplyr::rename_with(.cols = dplyr::matches("Average \\(median\\) waiting time \\(in weeks\\)"), .fn = ~"med_wait") %>%
    dplyr::rename_with(.cols = dplyr::matches("Total number of completed pathways \\(all\\)"), .fn = ~glue::glue("n_clckstp_{clckstp_var}")) %>%
    dplyr::mutate(period = period) %>%
    dplyr::group_by(dplyr::across(dplyr::any_of(c("period",
                                                  "Provider Name",
                                                  "trust",
                                                  "specialty"#,
                                                  #"Treatment Function Code"
    )))) %>%
    dplyr::summarise(dplyr::across(dplyr::any_of(c(">0-1", ">1-2", ">2-3", ">3-4",
                                                   ">4-5", ">5-6", ">6-7",
                                                   ">7-8", ">8-9", ">9-10",
                                                   ">10-11", ">11-12", ">12-13",
                                                   ">13-14", ">14-15", ">15-16", 
                                                   ">16-17", ">17-18", ">18-19",
                                                   ">19-20", ">20-21", ">21-22",
                                                   ">22-23", ">23-24", ">24-25",
                                                   ">25-26", ">26-27", ">27-28",
                                                   ">28-29", ">29-30", ">30-31",
                                                   ">31-32", ">32-33", ">33-34",
                                                   ">34-35", ">35-36", ">36-37",
                                                   ">37-38", ">38-39", ">39-40",
                                                   ">40-41", ">41-42", ">42-43",
                                                   ">43-44", ">44-45", ">45-46",
                                                   ">46-47", ">47-48", ">48-49",
                                                   ">49-50", ">50-51", ">51-52",
                                                   "52 plus", ">52-53", ">53-54",
                                                   ">54-55", ">55-56", ">56-57",
                                                   ">57-58", ">58-59", ">59-60",
                                                   ">60-61", ">61-62", ">62-63",
                                                   ">63-64", ">64-65", ">65-66",
                                                   ">66-67", ">67-68", ">68-69",
                                                   ">69-70", ">70-71", ">71-72",
                                                   ">72-73", ">73-74", ">74-75",
                                                   ">75-76", ">76-77", ">77-78",
                                                   ">78-79", ">79-80", ">80-81",
                                                   ">81-82", ">82-83", ">83-84",
                                                   ">84-85", ">85-86", ">86-87",
                                                   ">87-88", ">88-89", ">89-90",
                                                   ">90-91", ">91-92", ">92-93", 
                                                   ">93-94", ">94-95", ">95-96",
                                                   ">96-97", ">97-98", ">98-99",
                                                   ">99-100", ">100-101", ">101-102",
                                                   ">102-103", ">103-104", "104 plus",
                                                   "n_wl", "n_clckstrt", "Total 52 plus weeks", "Total number of completed pathways (all)", 
                                                   glue::glue("n_clckstp_{clckstp_var}"))), sum, na.rm = TRUE),
                     .groups = "keep")
  if(pathway == "Incomplete"){
    dat <- dat %>%
      dplyr::mutate(across(dplyr::one_of(incpl_pthwy_cols_gt18), as.numeric)) %>%
      dplyr::mutate(n_gt18 = rowSums(dplyr::across(dplyr::one_of(incpl_pthwy_cols_gt18)))) %>% 
      dplyr::mutate(rtt_18_emp = 1 - n_gt18/n_wl) 
  }
  if(pathway %in% c("NonAdmitted", "Admitted")) {
    
    wait_var <- glue::glue("mean_wait_{clckstp_var}")
    
    dat <- dat %>%
      dplyr::group_by(dplyr::across(glue::glue("n_clckstp_{clckstp_var}")), .add = TRUE) %>%
      tidyr::nest() %>%
      dplyr::mutate(counts = purrr::map(data, ~as.numeric(dplyr::select(.x, any_of(wait_cols)))),
                    {{wait_var}} := purrr::map_dbl(counts, ~sum(.x*w_i[seq_along(.x)]/sum(.x, na.rm = TRUE), na.rm = TRUE)))
  }
  dat <- dat %>%
    dplyr::rename_with(.cols = dplyr::matches("(?:Total )?52 plus"), .fn = ~"n_52") %>%
    dplyr::ungroup() %>%
    dplyr::select(dplyr::one_of(
      c(
        "specialty",
        "rtt_18",
        "rtt_18_emp",
        "n_wl",
        "n_52",
        "n_clckstp_nonad",
        "n_clckstp_ad",
        "n_clckstrt",
        "med_wait",
        "mean_wait_ad",
        "mean_wait_nonad",
        "Provider Name",
        "trust",
        "Treatment Function",
        "Treatment Function Code",
        "period"
      )
    )
    )
  dat
}

future::plan(future::multisession, workers = parallel::detectCores() - 2)
rtt_dat <-
  purrr::map(urls,
             .f = function(year)
               purrr::imap(
                 year,
                 .f = function(pth, name = .y)
                 furrr::future_map_dfr(pth,
                                  ~ read_rtt_trust_data(.x,
                                                        pathway = name,
                                                        provider = providers,
                                                        year_range = year_range)
                                  )
               )
  )  %>%
  purrr::map_df(purrr::reduce, dplyr::full_join) %>%
  dplyr::group_by(`Provider Name`, specialty) %>%
  tidyr::nest() %>%
  dplyr::mutate(data = purrr::map(data, ~{.x %>% dplyr::arrange(period) %>%
      dplyr::mutate(
        wl_diff = c(NA_real_, diff(n_wl)),
        rtt_52 = 1 - (n_52/n_wl),
        n_clckstp = n_clckstp_nonad + n_clckstp_ad,
        n_rn = pmax((n_clckstrt - n_clckstp - wl_diff), 0)
      )}
  )) %>%
  tidyr::unnest(data) %>%
  dplyr::ungroup()

rtt_dat %>%
  dplyr::rename(trust = `Provider Name`) %>%
  dplyr::mutate(period = lubridate::rollforward(period)) %>%
  dplyr::mutate(mean_wait = ((n_clckstp_nonad*mean_wait_nonad)+(n_clckstp_ad*mean_wait_ad))/(n_clckstp_nonad+n_clckstp_ad)) %>%
  dplyr::select(-n_clckstp_nonad,
                -n_clckstp_ad,
                -wl_diff,
                -mean_wait_nonad,
                -mean_wait_ad,
                -rtt_18_emp,
                -rtt_52,
                -n_52) %>%
  tidyr::pivot_longer(
    cols = -c(specialty, trust, period),
    names_to = "metric",
    values_to = "value"
  ) %>%
  readr::write_csv(here::here("calc-dat.csv"))

rm(list=ls())