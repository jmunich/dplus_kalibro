library(qgraph)
library(tidyverse)
library(psych)
library(lavaan)

if(.Platform$OS.type == "windows"){
  Sys.setlocale(category = "LC_ALL", "English_United States.1250")}else{
  Sys.setlocale(category = "LC_ALL", "en_US.UTF-8")
}

data_raw <- readxl::read_xlsx("data/kalibro_data.xlsx")

names_new <- c(
  "i_time",
  "i_school",
  "i_region",
  "i_town_size",
  "i_school_org",
  "d_sex",
  "d_age",
  "q_level",
  "q_teacher",
  "q_practice_time",
  "q_subject_history",
  "q_subject_other",
  paste0("A_relevance_", 1:8),
  paste0("B_material_", 1:8),
  "C_a_conditions",
  "t_conditions",
  paste0("D_training_form_", 1:7),
  paste0("E_training_content_", 1:14),
  "F_training_time_",
  paste0("G_attitudes_", 1:10),
  "t_HL",
  "H_time_admin",
  "H_time_order",
  "H_time_teach",
  paste0("I_situations_", 1:7),
  paste0("I_assesment_", 1:6),
  "J_material_1",
  "J_material_2",
  "t_material_3",
  paste0("K_change_", 1:4),
  "L_new",
  "t_enjoy",
  "t_proud")

names_old <- names(data_raw)
names(names_old) <- names_new


data_use <- data_raw %>%
  `names<-`(names_new)

data_use %>%
  select(starts_with("A", ignore.case = FALSE)) %>%
  apply(MARGIN = 2, FUN = table)

vals_A <- data_use %>%
  select(starts_with("A", ignore.case = FALSE)) %>%
  unlist() %>%
  unique() %>%
  .[c(5,2,1,3)]

vals_B <- data_use %>%
  select(starts_with("B", ignore.case = FALSE)) %>%
  unlist() %>%
  unique() %>%
  .[c(5,1,6,3,2)]

vals_C <- data_use %>%
  select(starts_with("C", ignore.case = FALSE)) %>%
  unlist() %>%
  unique() %>%
  .[c(4,2,1,3)]

vals_D <- data_use %>%
  select(starts_with("D", ignore.case = FALSE)) %>%
  unlist() %>%
  unique() %>%
  .[1] %>%
  c("ne", .)

vals_E <- data_use %>%
  select(starts_with("E", ignore.case = FALSE)) %>%
  unlist() %>%
  unique() %>%
  .[2] %>%
  c("ne", .)

vals_F <- data_use %>%
  select(starts_with("F", ignore.case = FALSE)) %>%
  unlist() %>%
  unique() %>%
  .[-4]

vals_G <- data_use %>%
  select(starts_with("G", ignore.case = FALSE)) %>%
  unlist() %>%
  unique() %>%
  .[c(4,1,3,2)]

vals_I <- data_use %>%
  select(starts_with("I", ignore.case = FALSE)) %>%
  unlist() %>%
  unique() %>%
  .[c(4,3,1,2)]

vals_K <- data_use %>%
  select(starts_with("K", ignore.case = FALSE)) %>%
  unlist() %>%
  unique() %>%
  .[c(4,3,2,1)]

vals_L <- data_use %>%
  select(starts_with("L", ignore.case = FALSE)) %>%
  unlist() %>%
  unique() %>%
  .[c(4,3,1,2)]

data_recode <- data_use %>%
  mutate_at(vars(starts_with("A", ignore.case = FALSE)),
            function(x){ordered(x, vals_A)}) %>%
  mutate_at(vars(starts_with("B", ignore.case = FALSE)),
            function(x){ordered(x, vals_B)}) %>%
  mutate_at(vars(starts_with("C", ignore.case = FALSE)),
            function(x){ordered(x, vals_C)}) %>%
  mutate_at(vars(starts_with("D", ignore.case = FALSE)),
            function(x){
              ifelse(is.na(x), "ne", x) %>%
                ordered(vals_D)}) %>%
  mutate_at(vars(starts_with("E", ignore.case = FALSE)),
            function(x){
              ifelse(is.na(x), "ne", x) %>%
                ordered(vals_E)}) %>%
  mutate_at(vars(starts_with("F", ignore.case = FALSE)),
            function(x){ordered(x, vals_F)}) %>%
  mutate_at(vars(starts_with("G", ignore.case = FALSE)),
            function(x){ordered(x, vals_G)}) %>%
  mutate_at(vars(starts_with("I", ignore.case = FALSE)),
            function(x){ordered(x, vals_I)}) %>%
  mutate_at(vars(starts_with("K", ignore.case = FALSE)),
            function(x){ordered(x, vals_K)}) %>%
  mutate_at(vars(starts_with("L", ignore.case = FALSE)),
            function(x){ordered(x, vals_L)}) %>%
  mutate(H_time_admin = H_time_admin %>%
           as.numeric() %>%
           cut(c(0,10,100)) %>%
           ordered(),
         H_time_order = data_use$H_time_order %>%
           parse_number() %>%
           as.numeric() %>%
           cut(c(0,1,5,10,100)) %>%
           ordered(),
         H_time_teach = H_time_teach %>%
           as.numeric() %>%
           cut(c(0,70,80,90,100)) %>%
           ordered()
  ) %>%
  mutate(
    J_material_1 = (grepl("emocion", J_material_1) +
                      grepl("lu pro ot", J_material_1) +
                      grepl("provokativnost", J_material_1)) %>%
      ordered(),
    J_material_2 = (grepl("emocion", J_material_2) +
                      grepl("Havel si po", J_material_2) +
                      grepl("ci VB ml", J_material_2)) %>%
      ordered()
  ) %>%
  mutate_at(c("G_attitudes_3","G_attitudes_4","G_attitudes_6"), function(x){ordered(-as.numeric(x))})

var_groups <- names_old %>%
  gsub(" \\[.+\\]", "", .)

varsel <- names(data_recode) %>%
  grepl("A|B|G|I|J", .)

data_mod <- data_recode[,varsel] %>%
  mutate_all(ordered)


data_selection <- data_recode %>%
  select(starts_with("A", ignore.case = FALSE),
         starts_with("B", ignore.case = FALSE),
         starts_with("G", ignore.case = FALSE),
         starts_with("I_sit", ignore.case = FALSE))

data_use <- data_selection
