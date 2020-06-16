library(dplyr)

dat_t0 <- readxl::read_excel(
  "data-raw/NZ ethnicity standard classification concordance.xlsx"
  , sheet = 1, skip = 9)

## ETHNIC05 V1.0.0
dat_t1_v1 <- dat_t0 %>%
  mutate(Level = nchar(`Source Code`)) %>%
  distinct(Level, Code = `Source Code`, Ethnicity = Descriptor...2)

ethnic05_v1 <- dat_t1_v1[dat_t1_v1$Level == 1,] %>%
  select(l1_code = Code, l1_label = Ethnicity) %>%
  
  left_join(
    
    dat_t1_v1[dat_t1_v1$Level == 2,] %>%
      mutate(l1_code = substr(Code, 1, 1)) %>%
      select(l1_code, l2_code = Code, l2_label = Ethnicity) %>%
      
      left_join(
        
        dat_t1_v1[dat_t1_v1$Level == 3,] %>%
          mutate(l2_code = substr(Code, 1, 2)) %>%
          select(l2_code, l3_code = Code, l3_label = Ethnicity) %>%
          
          left_join(
            
            dat_t1_v1[dat_t1_v1$Level == 5,] %>%
              mutate(l3_code = substr(Code, 1, 3)) %>%
              select(l3_code, l4_code = Code, l4_label = Ethnicity)
            
          )
        
      )
    
  ) %>%
  mutate_at(vars(ends_with("code")), as.integer) %>%
  mutate_if(is.character, forcats::fct_inorder) %>%
  distinct()


## ETHNIC05 V2.0.0
dat_t1_v2 <- dat_t0 %>%
  mutate(Level = nchar(`Target Code`)) %>%
  distinct(Level, Code = `Target Code`, Ethnicity = Descriptor...5)

ethnic05_v2 <- dat_t1_v2[dat_t1_v2$Level == 1,] %>%
  select(l1_code = Code, l1_label = Ethnicity) %>%

  left_join(

    dat_t1_v2[dat_t1_v2$Level == 2,] %>%
      mutate(l1_code = substr(Code, 1, 1)) %>%
      select(l1_code, l2_code = Code, l2_label = Ethnicity) %>%

      left_join(

        dat_t1_v2[dat_t1_v2$Level == 3,] %>%
          mutate(l2_code = substr(Code, 1, 2)) %>%
          select(l2_code, l3_code = Code, l3_label = Ethnicity) %>%

          left_join(

            dat_t1_v2[dat_t1_v2$Level == 5,] %>%
              mutate(l3_code = substr(Code, 1, 3)) %>%
              select(l3_code, l4_code = Code, l4_label = Ethnicity)

          )

      )

  ) %>%
  mutate_at(vars(ends_with("code")), as.integer) %>%
  mutate_if(is.character, forcats::fct_inorder) %>%
  distinct()


dat_kiwi <- ethnic05_v2[ethnic05_v2$l4_label == "New Zealander",]
dat_kiwi$l4_label <- "Kiwi"

ethnic05 <- list(
  v1 = rbind(ethnic05_v1, dat_kiwi),
  v2 = rbind(ethnic05_v2, dat_kiwi)
)


# save(ethnic05, file = "R/sysdata.Rda")

usethis::use_data(ethnic05, internal = TRUE, overwrite = TRUE)


