CDC_fulfilled_crit2 <- apply(df_singlecases, 1, function(row) {
  pat_id <- row["patientID_int"]
  
  # fever?
  fever <- row["symp_fever"] == TRUE | row["kawasaki_fever"] == TRUE
  
  inflamm <- any(fever)
  
  # lab values evidence for inflammation
  neutrophilia <- as.numeric(row["lab_neutrophils"]) > co_neutrophilia
  elevated_CRP <- (as.numeric(row["lab_CRP_admis"]) > co_CRP | as.numeric(row["lab_CRP_NS"]) > co_CRP | as.numeric(row["lab_CRP_peak"]) > co_CRP )
  lymphopenia <- as.numeric(row["lab_lymphocytes_lowest"]) < co_lympho
  fibrinogen <- as.numeric(row["lab_fibrino"]) > co_fibrino
  Ddimers <- as.numeric(row["lab_Ddim_peak"]) > co_Ddim |  as.numeric(row["lab_Ddim_NS"]) > co_Ddim
  ferritin <- (as.numeric(row["lab_ferritin_NS"]) > co_ferritin | as.numeric(row["lab_ferritin_admis"]) > co_ferritin | as.numeric(row["lab_ferritin_peak"]) > co_ferritin)
  albumin <- as.numeric(row["lab_albumin_admis"]) < co_albu | as.numeric(row["lab_albumin_lowest"]) < co_albu | as.numeric(row["lab_albumin_NS"]) < co_albu
  PCT <- as.numeric(row["lab_PCT_admis"]) > co_PCT | as.numeric(row["lab_PCT_peak"]) > co_PCT | as.numeric(row["lab_PCT_NS"]) > co_PCT 
  LDH <- as.numeric(row["lab_LDH"]) > co_LDH
  IL6 <- as.numeric(row["lab_IL6"]) > co_IL6
  ESR <- as.numeric(row["lab_ESR"]) > co_ESR
  
  lab_vals <- any(neutrophilia, elevated_CRP, lymphopenia, fibrinogen, Ddimers, ferritin, albumin, PCT, LDH, IL6, ESR)
  
  # Ilness requiring hospitalisation
  ## used surrogate parameters for hosp
  hosp_ICU <- row["admis_hosp_days"] > 1 | row["admis_ICU_days"] > 1 | row["admis_PICU_admis"] == TRUE
  NIV <- row["critcare_NIV"] == TRUE | row["critcare_NIV_days"] > 1
  MV <- row["critcare_MV"] == TRUE | row["critcare_MV_days"] > 1
  inotrop <- row["critcare_inotrop"] == TRUE | row["critcare_inotrop_days"] > 1
  ECMO <- row["critcare_ECMO"] == TRUE 
  IVIg <- row["rx_IVIg_once"] == TRUE  |  row["rx_IVIg_multip"] == TRUE 
  biologicals <- row["rx_anakinra"] == TRUE | row["rx_tocilizumab"] == TRUE | row["rx_infliximab"] == TRUE | row["rx_antibiotics"] == TRUE | row["rx_plasma"] == TRUE | row["rx_remdesivir"] == TRUE 
  heparin <- row["rx_heparin"] == TRUE
  
  
  req_hosp <- any(hosp_ICU, NIV, MV, inotrop, ECMO, IVIg, biologicals, heparin)
  
  ## multisystem involvement >= 2
  ## respiratory
  pneumonia <- row["symp_resp_pneumonia"] == TRUE
  resp_failure <- row["symp_resp_failure"] == TRUE
  resp <- any(pneumonia, resp_failure)
  
  AKI <- row["symp_renal_AKI"] == TRUE
  RRT <- row["critcare_RRT"] == TRUE
  renal <- any(AKI, RRT)
  
  myocarditis <- row["symp_cardiovasc_myocard"] == TRUE
  pericarditis <- row["symp_cardiovasc_pericard"] == TRUE
  LVEF_under30 <- row["symp_cardiovasc_LV_less30"] == TRUE
  LVEF_30to55 <- row["symp_cardiovasc_LV_30to55"] == TRUE
  BNP <- (as.numeric(row["lab_BNP_admis"]) > co_BNP | as.numeric(row["lab_BNP_max"]) > co_BNP ) 
  NTproBNP <- as.numeric(row["lab_NTproBNP"]) > co_NTproBNP
  tropo <- as.numeric(row["lab_troponin_admis"]) > co_tropo
  shock <- row["symp_cardiovasc_shock"] == TRUE
  
  cardiovasc <- any(myocarditis, LVEF_under30, LVEF_30to55, NTproBNP, BNP, tropo, shock)
  
  rash <- row["kawasaki_exanthema"] == TRUE
  dermato <- any(rash)
  
  organ_dysfunc <- sum(resp, renal, cardiovasc, dermato, na.rm = TRUE) >= 2
  

  #criteria_fulfilled <- sum(criteria1, criteria2, criteria3, criteria4, na.rm = TRUE) == 4
  #return(c(pat_id, "criteria2_inflamm" = inflamm, "criteria2_labvals" = lab_vals, "criteria2_req_hosp" = req_hosp, "criteria2_organ_dysfunc" = organ_dysfunc))
  return(c(pat_id, "criteria2_resp" = resp, "criteria2_renal" = renal, "criteria2_cardiovasc" = cardiovasc, "criteria2_dermato" = dermato, crit2_fulfilled = organ_dysfunc))
  
  })
CDC_fulfilled_crit2 <- CDC_fulfilled_crit2
CDC_fulfilled_crit2 <- CDC_fulfilled_crit2 %>% t() %>% as_tibble()
CDC_fulfilled_crit2 <- type_convert(CDC_fulfilled_crit2)
CDC_fulfilled_crit2_heatmap <- CDC_fulfilled_crit2
cols <- sapply(CDC_fulfilled_crit2_heatmap, is.logical)
CDC_fulfilled_crit2_heatmap[,cols] <- lapply(CDC_fulfilled_crit2_heatmap[,cols], as.numeric)
CDC_fulfilled_crit2_heatmap_melt <- CDC_fulfilled_crit2_heatmap %>% melt()
CDC_fulfilled_crit2_heatmap_melt[is.na(CDC_fulfilled_crit2_heatmap_melt)] <- 2
CDC_fulfilled_crit2_heatmap_melt$criteria <- "CDC criteria 2"
skim(CDC_fulfilled_crit2)


ggplot(CDC_fulfilled_crit2_heatmap_melt, aes(x = variable, y = as.character(patientID_int), fill = as.factor(value))) + geom_tile() + 
  theme_classic() + theme(axis.line=element_blank()) + 
  labs(y = "Patient ID", x = "criteria", fill = "criteria met", title = "Overview of which single cases fulfill criterium 2 of the CDC case definition") +  
  scale_fill_manual(labels = c("No", "Yes", "Missing"), values = wes_palette("Zissou1")) + 
  theme(axis.text.x=element_text(angle=90, hjust=1)) + facet_wrap(~ criteria, scales = "free_x")


WHO_fulfilled_crit3 <- apply(df_singlecases, 1, function(row) {
  pat_id <- row["patientID_int"]


  # criteria 3: clinical signs of multisystem involvement (at least 2)
  ## Rash, bilateral nonpurulent conjunctivitis, or mucocutaneous inflammation signs (oral, hands, or feet)
  rash <- row["kawasaki_exanthema"] == TRUE
  conjunctivitis <- row["kawasaki_conjunctivitis"] == TRUE
  mucocutaneaous <- row["kawasaki_mouth"] == TRUE | row["kawasaki_extremity"] == TRUE
  
  criteria3_a <- any(rash, conjunctivitis, mucocutaneaous)
  
  ## hypotension or shock
  shock <- row["symp_cardiovasc_shock"] == TRUE
  criteria3_b <- any(shock)
  
  ## cardiac dysfunction
  myocarditis <- row["symp_cardiovasc_myocard"] == TRUE
  pericarditis <- row["symp_cardiovasc_pericard"] == TRUE
  LVEF_under30 <- row["symp_cardiovasc_LV_less30"] == TRUE
  LVEF_30to55 <- row["symp_cardiovasc_LV_30to55"] == TRUE
  BNP <- (as.numeric(row["lab_BNP_admis"]) > co_BNP | as.numeric(row["lab_BNP_max"]) > co_BNP ) 
  NTproBNP <- as.numeric(row["lab_NTproBNP"]) > co_NTproBNP
  tropo <- as.numeric(row["lab_troponin_admis"]) > co_tropo
  coronary <- row["symp_cardiovasc_cordilat"] == TRUE | row["symp_cardiovasc_aneurysm"] == TRUE
  
  criteria3_c <- any(myocarditis, LVEF_under30, LVEF_30to55, NTproBNP, BNP, tropo, coronary)
  
  ## coagulopathy
  fibrinogen <- as.numeric(row["lab_fibrino"]) > co_fibrino
  Ddimers <- as.numeric(row["lab_Ddim_peak"]) > co_Ddim |  as.numeric(row["lab_Ddim_NS"]) > co_Ddim
  
  criteria3_d <- any(fibrinogen, Ddimers)
  
  ## acute GI symptoms
  GIsymp <- row["symp_GI_NS"] == TRUE | row["symp_GI_abdopain"] == TRUE | row["symp_GI_vomiting"] == TRUE | row["symp_GI_diarrh"] == TRUE | row["symp_GI_colitis"] == TRUE 
  
  criteria3_e <- any(GIsymp)
  
  criteria3 <- sum(criteria3_a, criteria3_b, criteria3_c, criteria3_d, criteria3_e, na.rm = TRUE) >= 2

  
  return(c(pat_id, "crit3_rashconjunc" = criteria3_a, "crit3_shock" = criteria3_b, "criteria3_cardiodysfunc" = criteria3_c, "criteria3_coagulopathy" = criteria3_d, "criteria3_GIsymp" = criteria3_e, "crit3_fulfilled" = criteria3))
})


WHO_fulfilled_crit3 <- WHO_fulfilled_crit3 %>% t() %>% as_tibble()
WHO_fulfilled_crit3 <- type_convert(WHO_fulfilled_crit3)
WHO_fulfilled_crit3_heatmap <- WHO_fulfilled_crit3
cols <- sapply(WHO_fulfilled_crit3_heatmap, is.logical)
WHO_fulfilled_crit3_heatmap[,cols] <- lapply(WHO_fulfilled_crit3_heatmap[,cols], as.numeric)
WHO_fulfilled_crit3_heatmap_melt <- WHO_fulfilled_crit3_heatmap %>% melt()
WHO_fulfilled_crit3_heatmap_melt[is.na(WHO_fulfilled_crit3_heatmap_melt)] <- 2
WHO_fulfilled_crit3_heatmap_melt$criteria <- "WHO criteria 3"
skim(WHO_fulfilled_crit3)

ggplot(WHO_fulfilled_crit3_heatmap_melt, aes(x = variable, y = as.character(patientID_int), fill = as.factor(value))) + geom_tile() + 
  theme_classic() + theme(axis.line=element_blank()) + 
  labs(y = "Patient ID", x = "criteria", fill = "criteria met", title = "Overview of which single cases fulfill criterium 3 of the WHO case definition") +  
  scale_fill_manual(labels = c("No", "Yes", "Missing"), values = wes_palette("Zissou1")) + 
  theme(axis.text.x=element_text(angle=90, hjust=1)) + facet_wrap(~ criteria, scales = "free_x")


full_heatmap_organdysfunc <- rbind(CDC_fulfilled_crit2_heatmap_melt, WHO_fulfilled_crit3_heatmap_melt)
ggplot(full_heatmap_organdysfunc, aes(x = variable, y = as.character(patientID_int), fill = as.factor(value))) + geom_tile() + 
  theme_classic() + theme(axis.line=element_blank()) + 
  labs(y = "Patient ID", x = "criteria", fill = "criteria met", title = "Overview of which single cases fulfill organ dysfunction criterium") +  
  scale_fill_manual(labels = c("No", "Yes", "Missing"), values = wes_palette("Zissou1")) + 
  theme(axis.text.x=element_text(angle=90, hjust=1)) + facet_wrap(~ criteria, scales = "free_x")
