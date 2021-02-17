

# Final figures

## Sex
```{r}
var_cohort <- df_cohort %>% select(contains("sex") | ("cohort_id") | "tot_cases_n")
var_cohort$cohort_id <- paste0(var_cohort$cohort_id, " (n = ", var_cohort$tot_cases_n,")")
sex_f <- var_cohort %>% group_by(cohort_id) %>% summarize(prct = sex_f/tot_cases_n) %>%  mutate(sex = "female")
sex_m <- var_cohort %>% group_by(cohort_id) %>% summarize(prct = sex_m/tot_cases_n) %>% mutate(sex = "male")
sex_all <- rbind(sex_f, sex_m)

p_sex_cohort <- ggplot(sex_all, aes(y = cohort_id, x = prct, fill = sex)) + 
  geom_bar(stat = "identity", position = "fill") + 
  theme_bw() + labs(x = "") +  labs(y = "") +
  scale_fill_manual(values = wes_palette("Royal1")) + theme(legend.position = "top", legend.title=element_blank())+
  rremove("y.text") 

var_controls <- df_cohort_controls %>% filter(cohort_type != "MIS-C") %>% select(contains("sex") | ("cohort_id") | "tot_cases_n")
var_controls$cohort_id <- paste0(var_controls$cohort_id, " (n = ", var_controls$tot_cases_n,")")
sex_f <- var_controls %>% group_by(cohort_id) %>% summarize(prct = sex_f/tot_cases_n) %>% mutate(sex = "female")
sex_m <- var_controls %>% group_by(cohort_id) %>% summarize(prct = sex_m/tot_cases_n) %>% mutate(sex = "male")
sex_all <- rbind(sex_f, sex_m)

p_sex_controls <- ggplot(sex_all, aes(y = cohort_id, x = prct, fill = sex)) + 
  geom_bar(stat = "identity", position = "fill") + 
  theme_bw() + labs(x = "") + 
  scale_fill_manual(values = wes_palette("Royal1"))+
  theme(legend.position = "none")  + labs(y = "")+
  rremove("y.text") 

n_single <- df_singlecases %>% nrow()
var_single <- df_singlecases %>% select(contains("sex"))
var_single$sex_m <- ifelse(var_single$sex == "M", TRUE, FALSE)
var_single$sex_f <- ifelse(var_single$sex == "F", TRUE, FALSE)
cols <- sapply(var_single, is.logical)
var_single[,cols] <- lapply(var_single[,cols], as.numeric)
var_single <- colSums(var_single %>% select(-sex), na.rm = TRUE)
var_single <- var_single/nrow(df_singlecases)*100

sex_single <- data.frame(cohort_id = paste0("single cases (n = ", n_single_cases, ")"), prct = c(var_single["sex_m"], var_single["sex_f"]), sex = c("male", "female"))

p_sex_single <- ggplot(sex_single, aes(y = cohort_id, x = prct, fill = sex)) + 
  geom_bar(stat = "identity", position = "fill") + 
  theme_bw() + 
  scale_fill_manual(values = wes_palette("Royal1"))+
  theme(legend.position = "none") + labs(y = "", x = "Fraction")+ rremove("y.text") 

plot_sex <- plot_grid(p_sex_cohort, p_sex_controls, p_sex_single, align = "v", nrow = 3, rel_heights = c(2/3, 1/5, 1/3))
plot_sex
```

## Age distribution

```{r}
cohort_age <- df_cohort_controls %>% select(contains("cohort_id") | contains("age") | contains("cohort_type")  | contains("tot_cases_n"))
cohort_age$cohort_id <- paste0(cohort_age$cohort_id, " (n = ", cohort_age$tot_cases_n,")")
cohort_age$age_med_yrs <- as.numeric(cohort_age$age_med_yrs )
cohort_age$age_Q1_yrs <- as.numeric(cohort_age$age_Q1_yrs)
cohort_age$age_Q3_yrs <- as.numeric(cohort_age$age_Q3_yrs)
cohort_age$age_min_yrs <- as.numeric(cohort_age$age_min_yrs)
cohort_age$age_max_yrs <- as.numeric(cohort_age$age_max_yrs)

cohort_age$data_descr <- ifelse(!is.na(cohort_age$age_Q1_yrs) & is.na(cohort_age$age_min_yrs) , "IQR", 
                                ifelse(is.na(cohort_age$age_Q1_yrs) & !is.na(cohort_age$age_min_yrs), "range", 
                                       ifelse(!is.na(cohort_age$age_Q1_yrs) & !is.na(cohort_age$age_min_yrs), "IQR + range", "none")))

p_age_cohort <- ggplot(cohort_age %>% filter(cohort_type == "MIS-C"), aes(y = cohort_id, x = age_med_yrs, col = data_descr)) + 
  geom_point(size = 4) + 
  geom_errorbar(aes(xmin=age_Q1_yrs, xmax=age_Q3_yrs), width=.8, position=position_dodge(.9)) +
  geom_errorbar(aes(xmin=age_min_yrs,  xmax=age_max_yrs), width=.2, position=position_dodge(.9)) +
  theme_bw() + lims(x = c(0,21)) + 
  labs(y = "", x = "", col = "bars") + theme(legend.position="top", legend.title=element_blank())+
  scale_color_manual(values = c(wes_palette("BottleRocket2")[1:3], wes_palette("BottleRocket1")[2]))

p_age_controls <- ggplot(cohort_age %>% filter(cohort_type != "MIS-C"), aes(y = cohort_id, x = age_med_yrs, col = data_descr)) + 
  geom_point(size = 4) + 
  geom_errorbar(aes(xmin=age_Q1_yrs, xmax=age_Q3_yrs), width=.2, position=position_dodge(.9)) +
  geom_errorbar(aes(xmin=age_min_yrs,  xmax=age_max_yrs), width=.2, position=position_dodge(.9)) +
  theme_bw() + lims(x = c(0,21)) +
  labs(y = "", x = "", col = "bars") + theme(legend.position="none")+
  scale_color_manual(values = wes_palette("BottleRocket2")[1])

p_age_single <- ggplot(df_singlecases, aes(x = as.numeric(age), y = paste0("single cases (n = ", n_single,")"))) +
  geom_violin(fill = wes_palette("Darjeeling2")[4]) + 
  geom_boxplot(width=.3, fill = wes_palette("Darjeeling2")[1]) + 
  theme_bw() + geom_beeswarm(groupOnX=FALSE, alpha = 0.5) + lims(x = c(0,21)) + 
  labs(y = "", x = "Age (years)")

plot_age <- plot_grid(p_age_cohort, p_age_controls, p_age_single, align = "v", nrow = 3, rel_heights = c(2/3, 1/5, 1/3))
plot_age
```


```{r, fig.height= 10, fig.width=16}
figure <- ggarrange(plot_age, plot_sex, labels = c("A", "B"), widths = c(1.25,1))
ggsave(figure, filename = "./plots/demo_grid_plots.png", height=7, width=10)
ggsave(figure, filename = "./plots/demo_grid_plots.svg", height=7, width=10)
ggsave(figure, filename = "./plots/demo_grid_plots.pdf", height=7, width=10)
figure
```


## Lab values
### C-reactive protein

```{r}
crp_collapse_cohort <- collapse_labvals_cohort(df_cohort_controls, "max", "CRP")
crp_collapse_single <- collapse_labvals_single(df_singlecases, "max", "CRP")
missing <- sum(is.na(crp_collapse_single$CRP_max))

crp_collapse_cohort$cohort_id = factor(crp_collapse_cohort$cohort_id , levels=unique(crp_collapse_cohort$cohort_id[order(crp_collapse_cohort$cohort_type)]), ordered=TRUE)


p_crp_cohort <- ggplot(crp_collapse_cohort, aes(y = cohort_id, x = CRP_med, col = cohort_type)) + 
  geom_point() +  
  geom_errorbar(aes(xmin=CRP_min, xmax=CRP_max), width=.2, position=position_dodge(.9)) + lims(x = c(0,600)) + 
  theme_bw() + labs(title = "CRP", y = "cohort", x = "") +
  geom_vline(xintercept = co_CRP, linetype = "dashed", color = "black") + theme(legend.justification = c(1, 1), legend.position = c(0.98, 0.98), legend.title=element_blank()) +
  scale_color_manual(values = wes_palette("Royal1")[c(4,2,1)])

p_crp_single <- ggplot(crp_collapse_single, aes(x = as.numeric(CRP_max), y = cohort_id)) +
  geom_violin(fill = wes_palette("Darjeeling2")[4]) + 
  geom_boxplot(width=.3, fill =  wes_palette("Darjeeling2")[1]) + 
  theme_bw() + geom_beeswarm(groupOnX=FALSE, alpha = 0.5) + lims(x = c(0,600)) + labs(y = "", x = "CRP (mg/dL)", subtitle = paste0("missing data for ", missing, " (", round(missing/n_single_cases*100, 2), "%) cases"))  +
  geom_hline(yintercept = co_CRP, linetype = "dashed", color = "black")

CRP_grid <- plot_grid(p_crp_cohort, p_crp_single, align = "v", nrow = 2, rel_heights = c(2/3, 1/3))
CRP_grid
```

### Ferritin
```{r}
ferritin_collapse_cohort <- collapse_labvals_cohort(df_cohort_controls, "max", "ferrit")
ferritin_collapse_single <- collapse_labvals_single(df_singlecases, "max", "ferrit")
missing <- sum(is.na(ferritin_collapse_single$ferrit_max))


ferritin_collapse_cohort$cohort_id = factor(ferritin_collapse_cohort$cohort_id , levels=unique(ferritin_collapse_cohort$cohort_id[order(ferritin_collapse_cohort$cohort_type)]), ordered=TRUE)


p_ferritin_cohort <- ggplot(ferritin_collapse_cohort, aes(y = cohort_id, x = ferrit_med, col = cohort_type)) + 
  geom_point() +  
  geom_errorbar(aes(xmin=ferrit_min, xmax=ferrit_max), width=.2, position=position_dodge(.9)) + lims(x = c(0,11000)) + 
  theme_bw() + labs(title = "Ferritin", y = "cohort", x = "") +
  geom_vline(xintercept = co_ferritin, linetype = "dashed", color = "black") + theme(legend.justification = c(1, 1), legend.position = "none", legend.title=element_blank()) +
  scale_color_manual(values = wes_palette("Royal1")[c(4,2,1)])

p_ferritin_single <- ggplot(ferritin_collapse_single, aes(x = as.numeric(ferrit_max), y = cohort_id)) +
  geom_violin(fill = wes_palette("Darjeeling2")[4]) + 
  geom_boxplot(width=.3, fill = wes_palette("Darjeeling2")[1]) + 
  theme_bw() + geom_beeswarm(groupOnX=FALSE, alpha = 0.5) + labs(y = "", x = "Ferritin (µg/l)", subtitle = paste0("missing data for ", missing, " (", round(missing/n_single_cases*100, 2), "%) cases")) + lims(x = c(0,11000)) +
  geom_vline(xintercept = co_ferritin, linetype = "dashed", color = "black") 

ferritin_grid <- plot_grid(p_ferritin_cohort, p_ferritin_single, align = "v", nrow = 2, rel_heights = c(2/3, 1/3))
ferritin_grid
```


### IL-6
Note: The cases from Pouletty et al are added to the single cases as they report on IL6 values. 

```{r}
IL6_collapse_cohort <- collapse_labvals_cohort(df_cohort_controls, "max", "IL6")
IL6_collapse_single <- collapse_labvals_single(df_singlecases_inclPouletty, "max", "IL6")
missing <- sum(is.na(IL6_collapse_single$IL6_max))

IL6_collapse_cohort$cohort_id = factor(IL6_collapse_cohort$cohort_id , levels=unique(IL6_collapse_cohort$cohort_id[order(IL6_collapse_cohort$cohort_type)]), ordered=TRUE)

p_IL6_cohort <- ggplot(IL6_collapse_cohort, aes(y = cohort_id, x = IL6_med, col = cohort_type)) + 
  geom_point() +  
  geom_errorbar(aes(xmin=IL6_min, xmax=IL6_max), width=.2, position=position_dodge(.9)) + lims(x = c(0,2500)) + 
  theme_bw() + labs(title = "IL6", y = "cohort", x = "")  +
  geom_vline(xintercept = co_IL6, linetype = "dashed", color = "black")  + theme(legend.justification = c(1, 1), legend.position = "none", legend.title=element_blank()) +
  scale_color_manual(values = wes_palette("Royal1")[c(4,2,1)])

p_IL6_single <- ggplot(IL6_collapse_single, aes(x = as.numeric(IL6_max), y = cohort_id)) +
  geom_violin(fill = wes_palette("Darjeeling2")[4]) + 
  geom_boxplot(width=.3, fill = wes_palette("Darjeeling2")[1]) + 
  theme_bw() + geom_beeswarm(groupOnX=FALSE, alpha = 0.5) + labs(y = "", x = "IL6 (pg/ml)", subtitle = paste0("missing data for ", missing, " (", round(missing/n_single_cases*100, 2), "%) cases"))  + lims(x = c(0,2500))  +
  geom_vline(xintercept = co_IL6, linetype = "dashed", color = "black") 

IL6_grid <- plot_grid(p_IL6_cohort, p_IL6_single, align = "v", nrow = 2, rel_heights = c(2/3, 1/3))
IL6_grid
```


### White blood cells
```{r}
wbc_collapse_cohort <- collapse_labvals_cohort(df_cohort_controls, "max", "WBC")
wbc_collapse_single <- collapse_labvals_single(df_singlecases, "max", "WBC")
missing <- sum(is.na(wbc_collapse_single$WBC_max))

wbc_collapse_cohort$cohort_id = factor(wbc_collapse_cohort$cohort_id , levels=unique(wbc_collapse_cohort$cohort_id[order(wbc_collapse_cohort$cohort_type)]), ordered=TRUE)


p_wbc_cohort <- ggplot(wbc_collapse_cohort, aes(y = cohort_id, x = WBC_med, col = cohort_type)) + 
  geom_point() +  
  geom_errorbar(aes(xmin=WBC_min, xmax=WBC_max), width=.2, position=position_dodge(.9)) + lims(x = c(0,50000)) + 
  theme_bw() + labs(title = "White blood cells", y = "", x = "")  +
  geom_vline(xintercept = co_WBC, linetype = "dashed", color = "black")  + theme(legend.justification = c(1, 1), legend.position = "none", legend.title=element_blank()) +
  scale_color_manual(values = wes_palette("Royal1")[c(4,2,1)])+ rremove("y.text") 

p_wbc_single <- ggplot(wbc_collapse_single, aes(x = as.numeric(WBC_max), y = cohort_id)) +
  geom_violin(fill = wes_palette("Darjeeling2")[4]) + 
  geom_boxplot(width=.3, fill = wes_palette("Darjeeling2")[1]) + 
  theme_bw() + geom_beeswarm(groupOnX=FALSE, alpha = 0.5) + labs(y = "", x = "WBC (/µL)", subtitle = paste0("missing data for ", missing, " (", round(missing/n_single_cases*100, 2), "%) cases"))  + lims(x = c(0,50000)) +
  geom_vline(xintercept = co_WBC, linetype = "dashed", color = "black") + rremove("y.text") 

WBC_grid <- plot_grid(p_wbc_cohort, p_wbc_single, align = "v", nrow = 2, rel_heights = c(2/3, 1/3))
WBC_grid
```

### Lymphocytes
```{r}
lympho_collapse_cohort <- collapse_labvals_cohort(df_cohort_controls, "min", "lympho")
lympho_collapse_single <- collapse_labvals_single(df_singlecases, "min", "lympho")
missing <- sum(is.na(lympho_collapse_single$lympho_min))

lympho_collapse_cohort$cohort_id = factor(lympho_collapse_cohort$cohort_id , levels=unique(lympho_collapse_cohort$cohort_id[order(lympho_collapse_cohort$cohort_type)]), ordered=TRUE)

p_lympho_cohort <- ggplot(lympho_collapse_cohort, aes(y = cohort_id, x = lympho_med, col = cohort_type)) + 
  geom_point() +  
  geom_errorbar(aes(xmin=lympho_min, xmax=lympho_max), width=.2, position=position_dodge(.9)) + 
  theme_bw() + labs(title = "Lymphocytes", y = "", x = "") + lims(x = c(0,7500))  +
  geom_vline(xintercept = co_lympho, linetype = "dashed", color = "black") + theme(legend.justification = c(1, 1), legend.position = "none", legend.title=element_blank()) +
  scale_color_manual(values = wes_palette("Royal1")[c(4,2,1)])+
  rremove("y.text") 

p_lympho_single <- ggplot(lympho_collapse_single, aes(x = as.numeric(lympho_min), y = cohort_id)) +
  geom_violin(fill = wes_palette("Darjeeling2")[4]) + 
  geom_boxplot(width=.3, fill = wes_palette("Darjeeling2")[1]) + 
  lims(x = c(0,7500))+
  theme_bw() + geom_beeswarm(groupOnX=FALSE, alpha = 0.5)  + labs(y = "", x = "Lymphocytes (/µL)", subtitle = paste0("missing data for ", missing, " (", round(missing/n_single_cases*100, 2), "%) cases"))  +
  geom_vline(xintercept = co_lympho, linetype = "dashed", color = "black") +  rremove("y.text") 

lympho_grid <- plot_grid(p_lympho_cohort, p_lympho_single, align = "v", nrow = 2, rel_heights = c(2/3, 1/3))
lympho_grid
```


### Troponin
```{r}
troponin_collapse_cohort <- collapse_labvals_cohort(df_cohort_controls, "max", "troponin")
troponin_collapse_single <- collapse_labvals_single(df_singlecases, "max", "troponin")
missing <- sum(is.na(troponin_collapse_single$troponin_max))

troponin_collapse_cohort$cohort_id = factor(troponin_collapse_cohort$cohort_id , levels=unique(troponin_collapse_cohort$cohort_id[order(troponin_collapse_cohort$cohort_type)]), ordered=TRUE)


p_troponin_cohort <- ggplot(troponin_collapse_cohort, aes(y = cohort_id, x = troponin_med, col = cohort_type)) + 
  geom_point() +  
  geom_errorbar(aes(xmin=troponin_min, xmax=troponin_max), width=.2, position=position_dodge(.9)) + lims(x = c(0,7000)) + 
  theme_bw() + labs(title = "Troponin", y = "", x = "")  +
  geom_vline(xintercept = co_tropo, linetype = "dashed", color = "black")  + theme(legend.justification = c(1, 1), legend.position = "none", legend.title=element_blank()) +
  scale_color_manual(values = wes_palette("Royal1")[c(4,2,1)])+ rremove("y.text") 

p_troponin_single <- ggplot(troponin_collapse_single, aes(x = as.numeric(troponin_max), y = cohort_id)) +
  geom_violin(fill = wes_palette("Darjeeling2")[4]) + 
  geom_boxplot(width=.3, fill = wes_palette("Darjeeling2")[1]) + 
  theme_bw() + geom_beeswarm(groupOnX=FALSE, alpha = 0.5) + labs(y = "", x = "Troponin (ng/L)", subtitle = paste0("missing data for ", missing, " (", round(missing/n_single_cases*100, 2), "%) cases"))  + lims(x = c(0,7000)) +
  geom_vline(xintercept = co_tropo, linetype = "dashed", color = "black") + rremove("y.text") 

troponin_grid <- plot_grid(p_troponin_cohort, p_troponin_single, align = "v", nrow = 2, rel_heights = c(2/3, 1/3))
troponin_grid
```


### Platelets

```{r}
collapse_cohort <- collapse_labvals_cohort(df_cohort_controls, "min", "platelet")
collapse_single <- collapse_labvals_single(df_singlecases, "min", "platelet")
missing <- sum(is.na(collapse_single$platelet_min))

collapse_cohort$cohort_id = factor(collapse_cohort$cohort_id , levels=unique(collapse_cohort$cohort_id[order(collapse_cohort$cohort_type)]), ordered=TRUE)

p_platelet_cohort <- ggplot(collapse_cohort, aes(y = cohort_id, x = platelet_med/1000, col = cohort_type)) + 
  geom_point() +  
  geom_errorbar(aes(xmin=platelet_min/1000, xmax=platelet_max/1000, col=cohort_type), width=.2, position=position_dodge(.9)) + lims(x = c(0,750)) + 
  theme_bw() + labs(title = "Platelets", y = "", x = "")  +
  geom_vline(xintercept = co_platelet/1000, linetype = "dashed", color = "black")  + theme(legend.justification = c(1, 1), legend.position = "none", legend.title=element_blank()) +
  scale_color_manual(values = wes_palette("Royal1")[c(4,2,1)])+ rremove("y.text") 

p_platelet_single <- ggplot(collapse_single, aes(x = as.numeric(platelet_min)/1000, y = cohort_id)) +
  geom_violin(fill = wes_palette("Darjeeling2")[4]) + 
  geom_boxplot(width=.3, fill = wes_palette("Darjeeling2")[1]) + 
  theme_bw() + geom_beeswarm(groupOnX=FALSE, alpha = 0.5) + labs(y = "", x = "Platelets (x1000/µL)", subtitle = paste0("missing data for ", missing, " (", round(missing/n_single_cases*100, 2), "%) cases"))   + lims(x = c(0,750)) +
  geom_vline(xintercept = co_platelet, linetype = "dashed", color = "black") + rremove("y.text") 

platelet_grid <- plot_grid(p_platelet_cohort, p_platelet_single, align = "v", nrow = 2, rel_heights = c(2/3, 1/3))
platelet_grid
```


### D-dimers

```{r}
collapse_cohort <- collapse_labvals_cohort(df_cohort_controls, "max", "Ddim")
collapse_single <- collapse_labvals_single(df_singlecases, "max", "Ddim")
missing <- sum(is.na(collapse_single$Ddim_max))

collapse_cohort$cohort_id = factor(collapse_cohort$cohort_id , levels=unique(collapse_cohort$cohort_id[order(collapse_cohort$cohort_type)]), ordered=TRUE)

p_Ddim_cohort <- ggplot(collapse_cohort, aes(y = cohort_id, x = Ddim_med, col = cohort_type)) + 
  geom_point() +  
  geom_errorbar(aes(xmin=Ddim_min, xmax=Ddim_max, col=cohort_type), width=.2, position=position_dodge(.9)) + lims(x = c(0,11000)) + 
  theme_bw() + labs(title = "D-dimers", y = "", x = "")  +
  geom_vline(xintercept = co_Ddim, linetype = "dashed", color = "black")  + theme(legend.justification = c(1, 1), legend.position = "none", legend.title=element_blank()) +
  scale_color_manual(values = wes_palette("Royal1")[c(4,2,1)])+ rremove("y.text") 

p_Ddim_single <- ggplot(collapse_single, aes(x = as.numeric(Ddim_max), y = cohort_id)) +
  geom_violin(fill = wes_palette("Darjeeling2")[4]) + 
  geom_boxplot(width=.3, fill = wes_palette("Darjeeling2")[1]) + 
  theme_bw() + geom_beeswarm(groupOnX=FALSE, alpha = 0.5) + labs(y = "", x = "D-dimers (ng/ml)", subtitle = paste0("missing data for ", missing, " (", round(missing/n_single_cases*100, 2), "%) cases")) + lims(x = c(0,11000)) +
  geom_vline(xintercept = co_Ddim, linetype = "dashed", color = "black") + rremove("y.text") 

Ddim_grid <- plot_grid(p_Ddim_cohort, p_Ddim_single, align = "v", nrow = 2, rel_heights = c(2/3, 1/3))
Ddim_grid
```


### Sodium

```{r}
collapse_cohort <- collapse_labvals_cohort(df_cohort_controls, "min", "sodium")
collapse_single <- collapse_labvals_single(df_singlecases, "min", "sodium")
missing <- sum(is.na(collapse_single$sodium_min))

collapse_cohort$cohort_id = factor(collapse_cohort$cohort_id , levels=unique(collapse_cohort$cohort_id[order(collapse_cohort$cohort_type)]), ordered=TRUE)

p_sodium_cohort <- ggplot(collapse_cohort, aes(y = cohort_id, x = sodium_med, col = cohort_type)) + 
  geom_point() +  
  geom_errorbar(aes(xmin=sodium_min, xmax=sodium_max, col=cohort_type), width=.2, position=position_dodge(.9)) + lims(x = c(100,150)) + 
  theme_bw() + labs(title = "Sodium", y = "", x = "")  +
  geom_vline(xintercept = co_sodium, linetype = "dashed", color = "black")  + theme(legend.justification = c(1, 1), legend.position = "none", legend.title=element_blank()) +
  scale_color_manual(values = wes_palette("Royal1")[c(4,2,1)])+ rremove("y.text") 

p_sodium_single <- ggplot(collapse_single, aes(x = as.numeric(sodium_min), y = cohort_id)) +
  geom_violin(fill = wes_palette("Darjeeling2")[4]) + 
  geom_boxplot(width=.3, fill = wes_palette("Darjeeling2")[1]) + 
  theme_bw() + geom_beeswarm(groupOnX=FALSE, alpha = 0.5) + labs(y = "", col = "", x = "Sodium (mmol/L)", subtitle = paste0("missing data for ", missing, " (", round(missing/n_single_cases*100, 2), "%) cases"))  + lims(x = c(100,150)) +
  geom_vline(xintercept = co_sodium, linetype = "dashed", color = "black") + rremove("y.text") 

sodium_grid <- plot_grid(p_sodium_cohort, p_sodium_single, align = "v", nrow = 2, rel_heights = c(2/3, 1/3))
sodium_grid
```


### Grid plots

Dashed lines equals reference value cut-off.

```{r, fig.height= 16, fig.width=12}
figure <- ggarrange(CRP_grid, WBC_grid, sodium_grid, ferritin_grid, lympho_grid, Ddim_grid, IL6_grid, platelet_grid, troponin_grid, labels = c("A", "B", "C", "D", "E", "G", "G", "H", "I"), widths = c(1.25,1,1))
ggsave(figure, filename = "./plots/lab_grid_plots.png", dpi = 300, height=16, width=12)
ggsave(figure, filename = "./plots/lab_grid_plots.svg", dpi = 300, height=16, width=12)
ggsave(figure, filename = "./plots/lab_grid_plots.pdf", dpi = 300, height=16, width=12)
figure
```

