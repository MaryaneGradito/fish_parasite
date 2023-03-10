---
format:
 docx: 
    reference-doc: "./bib/template.docx"
execute: 
  echo: false
  error: false
  cache: true
  warning: false
crossref: 
  fig-title: Fig.     # (default is "Figure")
  tbl-title: Table     # (default is "Table")
  title-delim: —     # (default is ":")
  fig-prefix: Fig.   # (default is "Figure")
  tbl-prefix: Tab.    # (default is "Table")
editor_options: 
  chunk_output_type: console
---

# Results

```{r}
#| label: packages
#| output: false
#| warning: false
    source("./R/func.R")
    pacman::p_load(tidyverse, brms, flextable)
```

```{r}
#| label: load_data
#| output: false
#| 

############################
# Import processed data
#############################
  all_data <- read.table("./output/all_data_p.csv",header=T, sep=",")
  all_data
  
############################
# Parasites count
#############################
  
  
  The caging experiment successfully infected our treatment fish. Control fish that stayed in the laboratory had no alive parasite, which indicate that the praziquantel treatment was effective. The two most abundant species found in the experimentally infected fish were trematodes causing the blackspot disease (Trematoda: *Apophallus sp.* and *Uvulifer sp.*; min-max: `r min_BS`- `r max_BS`; median = 9) and the bass tapeworm (Cestoda: *Proteocephalus ambloplites*; min-max: 0-36; median = 4). The most abundant species of trematode causing blackspots was *Apophallus sp*. (Binning, Lanthier, unpublished data), but *Uvulifer sp.* was found more frequently inside the muscles (MG, personal observations).Experimentally fish gained in mean 8 blackspots, and were found on the fines, body, gills and inside the muscles. Bass tapeworm were mostly found in the liver, stomach and digestive tract, occasionally around the spleen (parasite count: 9), and rarely on the gills (parasite count: 3) or the heart (parasite count: 2). Unknown nematode species were found rarely in the body cavity (alive parasite count: 4).

############################
# Load model
############################

  model1 <- readRDS(file = "./output/models/model1.rds")

# Calculate repeatability
    post_sd <- as_draws_df(model1, variable = "^sd", regex = TRUE)
    post_sd_C <- post_sd[,grepl("C", colnames(post_sd))]
    post_sd_E <- post_sd[,grepl("E", colnames(post_sd))]
    post_sd_cage <- post_sd[,grepl("cage", colnames(post_sd))]
    post_sd_sig <- as_draws_df(model1, variable = "^sigma", regex = TRUE)
    
# Repeatability for the traits across treatment

       R_boldness <- repeatability(post_sd_C, post_sd_E, post_sd_cage, post_sd_sig, trait = "logboldness")
       R_Blb <- round(quantile(R_boldness[,1], c(0.025, 0.975))[1], 2)
       R_Bub <- round(quantile(R_boldness[,1], c(0.025, 0.975))[2], 2)
       R_explore <- repeatability(post_sd_C, post_sd_E, post_sd_cage, post_sd_sig, trait = "exploration")
       R_activity <- repeatability(post_sd_C, post_sd_E, post_sd_cage, post_sd_sig, trait = "logactivity")
      
       #I don't remember what this is ? 
      R_contrast <- R_explore - R_boldness
```

The repeatbility of boldness was `r round(mean(R_boldness[,1]), 2)` (95% CI: `r R_Blb` to `r R_Bub`) (@fig-fig1). See @tbl-tab1

#### *Boldness*

```{r}
#| label: fig-fig1
#| fig-cap: Repeatbility of behavioural traits
#| 
hist(R_boldness[,1])
```



```{r}
#| label: tbl-tab1
#| tbl-cap: Repeatbility of traits



tab1 <- data.frame(Repeatability = c(mean(R_boldness[,1]), mean(R_explore[,1]), mean(R_activity[,1])),
                    `95% CI` = c(round(quantile(R_boldness[,1], c(0.025, 0.975))[1], 2), round(quantile(R_explore[,1], c(0.025, 0.975))[1], 2), round(quantile(R_activity[,1], c(0.025, 0.975))[1], 2)), check.names = FALSE)

flextable(tab1) %>% flextable::bold(part = "header") %>% flextable::autofit()

```



