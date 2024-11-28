## Test HESPER script to read, compose, analyse and visualise HESPER data, using DRC MSNA data as example 

rm(list=ls())
require(pacman)
p_load(readxl, writexl, tidyverse, srvyr, purrr, rio, data.table, assertr)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

### 0. Source needed functions  -------------------------------------------------------------
## list all scripts in R
map(list.files(pattern = ".R$", full.names = TRUE) %>% str_subset("hesper analysis", T), source) %>% invisible

### 1. Read data, survey, labels and define column names, choices and define parameters  ----

## read data
path.data <- "C:/Users/raphael.bacot/ACTED/IMPACT HQ-Accountability & Inclusion - Documents/General/12_Accountability to Affected People Specialist/07_Hesper Scale/4. REACH HESPER Pilots/DRC MSNA/Data/REACH_DRC2404_MSNA2024_Clean-Data.xlsx"
sheet.data <- "hh data"
data <- read_excel(path.data, sheet=sheet.data, guess_max=10000)

## convert as numeric if unique values are in "0" "1" and "NA" NA
data <- data %>% 
  mutate(across(matches("\\."), as.numeric)) %>%
  mutate(across(where(is.character), ~ifelse(. %in% c("0", "1", "NA"), as.numeric(.), .))) %>% 
  mutate(weights=as.numeric(weights))
# str(data)

## read tool and key hesper table
path.tool <- "../resources/REACH_DRC2404_MSNA2024_tool.xlsx"
path.hesper.key <- "../resources/country_hesper_key.xlsx"

survey <- read_excel(path.tool, "survey") %>% mutate(row=row_number()) %>% separate(type, into = c("q.type", "list_name"), sep = " ", remove = F)
choices <- read_excel(path.tool, "choices") %>% rename("label::french"=any_of("label"))
col_label <- "label::french"

## Extract variables names
so.questions <- survey %>% filter(q.type=="select_one") %>% pull(name) %>% keep(. %in% colnames(data)) %>% str_subset("hesper")
sm.questions <- survey %>% filter(q.type=="select_multiple") %>% pull(name) %>% keep(. %in% colnames(data)) %>% str_subset("hesper")

key_hesper <- read_excel(path.hesper.key, "key")
ind_lab_hesper <- read_excel(path.hesper.key, "lab")

## combine survey and choices
survey_combined <- survey %>%
  filter(q.type %in% c("select_one", "select_multiple")) %>% 
  right_join(choices %>% select(list_name, choice_name=name, choice_label=!!sym(col_label))) %>%
  bind_rows(survey %>% filter(!str_detect(type, "^select"))) %>% arrange(row) %>% select(-row) %>% filter(!is.na(name))

## extract colnames from key hesper 
## to be updated if there are more/less subset questions in the hesper tool
col.hesp <- key_hesper %>% pull(name)
col.men <- key_hesper %>% filter(subset=="men") %>% pull(name)
col.women <- key_hesper %>% filter(subset=="women") %>% pull(name)
col.displaced <- key_hesper %>% filter(subset=="displaced") %>% pull(name)
col.host <- key_hesper %>% filter(subset=="host") %>% pull(name)
col.subset <- c(col.men, col.women, col.host, col.displaced)

## define name pattern in kobo tool associated with the hesper priority needs question
pattern.prio <- "hesper_priority_"
col.prio <- survey %>% filter(str_detect(name, pattern.prio)) %>% pull(name) ## to be updated depending on name of priority questions in tool

## Add as well choice list for "hesper_top_three_priorities" to have label
survey_combined <- survey_combined %>% 
  bind_rows(survey_combined %>% filter(name==col.prio[1]) %>% 
              mutate(name="hesper_top_three_priorities",
                     # label="What are the top three priorities from the selected HESPER serious problems?"
                     label="Quels sont les trois priorités les plus importantes parmi les problèmes graves HESPER sélectionnés?"))

## extract the hesper items choices from the key_hesper 
## to be updated if there are more/less subset questions in the hesper tool
val.hesper <-  key_hesper %>% pull(choice_name)
val.hesper.men <- key_hesper %>% filter(subset == "men") %>% pull(choice_name)
val.hesper.women <- key_hesper %>% filter(subset == "women") %>% pull(choice_name)
val.hesper.displaced <- key_hesper %>% filter(subset == "displaced") %>% pull(choice_name)
val.hesper.host <- key_hesper %>% filter(subset == "host") %>% pull(choice_name)

## extract categories' names from hesper.key xls // categorize hesper items into cat 
## four different grouping of items explored
section_wide <- key_hesper %>% select(section_wide=`Section Wide`, name) %>% split(.$section_wide) %>% map(.,~pull(.,name)) ### Section wide
section <- key_hesper %>% select(section=`Section`, name) %>% split(.$section) %>% map(.,~pull(.,name)) ### Section 
area_wellbeing <- key_hesper %>% select(area_wellbeing=`Area of well-being`, name) %>% split(.$area_wellbeing) %>% map(.,~pull(.,name)) ### Area of well-being
sector <- key_hesper %>% select(sector_theme=`Sector/Theme`, name) %>% split(.$sector_theme) %>% map(.,~pull(.,name)) ### Sector/Theme

## for priority items
section_wide_priority <- key_hesper %>% select(section_wide=`Section Wide`, name_priority) %>% split(.$section_wide) %>% map(.,~pull(.,name_priority)) ### Section wide
section_priority <- key_hesper %>% select(section=`Section`, name_priority) %>% split(.$section) %>% map(.,~pull(.,name_priority)) ### Section
area_wellbeing_priority <- key_hesper %>% select(area_wellbeing=`Area of well-being`, name_priority) %>% split(.$area_wellbeing) %>% map(.,~pull(.,name_priority)) ### Area of well-being
sector_priority <- key_hesper %>% select(sector_theme=`Sector/Theme`, name_priority) %>% split(.$sector_theme) %>% map(.,~pull(.,name_priority)) ### Sector/Theme

## function to put "other" is at the end, only if other is in the names
other_end <- function(var){if("other" %in% names(var)){var[c(names(var)[!names(var) %in% "other"],"other")]} else {var}}
group_name <- c("section_wide", "section", "area_wellbeing", "sector")
for (var in c(group_name, paste0(group_name, "_priority"))) {assign(var, other_end(get(var)))}

## Define all parameters at once here
parameters <- list(
  choice_serious = "serious_problem",
  choice_no_serious = "no_serious_problem",
  choice_dnk = "dnk",
  choice_pnta = "pnta",
  choice_na = "not_applicable",
  col_gender = "resp_gender",
  choice_male = "male",
  choice_female = "female",
  col_displacement = "hoh_dis",
  choice_displaced = c("host_family", "diplaced_camp"),
  choice_host = c("host", "retourne")
  )

### 2. Composition of needed columns  -------------------------------------------------------

## Use add_hesper_main function to calculate nb hesper serious problems, binary columns for all items, with subset when relevant
## unite the three priority columns to have a single select multiple column with space separator and its child binary columns
## Create as many child binary columns as subset choice (relevant for sub-population group only) set as NA when choice not available for respondent

data <- data %>%
  add_hesper_main(col_items = col.hesp,
                  choice_serious = parameters$choice_serious,
                  choice_no_serious = parameters$choice_no_serious,
                  choice_dnk = parameters$choice_dnk,
                  choice_pnta = parameters$choice_pnta,
                  choice_na = parameters$choice_na,
                  cols_priority = col.prio,
                  col_name_hesper_top_three = "hesper_top_three_priorities",
                  col_gender = parameters$col_gender,
                  choice_male = parameters$choice_male,
                  choice_female = parameters$choice_female,
                  hesper_item_male = val.hesper.men,
                  hesper_item_female = val.hesper.women,
                  col_displacement = parameters$col_displacement,
                  choices_displaced = parameters$choice_displaced,
                  choices_non_displaced = parameters$choice_host,
                  col_hesper_subset = col.subset,
                  hesper_item_displaced = val.hesper.displaced,
                  hesper_item_non_displaced = val.hesper.host,
                  add_binaries = T,
                  add_binaries_subset = T,
                  subset = T)

### use HESPER functions to calculate composite indicators based on categories regrouping hesper items 
## ensure that you update the choice serious / choice no serious arguments
data <- data %>% 
  add_hesper_cat(list_group=section_wide, choice_serious = parameters$choice_serious, choice_no_serious = parameters$choice_no_serious, choice_dnk = parameters$choice_dnk, choice_pnta = parameters$choice_pnta, choice_na = parameters$choice_na) %>%
  add_hesper_cat(list_group=section, choice_serious = parameters$choice_serious, choice_no_serious = parameters$choice_no_serious, choice_dnk = parameters$choice_dnk, choice_pnta = parameters$choice_pnta, choice_na = parameters$choice_na) %>%
  add_hesper_cat(list_group=area_wellbeing, choice_serious = parameters$choice_serious, choice_no_serious = parameters$choice_no_serious, choice_dnk = parameters$choice_dnk, choice_pnta = parameters$choice_pnta, choice_na = parameters$choice_na) %>%
  add_hesper_cat(list_group=sector, choice_serious = parameters$choice_serious, choice_no_serious = parameters$choice_no_serious, choice_dnk = parameters$choice_dnk, choice_pnta = parameters$choice_pnta, choice_na = parameters$choice_na) %>%
  add_hesper_cat(list_group=section_wide_priority, choice_serious = 1, choice_no_serious = 0, choice_na = NA) %>%
  add_hesper_cat(list_group=section_priority, choice_serious = 1, choice_no_serious = 0, choice_na = NA) %>%
  add_hesper_cat(list_group=area_wellbeing_priority, choice_serious = 1, choice_no_serious = 0, choice_na = NA) %>%
  add_hesper_cat(list_group=sector_priority, choice_serious = 1, choice_no_serious = 0, choice_na = NA)

## write data with composite indicators
data %>% fwrite("../data_hesper.csv", row.names = F)

### Dual voices formatting
## Get all child columns for select one questions to then aggregate select one with binary columns only
data_expanded <- data %>% expand.select.one.vec(so.questions) 

### 3. Analysis HESPER [Aggregation + table formatting ---------------------------------------

## Defin colname of geographical disaggregation
col_geo <- "admin1"

# Define strata and weight columns
col.weight <- "weights"
col.strata <- "admin3_group_population"

## Define variables to analyse and expand child columns for relevant columns [except hesper_to_three_priorities that has been done with function]
all_var_parent <- c("hesper_top_three_priorities", col.prio)
binary_var <- str_subset(colnames(data), "^(prop_hesper|nb_hesper)") %>% str_subset("items", negate=T)
binary_var2 <- str_subset(colnames(data), "^(nb_hesper_items_|prop_hesper_items_|hesper_item_|overall_prop_hesper|at_least_one)")
binary_var3 <- paste0(col.hesp, ".binary") %>% append(paste0(col.subset, ".binary_subset")) %>% append(paste0(col.hesp, ".undefined"))
binary_prio <- c(col.prio, "hesper_top_three_priorities") %>% map(~paste0(.x, ".") %>% str_subset(colnames(data_expanded), .)) %>% unlist
var_analyse <- c(binary_var, binary_var2, binary_var3, binary_prio)

## Label for the groups for each section/group in key_hesper, summarise to have the name of the components and the number
sum_key <- function(key=key_hesper %>% filter(subset!="women"), col_group="Section Wide"){
  key %>% 
    group_by(group_cat=!!sym(col_group)) %>%
    summarise(group=col_group, nb_items=n(), 
              item_name=paste0(name, collapse=", "), 
              question_name=paste0(Question, collapse=", "), 
              severity_criteria=paste0(`Severity Criteria`, collapse=", "), 
              label=paste0(name, collapse="\n")) %>% ungroup %>% relocate(group, .before=1)
}

key_section_wide <- key_hesper %>% sum_key(col_group="Section Wide") %>% mutate(group="section_wide")
key_section <- key_hesper %>% sum_key(col_group="Section") %>% mutate(group="section")
key_area_wellbeing <- key_hesper %>% sum_key(col_group="Area of well-being") %>% mutate(group="area_wellbeing")
key_sector <- key_hesper %>% sum_key(col_group="Sector/Theme") %>% mutate(group="sector")
key_all <- bind_rows(key_section_wide, key_section, key_area_wellbeing, key_sector)
key_all <- key_all %>% bind_rows(key_all %>% mutate(group=paste0(group, "_priority")))

## Function to format hesper result table to incorporate labels for all composite indicators
format_hesper <- function(df=res_hesper, 
                          df_key_hesper=key_hesper,
                          col_lab = "label::french",
                          col_prio = c(col.prio, "hesper_top_three_priorities")){
  df %>% 
    mutate(group = case_when(str_detect(question, "item|overall_prop") ~ str_replace_all(question, ".*item(|s)_|overall_prop_hesper_", ""),
                             str_detect(choice, "subset$") ~ "subset", T ~ NA_character_), 
           metric= case_when(str_detect(question, group) ~ str_replace_all(question, paste0("_", group), ""), 
                             str_detect(choice, "binary") ~ "binary", question %in% c("nb_hesper", "prop_hesper") ~ paste0(question, ".", choice), 
                             T ~ NA_character_), 
           choice = case_when(str_detect(choice, "binary") ~ question, str_detect(choice, "subset") ~ str_replace_all(choice, "_subset$", ""), T ~ choice),
           question = case_when(metric=="binary" & choice %in% col.hesp ~ "hesper_serious_problem", T ~ question), 
           type = case_when(question %in% col_prio | str_detect(group, "_priority$") ~ "priority",
                            (!str_detect(group, "_priority$") & !is.na(group)) | question == "hesper_serious_problem" ~ "serious_problem"),
           .before="choice") %>%
    left_join(survey %>% select(name, question_label=!!sym(col_lab)), by=c("question"="name")) %>%
    left_join(survey_combined %>% select(name, choice_name, choice_label), by=c("question"="name", "choice"="choice_name")) %>%
    left_join(key_all %>% select(-label, -item_name), by=c("group", "choice"="group_cat")) %>%
    mutate(choice_label = coalesce(choice_label, question_name), 
           metric = coalesce(metric, choice), 
           choice=ifelse(metric=="undefined", question, choice),
           question=ifelse(metric=="undefined", "item_undefined", question)) %>% 
    select(-question_name) %>%
    left_join(ind_lab_hesper, by=c("metric")) %>% mutate(question_label=coalesce(question_label, metric_label)) %>% select(-metric_label) %>%
    left_join(df_key_hesper %>% select(choice=name, new_choice=choice_name)) %>% mutate(choice=coalesce(new_choice, choice)) %>% select(-new_choice) %>%
    left_join(df_key_hesper %>% select(choice=choice_name, item_label=Question, criteria=`Severity Criteria`)) %>%
    mutate(choice_label = coalesce(item_label, choice_label), severity_criteria = coalesce(severity_criteria, criteria)) %>% select(-item_label, -criteria) %>%
    mutate(question_label = case_when(
      question == "hesper_serious_problem" ~ "Hesper - Serious problem reported",
      question == "hesper_top_three_priorities" ~ "Out of these problems, which are the three most serious problems for the household?",
      T ~ question_label)) 
}

### ANALYSIS
## Aggregate with different disaggregation variables
res_hesper <- analyse(df = data_expanded , group_var = NULL, var = var_analyse, col_weight=col.weight, col_strata = col.strata)
result_hesper <- res_hesper %>% format_hesper

res_hesper_gender <- analyse(df = data_expanded, group_var=parameters$col_gender, var = var_analyse, col_weight=col.weight, col_strata = col.strata)
result_hesper_gender <- res_hesper_gender %>% format_hesper

res_hesper_pop <- analyse(df = data_expanded, group_var=parameters$col_displacement, var = var_analyse, col_weight=col.weight, col_strata = col.strata)
result_hesper_pop <- res_hesper_pop %>% format_hesper

res_hesper_region <- analyse(df = data_expanded, group_var=col_geo, var = var_analyse, col_weight=col.weight, col_strata = col.strata)
result_hesper_region <- res_hesper_region %>% format_hesper

res_hesper_group <- result_hesper %>% filter(str_detect(question, "at_least")) %>%
  mutate(type=case_when(str_detect(group, "priority") ~ "priority_problem", T ~ "serious_problem"),
         across(all_of(c("group", "question")), ~str_replace_all(., "_priority", ""))) %>%
  pivot_wider(names_from=type, values_from=c(mean, count, n))

dir.create("exploratory", showWarnings = F)
list(result_hesper=result_hesper, res_hesper_group=res_hesper_group) %>% write_xlsx("exploratory/result_hesper.xlsx")

## Formatting function to pivot wider priority needs results
format_top_three <- function(df = result_hesper, 
                             col_priority = all_var_parent){
 df %>% 
   filter(! question %in% "nb_hesper") %>%
   filter(str_detect(question, paste0(col_priority, collapse="|")) | metric=="binary" | metric %in% "undefined") %>% 
   mutate(choice_subset=case_when(group=="subset" ~ paste0(choice, "_", group), T ~ choice), .after="choice") %>%
   select(-nb_items, -question_label, -metric, -type)  %>% mutate(stat=paste0(count, " (", 100*round(mean,3), "%)")) %>%
   pivot_wider(names_from = question, values_from = any_of(c("stat", "mean", "count", "n", "mean/low", "mean/upp"))) %>%
   rename_with(~str_replace_all(., "(stat_hesper_|stat_.*_hesper_|^stat_)", "")) 
}

top_three_formatted <- result_hesper %>% format_top_three
top_three_formatted_gender <- result_hesper_gender %>% format_top_three %>% arrange(-mean_hesper_serious_problem) %>%
  pivot_wider(names_from = !!sym(parameters$col_gender), values_from = -c(!!sym(parameters$col_gender):severity_criteria))
top_three_formatted_pop <- result_hesper_pop %>% format_top_three %>% arrange(-mean_hesper_serious_problem) %>%
  pivot_wider(names_from = !!sym(parameters$col_displacement), values_from = -c(!!sym(parameters$col_displacement):severity_criteria))
top_three_formatted_region <- result_hesper_region %>% format_top_three %>% arrange(-mean_hesper_serious_problem) %>%
  pivot_wider(names_from = !!sym(col_geo), values_from = -c(!!sym(col_geo):severity_criteria))

list(top_serious=top_three_formatted, top_serious_gender=top_three_formatted_gender, 
     top_three_formatted_pop=top_three_formatted_pop, top_three_formatted_region=top_three_formatted_region) %>%
  write_xlsx("priority_needs_table.xlsx")

## 4. Standard HESPER visualisation/graph --------------------------------------------------

## Function to plot the number of serious problems by residence status
## Histogram for numerical variables (function in utils.R)
nb_items <- data %>% plot.hist(title="Total number of serious problems reported\n", bins.val=15) + theme(plot.title = element_text(hjust = .5), plot.subtitle = element_text(hjust = .5))
nb_items_gender <- data %>% plot.hist(title="Total number of serious problems reported\n", subtitle="by respondent gender", fill=parameters$col_gender, bins.val=15) + theme(plot.title = element_text(hjust = .5), plot.subtitle = element_text(hjust = .5))
nb_items_region <- data %>% plot.hist(title="Total number of serious problems reported\n", subtitle="by region", fill=col_geo, bins.val=15) + theme(plot.title = element_text(hjust = .5), plot.subtitle = element_text(hjust = .5))
nb_items_pop <- data %>% filter(!(!!sym(parameters$col_displacement) %in% "nsp")) %>%
  plot.hist(title="Total number of serious problems reported\n", subtitle="by population group", fill=parameters$col_displacement, bins.val=15) +
  theme(plot.title = element_text(hjust = .5), plot.subtitle = element_text(hjust = .5))

list.p <- list("nb_items"=nb_items, "nb_items_gender"=nb_items_gender, "nb_items_location"=nb_items_region, "nb_items_pop"=nb_items_pop)
dir.create("exploratory/graph", showWarnings = F)
for(i in 1:length(list.p)) {ggsave(filename=paste0("exploratory/graph/", names(list.p)[i], ".png"), plot=list.p[[i]], bg="white", width=8, height=6)}

## check proportion of hesper items compared to all other items
plot.physical.non.physical <- function(data.frame=data, group=parameters$col_gender) {
  prop_physical <- plot.hist(df=data.frame, scale_density = 1.5, var="nb_hesper_items_section_wide.physical", fill=group,
                             subtitle="Total number of physical items: 10\n", title="Number of physical serious problems reported", bins.val=10) + 
    theme(plot.title = element_text(hjust = .5), plot.subtitle = element_text(hjust = .5)) 
  prop_non_physical <- plot.hist(df=data.frame, scale_density = 2, var="nb_hesper_items_section_wide.non_physical", fill=group,
                                 subtitle="Total number of non-physical items: 17\n", title="Number of non-physical serious problems reported", bins.val=8) + 
    theme(plot.title = element_text(hjust = .5), plot.subtitle = element_text(hjust = .5))
  plot.section_wide <- gridExtra::grid.arrange(prop_physical, prop_non_physical, ncol=2)
  ggsave(filename=paste0("exploratory/graph/prop_hesper_items_section_wide_",group,".png"), plot=plot.section_wide, bg="white", width=10, height=6)
}
plot.physical.non.physical(group=NULL)
plot.physical.non.physical(group=parameters$col_gender)
plot.physical.non.physical(data.frame = data %>% filter(!(!!sym(parameters$col_displacement) %in% c("nsp"))), group=parameters$col_displacement)

## Plot medians / Averages (function plot.num in utils)
## get median result table
p_load(impactR.analysis, srvyr)
design <- srvyr::as_survey(data_expanded , weight=col.weight, strata=col.strata)

## define grouping variables (to be updated)
group_var <- c(parameters$col_gender, parameters$col_displacement, col_geo)
median_nb_serious <- map(group_var, function(var) {
  design %>% group_by(!!sym(var)) %>% 
    summarize(median = survey_median(nb_hesper.all, na.rm = TRUE, vartype="ci"),
              n_unw=sum(!is.na(nb_hesper.all))) %>% ungroup() %>% 
    mutate(group = var) %>% rename(group_val = !!sym(var)) %>% select(group, group_val, everything())
}) %>% bind_rows() %>% filter(!group_val %in% "nsp")

plot.dis <- plot.num(df= median_nb_serious %>% mutate(pop_group=group_val) %>% filter(group==parameters$col_displacement), 
         col_group = "pop_group", col_stat = "median", save=F)
plot.geo <- plot.num(df= median_nb_serious %>% mutate(region=group_val) %>% filter(group==col_geo), 
         col_group = "region", col_stat = "median", save=F)
plot.gender <- plot.num(df= median_nb_serious %>% mutate(gender=group_val) %>% filter(group==parameters$col_gender),
                        col_group = "gender", col_stat = "median", save=F)
  
#### Create graphs for all composite indicators with at_least one item from a category (function in utils.R)
## Can be done further for other indicators
result_hesper$question %>% unique %>% str_subset("at_least") %>% str_subset("_priority", negate = T)

all.ind <- c("at_least_one_hesper_item_section", "at_least_one_hesper_item_section_wide", "at_least_one_hesper_item_area_wellbeing", 
             "at_least_one_hesper_item_sector")
for (ind in all.ind){
  # ind <- all.ind[1]
  create_graph_facet_disag(df = result_hesper, indicator = ind)
  create_graph_facet_disag(df = result_hesper_gender %>% mutate(gender=resp_gender), indicator = ind, col_disag = "gender")
  create_graph_facet_disag(df = result_hesper %>% mutate(question = str_replace_all(question, "_priority$", ""), Type=str_replace_all(type, "_", " ")),  indicator = ind, col_disag = "Type")
  create_graph_facet_disag(df = result_hesper_pop, indicator = ind, col_disag = parameters$col_displacement)
  create_graph_facet_disag(df = result_hesper_region, indicator = ind, col_disag = col_geo)
}

## extract the HESPER items serious problem vs top three priority and plot it
res_hesper_main <- result_hesper %>% mutate(choice_label=str_wrap(choice_label, 40)) %>%
  filter(question %in% c("hesper_serious_problem", "hesper_top_three_priorities"), !group %in% "subset") %>%
  mutate(question = "HESPER Item", Type=str_replace_all(first_up(type), "_", " ")) %>% arrange(desc(Type), mean)
res_hesper_main_order2 <- res_hesper_main %>% arrange(Type, mean) 
lab.order <- res_hesper_main$choice_label %>% unique
lab.order.2 <- res_hesper_main_order2 %>% filter(Type=="Priority") %>% pull(choice_label) %>% unique
res_hesper_main <- res_hesper_main %>% mutate(choice_label=factor(choice_label, levels=lab.order))
create_graph_facet_disag(df=res_hesper_main, indicator="HESPER Item", col_disag = "Type", custom.factor = T,
                         custom.folder="priority vs serious problem/", 
                         custom.file = "serious vs top 3",
                         add.ggplot = labs(title="% respondent reporting HESPER item", subtitle="Serious problem vs top three priority", fill=""), figheight = 8 , fwidth = 9)
## with no CI
create_graph_facet_disag(df=res_hesper_main, indicator="HESPER Item", col_disag = "Type", custom.factor = T, 
                         custom.folder="priority vs serious problem/",
                         custom.file = "serious vs top 3 - no CI",
                         add.ggplot = labs(title="% respondent reporting HESPER item", subtitle="Serious problem vs top three priority", fill=""), error_bar = F, figheight = 8 , fwidth = 9)
## Opposite ordering
res_hesper_main_order2 <- res_hesper_main %>% mutate(choice_label=factor(choice_label, levels=lab.order.2)) %>% filter(!choice_label %in% "Other")
create_graph_facet_disag(df=res_hesper_main_order2, indicator="HESPER Item", col_disag = "Type", custom.factor = T, 
                         custom.folder="priority vs serious problem/",
                         custom.file = "serious vs top 3 - order 2",
                         add.ggplot = labs(title="% respondent reporting HESPER item", subtitle="Serious problem vs top three priority", fill=""), figheight = 8 , fwidth = 9)
create_graph_facet_disag(df=res_hesper_main_order2, indicator="HESPER Item", col_disag = "Type", custom.factor = T, 
                         custom.folder="priority vs serious problem/",
                         custom.file = "serious vs top 3 - no CI - order 2",
                         add.ggplot = labs(title="% respondent reporting HESPER item", subtitle="Serious problem vs top three priority", fill=""), error_bar = F, figheight = 8 , fwidth = 9)

## Side by side with facet, not with Fill
create_graph_facet_disag(df=res_hesper_main_order2, indicator="HESPER Item", custom.factor = T, 
                         custom.folder="priority vs serious problem/",
                         custom.file = "serious vs top 3 - order 2 FACET",
                         add.ggplot = list(
                           labs(title="% respondent reporting HESPER item", subtitle="Serious problem vs top three priority", fill=""),
                           facet_wrap(~Type, scales = "free"),
                           scale_x_discrete(labels=~str_wrap(.,25))), figheight = 7.5 , fwidth = 11)
create_graph_facet_disag(df=res_hesper_main_order2, indicator="HESPER Item", custom.factor = T, 
                         custom.folder="priority vs serious problem/",
                         custom.file = "serious vs top 3 - no CI - order 2 FACET",
                         add.ggplot = list(
                           labs(title="% respondent reporting HESPER item", subtitle="Serious problem vs top three priority", fill=""),
                           facet_wrap(~Type, scales = "free"),
                           scale_x_discrete(labels=~str_wrap(.,25))), 
                         error_bar = F, figheight = 7.5 , fwidth = 11)



## SAME BY GENDER
res_hesper_main_gender <- result_hesper_gender %>%  mutate(choice_label=str_wrap(choice_label, 40)) %>%
  mutate(gender=str_replace_all(!!sym(parameters$col_gender), setNames(c("Women respondent", "Men respondent"), c(parameters$choice_female, parameters$choice_male))), 
         choice_label=str_wrap(choice_label, 40)) %>%
  filter(question %in% c("hesper_serious_problem", "hesper_top_three_priorities"), !group %in% "subset") %>%
  mutate(question = "HESPER Item", Type=str_replace_all(first_up(type), "_", " "))
lab.order <- res_hesper_main_gender %>% group_by(choice_label) %>% filter(mean==max(mean)) %>% arrange(mean) %>% pull(choice_label) %>% unique
res_hesper_main_gender <- res_hesper_main_gender %>% mutate(choice_label=factor(choice_label, levels=lab.order.2)) %>% filter(!choice_label %in% "Other")
create_graph_facet_disag(df=res_hesper_main_gender, indicator="HESPER Item", col_disag = "Type", custom.factor = T, 
                         custom.folder="priority vs serious problem/",
                         custom.file = "gender - serious vs top3",
                         add.ggplot = list(labs(title="% respondent reporting HESPER item", subtitle="Serious problem vs top three priority, by respondent gender", fill=""),
                                           facet_wrap(~gender, scales="free_y")), figheight = 12 , fwidth = 14)
create_graph_facet_disag(df=res_hesper_main_gender, indicator="HESPER Item", col_disag = "Type", error_bar = F ,custom.factor = T, 
                         custom.folder="priority vs serious problem/",
                         custom.file = "gender - serious vs top 3 - no CI",
                         add.ggplot = list(labs(title="% respondent reporting HESPER item", subtitle="Serious problem vs top three priority, by respondent gender", fill=""),
                                           facet_wrap(~gender, scales="free_y")), figheight = 12 , fwidth = 14)

## Different visual
create_graph_facet_disag(df=res_hesper_main_gender, indicator="HESPER Item", col_disag = "gender", custom.factor = T, 
                         custom.folder="priority vs serious problem/",
                         custom.file = "gender - serious vs top 3 - V2",
                         add.ggplot = list(labs(title="% respondent reporting HESPER item", subtitle="Serious problem vs top three priority, by respondent gender", fill=""),
                                           facet_wrap(~Type, scales="free_y")), figheight = 12 , fwidth = 15, text_vpos = .057)
create_graph_facet_disag(df=res_hesper_main_gender, indicator="HESPER Item", col_disag = "gender", error_bar = F ,custom.factor = T, 
                         custom.folder="priority vs serious problem/",
                         custom.file = "gender - serious vs top 3 - no CI - V2",
                         add.ggplot = list(labs(title="% respondent reporting HESPER item", subtitle="Serious problem vs top three priority, by respondent gender", fill=""),
                                           facet_wrap(~Type, scales="free_y")), figheight = 12 , fwidth = 15, text_vpos = .057)

## By pop group
res_hesper_main_pop <- result_hesper_pop %>%  mutate(choice_label=str_wrap(choice_label, 40)) %>% filter(!(!!sym(parameters$col_displacement) %in% "nsp")) %>%
  mutate(pop_group=str_replace_all(!!sym(parameters$col_displacement), c("_"=" "))) %>% 
  filter(question %in% c("hesper_serious_problem", "hesper_top_three_priorities"), !group %in% "subset") %>%
  mutate(question = "HESPER Item", Type=str_replace_all(first_up(type), "_", " "))
lab.order <- res_hesper_main_pop %>% group_by(choice_label) %>% filter(mean==max(mean)) %>% arrange(mean) %>% pull(choice_label) %>% unique
res_hesper_main_pop <- res_hesper_main_pop %>% mutate(choice_label=factor(choice_label, levels=lab.order))
create_graph_facet_disag(df=res_hesper_main_pop, indicator="HESPER Item", col_disag = "Type", custom.factor = T, text_vpos = .05,
                         custom.folder="priority vs serious problem/",
                         custom.file = "pop group - serious vs top three",
                         add.ggplot = list(labs(title="% respondent reporting HESPER item", subtitle="Serious problem vs top three priority, by population group", fill=""),
                                           facet_wrap(~pop_group, scales="free_y")), figheight = 14 , fwidth = 14)
## without CI
create_graph_facet_disag(df=res_hesper_main_pop, indicator="HESPER Item", col_disag = "Type", custom.factor = T, text_vpos = .05,
                         custom.folder="priority vs serious problem/",
                         custom.file = "pop group - serious vs top three - no CI",
                         add.ggplot = list(labs(title="% respondent reporting HESPER item", subtitle="Serious problem vs top three priority, by population group", fill=""),
                                           facet_wrap(~pop_group, scales="free_y")), error_bar = F, figheight = 14 , fwidth = 14)
## Different visual
create_graph_facet_disag(df=res_hesper_main_pop, indicator="HESPER Item", col_disag = "pop_group", custom.factor = T, text_vpos = .05,
                         custom.folder="priority vs serious problem/",
                         custom.file = "pop group - serious vs top three - V2",
                         add.ggplot = list(labs(title="% respondent reporting HESPER item", subtitle="Serious problem vs top three priority, by population group", fill=""),
                                           facet_wrap(~Type, scales="free_y")), figheight = 18 , fwidth = 15)
create_graph_facet_disag(df=res_hesper_main_pop, indicator="HESPER Item", col_disag = "pop_group", custom.factor = T, text_vpos = .05,
                         custom.folder="priority vs serious problem/",
                         custom.file = "pop group - serious vs top three-noCI V2",
                         add.ggplot = list(labs(title="% respondent reporting HESPER item", subtitle="Serious problem vs top three priority, by population group", fill=""),
                                           facet_wrap(~Type, scales="free_y")), error_bar = F, figheight = 18 , fwidth = 15)

## By Region / col_geo
res_hesper_main_region <- result_hesper_region %>%  mutate(choice_label=str_wrap(choice_label, 40)) %>%
  mutate(region = str_replace_all(admin1, "_", " ")) %>%
  filter(question %in% c("hesper_serious_problem", "hesper_top_three_priorities"), !group %in% "subset") %>%
  mutate(question = "HESPER Item", Type=str_replace_all(first_up(type), "_", " "))
lab.order <- res_hesper_main_region %>% group_by(choice_label) %>% filter(mean==max(mean)) %>% arrange(mean) %>% pull(choice_label) %>% unique
res_hesper_main_region <- res_hesper_main_region %>% mutate(choice_label=factor(choice_label, levels=lab.order))
create_graph_facet_disag(df=res_hesper_main_region, indicator="HESPER Item", col_disag = "Type", custom.factor = T, 
                         custom.folder="priority vs serious problem/",
                         custom.file = "region - serious vs top3",
                         add.ggplot = list(labs(title="% respondent reporting HESPER item", subtitle="Serious problem vs top three priority, by region", fill=""),
                                           facet_wrap(~region, scales="free_y")), figheight = 8 , fwidth = 14, text_vpos = .125)
## without CI
create_graph_facet_disag(df=res_hesper_main_region, indicator="HESPER Item", col_disag = "Type", custom.factor = T, 
                         custom.folder="priority vs serious problem/",
                         custom.file = "region - serious vs top 3 - no CI",
                         add.ggplot = list(labs(title="% respondent reporting HESPER item", subtitle="Serious problem vs top three priority, by region", fill=""),
                                           facet_wrap(~region, scales="free_y")), error_bar = F, figheight = 8 , fwidth = 14, text_vpos = .125)

## Different visual
create_graph_facet_disag(df=res_hesper_main_region, indicator="HESPER Item", col_disag = "region", custom.factor = T, 
                         custom.folder="priority vs serious problem/",
                         custom.file = "region - serious vs top3 - V2",
                         add.ggplot = list(labs(title="% respondent reporting HESPER item", subtitle="Serious problem vs top three priority, by region", fill=""),
                                           facet_wrap(~Type, scales="free_y")), figheight = 14 , fwidth = 15, text_vpos = .08)
create_graph_facet_disag(df=res_hesper_main_region, indicator="HESPER Item", col_disag = "region", custom.factor = T,
                         custom.folder="priority vs serious problem/",
                         custom.file = "region - serious vs top3-noCI V2",
                         add.ggplot = list(labs(title="% respondent reporting HESPER item", subtitle="Serious problem vs top three priority, by region", fill=""),
                                           facet_wrap(~Type, scales="free_y")), error_bar = F, figheight = 14 , fwidth = 15, text_vpos = .08)
## Opposite ordering
create_graph_facet_disag(df=res_hesper_main_region %>% mutate(choice_label=factor(choice_label, levels=lab.order.2)), 
                         indicator="HESPER Item", col_disag = "region", custom.factor = T, 
                         custom.folder="priority vs serious problem/",
                         custom.file = "region - serious vs top3 - V3",
                         add.ggplot = list(labs(title="% respondent reporting HESPER item", subtitle="Serious problem vs top three priority, by region", fill=""),
                                           facet_wrap(~Type, scales="free_y")), figheight = 14 , fwidth = 15, text_vpos = .068)
create_graph_facet_disag(df=res_hesper_main_region %>% mutate(choice_label=factor(choice_label, levels=lab.order.2)),
                         indicator="HESPER Item", col_disag = "region", custom.factor = T,
                         custom.folder="priority vs serious problem/",
                         custom.file = "region - serious vs top3-noCI V3",
                         add.ggplot = list(labs(title="% respondent reporting HESPER item", subtitle="Serious problem vs top three priority, by region", fill=""),
                                           facet_wrap(~Type, scales="free_y")), error_bar = F, figheight = 14 , fwidth = 15, text_vpos = .068)
