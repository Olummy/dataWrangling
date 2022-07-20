
# load packages

if(!require("pacman"))install.packages("pacman")

pacman::p_load(
  readxl,
  tidyverse,
  openxlsx
)


## New file Obajana

excel_sheets("Data/Wide/updated Line wise BW CC Zafar.xlsx")

line1_2 <- read_excel("Data/Wide/updated Line wise BW CC Zafar.xlsx", sheet = 3) %>% 
  select(-`Cost Center`)

line3 <- read_excel("Data/Wide/updated Line wise BW CC Zafar.xlsx", sheet = 2) %>% 
  select(-`Cost Center`)

line4 <- read_excel("Data/Wide/updated Line wise BW CC Zafar.xlsx", sheet = 1) %>% 
  select(-`Cost Center`)



line1_2_long <- line1_2[!duplicated(line1_2),] %>%
  gather(key = "key", value = "Cost Center", -`Company Code`: -`Heads`, na.rm = TRUE) %>%
  mutate(`Profit Center` = as.character(`Profit Center`),
         `G/L Account` =as.character(`G/L Account`)) %>% 
  select(-key) %>% 
  select(`Company Code`, Heads, `Profit Center`, `G/L Account`, 
         `Cost Center`, `Controlling Area`, `Chart of Accounts`, 
         `DCP & DIL Flag`) 


line3_long <- line3[!duplicated(line3),] %>%
  gather(key = "key", value = "Cost Center", -`Company Code`: -`Heads`, na.rm = TRUE) %>%
  mutate(`Profit Center` = as.character(`Profit Center`),
         `G/L Account` =as.character(`G/L Account`)) %>% 
  select(-key) %>% 
  select(`Company Code`, Heads, `Profit Center`, `G/L Account`, 
         `Cost Center`, `Controlling Area`, `Chart of Accounts`, 
         `DCP & DIL Flag`)


line4_long <- line4[!duplicated(line4),] %>%
  gather(key = "key", value = "Cost Center", -`Company Code`: -`Heads`, na.rm = TRUE) %>%
  mutate(`Profit Center` = as.character(`Profit Center`),
         `G/L Account` =as.character(`G/L Account`)) %>% 
  select(-key) %>% 
  select(`Company Code`, Heads, `Profit Center`, `G/L Account`, 
         `Cost Center`, `Controlling Area`, `Chart of Accounts`, 
         `DCP & DIL Flag`)
  

combine_obajana <- line1_2_long %>% bind_rows(line3_long) %>% bind_rows(line4_long)



# New file Elijah!

corp_TB <- read_csv("Data/Wide/Corp TB July 14th.csv") %>% 
  rename(`G/L Account` = `Gl Account`,
         `Controlling Area` = `CO AREA`,
         `Chart of Accounts` = `Charts of Accounts`,
         `DCP & DIL Flag` = ZIOCFFLAG)
  

WorkFile <- read_csv("Data/Wide/Work File DCP - HO  July 14th.csv") %>% 
  rename(`G/L Account` = `Gl Account`,
         `Controlling Area` = `CO AREA`,
         `Chart of Accounts` = `Charts of Accounts`,
         `DCP & DIL Flag` = ZIOCFFLAG)


combine_file <- corp_TB %>% 
  bind_rows(WorkFile) %>% 
  mutate(`Profit Center` = paste0("000000",`Profit Center`),
         `G/L Account` = paste0("0000", `G/L Account`))




# New obajana file

sheet1 <- read_excel("Data/Wide/Copy of Line wise BW CC Zafar.xlsx", sheet = 1)
sheet2 <- read_excel("Data/Wide/Copy of Line wise BW CC Zafar.xlsx", sheet = 2)
sheet3 <- read_excel("Data/Wide/Copy of Line wise BW CC Zafar.xlsx", sheet = 3)

# remove the cost center field

sheet1 <- sheet1 %>% 
  select(-`Cost Center`)

sheet2 <- sheet2 %>% 
  select(-`Cost Center`)

sheet3 <- sheet3 %>% 
  select(-`Cost Center`)

sheet1_long <- sheet1[!duplicated(sheet1),] %>%
  gather(key = "key", value = "Cost Center", -`Company Code`: -`Heads`, na.rm = TRUE) %>%
  mutate(`Profit Center` = as.character(`Profit Center`),
         `G/L Account` =as.character(`G/L Account`)) %>% 
  select(-key) %>% 
  select(`Company Code`, Heads, `Profit Center`, `G/L Account`, 
         `Cost Center`, `Controlling Area`, `Chart of Accounts`, 
         `DCP & DIL Flag`)

sheet2_long <- sheet2[!duplicated(sheet2),] %>%
  gather(key = "key", value = "Cost Center", -`Company Code`: -`Heads`, na.rm = TRUE) %>%
  mutate(`Profit Center` = as.character(`Profit Center`),
         `G/L Account` =as.character(`G/L Account`)) %>% 
  select(-key) %>% 
  select(`Company Code`, Heads, `Profit Center`, `G/L Account`, 
         `Cost Center`, `Controlling Area`, `Chart of Accounts`, 
         `DCP & DIL Flag`)


sheet3_long <- sheet3[!duplicated(sheet3),] %>%
  gather(key = "key", value = "Cost Center", -`Company Code`: -`Heads`, na.rm = TRUE) %>%
  mutate(`Profit Center` = as.character(`Profit Center`),
         `G/L Account` =as.character(`G/L Account`)) %>% 
  select(-key) %>% 
  select(`Company Code`, Heads, `Profit Center`, `G/L Account`, 
         `Cost Center`, `Controlling Area`, `Chart of Accounts`, 
         `DCP & DIL Flag`)

obajana_combine_df <- sheet1_long %>% 
  bind_rows(sheet2_long) %>% 
  bind_rows(sheet3_long) %>% 
  mutate(`G/L Account` = case_when(nchar(`G/L Account`) == 6 ~ paste0("0000", `G/L Account`),
                                 TRUE ~ `G/L Account`))

obajana_combine_df %>% 
  mutate(length_char = nchar(`G/L Account`)) %>% 
  group_by(length_char) %>% 
  summarise(Count = n())


hs <- createStyle(
  textDecoration = "BOLD", fontColour = "#FFFFFF", fontSize = 12,
  fontName = "Arial Narrow", fgFill = "#4F80BD"
)


write.xlsx(obajana_combine_df, "./Data/Long/Obajana_New_v2.xlsx",
           colNames = TRUE, borders = "rows", headerStyle = hs
)


dcp_budget <- read_csv("Data/Long/DCP 2022 FI Budget HO 16.6.csv") %>% 
  .[, c(1:8)]

dcp_budget %>% 
  mutate(`Profit Center` = paste0("000000", `Profit Center`),
         `Gl Account` = paste0("0000", `Gl Account`)) %>% 
  write.xlsx(., "Data/Long/DCP 2022 FI Budget HO 16.6_v2.xlsx", colNames = TRUE,
             borders = "rows", headerStyle = hs)


View(wide_gboko)
duplicated(wide_gboko)
wide_gboko[!duplicated(wide_gboko),]
non_dup_gboko <- wide_gboko[!duplicated(wide_gboko),]
View(non_dup_gboko)
duplicated(non_dup_gboko)
sum(duplicated(non_dup_gboko))
sum(duplicated(wide_gboko))
sum(!duplicated(wide_gboko))
names(non_dup_gboko)
non_dup_gboko <- wide_gboko[!duplicated(wide_gboko),] %>%
  gather(key = "key", value = "Cost Center", -`Company Code`: -`Heads`, na.rm = TRUE) %>%
  mutate(`Profit Center` = as.character(`Profit Center`),
         `G/L Account` =as.character(`G/L Account`))



wide_gboko <- read_excel("Data/Wide/BW Cost Center Mapping Template for Upload -Gboko.xlsx")
non_dup_gboko <- wide_gboko[!duplicated(wide_gboko),] %>%
  gather(key = "key", value = "Cost Center", -`Company Code`: -`Heads`, na.rm = TRUE) %>%
  mutate(`Profit Center` = as.character(`Profit Center`),
         `G/L Account` =as.character(`G/L Account`)) %>%
  select(-key) %>%
  select(`Company Code`, Heads, `Profit Center`, `G/L Account`, `Cost Center`, `Controlling Area`, `Chart of Accounts`, `DCP & DIL Flag`)


hs <- createStyle(
  textDecoration = "BOLD", fontColour = "#FFFFFF", fontSize = 12,
  fontName = "Arial Narrow", fgFill = "#4F80BD"
)
write.xlsx(combine_obajana, "./Data/Long/New_Obajana_15th_July.xlsx",
           colNames = TRUE, borders = "rows", headerStyle = hs
)


obajana_15th <- read_excel("Data/Long/New_Obajana_15th_July.xlsx")

obajana_15th_tbl <- obajana_15th %>% 
  mutate(`G/L Account` = case_when(nchar(`G/L Account`) == 6 ~ paste0("0000", `G/L Account`),
                                   TRUE ~ `G/L Account`),
        `Company Code` = "1000",
        `Controlling Area` = "1000",
        `Chart of Accounts` = "1000",
        `DCP & DIL Flag` = "C")


hs <- createStyle(
  textDecoration = "BOLD", fontColour = "#FFFFFF", fontSize = 12,
  fontName = "Arial Narrow", fgFill = "#4F80BD"
)
write.xlsx(obajana_15th_tbl, "./Data/Long/New_Obajana_15th_July_v2.xlsx",
           colNames = TRUE, borders = "rows", headerStyle = hs
)
