
# load packages

if(!require("pacman"))install.packages("pacman")

pacman::p_load(
  readxl,
  tidyverse,
  openxlsx
)

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
write.xlsx(non_dup_gboko, "./Data/Long/Gboko.xlsx",
           colNames = TRUE, borders = "rows", headerStyle = hs
)