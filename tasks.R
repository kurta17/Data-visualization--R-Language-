# Tidy exercises
#===============================================================================

# Ex 1. Using deaths dataframe #######
deaths <- read_excel(path = "data-denmark.xlsx", sheet = "deaths")
pop <- read_excel(path = "data-denmark.xlsx", sheet = "pop")

# - subset only total number of deaths among men in year 2003 (filter)
subset_data <- deaths[deaths$sex == "m" & deaths$year == "y2003" & deaths$age == "total", ]


# Q: which region had the largest number of deaths?
deaths|>
  filter(year=="y2003",sex=="m",age=="total")|>
  arrange(-value)|>
  slice_head(n=1)|>
  select(region)

# answer:
#   # A tibble: 1 × 1
#   region
# <chr> 
#   1 DK01 

# Ex 2. Using pop dataframe #######
pop <- read_excel(path = "data-denmark.xlsx", sheet = "pop")

pop |> 
  filter(year == "y2004") |> 
  pivot_wider(names_from = sex, values_from = value) |> 
  select(-b)|> 
  mutate(sr = m / f,
         region_name = case_when(region == "DK01" ~ "Hovedstaden", 
                                 region == "DK02" ~ "Midtjylland", 
                                 region == "DK03" ~ "Nordjylland", 
                                 region == "DK04" ~ "Sjælland", 
                                 region == "DK05" ~ "Syddanmark")) |> 
  filter(age %in% c("a15", "a45", "open"))|> 
  group_by(age)|> 
  arrange(desc(sr)) |> 
  slice(1)|>  # with this line we take only the first observation from each group in a grouped dataframe
  select(year,age, region_name)
#answer:
# A tibble: 3 × 3
# Groups:   age [3]
#year  age   region_name
#<chr> <chr> <chr>      
#  1 y2004 a15   Hovedstaden
# 2 y2004 a45   Nordjylland
# 3 y2004 open  Syddanmark 



# Ex 3. joined dataframe #######
# Join the two dataframes (assuming 'pop' and 'deaths' are the dataframes)
df<- pop |> 
  inner_join(deaths, by = c("year", "region", "sex", "age"))|> 
  filter(age!="total")|>
  rename(pop = value.x, deaths = value.y) |> 
  mutate(age=as.numeric(str_sub(if_else(age == "open", "y75",age),2,3)),
         mx = deaths / pop) 


df|> filter(age %in% 15:59, year == "y2001") |> 
  select(-year, -pop, -deaths) |> 
  pivot_wider(names_from = sex, values_from = mx) |> 
  mutate(sr_mx = (m / f) |>  na_if(Inf),
         region_name = case_when(region == "DK01" ~ "Hovedstaden", 
                                 region == "DK02" ~ "Midtjylland", 
                                 region == "DK03" ~ "Nordjylland", 
                                 region == "DK04" ~ "Sjælland", 
                                 region == "DK05" ~ "Syddanmark")) |>  # replace Inf with NAs: note the `` -- bare numbers are not allowed for R objects
  group_by(region_name) |> 
  summarise(avg_mx_sr =  mean(sr_mx,na.rm = TRUE)) |> 
  ungroup()


# answer: 
#   # A tibble: 5 × 2
#   region_name avg_mx_sr
# <chr>           <dbl>
#   1 Hovedstaden      1.77
# 2 Midtjylland      2.22
# 3 Nordjylland      2.34
# 4 Sjælland         2.06
# 5 Syddanmark       1.93
# > View(df)


# Ex 4. joined dataframe (df) #######

df |> 
  filter(sex == "b") |> 
  select(-pop, -deaths) |> 
  pivot_wider(names_from = year, values_from = mx) |> 
  mutate(mx_growth = (y2005/y2001)%>% na_if(Inf),
         region_name = case_when(region == "DK01" ~ "Hovedstaden", 
                                 region == "DK02" ~ "Midtjylland", 
                                 region == "DK03" ~ "Nordjylland", 
                                 region == "DK04" ~ "Sjælland", 
                                 region == "DK05" ~ "Syddanmark")) |>  # note the `` -- bare numbers are not allowed for R objects
  group_by(region_name) |> 
  summarise(avg_mx_growth = mean(mx_growth, na.rm = TRUE)) |> 
  arrange(desc(avg_mx_growth))|> 
  slice(1,5)|> 
  mutate(answer=c("Largest growth", "Largest decrease"))

# answer:
#   # A tibble: 2 × 3
#   region_name avg_mx_growth answer          
# <chr>               <dbl> <chr>           
#   1 Hovedstaden         1.03  Largest growth  
# 2 Nordjylland         0.899 Largest decrease



# EX 5. #####

# Create a new data frame with the Total population born in Spain and
# born abroad in each Spanish autonomous community (hint: "COM") for the year 2016.
# Q: in which autonomous community (COM) the percentage of foreign-born was higher?

df_Ex5<-data|> 
  filter(YEAR==2016)|> 
  mutate(POP_EXT = rowSums(across(7:11)))|> 
  group_by(COM)|>
  summarise(Spanish=sum(POP_SPANISH),
            EXT=sum(POP_EXT))|> 
  mutate(prop_ext=EXT/rowSums(across(2:3))*100,
         prop_extc= if_else(prop_ext<5, "<5%",
                            if_else(prop_ext<10, "(5-10%]",
                                    if_else(prop_ext<15, "(10-15%]",
                                            if_else(prop_ext<20, "(15-20%]",
                                                    ">20%")))))|> 
  arrange(desc(prop_ext));df_Ex5

#answer:
# # A tibble: 17 × 5
# COM                Spanish     EXT prop_ext prop_extc
# <chr>                <dbl>   <dbl>    <dbl> <chr>    
#   1 Balearic Islands    867148  237770    21.5  >20%     
# 2 Canary Islands     1729208  370792    17.7  (15-20%] 
# 3 Madrid             5316325 1132954    17.6  (15-20%] 
# 4 Catalonia          6229822 1281154    17.1  (15-20%] 
# 5 Valencia           4424323  861019    16.3  (15-20%] 
# 6 Murcia             1245494  218696    14.9  (10-15%] 
# 7 La Rioja            272885   42715    13.5  (10-15%] 
# 8 Navarra             555040   84833    13.3  (10-15%] 
# 9 Arago              1147739  159449    12.2  (10-15%] 
# 10 Castilla la Mancha 1843924  196767     9.64 (5-10%]  
# 11 Andalusia          7604827  766401     9.16 (5-10%]  
# 12 Basque Country     2000938  186484     8.53 (5-10%]  
# 13 Cantabria           534316   47167     8.11 (5-10%]  
# 14 Galicia            2506411  209228     7.70 (5-10%]  
# 15 Castile and Leon   2267168  178889     7.31 (5-10%]  
# 16 Asturies            968589   73027     7.01 (5-10%]  
# 17 Extremadura        1042137   45301     4.17 <5%  

CAT<-df_Ex5|>
  group_by(prop_extc)|>
  summarise(regions=n())|>
  mutate(n_rel = round(regions / sum(regions)*100,2))|>
  mutate(prop_extc=as.factor(prop_extc),
         prop_extc=fct_relevel(prop_extc,c("<5%",
                                           "(5-10%]",
                                           "(10-15%]",
                                           "(15-20%]",
                                           ">20%")))|>
  arrange(prop_extc);CAT


# answer:
# # A tibble: 5 × 3
# prop_extc regions n_rel
# <fct>       <int> <dbl>
#   1 <5%             1  5.88
# 2 (5-10%]         7 41.2 
# 3 (10-15%]        4 23.5 
# 4 (15-20%]        4 23.5 
# 5 >20%            1  5.88






# Ex 6. #####

# Create a data frame with the total population living  in the municipality 
# of Madrid between the years 2000 and 2010 in which the rows are ordered 
# according to the year (from 2000 to 2010).

EX6<-data|> 
  filter(MUNICIPALITY=="Madrid",YEAR<=2010)|> 
  mutate(POP_TOT = rowSums(across(6:12)))|> 
  select(MUNICIPALITY,YEAR,POP_TOT)|> 
  arrange(YEAR);EX6


# answer:
#   MUNICIPALITY YEAR POP_TOT
# 1        Madrid 2010 3273049
# 2        Madrid 2009 3255944
# 3        Madrid 2008 3213271
# 4        Madrid 2007 3132463
# 5        Madrid 2006 3128600
# 6        Madrid 2005 3155359
# 7        Madrid 2004 3099834
# 8        Madrid 2003 3092759
# 9        Madrid 2002 3016788
# 10       Madrid 2001 2957058
# 11       Madrid 2000 2880148



# Ex 7.#######

DF <- data |>
  filter(COM == "Catalonia" & YEAR == 2000) |>
  mutate(
    POP_EU = rowSums(across(POP_WESTERNEUROPE:POP_EASTERNEUROPE)),
    POP_TOT = rowSums(across(6:12)),
    PROP_EU = round(POP_EU / POP_TOT * 100, 2)
  ) |>
  select(COM, PROVINCE, MUNICIPALITY, POP_EU, POP_TOT, PROP_EU) |>
  mutate(POP_EUP = sum(POP_EU), .by = c(PROVINCE))  |>
  mutate(POP_EUC = sum(POP_EU), .by = c(COM))

bind_rows(
  DF |> slice_max(PROP_EU),
  DF |> slice_max(POP_EU),
  DF |> slice_min(POP_EUP) |> slice_head())

# output:
#   COM  PROVINCE          MUNICIPALITY POP_EU POP_TOT PROP_EU POP_EUP POP_EUC
# 1 Catalonia Barcelona Castellet i la Gornal    342    1404   24.36   55626   83277
# 2 Catalonia Barcelona             Barcelona  20705 1494970    1.38   55626   83277
# 3 Catalonia    Lleida    Abella de la Conca     15     197    7.61    3324   83277


# Q1: Which was the Catalan municipality with the higher share of European-born population?
# Answer: Castellet i la Gornal - 24.36%
# Q2: Which was the Catalan municipality with the higher amount of Western Europeans?
# Answer: Barcelona - 20,705
# Q3: And the province with the lower amount of Western Europeans?
# Answer: The province - "Lleida". The municipality: "iAbella de la Conca" - 15 
# Q4: How many Western Europeans were living in Catalonia in the year 2000?
# Answer: 83,277 
