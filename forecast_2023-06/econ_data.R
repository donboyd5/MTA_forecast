
# https://fred.stlouisfed.org/series/NYWTOT bea wages nys through q4 2022

# Source: U.S. Bureau of Economic Analysis  Source: Federal Reserve Bank of St. Louis  Release: Personal Income by State  
# Units:  Thousands of Dollars, Seasonally Adjusted Annual Rate
# Frequency:  Quarterly
# Suggested Citation:
#   U.S. Bureau of Economic Analysis and Federal Reserve Bank of St. Louis, Total Wages and Salaries in New York [NYWTOT], retrieved from FRED, Federal Reserve Bank of St. Louis; https://fred.stlouisfed.org/series/NYWTOT, May 27, 2023.


# Total Quarterly Wages in New York-Newark-Jersey City, NY-NJ-PA (MSA) (ENUC356230010SA)
# https://fred.stlouisfed.org/series/ENUC356230010SA


# Total Quarterly Wages in New York-Newark-Jersey City, NY-NJ-PA (MSA) [ENUC356230010]
# Units:  Dollars, Not Seasonally Adjusted
# Frequency:  Quarterly
# Total wages are the wages paid by Unemployment Insurance covered employers during the calendar quarter, regardless of when the services were performed. Included in wages are pay for vacation and other paid leave, bonuses, stock options, tips, the cash val
# Suggested Citation:
#   U.S. Bureau of Labor Statistics and Federal Reserve Bank of St. Louis, Total Quarterly Wages in New York-Newark-Jersey City, NY-NJ-PA (MSA) [ENUC356230010], retrieved from FRED, Federal Reserve Bank of St. Louis; https://fred.stlouisfed.org/series/ENUC356230010, May 28, 2023.
# https://fred.stlouisfed.org/series/ENUC356230010 through q32022


#   New York (NY):
# Bronx County
# Kings County (Brooklyn)
# New York County (Manhattan)
# Queens County
# Richmond County (Staten Island)

#   New Jersey (NJ):
# Bergen County
# Essex County
# Hudson County
# Hunterdon County
# Middlesex County
# Monmouth County
# Morris County
# Ocean County
# Passaic County
# Somerset County
# Sussex County
# Union County
# Warren County

#   Pennsylvania (PA):
# Pike County



# includes ----------------------------------------------------------------

source(here::here("r", "libraries.r"))
source(here::here("r", "libraries_ts.r"))
source(here::here("r", "constants.r"))
source(here::here("r", "functions.r"))


# FRED --------------------------------------------------------------------

# https://fred.stlouisfed.org/series/NYWTOT bea wages nys through q4 2022
# https://fred.stlouisfed.org/series/ENUC356230010SA bls qcew ny nj msa wages sa through q2 2022
# https://fred.stlouisfed.org/series/ENUC356230010 bls qcew ny nj msa wages ns through q2 2022


vroom("https://fred.stlouisfed.org/series/ENUC105830010")

# fredr(
#   series_id = "UNRATE",
#   observation_start = as.Date("1990-01-01"),
#   observation_end = as.Date("2000-01-01")
# )

fredr(
  series_id = "UNRATE",
  observation_start = as.Date("1990-01-01"),
  observation_end = as.Date("2000-01-01")
)

df <- fredr(series_id="ENUC356230010")
ht(df)

# NYS DOL -----
# https://dol.ny.gov/quarterly-census-employment-and-wages
# https://gcc02.safelinks.protection.outlook.com/?url=https%3A%2F%2Fstatistics.labor.ny.gov%2Fqcew.zip&data=05%7C01%7CDOLCommun%40labor.ny.gov%7Ce17924198ffe48b036d908dad3bab9c6%7Cf46cb8ea79004d108ceb80e8c1c81ee7%7C0%7C0%7C638055096177546638%7CUnknown%7CTWFpbGZsb3d8eyJWIjoiMC4wLjAwMDAiLCJQIjoiV2luMzIiLCJBTiI6Ik1haWwiLCJXVCI6Mn0%3D%7C3000%7C%7C%7C&sdata=AJa0N6xpatr66uFPqtYLzDheBFOBkjDCG87BGQS1ZHc%3D&reserved=0

# DATA ELEMENTS AND DEFINITIONS for qcew_quarterly.txt
# Data Element	Definition
# AREATYPE        Type of Area (State, MSA, Labor Market Region, LWIA, County)
# AREA		    Area Name
# YEAR		    Year
# QUARTER         Quarter of the year (1,2,3,4)
# NAICS           NAICS industry classification code
# NAICS_LEVEL           NAICS industry level
# TITLE           NAICS Title
# ESTAB		    Number of establishments
# MNTH1EMP        Number employed for first month of given quarter
# MNTH2EMP        Number employed in second month
# MNTH3EMP        Number employed in third month
# TOTWAGE	    Total wages
# OWNER	Private or Government


files <- fs::dir_ls(path=here::here("data", "nyqcew"),
                    glob = "*csv")
files

df <- vroom(files)
glimpse(df)
names(df)
# [1] "AREATYPE"    "AREA"        "YEAR"        "QUARTER"     "NAICS"       "NAICS_LEVEL" "NAICS_TITLE" "ESTAB"       "MNTH1EMP"    "MNTH2EMP"   
# [11] "MNTH3EMP"    "TOTWAGE"     "Owner"  
count(df, AREATYPE) # County, State
count(df |> filter(AREATYPE=="County"), AREA)
count(df, Owner)
# Owner                              n
# <chr>                          <int>
# 1 Federal Government            102215
# 2 Local Government              245916
# 3 Private                      7052970
# 4 State Government              134571
# 5 Total Government              558464
# 6 Total Private and Government 8068156

tmp <- count(df, NAICS, NAICS_LEVEL, NAICS_TITLE)
# 00 All industries
# 61 NAICS 61: Educational Services
# 6111 4 NAICS 6111: Elementary and Secondary Schools [level 4]

tmp |> filter(str_detect(NAICS_TITLE, "Colleg"))
tmp |> filter(str_starts(NAICS_TITLE, "NAICS 61"))

mtaregion <- read_csv("area
Bronx County
Kings County
New York County
Queens County
Richmond County

Dutchess County
Nassau County
Orange County
Putnam County
Rockland County
Suffolk County
Westchester County
")

df1 <- df |> 
  rename_with(tolower) |> 
  filter(areatype %in% c("County", "State"),
         owner %in% c("Total Private and Government", "Local Government", "Federal Government")) |> 
  filter(naics=="00" | (naics_level==4 & naics=="6111"))

saveRDS(df1, here::here("data", "nyqcew", "mtaqcew_raw.rds"))


# -------------------------------------------------------------------------

mtawage1 <- readRDS(here::here("data", "nyqcew", "mtaqcew_raw.rds"))
count(mtawage1, year, quarter) |> ht()

mtawage2 <- mtawage1 |> 
  filter(area=="New York State" | area %in% mtaregion$area) |> 
  mutate(date=yq(paste0(year, "-", quarter)),
         areatype=str_to_lower(areatype),
         region=case_when(area %in% mtaregion$area[1:5] ~ "nyc",
                          area %in% mtaregion$area[6:12] ~ "suburban",
                          area == "New York State" ~ "nys",
                          TRUE ~ "ERROR"),
         owncode=factor(owner, 
                        levels=c("Total Private and Government", "Local Government", "Federal Government"),
                        labels=c("total", "locgov", "fedgov")),
         ind=case_when(naics=="00" ~ "allind",
                       naics=="6111" ~ "k12"))
count(mtawage2, areatype, region, area)
count(mtawage2, owncode, owner)
count(mtawage2, ind, naics, naics_level, naics_title)

mtawage3 <- mtawage2 |> 
  select(date, year, qtr=quarter, region, area, ind, owncode, estab:totwage) |> 
  pivot_longer(cols=estab:totwage)

saveRDS(mtawage3, here::here("data", "nyqcew", "mtaqcew_slim.rds"))


# mta base ----------------------------------------------------------------

mtawage3 <- readRDS(here::here("data", "nyqcew", "mtaqcew_slim.rds"))

mtawage3 |> 
  filter(area=="Dutchess County",
         ind=="allind",
         name=="totwage") |> 
  filter(year(date)>=2010) |> 
  ggplot(aes(date, value)) +
  geom_line() +
  geom_point() +
  geom_smooth() +
  scale_x_date(breaks = "3 months") +
  facet_wrap(~owncode, scales="free_y", ncol=1) +
  theme(axis.text.x = element_text(angle = -90, vjust = 0, hjust=0.5)) 

check <- mtawage3  |> filter(area=="New York County", date==max(date))
check <- mtawage3  |> filter(region=="nyc", date==max(date))

xk12 <-  mtawage3 |> 
  filter(ind=="k12" & owncode=="locgov")

xk12all <-  mtawage3 |> 
  filter(ind=="k12")
xk12all |> filter(region=="nys", date==max(date))
xk12all |> filter(region=="nyc", date==max(date)) # no local gov k12 data for nyc -- so must subtract the total owner

xfed <- mtawage3 |> 
  filter(ind=="allind" & owncode=="fedgov")
xfed |> filter(region=="nys", date==max(date))
xfed |> filter(region=="nyc", date==max(date))

count(xk12, area)


mtabase1 <- mtawage3 |> 
  filter(ind=="allind", owncode=="total") |> 
  select(-c(ind, owncode)) |> 
  rename(total=value) |> 
  left_join(xk12all |> 
              filter(owncode=="total") |> # use this due to data limitations
              select(date, area, name, xk12=value),
            by = join_by(date, area, name)) |> 
  left_join(xfed |> 
              select(date, area, name, xfed=value),
            by = join_by(date, area, name)) |> 
  mutate(pmtbase=total - naz(xk12) - naz(xfed))

count(mtabase1, area, region)

mtabase1 |> filter(name=="totwage") |> select(total, pmtbase) |> cor()

mtabase1 |> 
  filter(name=="totwage") |> 
  summarise(total=sum(total), .by=c(date, region)) |> 
  ggplot(aes(date, total)) +
  geom_line() +
  geom_point() +
  geom_smooth() +
  scale_x_date(breaks = "3 months") +
  facet_wrap(~region, scales="free_y", ncol=1) +
  theme(axis.text.x = element_text(angle = -90, vjust = 0, hjust=0.5)) 


mtabase1 |> 
  filter(name=="totwage") |> 
  summarise(total=sum(total), .by=c(date, region)) |> 
  arrange(date) |> 
  mutate(pch=total / lag(total) - 1, .by=region) |> 
  mutate(region=factor(region, levels=c("nyc", "suburban", "nys"))) |> 
  filter(year(date) >= 2010) |> 
  ggplot(aes(date, pch)) +
  geom_line() +
  geom_point() +
  geom_smooth() +
  geom_hline(yintercept = 0) +
  scale_x_date(name=NULL, breaks = "3 months") +
  scale_y_continuous(name="% change from year ago", 
                     breaks=seq(-.5, .5, .05),
                     labels=label_percent(accuracy=1)) +
  facet_wrap(~region, ncol=1) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = -90, vjust = 0, hjust=0.5))



