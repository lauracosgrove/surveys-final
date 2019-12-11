Mammogram
================

Read in the data.

``` r
# read in cancer module data
cancer = read_csv("./data/cancerxx.csv") %>%
  select(HHX, FMX, FPX, WTFA_SA, STRAT_P, PSU_P, REGION, MAMHAD, MAM6YR, 
         RMAM1_MT, RMAM1YR, RMAM1N, RMAM1T, RMAM2, RMAM3A, RMAM3B, MAMPAY, 
         MAMREAS, MDRECMAM, MAMDNBR, MAMABN1, MFOLLOW0, MFOLLO01, MFOLLO02, 
         MFOLLO03, MFOLLO04, MFOLLO05, MNOTFOL1, MAMMODE, MAMCAN1)

# read in adult data
adult = read_csv("./data/samadult.csv") %>%
  select(HHX, FMX, FPX, AUSUALPL, AHCPLROU, AHCPLKND, FLA1AR)

# read in family data
family = read_csv("./data/familyxx.csv") %>%
  select(HHX, FMX, RAT_CAT4, RAT_CAT5)

# read in person data
person = read_csv("./data/personsx.csv") %>%
  select(HHX, FMX, FPX, AGE_P, EDUC1, SEX, NOTCOV, COVER65, COVER65O, LA1AR,
         LCONDRT, LACHRONR, HISCODI3, RACRECI3, COVER, YRSINUS, PLBORN)

# join these data files into one for analysis
mam_dat = cancer %>%
  left_join(adult, by = c("HHX", "FMX", "FPX")) %>%
  left_join(person, by = c("HHX", "FMX", "FPX")) %>%
  left_join(family, by = c("HHX", "FMX")) 
```

Data Manipulation

``` r
# CREATE VARIABLES FOR ANALYSIS: OUTCOME AND DEMOGRAPHIC

# outcome is having a mammogram in the last 2 years:RMAM3A = 1,2
# create immigration status variable based on PLBORN and YRSINUS
mam_dat = mam_dat %>%
  mutate(mam_2 = if_else(RMAM3A <= 2, 1, 0),
         imm_stat = case_when(YRSINUS < 4 ~ "In U.S. < 10 yrs",
                              YRSINUS == 4 | YRSINUS == 5 ~ "In U.S. >= 10 yrs",
                              PLBORN == 1 ~ "Born in U.S."))

# create the age category
mam_dat = mam_dat %>% 
  mutate(age_cat = case_when(AGE_P >= 25 & AGE_P < 40 ~ "25–39",
                             AGE_P >= 40 & AGE_P < 50 ~ "40–49",
                             AGE_P >= 50 & AGE_P < 65 ~ "50–64",
                             AGE_P >= 65 ~ "65+"))
# create educ category
mam_dat = mam_dat %>% 
  mutate(educ_cat = case_when(EDUC1 < 13 ~ "Less than high school",
                              EDUC1 >= 13 & EDUC1 < 15 ~ "High school",
                              EDUC1 >= 15 & EDUC1 < 18 ~ "Some college",
                              EDUC1 >= 18 & EDUC1 <= 21 ~ "College graduate"))

# create financial category
mam_dat = mam_dat %>% 
  mutate(finc_cat = case_when(RAT_CAT5 <= 7 |  RAT_CAT5 %in% c(15, 16) ~ "<200%",
                              RAT_CAT5 %in% c(8, 9) ~ "200–299%", 
                              RAT_CAT5 %in% c(10, 11) ~ "300–399%",
                              RAT_CAT5 >= 18 & EDUC1 <= 21 ~ "400–499%",
                              RAT_CAT5 == 14  ~">=500%",
                              RAT_CAT5 == 17  ~">=200%, no further detail",
                              RAT_CAT5 %in% c(96, 99) ~ "Unknown"))

# create usual care category
mam_dat = mam_dat %>% 
  mutate(ausualpl_cat  = case_when(AUSUALPL == 2 ~ "No",
                                   AUSUALPL %in% c(1, 3) ~ "Yes",
                                   AUSUALPL %in% c(7, 8, 9) ~ "Other"))
# create coverage status
mam_dat = mam_dat %>% 
  mutate(cover_cat  = case_when(NOTCOV == 1 | COVER == 4 | COVER65 == 6 ~ "None",
                                COVER == 2 | COVER65 %in% 2:4 ~ "Public",
                                COVER %in% c(1, 3) | COVER65 %in% c(1, 5) ~ "Private/Military"))

# create disability variable
mam_dat = mam_dat %>% 
  mutate(lcond_chronic_cat = if_else(LCONDRT == 1, "Yes", "No"))

# create race & ethnic categories
mam_dat = mam_dat %>% 
  mutate(race_cat = case_when(RACRECI3 == 1 ~ "White",
                              RACRECI3 == 2 ~ "Black",
                              RACRECI3 == 3 ~ "Asian",
                              RACRECI3 == 4 ~ "AN/AI"),
         eth_cat = case_when(HISCODI3 == 1 ~ "Hispanic",
                             HISCODI3 == 2 ~ "Non-Hispanic White",
                             HISCODI3 == 3 ~ "Non-Hispanic Black",
                             HISCODI3 == 4 ~ "Non-Hispanic Asian",
                             HISCODI3 == 5 ~ "Non-Hispanic AN/AI"))
```

Survey Design

``` r
# create domain variable for inclusion criteria
mam_dat = mam_dat %>%
  mutate(domain = if_else(SEX == 2 & AGE_P >= 40, 1, 0))

# create survey design object
des = svydesign(ids = ~PSU_P, strata = ~STRAT_P, weights = ~WTFA_SA, nest = TRUE, data = mam_dat)
```

Tables

``` r
# percent of women who have had mammogram in the last two years by age

age_pct = svyby(~mam_2, by = ~domain+age_cat, svymean, na.rm = TRUE, 
                design = des, vartype = c("ci", "se"))
age_pct %>% filter(domain == 1) %>% select(-domain, -se) %>% knitr::kable()
```

| age\_cat |     mam\_2|      ci\_l|      ci\_u|
|:---------|----------:|----------:|----------:|
| 40–49    |  0.7609720|  0.7348160|  0.7871280|
| 50–64    |  0.7510356|  0.7339199|  0.7681512|
| 65+      |  0.6669962|  0.6481919|  0.6858006|

``` r
# percent of women who have had mammogram by education
edu_pct = svyby(~mam_2, by = ~domain+educ_cat, svymean, na.rm = TRUE, 
                design = des, vartype = c("se", "ci")) %>%
  filter(domain == 1) %>%
  select(-domain, -se)

edu_pct %>% 
  knitr::kable()
```

| educ\_cat             |     mam\_2|      ci\_l|      ci\_u|
|:----------------------|----------:|----------:|----------:|
| College graduate      |  0.7952259|  0.7759755|  0.8144763|
| High school           |  0.6785310|  0.6548180|  0.7022440|
| Less than high school |  0.6233975|  0.5880548|  0.6587402|
| Some college          |  0.7298794|  0.7085698|  0.7511891|

``` r
# percent of women who have had mammogram by financial category
finc_pct = svyby(~mam_2, by = ~domain+finc_cat, svymean, na.rm = TRUE, 
                 design = des, vartype = c("se", "ci")) %>%
  filter(domain == 1) %>%
  select(-domain, -se)

finc_pct %>% knitr::kable()
```

| finc\_cat                    |     mam\_2|      ci\_l|      ci\_u|
|:-----------------------------|----------:|----------:|----------:|
| &lt;200%                     |  0.6336404|  0.6102688|  0.6570120|
| &gt;=200%, no further detail |  0.6829528|  0.6182815|  0.7476241|
| &gt;=500%                    |  0.8049806|  0.7828196|  0.8271415|
| 200–299%                     |  0.6846054|  0.6473660|  0.7218448|
| 300–399%                     |  0.7389614|  0.7026038|  0.7753189|
| 400–499%                     |  0.7582742|  0.7263519|  0.7901964|
| Unknown                      |  0.4531864|  0.1993634|  0.7070094|

``` r
# percent of women who have had mammogram by usual care category
ausualp_pct = svyby(~mam_2, by = ~domain+ausualpl_cat, svymean, na.rm = TRUE, 
                    design = des, vartype = c("se", "ci")) %>%
  filter(domain == 1) %>%
  select(-domain, -se)

ausualp_pct %>% knitr::kable()
```

| ausualpl\_cat |     mam\_2|       ci\_l|      ci\_u|
|:--------------|----------:|-----------:|----------:|
| No            |  0.4426809|   0.3857382|  0.4996237|
| Other         |  0.6349977|  -0.0074398|  1.2774351|
| Yes           |  0.7374589|   0.7258591|  0.7490586|

``` r
# percent of women who have had mammogram by health coverage
cover_pct = svyby(~mam_2, by = ~domain+cover_cat, svymean, na.rm = TRUE, 
                  design = des, vartype = c("se", "ci")) %>%
  filter(domain == 1) %>%
  select(-domain, -se)

cover_pct %>% knitr::kable()
```

| cover\_cat       |     mam\_2|      ci\_l|      ci\_u|
|:-----------------|----------:|----------:|----------:|
| None             |  0.4711737|  0.4004064|  0.5419410|
| Private/Military |  0.7605363|  0.7467293|  0.7743434|
| Public           |  0.6643714|  0.6409525|  0.6877904|

``` r
# percent of women who have had mammogram by chronic conditions
lcond_chronic_pct = svyby(~mam_2, by = ~domain+lcond_chronic_cat, svymean, 
                          na.rm = TRUE, design = des, vartype = c("se", "ci")) %>%
  filter(domain == 1) %>%
  select(-domain, -se)

lcond_chronic_pct %>% knitr::kable()
```

| lcond\_chronic\_cat |     mam\_2|      ci\_l|      ci\_u|
|:--------------------|----------:|----------:|----------:|
| No                  |  0.6235415|  0.4602961|  0.7867868|
| Yes                 |  0.6105575|  0.5844282|  0.6366868|

``` r
# percent of women who have had mammogram by race
race_pct = svyby(~mam_2, by = ~domain+race_cat, svymean, na.rm = TRUE, 
                 design = des, vartype = c("se", "ci")) %>%
  filter(domain == 1) %>%
  select(-domain, -se)

race_pct %>% knitr::kable()
```

| race\_cat |     mam\_2|      ci\_l|      ci\_u|
|:----------|----------:|----------:|----------:|
| AN/AI     |  0.6382206|  0.5275676|  0.7488735|
| Asian     |  0.7023464|  0.6479896|  0.7567032|
| Black     |  0.7688270|  0.7385748|  0.7990792|
| White     |  0.7203100|  0.7071951|  0.7334248|

``` r
# percent of women who have had mammogram by ethnicity
eth_pct = svyby(~mam_2, by = ~domain+eth_cat, svymean, na.rm = TRUE, 
                design = des, vartype = c("se", "ci")) %>%
  filter(domain == 1) %>%
  select(-domain, -se)

eth_pct %>% knitr::kable()
```

| eth\_cat           |     mam\_2|      ci\_l|      ci\_u|
|:-------------------|----------:|----------:|----------:|
| Hispanic           |  0.7434129|  0.7136199|  0.7732058|
| Non-Hispanic AN/AI |  0.6787829|  0.5493178|  0.8082481|
| Non-Hispanic Asian |  0.7005102|  0.6454834|  0.7555370|
| Non-Hispanic Black |  0.7684556|  0.7377942|  0.7991169|
| Non-Hispanic White |  0.7165402|  0.7025792|  0.7305012|

``` r
# percent of women who have had mammogram by immigration status
imm_pct = svyby(~mam_2, by = ~domain+imm_stat, svymean, na.rm = TRUE,
                design = des, vartype = c("se", "ci")) %>%
  filter(domain == 1) %>%
  select(-domain, -se)

imm_pct %>% knitr::kable()
```

| imm\_stat            |     mam\_2|      ci\_l|      ci\_u|
|:---------------------|----------:|----------:|----------:|
| Born in U.S.         |  0.7247355|  0.7124950|  0.7369760|
| In U.S. &lt; 10 yrs  |  0.6920384|  0.5859504|  0.7981264|
| In U.S. &gt;= 10 yrs |  0.7232266|  0.6925968|  0.7538563|

Tables By Age Group

``` r
# percent of women who have had mammogram by education and age 
edu_pct_strat = svyby(~mam_2, by = ~domain + age_cat + educ_cat, svymean, 
                      na.rm = TRUE, design = des, vartype = c("se", "ci"))
edu_tab = edu_pct_strat %>% 
  filter(domain == 1) %>% 
  select(-domain, -se) 

edu_tab %>% knitr::kable()
```

| age\_cat | educ\_cat             |     mam\_2|      ci\_l|      ci\_u|
|:---------|:----------------------|----------:|----------:|----------:|
| 40–49    | College graduate      |  0.8085672|  0.7707166|  0.8464178|
| 50–64    | College graduate      |  0.8109270|  0.7831941|  0.8386598|
| 65+      | College graduate      |  0.7537526|  0.7172640|  0.7902411|
| 40–49    | High school           |  0.6818410|  0.6144456|  0.7492364|
| 50–64    | High school           |  0.7247121|  0.6848474|  0.7645768|
| 65+      | High school           |  0.6336142|  0.5983451|  0.6688834|
| 40–49    | Less than high school |  0.6999305|  0.6228325|  0.7770285|
| 50–64    | Less than high school |  0.6506867|  0.5928630|  0.7085104|
| 65+      | Less than high school |  0.5719782|  0.5172242|  0.6267323|
| 40–49    | Some college          |  0.7628288|  0.7164342|  0.8092234|
| 50–64    | Some college          |  0.7406601|  0.7105614|  0.7707588|
| 65+      | Some college          |  0.6909571|  0.6565986|  0.7253157|

``` r
# percent of women who have had mammogram by financial category and age
finc_pct_strat = svyby(~mam_2, by = ~domain + age_cat + finc_cat, svymean, 
                       na.rm = TRUE, design = des, vartype = c("se", "ci"))

finc_tab = finc_pct_strat %>% 
  filter(domain == 1) %>% 
  select(-domain, -se) 

finc_tab %>% knitr::kable()
```

| age\_cat | finc\_cat                    |     mam\_2|       ci\_l|      ci\_u|
|:---------|:-----------------------------|----------:|-----------:|----------:|
| 40–49    | &lt;200%                     |  0.6956259|   0.6448771|  0.7463748|
| 50–64    | &lt;200%                     |  0.6495575|   0.6101441|  0.6889709|
| 65+      | &lt;200%                     |  0.5848766|   0.5493209|  0.6204322|
| 40–49    | &gt;=200%, no further detail |  0.8594243|   0.7506689|  0.9681797|
| 50–64    | &gt;=200%, no further detail |  0.7350658|   0.6237214|  0.8464101|
| 65+      | &gt;=200%, no further detail |  0.6088342|   0.5150514|  0.7026171|
| 40–49    | &gt;=500%                    |  0.8120277|   0.7701024|  0.8539529|
| 50–64    | &gt;=500%                    |  0.8105147|   0.7804614|  0.8405680|
| 65+      | &gt;=500%                    |  0.7855235|   0.7417109|  0.8293361|
| 40–49    | 200–299%                     |  0.7072433|   0.6214273|  0.7930594|
| 50–64    | 200–299%                     |  0.7089483|   0.6459559|  0.7719406|
| 65+      | 200–299%                     |  0.6470099|   0.5991062|  0.6949135|
| 40–49    | 300–399%                     |  0.7310784|   0.6541823|  0.8079745|
| 50–64    | 300–399%                     |  0.7706667|   0.7051139|  0.8362195|
| 65+      | 300–399%                     |  0.7085731|   0.6507690|  0.7663771|
| 40–49    | 400–499%                     |  0.8174002|   0.7382089|  0.8965915|
| 50–64    | 400–499%                     |  0.7917000|   0.7382696|  0.8451305|
| 65+      | 400–499%                     |  0.7010436|   0.6564844|  0.7456028|
| 40–49    | Unknown                      |  0.0000000|   0.0000000|  0.0000000|
| 50–64    | Unknown                      |  0.9962176|   0.9879882|  1.0044469|
| 65+      | Unknown                      |  0.2035140|  -0.0544638|  0.4614919|

``` r
# percent of women who have had mammogram by usual care and age
ausualp_pct_strat = svyby(~mam_2, by = ~domain + age_cat + ausualpl_cat,
                          svymean, na.rm = TRUE, design = des, 
                          vartype = c("se", "ci"))

usual_tab = ausualp_pct_strat %>% 
  filter(domain == 1) %>% 
  select(-domain, -se) 

usual_tab %>% knitr::kable()
```

| age\_cat | ausualpl\_cat |     mam\_2|      ci\_l|      ci\_u|
|:---------|:--------------|----------:|----------:|----------:|
| 40–49    | No            |  0.5056792|  0.4057956|  0.6055628|
| 50–64    | No            |  0.3810310|  0.2889497|  0.4731123|
| 65+      | No            |  0.4840104|  0.3523452|  0.6156756|
| 40–49    | Other         |  0.0000000|  0.0000000|  0.0000000|
| 50–64    | Other         |  1.0000000|  1.0000000|  1.0000000|
| 65+      | Other         |  0.0000000|  0.0000000|  0.0000000|
| 40–49    | Yes           |  0.7802444|  0.7542141|  0.8062747|
| 50–64    | Yes           |  0.7700861|  0.7529459|  0.7872262|
| 65+      | Yes           |  0.6714421|  0.6525374|  0.6903469|

``` r
# percent of women who have had mammogram by health coverage and age
cover_pct_strat = svyby(~mam_2, by = ~domain + age_cat + cover_cat, svymean,
                        na.rm = TRUE, design = des, vartype = c("se", "ci"))

ins_tab = cover_pct_strat %>% 
  filter(domain == 1) %>% 
  select(-domain, -se)

ins_tab %>% knitr::kable()
```

| age\_cat | cover\_cat       |     mam\_2|      ci\_l|      ci\_u|
|:---------|:-----------------|----------:|----------:|----------:|
| 40–49    | None             |  0.4829030|  0.3629256|  0.6028804|
| 50–64    | None             |  0.4603852|  0.3769433|  0.5438272|
| 65+      | None             |  0.5160788|  0.2196597|  0.8124978|
| 40–49    | Private/Military |  0.7914360|  0.7633846|  0.8194874|
| 50–64    | Private/Military |  0.7802779|  0.7610562|  0.7994996|
| 65+      | Private/Military |  0.6871598|  0.6611642|  0.7131553|
| 40–49    | Public           |  0.7370242|  0.6725539|  0.8014945|
| 50–64    | Public           |  0.6818896|  0.6241168|  0.7396625|
| 65+      | Public           |  0.6486552|  0.6199508|  0.6773596|

``` r
# percent of women who have had mammogram by chronic conditions and age
lcond_chronic_pct_strat = svyby(~mam_2, 
                                by = ~domain + age_cat + lcond_chronic_cat,
                                svymean, na.rm = TRUE, design = des,
                                vartype = c("se", "ci"))

dis_tab = lcond_chronic_pct_strat %>% 
  filter(domain == 1) %>% 
  select(-domain, -se) 

dis_tab %>% knitr::kable()
```

| age\_cat | lcond\_chronic\_cat |     mam\_2|      ci\_l|      ci\_u|
|:---------|:--------------------|----------:|----------:|----------:|
| 40–49    | No                  |  0.7176907|  0.3453040|  1.0900773|
| 50–64    | No                  |  0.8619131|  0.7174233|  1.0064030|
| 65+      | No                  |  0.4720814|  0.2627660|  0.6813969|
| 40–49    | Yes                 |  0.6903587|  0.6133066|  0.7674109|
| 50–64    | Yes                 |  0.6848202|  0.6423921|  0.7272482|
| 65+      | Yes                 |  0.5381731|  0.5032309|  0.5731153|

``` r
# percent of women who have had mammogram by race and age
race_pct_strat = svyby(~mam_2, by = ~domain + age_cat + race_cat, svymean, 
                       na.rm = TRUE, design = des, vartype = c("se", "ci"))

race_tab = race_pct_strat %>% 
  filter(domain == 1) %>% 
  select(-domain, -se) 

race_tab %>% knitr::kable()
```

| age\_cat | race\_cat |     mam\_2|      ci\_l|      ci\_u|
|:---------|:----------|----------:|----------:|----------:|
| 40–49    | AN/AI     |  0.6820006|  0.4637318|  0.9002694|
| 50–64    | AN/AI     |  0.5832300|  0.4209941|  0.7454660|
| 65+      | AN/AI     |  0.6798373|  0.4792802|  0.8803944|
| 40–49    | Asian     |  0.7285098|  0.6249686|  0.8320510|
| 50–64    | Asian     |  0.7388677|  0.6510686|  0.8266667|
| 65+      | Asian     |  0.6051615|  0.5083183|  0.7020048|
| 40–49    | Black     |  0.8109375|  0.7584382|  0.8634368|
| 50–64    | Black     |  0.7983266|  0.7574828|  0.8391704|
| 65+      | Black     |  0.6804674|  0.6261574|  0.7347773|
| 40–49    | White     |  0.7567230|  0.7267415|  0.7867046|
| 50–64    | White     |  0.7467055|  0.7263464|  0.7670646|
| 65+      | White     |  0.6680536|  0.6481251|  0.6879822|

``` r
# percent of women who have had mammogram by ethnicity and age
eth_pct_strat = svyby(~mam_2, by = ~domain + age_cat + eth_cat, svymean, 
                      na.rm = TRUE, design = des, vartype = c("se", "ci"))

eth_tab = eth_pct_strat %>% 
  filter(domain == 1) %>% 
  select(-domain, -se) 

eth_tab %>% knitr::kable()
```

| age\_cat | eth\_cat           |     mam\_2|      ci\_l|      ci\_u|
|:---------|:-------------------|----------:|----------:|----------:|
| 40–49    | Hispanic           |  0.7350049|  0.6860044|  0.7840053|
| 50–64    | Hispanic           |  0.7887929|  0.7425060|  0.8350797|
| 65+      | Hispanic           |  0.6757057|  0.6174945|  0.7339170|
| 40–49    | Non-Hispanic AN/AI |  0.8614933|  0.7233204|  0.9996662|
| 50–64    | Non-Hispanic AN/AI |  0.5788007|  0.3741846|  0.7834168|
| 65+      | Non-Hispanic AN/AI |  0.6618173|  0.4220170|  0.9016176|
| 40–49    | Non-Hispanic Asian |  0.7255322|  0.6191024|  0.8319621|
| 50–64    | Non-Hispanic Asian |  0.7358176|  0.6461917|  0.8254434|
| 65+      | Non-Hispanic Asian |  0.6088310|  0.5113248|  0.7063372|
| 40–49    | Non-Hispanic Black |  0.8174351|  0.7624666|  0.8724036|
| 50–64    | Non-Hispanic Black |  0.7971501|  0.7556416|  0.8386587|
| 65+      | Non-Hispanic Black |  0.6774079|  0.6234387|  0.7313772|
| 40–49    | Non-Hispanic White |  0.7584481|  0.7227714|  0.7941248|
| 50–64    | Non-Hispanic White |  0.7405940|  0.7183667|  0.7628213|
| 65+      | Non-Hispanic White |  0.6676896|  0.6471933|  0.6881859|

``` r
# percent of women who have had mammogram by immigration and age
imm_pct_strat = svyby(~mam_2, by = ~domain + age_cat + imm_stat, svymean,
                      na.rm = TRUE, design = des, vartype = c("se", "ci"))

imm_tab = imm_pct_strat %>%
  filter(domain == 1) %>%
  select(-domain, -se)

imm_tab %>% knitr::kable()
```

| age\_cat | imm\_stat            |     mam\_2|      ci\_l|      ci\_u|
|:---------|:---------------------|----------:|----------:|----------:|
| 40–49    | Born in U.S.         |  0.7648196|  0.7336183|  0.7960209|
| 50–64    | Born in U.S.         |  0.7475203|  0.7283139|  0.7667267|
| 65+      | Born in U.S.         |  0.6740989|  0.6550582|  0.6931396|
| 40–49    | In U.S. &lt; 10 yrs  |  0.6718178|  0.5154111|  0.8282245|
| 50–64    | In U.S. &lt; 10 yrs  |  0.7269500|  0.5430927|  0.9108074|
| 65+      | In U.S. &lt; 10 yrs  |  0.6844641|  0.4369705|  0.9319578|
| 40–49    | In U.S. &gt;= 10 yrs |  0.7543553|  0.6990657|  0.8096448|
| 50–64    | In U.S. &gt;= 10 yrs |  0.7729488|  0.7269979|  0.8188997|
| 65+      | In U.S. &gt;= 10 yrs |  0.6163448|  0.5632557|  0.6694340|

``` r
# overall percent of women who have had mammogram
total = svyby(~mam_2, by = ~domain + age_cat, svymean, na.rm = TRUE, 
              design = des, vartype = c("se", "ci"))

tot_tab = total %>% 
  filter(domain == 1) %>% 
  select(-domain, -se) 

tot_tab %>% knitr::kable()
```

| age\_cat |     mam\_2|      ci\_l|      ci\_u|
|:---------|----------:|----------:|----------:|
| 40–49    |  0.7609720|  0.7348160|  0.7871280|
| 50–64    |  0.7510356|  0.7339199|  0.7681512|
| 65+      |  0.6669962|  0.6481919|  0.6858006|

``` r
tot_pct = svyby(~mam_2, by = ~domain, svymean, na.rm = TRUE, design = des,
                vartype = c("se", "ci")) %>%
  filter(domain == 1) %>%
  select(-domain, -se) %>%
  mutate(age_cat = "40+")
```

Combine into paper table

``` r
# COMBINE TABLES BY AGE WITH TABLES OVERALL

# overall
tot_tab = rbind(tot_pct, tot_tab)

tot_tab2 = tot_tab %>%
  mutate(type = "Total",
         level = "-")

# education
edu_pct2 = edu_pct %>%
  mutate(age_cat = "40+")

edu_tab = rbind(edu_pct2, edu_tab)

# factor levels for presentation
edu_tab2 = edu_tab %>%
  mutate(type = "Education") %>%
  rename(level = educ_cat) %>%
  mutate(level = factor(level, levels = c("Less than high school", "High school", "Some college", "College graduate"))) %>%
  arrange(level)

# financial category
finc_pct2 = finc_pct %>%
  mutate(age_cat = "40+")

finc_tab = rbind(finc_pct2, finc_tab)

# factor levels for presentation
finc_tab2 = finc_tab %>%
  mutate(type = "Family Income Poverty Ratio") %>%
  rename(level = finc_cat) %>%
  mutate(level = factor(level, levels = c("<200%", ">=200%, no further detail", "200–299%", "300–399%", "400–499%", ">=500%", "Unknown"))) %>%
  arrange(level)

# usual care category
ausualp_pct2 = ausualp_pct %>%
  mutate(age_cat = "40+")

usual_tab = rbind(ausualp_pct2, usual_tab)


# factor levels for presentation
usual_tab2 = usual_tab %>%
  mutate(type = "Usual Source of Care") %>%
  rename(level = ausualpl_cat) %>%
  mutate(level = factor(level, levels = c("No", "Yes", "Other"))) %>%
  arrange(level)

# insurance coverage category
cover_pct2 = cover_pct %>%
  mutate(age_cat = "40+")

ins_tab = rbind(cover_pct2, ins_tab)

# factor levels for presentation
ins_tab2 = ins_tab %>%
  mutate(type = "Insurance Type") %>%
  rename(level = cover_cat) %>%
  mutate(level = factor(level, levels = c("None", "Public", "Private/Military"))) %>%
  arrange(level)

# chronic condition category
lcond_chronic_pct2 = lcond_chronic_pct %>%
  mutate(age_cat = "40+")

dis_tab = rbind(lcond_chronic_pct2, dis_tab)

# factor levels for presentation
dis_tab2 = dis_tab %>%
  mutate(type = "Chronic Disability") %>%
  rename(level = lcond_chronic_cat) %>%
  mutate(level = factor(level, levels = c("Yes", "No"))) %>%
  arrange(level)

# ethnic category
eth_pct2 = eth_pct %>%
  mutate(age_cat = "40+")

eth_tab = rbind(eth_pct2, eth_tab)

# factor levels for presentation
eth_tab2 = eth_tab %>%
  mutate(type = "Ethnicity") %>%
  rename(level = eth_cat) %>%
  mutate(level = factor(level, levels = c("Hispanic", "Non-Hispanic White", "Non-Hispanic Black", "Non-Hispanic AN/AI", "Non-Hispanic Asian"))) %>%
  arrange(level)

# race category
race_pct2 = race_pct %>%
  mutate(age_cat = "40+")

race_tab = rbind(race_pct2, race_tab)

# factor levels for presentation
race_tab2 = race_tab %>%
  mutate(type = "Race") %>%
  rename(level = race_cat) %>%
  mutate(level = factor(level, levels = c("White", "Black", "AN/AI", "Asian"))) %>%
  arrange(level)

# immigration category
imm_pct2 = imm_pct %>%
  mutate(age_cat = "40+")

imm_tab = rbind(imm_pct2, imm_tab)

# factor levels for presentation
imm_tab2 = imm_tab %>%
  mutate(type = "Immigration") %>%
  rename(level = imm_stat) %>%
  mutate(level = factor(level, levels = c("In U.S. < 10 yrs", "In U.S. >= 10 yrs", "Born in U.S."))) %>%
  arrange(level)

# create table of percentages of women who have gotten mammograms within the last two years (still need to add CIs)
tab_one = rbind(tot_tab2, edu_tab2, finc_tab2, usual_tab2, ins_tab2, dis_tab2, eth_tab2, race_tab2, imm_tab2) %>%
  mutate(mam_2 = round(mam_2*100, 2),
         ci_l = round(ci_l*100, 2),
         ci_u = round(ci_u*100, 2),
         CI = str_c(ci_l, ", ", ci_u)) %>%
  rename(Percent = mam_2) %>%
  select(-ci_l, -ci_u) %>%
  pivot_wider(names_from = age_cat, values_from = c(Percent, CI)) %>%
  janitor::clean_names() %>%
  select(type, level, percent_40, ci_40, percent_40_49, ci_40_49, percent_50_64, ci_50_64, everything())

# print percentages
tab_one %>% knitr::kable()
```

| type                        | level                        |  percent\_40| ci\_40        |  percent\_40\_49| ci\_40\_49    |  percent\_50\_64| ci\_50\_64    |  percent\_65| ci\_65       |
|:----------------------------|:-----------------------------|------------:|:--------------|----------------:|:--------------|----------------:|:--------------|------------:|:-------------|
| Total                       | -                            |        72.42| 71.28, 73.57  |            76.10| 73.48, 78.71  |            75.10| 73.39, 76.82  |        66.70| 64.82, 68.58 |
| Education                   | Less than high school        |        62.34| 58.81, 65.87  |            69.99| 62.28, 77.7   |            65.07| 59.29, 70.85  |        57.20| 51.72, 62.67 |
| Education                   | High school                  |        67.85| 65.48, 70.22  |            68.18| 61.44, 74.92  |            72.47| 68.48, 76.46  |        63.36| 59.83, 66.89 |
| Education                   | Some college                 |        72.99| 70.86, 75.12  |            76.28| 71.64, 80.92  |            74.07| 71.06, 77.08  |        69.10| 65.66, 72.53 |
| Education                   | College graduate             |        79.52| 77.6, 81.45   |            80.86| 77.07, 84.64  |            81.09| 78.32, 83.87  |        75.38| 71.73, 79.02 |
| Family Income Poverty Ratio | &lt;200%                     |        63.36| 61.03, 65.7   |            69.56| 64.49, 74.64  |            64.96| 61.01, 68.9   |        58.49| 54.93, 62.04 |
| Family Income Poverty Ratio | &gt;=200%, no further detail |        68.30| 61.83, 74.76  |            85.94| 75.07, 96.82  |            73.51| 62.37, 84.64  |        60.88| 51.51, 70.26 |
| Family Income Poverty Ratio | 200–299%                     |        68.46| 64.74, 72.18  |            70.72| 62.14, 79.31  |            70.89| 64.6, 77.19   |        64.70| 59.91, 69.49 |
| Family Income Poverty Ratio | 300–399%                     |        73.90| 70.26, 77.53  |            73.11| 65.42, 80.8   |            77.07| 70.51, 83.62  |        70.86| 65.08, 76.64 |
| Family Income Poverty Ratio | 400–499%                     |        75.83| 72.64, 79.02  |            81.74| 73.82, 89.66  |            79.17| 73.83, 84.51  |        70.10| 65.65, 74.56 |
| Family Income Poverty Ratio | &gt;=500%                    |        80.50| 78.28, 82.71  |            81.20| 77.01, 85.4   |            81.05| 78.05, 84.06  |        78.55| 74.17, 82.93 |
| Family Income Poverty Ratio | Unknown                      |        45.32| 19.94, 70.7   |             0.00| 0, 0          |            99.62| 98.8, 100.44  |        20.35| -5.45, 46.15 |
| Usual Source of Care        | No                           |        44.27| 38.57, 49.96  |            50.57| 40.58, 60.56  |            38.10| 28.89, 47.31  |        48.40| 35.23, 61.57 |
| Usual Source of Care        | Yes                          |        73.75| 72.59, 74.91  |            78.02| 75.42, 80.63  |            77.01| 75.29, 78.72  |        67.14| 65.25, 69.03 |
| Usual Source of Care        | Other                        |        63.50| -0.74, 127.74 |             0.00| 0, 0          |           100.00| 100, 100      |         0.00| 0, 0         |
| Insurance Type              | None                         |        47.12| 40.04, 54.19  |            48.29| 36.29, 60.29  |            46.04| 37.69, 54.38  |        51.61| 21.97, 81.25 |
| Insurance Type              | Public                       |        66.44| 64.1, 68.78   |            73.70| 67.26, 80.15  |            68.19| 62.41, 73.97  |        64.87| 62, 67.74    |
| Insurance Type              | Private/Military             |        76.05| 74.67, 77.43  |            79.14| 76.34, 81.95  |            78.03| 76.11, 79.95  |        68.72| 66.12, 71.32 |
| Chronic Disability          | Yes                          |        61.06| 58.44, 63.67  |            69.04| 61.33, 76.74  |            68.48| 64.24, 72.72  |        53.82| 50.32, 57.31 |
| Chronic Disability          | No                           |        62.35| 46.03, 78.68  |            71.77| 34.53, 109.01 |            86.19| 71.74, 100.64 |        47.21| 26.28, 68.14 |
| Ethnicity                   | Hispanic                     |        74.34| 71.36, 77.32  |            73.50| 68.6, 78.4    |            78.88| 74.25, 83.51  |        67.57| 61.75, 73.39 |
| Ethnicity                   | Non-Hispanic White           |        71.65| 70.26, 73.05  |            75.84| 72.28, 79.41  |            74.06| 71.84, 76.28  |        66.77| 64.72, 68.82 |
| Ethnicity                   | Non-Hispanic Black           |        76.85| 73.78, 79.91  |            81.74| 76.25, 87.24  |            79.72| 75.56, 83.87  |        67.74| 62.34, 73.14 |
| Ethnicity                   | Non-Hispanic AN/AI           |        67.88| 54.93, 80.82  |            86.15| 72.33, 99.97  |            57.88| 37.42, 78.34  |        66.18| 42.2, 90.16  |
| Ethnicity                   | Non-Hispanic Asian           |        70.05| 64.55, 75.55  |            72.55| 61.91, 83.2   |            73.58| 64.62, 82.54  |        60.88| 51.13, 70.63 |
| Race                        | White                        |        72.03| 70.72, 73.34  |            75.67| 72.67, 78.67  |            74.67| 72.63, 76.71  |        66.81| 64.81, 68.8  |
| Race                        | Black                        |        76.88| 73.86, 79.91  |            81.09| 75.84, 86.34  |            79.83| 75.75, 83.92  |        68.05| 62.62, 73.48 |
| Race                        | AN/AI                        |        63.82| 52.76, 74.89  |            68.20| 46.37, 90.03  |            58.32| 42.1, 74.55   |        67.98| 47.93, 88.04 |
| Race                        | Asian                        |        70.23| 64.8, 75.67   |            72.85| 62.5, 83.21   |            73.89| 65.11, 82.67  |        60.52| 50.83, 70.2  |
| Immigration                 | In U.S. &lt; 10 yrs          |        69.20| 58.6, 79.81   |            67.18| 51.54, 82.82  |            72.70| 54.31, 91.08  |        68.45| 43.7, 93.2   |
| Immigration                 | In U.S. &gt;= 10 yrs         |        72.32| 69.26, 75.39  |            75.44| 69.91, 80.96  |            77.29| 72.7, 81.89   |        61.63| 56.33, 66.94 |
| Immigration                 | Born in U.S.                 |        72.47| 71.25, 73.7   |            76.48| 73.36, 79.6   |            74.75| 72.83, 76.67  |        67.41| 65.51, 69.31 |

Barplots of Usual Care and Insurance Coverage

``` r
# usual source of care barchart
usual_tab %>%
  filter(age_cat != "40+" & ausualpl_cat != "Other") %>%
  ggplot(aes(x = ausualpl_cat, y = mam_2, fill = ausualpl_cat)) +
  geom_col() +
  scale_y_continuous(limits = c(0,1), breaks = c(0, 0.25, 0.5, 0.75, 1)) +
  geom_errorbar(aes(ymin = ci_l, ymax = ci_u)) +
  facet_grid(~age_cat) + ggthemes::theme_few() + ggthemes::scale_fill_few() + theme(legend.position = "none") +
  labs(y = "Percent Had Mammogram, Last 2 Years", x = "Usual Source of Care (Have/Have Not)")
```

![](mammogram_files/figure-markdown_github/unnamed-chunk-4-1.png)

``` r
# insurance type barchart
ins_tab %>%
  filter(age_cat != "40+") %>%
  ggplot(aes(x = cover_cat, y = mam_2, fill = cover_cat)) +
  geom_col() + 
  scale_y_continuous(limits = c(0,1), breaks = c(0, 0.25, 0.5, 0.75, 1)) +
  geom_errorbar(aes(ymin = ci_l, ymax = ci_u)) +
  facet_grid(~age_cat) + ggthemes::theme_few() + ggthemes::scale_fill_few() + theme(legend.position = "none") +
  labs(y = "Percent Had Mammogram, Last 2 Years", x = "Type of Insurance Coverage")
```

![](mammogram_files/figure-markdown_github/unnamed-chunk-4-2.png)

Models

Age cateogry, education level, financial status, usual care, insurance type, and ethnicity are significant predictors of having a recent mammogram. 
The OR for having a recent mammogram when comparing 50-64 year old women to 40-49 year old women is 0.9448. 
The OR for having a recent mammogram when comparing 65+ women to 40-49 year old women is 0.6784. 
The OR for having a recent mammogram when comparing women who only have a high school degree to women who have a college degree is 0.6985. 
The OR for having a recent mammogram when comparing women who have less than a high school degree to women who have a college degree is 0.5821. 
The OR for having a recent mammogram when comparing women who have some college education to women who have a college degree is 0.7943. 
The OR for having a recent mammogram when comparing women who are only known as having a poverty ratio of &gt;200% to women who have a poverty ratio &lt;200% is 1.1436. 
The OR for having a recent mammogram when comparing women who have a poverty ratio between 200-299% to women who have a poverty ratio &lt;200% is 1.1586. 
The OR for having a recent mammogram when comparing women who have a poverty ratio between 300-399% to women who have a poverty ratio &lt;200% is 1.4398. 
The OR for having a recent mammogram when comparing women who have a poverty ratio between 400-499% to women who have a poverty ratio &lt;200% is 1.5428. 
The OR for having a recent mammogram when comparing women who have a poverty ratio &gt;=500% to women who have a poverty ratio &lt;200% is 1.8111. 
The OR for having a recent mammogram when comparing having a source of usual care to not having a source of usual care is 2.8878. 
The OR for having a recent mammogram when comparing having private/military insurance to none is 2.5213. 
The OR for having a recent mammogram when comparing having public insurance to none is 2.3978. 
The OR for having a recent mammogram when comparing Non-Hispanic AN/AI women to Hispanic women is 0.623. 
The OR for having a recent mammogram when comparing Non-Hispanic Asian women to Hispanic women is 0.4583. 
The OR for having a recent mammogram when comparing Non-Hispanic Black women to Hispanic women is 0.8474. 
The OR for having a recent mammogram when comparing Non-Hispanic White women to Hispanic women is 0.5621. 

The groups least likely to have had a recent mammogram are women aged 65+, women who have less than a high school degree, women who have a poverty ratio &lt;200%, women who do not have a usual source of care, women who have no insurance, and Non-Hispanic women.

``` r
# get sample sizes
# 12483
mam_temp = mam_dat %>%
  filter(domain == 1)

# 2863
mam_temp40 = mam_temp %>%
  filter(age_cat == "40–49")

# 4716
mam_temp50 = mam_temp %>%
  filter(age_cat == "50–64")

# 4904
mam_temp65 = mam_temp %>%
  filter(age_cat == "65+")
```

*collapsing some categories*

``` r
# make financial status and ethnicity only two levels 
mam_dat2 = mam_dat %>%
  mutate(finc_cat2 = if_else(finc_cat == "<200%", finc_cat,
                             if_else(finc_cat == "Unknown", finc_cat, ">=200%")),
         eth_cat2 = if_else(eth_cat == "Hispanic", eth_cat, "Non-Hispanic"))

# refit the design object
des2 = svydesign(ids = ~PSU_P, strata = ~STRAT_P, weights = ~WTFA_SA, nest = TRUE, data = mam_dat2)


# fit the full model on all categorical variables for the included women
mam2_fit = svyglm(mam_2 ~ as.factor(age_cat) + as.factor(educ_cat) + as.factor(finc_cat2) + as.factor(ausualpl_cat) + as.factor(cover_cat) + as.factor(lcond_chronic_cat) + as.factor(race_cat) + as.factor(eth_cat2) + as.factor(imm_stat), 
       design = des2, subset = domain == 1, family = binomial(link = "logit"))
```

    ## Warning in eval(family$initialize): non-integer #successes in a binomial
    ## glm!

``` r
summary(mam2_fit)
```

    ## 
    ## Call:
    ## svyglm(formula = mam_2 ~ as.factor(age_cat) + as.factor(educ_cat) + 
    ##     as.factor(finc_cat2) + as.factor(ausualpl_cat) + as.factor(cover_cat) + 
    ##     as.factor(lcond_chronic_cat) + as.factor(race_cat) + as.factor(eth_cat2) + 
    ##     as.factor(imm_stat), design = des2, subset = domain == 1, 
    ##     family = binomial(link = "logit"))
    ## 
    ## Survey design:
    ## svydesign(ids = ~PSU_P, strata = ~STRAT_P, weights = ~WTFA_SA, 
    ##     nest = TRUE, data = mam_dat2)
    ## 
    ## Coefficients:
    ##                                           Estimate Std. Error t value
    ## (Intercept)                                0.54793    0.92890   0.590
    ## as.factor(age_cat)50–64                    0.01181    0.22483   0.053
    ## as.factor(age_cat)65+                     -0.62217    0.22392  -2.778
    ## as.factor(educ_cat)High school            -0.49785    0.16646  -2.991
    ## as.factor(educ_cat)Less than high school  -0.62878    0.20467  -3.072
    ## as.factor(educ_cat)Some college           -0.22711    0.17594  -1.291
    ## as.factor(finc_cat2)>=200%                 0.04698    0.13703   0.343
    ## as.factor(ausualpl_cat)Other             -13.35945    1.12207 -11.906
    ## as.factor(ausualpl_cat)Yes                 0.89840    0.34510   2.603
    ## as.factor(cover_cat)Private/Military       1.15845    0.34510   3.357
    ## as.factor(cover_cat)Public                 1.15046    0.36553   3.147
    ## as.factor(lcond_chronic_cat)Yes           -0.06553    0.35072  -0.187
    ## as.factor(race_cat)Asian                  -0.73656    0.74814  -0.985
    ## as.factor(race_cat)Black                  -0.32110    0.60985  -0.527
    ## as.factor(race_cat)White                  -0.70976    0.59751  -1.188
    ## as.factor(eth_cat2)Non-Hispanic           -0.74697    0.26887  -2.778
    ## as.factor(imm_stat)In U.S. < 10 yrs       13.92456    0.39044  35.663
    ## as.factor(imm_stat)In U.S. >= 10 yrs      -0.11904    0.23054  -0.516
    ##                                          Pr(>|t|)    
    ## (Intercept)                              0.555825    
    ## as.factor(age_cat)50–64                  0.958138    
    ## as.factor(age_cat)65+                    0.005886 ** 
    ## as.factor(educ_cat)High school           0.003068 ** 
    ## as.factor(educ_cat)Less than high school 0.002366 ** 
    ## as.factor(educ_cat)Some college          0.197989    
    ## as.factor(finc_cat2)>=200%               0.732030    
    ## as.factor(ausualpl_cat)Other              < 2e-16 ***
    ## as.factor(ausualpl_cat)Yes               0.009799 ** 
    ## as.factor(cover_cat)Private/Military     0.000914 ***
    ## as.factor(cover_cat)Public               0.001853 ** 
    ## as.factor(lcond_chronic_cat)Yes          0.851938    
    ## as.factor(race_cat)Asian                 0.325835    
    ## as.factor(race_cat)Black                 0.599002    
    ## as.factor(race_cat)White                 0.236039    
    ## as.factor(eth_cat2)Non-Hispanic          0.005892 ** 
    ## as.factor(imm_stat)In U.S. < 10 yrs       < 2e-16 ***
    ## as.factor(imm_stat)In U.S. >= 10 yrs     0.606073    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 0.8289432)
    ## 
    ## Number of Fisher Scoring iterations: 13

``` r
summ(mam2_fit)
```

    ## MODEL INFO:
    ## Observations: 2669
    ## Dependent Variable: mam_2
    ## Type: Analysis of complex survey design 
    ##  Family: binomial 
    ##  Link function: logit 
    ## 
    ## MODEL FIT:
    ## Pseudo-R² (Cragg-Uhler) = 0.05
    ## Pseudo-R² (McFadden) = 0.21
    ## AIC = 2746.25 
    ## 
    ## --------------------------------------------------------------------------
    ##                                                Est.   S.E.   t val.      p
    ## ------------------------------------------ -------- ------ -------- ------
    ## (Intercept)                                    0.55   0.93     0.59   0.56
    ## as.factor(age_cat)50â€“64                      0.01   0.22     0.05   0.96
    ## as.factor(age_cat)65+                         -0.62   0.22    -2.78   0.01
    ## as.factor(educ_cat)High                       -0.50   0.17    -2.99   0.00
    ## school                                                                    
    ## as.factor(educ_cat)Less than                  -0.63   0.20    -3.07   0.00
    ## high school                                                               
    ## as.factor(educ_cat)Some                       -0.23   0.18    -1.29   0.20
    ## college                                                                   
    ## as.factor(finc_cat2)>=200%                     0.05   0.14     0.34   0.73
    ## as.factor(ausualpl_cat)Other                 -13.36   1.12   -11.91   0.00
    ## as.factor(ausualpl_cat)Yes                     0.90   0.35     2.60   0.01
    ## as.factor(cover_cat)Private/Military           1.16   0.35     3.36   0.00
    ## as.factor(cover_cat)Public                     1.15   0.37     3.15   0.00
    ## as.factor(lcond_chronic_cat)Yes               -0.07   0.35    -0.19   0.85
    ## as.factor(race_cat)Asian                      -0.74   0.75    -0.98   0.33
    ## as.factor(race_cat)Black                      -0.32   0.61    -0.53   0.60
    ## as.factor(race_cat)White                      -0.71   0.60    -1.19   0.24
    ## as.factor(eth_cat2)Non-Hispanic               -0.75   0.27    -2.78   0.01
    ## as.factor(imm_stat)In U.S. <                  13.92   0.39    35.66   0.00
    ## 10 yrs                                                                    
    ## as.factor(imm_stat)In U.S.                    -0.12   0.23    -0.52   0.61
    ## >= 10 yrs                                                                 
    ## --------------------------------------------------------------------------
    ## 
    ## Estimated dispersion parameter = 0.83

``` r
# Rao-scott LRT ANOVA

# test significance of full model
regTermTest(mam2_fit,  ~ as.factor(age_cat) + as.factor(educ_cat) + as.factor(finc_cat2) + as.factor(ausualpl_cat) + as.factor(cover_cat) + as.factor(lcond_chronic_cat) + as.factor(race_cat) + as.factor(eth_cat2) + as.factor(imm_stat),
            method = "LRT")
```

    ## Warning in eval(family$initialize): non-integer #successes in a binomial
    ## glm!

    ## Working (Rao-Scott+F) LRT for as.factor(age_cat) as.factor(educ_cat) as.factor(finc_cat2) as.factor(ausualpl_cat) as.factor(cover_cat) as.factor(lcond_chronic_cat) as.factor(race_cat) as.factor(eth_cat2) as.factor(imm_stat)
    ##  in svyglm(formula = mam_2 ~ as.factor(age_cat) + as.factor(educ_cat) + 
    ##     as.factor(finc_cat2) + as.factor(ausualpl_cat) + as.factor(cover_cat) + 
    ##     as.factor(lcond_chronic_cat) + as.factor(race_cat) + as.factor(eth_cat2) + 
    ##     as.factor(imm_stat), design = des2, subset = domain == 1, 
    ##     family = binomial(link = "logit"))
    ## Working 2logLR =  6362.691 p= < 2.22e-16 
    ## (scale factors:  2.1 1.7 1.6 1.5 1.3 1.2 1.1 1.1 0.97 0.88 0.82 0.79 0.73 0.65 0.6 1e-06 8.3e-07 );  denominator df= 244

``` r
# test significance of individual terms/term groups
regTermTest(mam2_fit, "as.factor(age_cat)", 
            method = "LRT") # sig
```

    ## Warning in eval(family$initialize): non-integer #successes in a binomial
    ## glm!

    ## Working (Rao-Scott+F) LRT for as.factor(age_cat)
    ##  in svyglm(formula = mam_2 ~ as.factor(age_cat) + as.factor(educ_cat) + 
    ##     as.factor(finc_cat2) + as.factor(ausualpl_cat) + as.factor(cover_cat) + 
    ##     as.factor(lcond_chronic_cat) + as.factor(race_cat) + as.factor(eth_cat2) + 
    ##     as.factor(imm_stat), design = des2, subset = domain == 1, 
    ##     family = binomial(link = "logit"))
    ## Working 2logLR =  25.55624 p= 5.9675e-06 
    ## (scale factors:  1.1 0.93 );  denominator df= 244

``` r
regTermTest(mam2_fit, "as.factor(educ_cat)",
            method = "LRT") # sig
```

    ## Warning in eval(family$initialize): non-integer #successes in a binomial
    ## glm!

    ## Working (Rao-Scott+F) LRT for as.factor(educ_cat)
    ##  in svyglm(formula = mam_2 ~ as.factor(age_cat) + as.factor(educ_cat) + 
    ##     as.factor(finc_cat2) + as.factor(ausualpl_cat) + as.factor(cover_cat) + 
    ##     as.factor(lcond_chronic_cat) + as.factor(race_cat) + as.factor(eth_cat2) + 
    ##     as.factor(imm_stat), design = des2, subset = domain == 1, 
    ##     family = binomial(link = "logit"))
    ## Working 2logLR =  34.65693 p= 8.4108e-07 
    ## (scale factors:  1.2 1 0.81 );  denominator df= 244

``` r
regTermTest(mam2_fit, "as.factor(finc_cat2)",
            method = "LRT") # sig
```

    ## Warning in eval(family$initialize): non-integer #successes in a binomial
    ## glm!

    ## Working (Rao-Scott+F) LRT for as.factor(finc_cat2)
    ##  in svyglm(formula = mam_2 ~ as.factor(age_cat) + as.factor(educ_cat) + 
    ##     as.factor(finc_cat2) + as.factor(ausualpl_cat) + as.factor(cover_cat) + 
    ##     as.factor(lcond_chronic_cat) + as.factor(race_cat) + as.factor(eth_cat2) + 
    ##     as.factor(imm_stat), design = des2, subset = domain == 1, 
    ##     family = binomial(link = "logit"))
    ## Working 2logLR =  68.83743 p= 8.176e-15 
    ## df=1;  denominator df= 244

``` r
regTermTest(mam2_fit, "as.factor(ausualpl_cat)",
            method = "LRT") # sig
```

    ## Warning in eval(family$initialize): non-integer #successes in a binomial
    ## glm!

    ## Working (Rao-Scott+F) LRT for as.factor(ausualpl_cat)
    ##  in svyglm(formula = mam_2 ~ as.factor(age_cat) + as.factor(educ_cat) + 
    ##     as.factor(finc_cat2) + as.factor(ausualpl_cat) + as.factor(cover_cat) + 
    ##     as.factor(lcond_chronic_cat) + as.factor(race_cat) + as.factor(eth_cat2) + 
    ##     as.factor(imm_stat), design = des2, subset = domain == 1, 
    ##     family = binomial(link = "logit"))
    ## Working 2logLR =  14.94767 p= 0.0070806 
    ## (scale factors:  2 1.9e-06 );  denominator df= 244

``` r
regTermTest(mam2_fit, "as.factor(cover_cat)",
            method = "LRT") # sig
```

    ## Warning in eval(family$initialize): non-integer #successes in a binomial
    ## glm!

    ## Working (Rao-Scott+F) LRT for as.factor(cover_cat)
    ##  in svyglm(formula = mam_2 ~ as.factor(age_cat) + as.factor(educ_cat) + 
    ##     as.factor(finc_cat2) + as.factor(ausualpl_cat) + as.factor(cover_cat) + 
    ##     as.factor(lcond_chronic_cat) + as.factor(race_cat) + as.factor(eth_cat2) + 
    ##     as.factor(imm_stat), design = des2, subset = domain == 1, 
    ##     family = binomial(link = "logit"))
    ## Working 2logLR =  15.27826 p= 0.00072295 
    ## (scale factors:  1.1 0.86 );  denominator df= 244

``` r
regTermTest(mam2_fit, "as.factor(lcond_chronic_cat)",
            method = "LRT") # sig
```

    ## Warning in eval(family$initialize): non-integer #successes in a binomial
    ## glm!

    ## Working (Rao-Scott+F) LRT for as.factor(lcond_chronic_cat)
    ##  in svyglm(formula = mam_2 ~ as.factor(age_cat) + as.factor(educ_cat) + 
    ##     as.factor(finc_cat2) + as.factor(ausualpl_cat) + as.factor(cover_cat) + 
    ##     as.factor(lcond_chronic_cat) + as.factor(race_cat) + as.factor(eth_cat2) + 
    ##     as.factor(imm_stat), design = des2, subset = domain == 1, 
    ##     family = binomial(link = "logit"))
    ## Working 2logLR =  6626.936 p= < 2.22e-16 
    ## df=1;  denominator df= 244

``` r
regTermTest(mam2_fit, "as.factor(race_cat)",
            method = "LRT") # not sig
```

    ## Warning in eval(family$initialize): non-integer #successes in a binomial
    ## glm!

    ## Working (Rao-Scott+F) LRT for as.factor(race_cat)
    ##  in svyglm(formula = mam_2 ~ as.factor(age_cat) + as.factor(educ_cat) + 
    ##     as.factor(finc_cat2) + as.factor(ausualpl_cat) + as.factor(cover_cat) + 
    ##     as.factor(lcond_chronic_cat) + as.factor(race_cat) + as.factor(eth_cat2) + 
    ##     as.factor(imm_stat), design = des2, subset = domain == 1, 
    ##     family = binomial(link = "logit"))
    ## Working 2logLR =  6.809882 p= 0.080765 
    ## (scale factors:  1.2 0.92 0.85 );  denominator df= 244

``` r
regTermTest(mam2_fit, "as.factor(eth_cat2)",
            method = "LRT") # sig
```

    ## Warning in eval(family$initialize): non-integer #successes in a binomial
    ## glm!

    ## Working (Rao-Scott+F) LRT for as.factor(eth_cat2)
    ##  in svyglm(formula = mam_2 ~ as.factor(age_cat) + as.factor(educ_cat) + 
    ##     as.factor(finc_cat2) + as.factor(ausualpl_cat) + as.factor(cover_cat) + 
    ##     as.factor(lcond_chronic_cat) + as.factor(race_cat) + as.factor(eth_cat2) + 
    ##     as.factor(imm_stat), design = des2, subset = domain == 1, 
    ##     family = binomial(link = "logit"))
    ## Working 2logLR =  8.116515 p= 0.0050298 
    ## df=1;  denominator df= 244

``` r
regTermTest(mam2_fit, "as.factor(imm_stat)",
            method = "LRT") # sig
```

    ## Warning in eval(family$initialize): non-integer #successes in a binomial
    ## glm!

    ## Working (Rao-Scott+F) LRT for as.factor(imm_stat)
    ##  in svyglm(formula = mam_2 ~ as.factor(age_cat) + as.factor(educ_cat) + 
    ##     as.factor(finc_cat2) + as.factor(ausualpl_cat) + as.factor(cover_cat) + 
    ##     as.factor(lcond_chronic_cat) + as.factor(race_cat) + as.factor(eth_cat2) + 
    ##     as.factor(imm_stat), design = des2, subset = domain == 1, 
    ##     family = binomial(link = "logit"))
    ## Working 2logLR =  15.3734 p= 0.0063202 
    ## (scale factors:  2 2.5e-06 );  denominator df= 244

``` r
# fit reduced model with significant predictors
mam2_fit2 = svyglm(mam_2 ~ as.factor(age_cat) + as.factor(educ_cat) + as.factor(finc_cat2) + as.factor(ausualpl_cat) + as.factor(cover_cat) + as.factor(lcond_chronic_cat) + as.factor(eth_cat2) + as.factor(imm_stat),
                   design = des2, subset = domain == 1, 
                   family = binomial(link = "logit"))
```

    ## Warning in eval(family$initialize): non-integer #successes in a binomial
    ## glm!

``` r
summary(mam2_fit2)
```

    ## 
    ## Call:
    ## svyglm(formula = mam_2 ~ as.factor(age_cat) + as.factor(educ_cat) + 
    ##     as.factor(finc_cat2) + as.factor(ausualpl_cat) + as.factor(cover_cat) + 
    ##     as.factor(lcond_chronic_cat) + as.factor(eth_cat2) + as.factor(imm_stat), 
    ##     design = des2, subset = domain == 1, family = binomial(link = "logit"))
    ## 
    ## Survey design:
    ## svydesign(ids = ~PSU_P, strata = ~STRAT_P, weights = ~WTFA_SA, 
    ##     nest = TRUE, data = mam_dat2)
    ## 
    ## Coefficients:
    ##                                            Estimate Std. Error t value
    ## (Intercept)                               -0.052925   0.680627  -0.078
    ## as.factor(age_cat)50–64                    0.015998   0.220965   0.072
    ## as.factor(age_cat)65+                     -0.646977   0.219612  -2.946
    ## as.factor(educ_cat)High school            -0.498641   0.165120  -3.020
    ## as.factor(educ_cat)Less than high school  -0.608279   0.204379  -2.976
    ## as.factor(educ_cat)Some college           -0.204383   0.175275  -1.166
    ## as.factor(finc_cat2)>=200%                 0.002496   0.135112   0.018
    ## as.factor(ausualpl_cat)Other             -13.503433   1.128254 -11.968
    ## as.factor(ausualpl_cat)Yes                 0.840210   0.378873   2.218
    ## as.factor(cover_cat)Private/Military       1.177763   0.341633   3.447
    ## as.factor(cover_cat)Public                 1.199125   0.362141   3.311
    ## as.factor(lcond_chronic_cat)Yes           -0.082223   0.358358  -0.229
    ## as.factor(eth_cat2)Non-Hispanic           -0.717933   0.256433  -2.800
    ## as.factor(imm_stat)In U.S. < 10 yrs       14.012023   0.395671  35.413
    ## as.factor(imm_stat)In U.S. >= 10 yrs      -0.164101   0.223060  -0.736
    ##                                          Pr(>|t|)    
    ## (Intercept)                              0.938082    
    ## as.factor(age_cat)50–64                  0.942343    
    ## as.factor(age_cat)65+                    0.003527 ** 
    ## as.factor(educ_cat)High school           0.002794 ** 
    ## as.factor(educ_cat)Less than high school 0.003208 ** 
    ## as.factor(educ_cat)Some college          0.244711    
    ## as.factor(finc_cat2)>=200%               0.985274    
    ## as.factor(ausualpl_cat)Other              < 2e-16 ***
    ## as.factor(ausualpl_cat)Yes               0.027489 *  
    ## as.factor(cover_cat)Private/Military     0.000665 ***
    ## as.factor(cover_cat)Public               0.001068 ** 
    ## as.factor(lcond_chronic_cat)Yes          0.818714    
    ## as.factor(eth_cat2)Non-Hispanic          0.005519 ** 
    ## as.factor(imm_stat)In U.S. < 10 yrs       < 2e-16 ***
    ## as.factor(imm_stat)In U.S. >= 10 yrs     0.462624    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 0.8273784)
    ## 
    ## Number of Fisher Scoring iterations: 13

``` r
summ(mam2_fit2)
```

    ## MODEL INFO:
    ## Observations: 2669
    ## Dependent Variable: mam_2
    ## Type: Analysis of complex survey design 
    ##  Family: binomial 
    ##  Link function: logit 
    ## 
    ## MODEL FIT:
    ## Pseudo-R² (Cragg-Uhler) = 0.05
    ## Pseudo-R² (McFadden) = 0.20
    ## AIC = 2750.10 
    ## 
    ## --------------------------------------------------------------------------
    ##                                                Est.   S.E.   t val.      p
    ## ------------------------------------------ -------- ------ -------- ------
    ## (Intercept)                                   -0.05   0.68    -0.08   0.94
    ## as.factor(age_cat)50â€“64                      0.02   0.22     0.07   0.94
    ## as.factor(age_cat)65+                         -0.65   0.22    -2.95   0.00
    ## as.factor(educ_cat)High                       -0.50   0.17    -3.02   0.00
    ## school                                                                    
    ## as.factor(educ_cat)Less than                  -0.61   0.20    -2.98   0.00
    ## high school                                                               
    ## as.factor(educ_cat)Some                       -0.20   0.18    -1.17   0.24
    ## college                                                                   
    ## as.factor(finc_cat2)>=200%                     0.00   0.14     0.02   0.99
    ## as.factor(ausualpl_cat)Other                 -13.50   1.13   -11.97   0.00
    ## as.factor(ausualpl_cat)Yes                     0.84   0.38     2.22   0.03
    ## as.factor(cover_cat)Private/Military           1.18   0.34     3.45   0.00
    ## as.factor(cover_cat)Public                     1.20   0.36     3.31   0.00
    ## as.factor(lcond_chronic_cat)Yes               -0.08   0.36    -0.23   0.82
    ## as.factor(eth_cat2)Non-Hispanic               -0.72   0.26    -2.80   0.01
    ## as.factor(imm_stat)In U.S. <                  14.01   0.40    35.41   0.00
    ## 10 yrs                                                                    
    ## as.factor(imm_stat)In U.S.                    -0.16   0.22    -0.74   0.46
    ## >= 10 yrs                                                                 
    ## --------------------------------------------------------------------------
    ## 
    ## Estimated dispersion parameter = 0.83

``` r
# Rao-Scott of full model
regTermTest(mam2_fit2,  ~ as.factor(age_cat) + as.factor(educ_cat) + as.factor(finc_cat2) + as.factor(ausualpl_cat) + as.factor(cover_cat) + as.factor(lcond_chronic_cat) + as.factor(eth_cat2) + as.factor(imm_stat))
```

    ## Wald test for as.factor(age_cat) as.factor(educ_cat) as.factor(finc_cat2) as.factor(ausualpl_cat) as.factor(cover_cat) as.factor(lcond_chronic_cat) as.factor(eth_cat2) as.factor(imm_stat)
    ##  in svyglm(formula = mam_2 ~ as.factor(age_cat) + as.factor(educ_cat) + 
    ##     as.factor(finc_cat2) + as.factor(ausualpl_cat) + as.factor(cover_cat) + 
    ##     as.factor(lcond_chronic_cat) + as.factor(eth_cat2) + as.factor(imm_stat), 
    ##     design = des2, subset = domain == 1, family = binomial(link = "logit"))
    ## F =  124.692  on  14  and  247  df: p= < 2.22e-16

``` r
# single term/term group significance
regTermTest(mam2_fit2, "as.factor(age_cat)",
            method = "LRT") # sig
```

    ## Warning in eval(family$initialize): non-integer #successes in a binomial
    ## glm!

    ## Working (Rao-Scott+F) LRT for as.factor(age_cat)
    ##  in svyglm(formula = mam_2 ~ as.factor(age_cat) + as.factor(educ_cat) + 
    ##     as.factor(finc_cat2) + as.factor(ausualpl_cat) + as.factor(cover_cat) + 
    ##     as.factor(lcond_chronic_cat) + as.factor(eth_cat2) + as.factor(imm_stat), 
    ##     design = des2, subset = domain == 1, family = binomial(link = "logit"))
    ## Working 2logLR =  28.35498 p= 1.5789e-06 
    ## (scale factors:  1 0.96 );  denominator df= 247

``` r
regTermTest(mam2_fit2, "as.factor(educ_cat)",
            method = "LRT") # sig
```

    ## Warning in eval(family$initialize): non-integer #successes in a binomial
    ## glm!

    ## Working (Rao-Scott+F) LRT for as.factor(educ_cat)
    ##  in svyglm(formula = mam_2 ~ as.factor(age_cat) + as.factor(educ_cat) + 
    ##     as.factor(finc_cat2) + as.factor(ausualpl_cat) + as.factor(cover_cat) + 
    ##     as.factor(lcond_chronic_cat) + as.factor(eth_cat2) + as.factor(imm_stat), 
    ##     design = des2, subset = domain == 1, family = binomial(link = "logit"))
    ## Working 2logLR =  35.26798 p= 6.7475e-07 
    ## (scale factors:  1.2 1 0.81 );  denominator df= 247

``` r
regTermTest(mam2_fit2, "as.factor(finc_cat2)",
            method = "LRT") # sig
```

    ## Warning in eval(family$initialize): non-integer #successes in a binomial
    ## glm!

    ## Working (Rao-Scott+F) LRT for as.factor(finc_cat2)
    ##  in svyglm(formula = mam_2 ~ as.factor(age_cat) + as.factor(educ_cat) + 
    ##     as.factor(finc_cat2) + as.factor(ausualpl_cat) + as.factor(cover_cat) + 
    ##     as.factor(lcond_chronic_cat) + as.factor(eth_cat2) + as.factor(imm_stat), 
    ##     design = des2, subset = domain == 1, family = binomial(link = "logit"))
    ## Working 2logLR =  69.82433 p= 5.2975e-15 
    ## df=1;  denominator df= 247

``` r
regTermTest(mam2_fit2, "as.factor(ausualpl_cat)",
            method = "LRT") # sig
```

    ## Warning in eval(family$initialize): non-integer #successes in a binomial
    ## glm!

    ## Working (Rao-Scott+F) LRT for as.factor(ausualpl_cat)
    ##  in svyglm(formula = mam_2 ~ as.factor(age_cat) + as.factor(educ_cat) + 
    ##     as.factor(finc_cat2) + as.factor(ausualpl_cat) + as.factor(cover_cat) + 
    ##     as.factor(lcond_chronic_cat) + as.factor(eth_cat2) + as.factor(imm_stat), 
    ##     design = des2, subset = domain == 1, family = binomial(link = "logit"))
    ## Working 2logLR =  10.98479 p= 0.02077 
    ## (scale factors:  2 1.5e-06 );  denominator df= 247

``` r
regTermTest(mam2_fit2, "as.factor(cover_cat)",
            method = "LRT") # sig
```

    ## Warning in eval(family$initialize): non-integer #successes in a binomial
    ## glm!

    ## Working (Rao-Scott+F) LRT for as.factor(cover_cat)
    ##  in svyglm(formula = mam_2 ~ as.factor(age_cat) + as.factor(educ_cat) + 
    ##     as.factor(finc_cat2) + as.factor(ausualpl_cat) + as.factor(cover_cat) + 
    ##     as.factor(lcond_chronic_cat) + as.factor(eth_cat2) + as.factor(imm_stat), 
    ##     design = des2, subset = domain == 1, family = binomial(link = "logit"))
    ## Working 2logLR =  15.67381 p= 0.00061215 
    ## (scale factors:  1.1 0.86 );  denominator df= 247

``` r
regTermTest(mam2_fit2, "as.factor(lcond_chronic_cat)",
            method = "LRT") # sig
```

    ## Warning in eval(family$initialize): non-integer #successes in a binomial
    ## glm!

    ## Working (Rao-Scott+F) LRT for as.factor(lcond_chronic_cat)
    ##  in svyglm(formula = mam_2 ~ as.factor(age_cat) + as.factor(educ_cat) + 
    ##     as.factor(finc_cat2) + as.factor(ausualpl_cat) + as.factor(cover_cat) + 
    ##     as.factor(lcond_chronic_cat) + as.factor(eth_cat2) + as.factor(imm_stat), 
    ##     design = des2, subset = domain == 1, family = binomial(link = "logit"))
    ## Working 2logLR =  6378.172 p= < 2.22e-16 
    ## df=1;  denominator df= 247

``` r
regTermTest(mam2_fit2, "as.factor(eth_cat2)",
            method = "LRT") # sig
```

    ## Warning in eval(family$initialize): non-integer #successes in a binomial
    ## glm!

    ## Working (Rao-Scott+F) LRT for as.factor(eth_cat2)
    ##  in svyglm(formula = mam_2 ~ as.factor(age_cat) + as.factor(educ_cat) + 
    ##     as.factor(finc_cat2) + as.factor(ausualpl_cat) + as.factor(cover_cat) + 
    ##     as.factor(lcond_chronic_cat) + as.factor(eth_cat2) + as.factor(imm_stat), 
    ##     design = des2, subset = domain == 1, family = binomial(link = "logit"))
    ## Working 2logLR =  8.236244 p= 0.0047162 
    ## df=1;  denominator df= 247

``` r
regTermTest(mam2_fit2, "as.factor(imm_stat)",
            method = "LRT") # sig
```

    ## Warning in eval(family$initialize): non-integer #successes in a binomial
    ## glm!

    ## Working (Rao-Scott+F) LRT for as.factor(imm_stat)
    ##  in svyglm(formula = mam_2 ~ as.factor(age_cat) + as.factor(educ_cat) + 
    ##     as.factor(finc_cat2) + as.factor(ausualpl_cat) + as.factor(cover_cat) + 
    ##     as.factor(lcond_chronic_cat) + as.factor(eth_cat2) + as.factor(imm_stat), 
    ##     design = des2, subset = domain == 1, family = binomial(link = "logit"))
    ## Working 2logLR =  14.32067 p= 0.008369 
    ## (scale factors:  2 2.1e-06 );  denominator df= 247

``` r
# give labels to the coefficients to look nicer on the OR graph

coef <- names(coef(mam2_fit2))
coef_new = stringr::str_remove(coef, "^[^_]*_cat[)]")
coef_new = stringr::str_remove(coef_new, "^[^_]*_cat2[)]")
coef_new = stringr::str_remove(coef_new, "^[^_]*_stat[)]")
coef_new = stringr::str_remove(coef_new, "^[^_]*_cat[)]")
names(coef) <- coef_new
coef1 = coef[-1] # remove intercept 
coef2 = coef1[-7] # remove coefficient for Usual Care = Other
coef3a = coef2[-12] 
coef3b = coef2[12] # pull out immigration status because of the intercept magnitude

# plot ORs with CIs
jtools::plot_summs(mam2_fit2, coefs = coef3a, exp = TRUE) +
  labs(title = "Mammogram Significant Predictors") +
  scale_x_continuous(limits = c(0, 6), breaks = c(0, 2, 4, 6))
```

    ## Warning: Removed 2 rows containing missing values (geom_pointrangeh).

![](mammogram_files/figure-markdown_github/unnamed-chunk-6-1.png)

``` r
jtools::plot_summs(mam2_fit2, coefs = coef3b, exp = TRUE) +
  labs(title = "Mammogram Significant Predictors") +
  scale_x_continuous(labels = scales::comma)
```

![](mammogram_files/figure-markdown_github/unnamed-chunk-6-2.png)

``` r
# write out analysis data set

# write_csv(mam_dat2, "C:/Users/court/Documents/Columbia/Fall 2019/Health Surveys/Final Project/mam_analysis.csv")
```
