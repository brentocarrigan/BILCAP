test difference

df <- read.table(text = "Queen Alexandra Hospital Dr Caroline Archer 4
Yeovil District Hospital Dr Erica Beaumont 4
University Hospital Dr Sharmila Sothi 4
James Paget Hospital Dr Ulrike Dernedde 3
Leicester General Hospital Dr Chinenye Iwuji 3
North Middlesex Hospital Prof John Bridgewater 2
Ninewells Hospital Dr Douglas Adamson 2
Royal Marsden Hospital London Prof David Cunningham 2
St Mary's Hospital Dr Umapathy Hombaiah 1
Leicester Royal Infirmary Dr Chinenye Iwuji 1
Calderdale Royal Hospital Dr Jo Dent 1
Basildon Hospital Dr Olivia Chan 1")

string <- "Site Name Principal Investigator Patients Randomised St James's University Hospital Dr Alan Anthoney 33 Hammersmith Hospital Dr Harpreet Wasan 25 The Queen Elizabeth Hospital Dr Yuk Ting Ma 25 Weston Park Hospital Dr Jonathan Wadsley 23 Christie Hospital Prof Juan Valle 22 Addenbrooke's Hospital Dr Pippa Corrie 21 St Thomas's Hospital Dr Paul Ross 18 Beatson West of Scotland Cancer Centre Prof Jeff Evans 17 Derriford Hospital Dr Geoffrey Cogill 16 Freeman Hospital Dr Kate Sumpter 16 Western General Hospital Dr Lucy Wall 16 Clatterbridge Cancer Centre Prof Daniel Palmer 15 Royal Surrey County Hospital Dr Sebastian Cummins 15 Bristol Haematology And Oncology Centre Dr Stephen Falk 14 Maidstone Hospital Dr Justin Waters 14 Southampton General Hospital Prof John Primrose 13 Royal Free Hospital Dr Roopinder Gillmore 13 Royal Bournemouth Hospital Dr Tamas Hickish 10 St Bartholomew's Hospital Dr Sarah Slater 10 University College London Hospital Prof John Bridgewater 9 Nottingham City Hospital Dr Srinivasan Madhusudan 9 Huddersfield Royal Infirmary Dr Jo Dent 8 North Manchester General Hospital Prof Juan Valle 7 Royal Liverpool University Hospital Prof Daniel Palmer 7 Royal Marsden Hospital Sutton Prof David Cunningham 7 Velindre Hospital Dr Tom Crosby 6 University Hospital Aintree Prof Daniel Palmer 5 Salisbury District Hospital Dr Tim Iveson 5 Basingstoke and North Hampshire Hospital Dr Charlotte Rees 4 Southend University Hospital Dr David Tsang 4 Royal Derby Hospital Dr Kate Shankland 4 Poole Hospital Dr Tamas Hickish 4 Princess Alexandra Hospital Prof John Bridgewater 4 Queen Alexandra Hospital Dr Caroline Archer 4 Yeovil District Hospital Dr Erica Beaumont 4 University Hospital Dr Sharmila Sothi 4 James Paget Hospital Dr Ulrike Dernedde 3 Leicester General Hospital Dr Chinenye Iwuji 3 North Middlesex Hospital Prof John Bridgewater 2 Ninewells Hospital Dr Douglas Adamson 2 Royal Marsden Hospital London Prof David Cunningham 2 St Mary's Hospital Dr Umapathy Hombaiah 1 Leicester Royal Infirmary Dr Chinenye Iwuji 1 Calderdale Royal Hospital Dr Jo Dent 1 Basildon Hospital Dr Olivia Chan 1"

string2 <- str_replace_all(string, "(?<=[0-9])\\s", "\n") %>% 
  str_replace_all("(?=(Dr|Prof))", "_") %>% 
  str_replace_all("\\s(?=[0-9]+)", "_")

writeLines(string2)

write.csv(string2, "~/Downloads/test2.txt")

BILCAP <- read_delim("~/Downloads/test2.txt", trim_ws = TRUE, delim = "_")
   
BILCAP %>% 
  ggplot(aes(x = reorder(Site.Name, Patients.Randomised), y = Patients.Randomised)) +
  geom_col() +
  geom_col(data = filter(BILCAP, Principal.Investigator == 'Prof John Bridgewater'), fill = 'red') +
  coord_flip()

by_pi <- BILCAP %>% 
  group_by(Principal.Investigator) %>% 
  mutate(n = sum(Patients.Randomised),
         pct = n / 447) %>% 
  distinct(Principal.Investigator, n, pct)
  
ggplot(by_pi, aes(x = reorder(Principal.Investigator, n), y = n)) +
  geom_col() +
  geom_col(data = filter(by_pi, Principal.Investigator == 'Prof John Primrose'), fill = 'red') +
  coord_flip() +
  theme_light() +
  labs(x = "",
       y = "Number of patients randomised",
       title = 'Number of patients randomised by PI',
       subtitle = 'BILCAP trial (Primrose et al, Lancet Oncol 2019)',
       caption = '@brentocarrigan')

ggplot(by_pi, aes(x = reorder(Principal.Investigator, pct), y = pct)) +
  geom_col() +
  geom_col(data = filter(by_pi, Principal.Investigator == 'Prof John Primrose'), fill = 'red') +
  coord_flip() +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  theme_light() +
  labs(x = "",
       y = "",
       title = 'Percent of patients randomised by PI',
       subtitle = 'BILCAP trial (Primrose et al, Lancet Oncol 2019)',
       caption = '@brentocarrigan')

length(BILCAP$Principal.Investigator)

BILCAP %>% 
  mutate(total_pts = sum(Patients.Randomised)) %>% 
  group_by(Principal.Investigator) %>% 
  mutate(n = sum(Patients.Randomised),
         pct = n / total_pts) %>% 
  filter(Principal.Investigator == '')

  
View(BILCAP)

select(-X)
  separate(x, into = c("Site", "PI", "Number", "x1"), sep = ",") %>% 
  unite(col = "Number", 3:4, sep = "")
