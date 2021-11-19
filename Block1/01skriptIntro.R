#Pakete laden. Normalerweise müsste jedes Paket zunächst mit install.packages("name") installiert und dann mit library(name) aufgerufen werden
# pacman hilft uns dabei, in dem es für jedes Paket schaut, ob es installiert ist, falls nicht, wird es isntalliert. 

pacman::p_load(here, tidyverse, readxl, writexl)


# Datentypen und variablen
a <- 1
b <- c(1,3,7,9)
c <- "hallo"
d <- y 
typeof()
# Aufrufen von Daten, hier das dritte Element aus der Variable b
b[3] 

# Komplexere Datenstrukturen 1: Matrix
#erstellen: Eine Matrix mit dem Namen mat, die Werte 1 bis 25 enthält, 5 x 5 Reihen und Spalten
# und reihenweise befüllt ist
mat <- matrix(1:25, 5,5, byrow = TRUE)
# und aufrufen der Matrix
mat
# Wie oben bei b können wir auch hier einzelne Werte aus der Matrix aufrufen
# Dabei folgt es immer folgender Logik: name[reihe, spalte]
mat[1,2]

# Datentyp kontrollieren
is.matrix(mat)

### Aufgaben 1 und 2 erledigen (siehe Slides)


## Operatoren
# Folgt folgende Gleichungen aus und schaut euch die Ergebnisse an. Was bedeuten die Zeichen jeweils?
x > 5
x!=3 
x >= 3 | x < 2
x != 3 & x < 5


# Komplexere Datenstrukturen 2: Matrix in ein  Data frame umschreiben

df <- data.frame(mat)
is.data.frame(df)
df

df[5,4]


# Dataframe mit Werten erstellen: Temperaturen an Wochentagen

temp <- c(24,26,20,10,15)
names(temp) <- c("Mo","Di","Mi","Do","Fr")


data.frame(temp)


# Erste mathematische Operationen um Daten zusammenzufassen

mean(temp)

max <- max(temp)

temp == max

temp < max

subset(temp, temp < max)


### Einen ersten Datensatz aufrufen und erste Operationen daran ausführen

chick <- ChickWeight

# Überblick gewinnen
head(chick)
str(chick)

mean(chick$weight)

range(chick$weight)


#Subsetting

#Indexing
testdiet <- ChickWeight[ChickWeight$Diet==4,]

#Subsetting

#Aufgabe: Erstelle ein Subset mit Hühnern die über 30 und unter 100 wiegen

mediumchicken <- subset(chick, weight > 30 | weight < 100)

#### 

# Datensätze aus Dateien einlesen
# einmal als csv und einmal als Excel, wir nutzen das Paket "here", was uns die Lokalisierung von files erleichtern. Wir müssen nur
# relativen path zu unserem vorhin erstellten R Projekt angeben

film <-read_csv(here("Block1","data","prime.csv"))   
film <-read_xl(here("Block1","data","prime.xlsx"))   



# Datensatz inspizieren
head(film)
str(film)
length(unique(film$Genre))
dim(film)
$tally(film)
$count(film$Genre)


# Aufgabe: Zeige nur englische Produktionen an
filmEnglish <- subset(film, Language == "English")

# dplyr ist ein Paket aus dem tidyverse, was Datenaufbereitung stark vereinfach
# Wir laden wir den iris Datensatz und schauen uns ein paar grundlegen dplyr Verben und ihre Funktion an
# Der Pipe Operator %>%  (Strg+M) hilft uns an einem Datensatz zu arbeiten, ohne ihn 
# immer wieder neu aufrufen zu müssen. Er führt uns von einem Bearbeitungsschritt zum nächsten

iris <- iris

# select (Auswahl für Spalten)

iris_species <- iris %>% 
  select(Species, Sepal.Length)

iris_ <- iris %>% 
  select(-Petal.Width)

# Filter ruft Reihen mit bestimmten Werten auf

big_iris <- iris %>% 
    filter(Species == "setosa")

big_setosa <- iris %>% 
  filter(Species == "setosa" & Petal.Length >1)


big_setosa <- iris %>% 
  filter(Species == "setosa" & Petal.Length >1) %>% 
  select(Species, Petal.Length)



### mutate erstellt neue Variablen auf Grundlage von vorhandenen Variablen nach Durchführung von 
# z.B. mathematischen Operationen an einer Variable
iris_ratio <- iris %>% 
  mutate(Petal.Ratio = Petal.Length/ Petal.Width,
         Sepal.Ratio = Sepal.Length/ Sepal.Width) 


iris2 <- iris %>% 
  mutate(is_wide = ifelse(Sepal.Length >5 | Petal.Length > 5, "yes", "no"))


###  arrange ordnet unseren Datensatz
iris <- iris

iris_arr <- iris %>%  
  arrange(Sepal.Width)
 

## group_by und summarize/ mutate
by_species2 <- iris %>% 
  group_by(Species) %>% 
  mutate(mn_sepal_len = mean(Sepal.Length))


#rename

iris_d <- iris %>% 
  rename(Kelchblatt.laenge = Petal.Length,
         Kelchblatt.weite = Petal.Width)




### Day 2

# Plots Histogramm

chick <- ChickWeight

# Data and aesthetics
pl1 <- ggplot(chick, aes(x=weight))
# geometry

pl1
pl2<-pl1 + geom_histogram(binwidth = 5, color = "red", fill = "pink")


pl2
#
pl3 <- pl2 + xlab("Gewicht") + ylab("Anzahl der Küken") + ggtitle("Verteilung Küken nach Gewicht")

pl4 <-pl1 + geom_histogram(binwidth = 5, aes(x= weight, fill = ..count..))
pl4



