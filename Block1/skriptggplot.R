#Pakete Laden
pacman::p_load(here, tidyverse, readxl, writexl)


# Plots darstellen mit ggplot

#Dataset laden
chick <- ChickWeight

#Jetzt bauen wir Layer für Layer unsere erste Grafik

# Data und Aesthetics
pl1 <- ggplot(chick, aes(x = weight))

# Geometrie


pl2 <- pl1 + geom_histogram()

# Grafik aufrufen
pl2

# Farben ändern
pl3 <- pl2 + geom_histogram(color = "red", fill = "pink")

# Achsenbeschriftungen ändern
pl4 <- pl3 + xlab("Gewicht") +
  ylab("Haeufigkeit") + 
  ggtitle("Verteilung Kueken nach Gewicht")

pl4 

# Die Füllfarbe kann auch in Abhängigkeit der Datenwerte geändert werden
pl5 <- pl1 + geom_histogram(aes(fill = ..count..))
pl5


# Balkendiagramm


pacman::p_load(carData)
data <- TitanicSurvival


## Daten anschauen


pl5 <- ggplot(data, aes(x=survived))


pl5 + geom_histogram(stat = "count")

### Stoesst an Grenzen, wir können mehr Infos aus den Daten ziehen


ggplot(data, aes(x = survived)) +
  geom_bar(aes(fill=passengerClass)) +
  xlab("Überlebt") + ylab("Anzahl") + 
  ggtitle("Überlebende nach Ticketklasse")

### Version 2

## Schrittweise in 3 Plots auführen und die entsprechenden Angaben ändern. Erst position="fill", position="dodge", dann position ="stack" Im zweiten Schritt, Facets und im dritten
#Schritt die Legende ändern

ggplot(data, aes(x = survived)) +
  geom_bar(aes(fill=passengerClass), position = "fill") +
  xlab("Ueberlebt") + ylab("Anzahl") + 
  ggtitle("Ueberlebende nach Ticketklasse") +
  #facet_grid(.~sex) +
  scale_fill_discrete(name  ="Ticketklasse",
                      breaks=c("1st", "2nd", "3rd"),
                      labels=c("1.Klasse", "2.Klasse", "3.Klasse"))


### Geom_point

iris <- iris

ggplot(iris, aes(x=Sepal.Width, y = Sepal.Length)) +
  geom_point(aes(shape = Species, color = Species))

###################### Facet grid und Legende entfernen

ggplot(iris, aes(x=Sepal.Width, y = Sepal.Length)) +
  geom_point(aes(color = Species)) +
  facet_grid(.~Species / Species~.)# +
#theme(legend.position = "none")

#################


#### Linegraph
tree <- Orange
#tree1 <-filter(Orange, Tree==1 | Tree==2)

ggplot(tree, aes(x=age, y = circumference)) +
  geom_line(aes(group=Tree, color = Tree)) 



#Aufgabe: Versucht ein Histogramm zu erstellen, was auf der x-Achse darstellt, wer
#den Untergang der Titanic überlebt hat


titan1 <- ggplot(data, aes(x=survived)) 

titan2 <- titan1 + geom_histogram(color ="blue", fill= "gray", stat = "count")

titan2

# Verschiedene Möglichkeiten für die Balkenanordnung
  ggplot(data, aes(x = survived)) +
  geom_bar(aes(fill = passengerClass), position = "dodge")

  ggplot(data, aes(x = survived)) +
  geom_bar(aes(fill = passengerClass), position = "fill")


titan4 <- titan1 + geom_histogram(color ="blue", fill= "gray", stat = "count")


### Streudiagramme

iris <- iris


ggplot(iris, aes(x = Petal.Width, y = Petal.Length)) +
  geom_point(aes(shape = Species), color = "red")

#Streudiagramme ohne Legende
ggplot(iris, aes(x = Petal.Width, y = Petal.Length)) +
  geom_point(aes(color = Species)) +
  facet_grid(.~Species) +
  theme(legend.position = "none")


# Linegraphs
tree <- Orange

# Nur eine Auswahl des gesamten Datensatzes plotten
tree1_2 <- filter(tree, Tree ==1 | Tree == 2)

ggplot(tree1_2, aes(x = age, y = circumference)) +
  geom_line(aes(color = Tree)) +
  theme_bw()

ggplot(data, aes(x = survived), stat = "count") +
  geom_bar(aes(fill = survived)) +
  xlab("Survived or not") +
  ylab("Anzahl") +
  facet_grid(.~passengerClass) +
  theme(legend.position = "none")

ggplot(data, aes(x = sex)) +
  geom_bar(aes(fill = survived)) +
  xlab("Survived or not") +
  ylab("Anzahl") +
  facet_grid(.~passengerClass)# +
  theme(legend.position = "none")

