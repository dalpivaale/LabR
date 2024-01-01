#Lez Visualization Geometries (line,barplot,boxplot,....)
#HISTROGRAM
ggplot(mtcars, aes(mpg, y = ..density..)) + # eguagliando y a density hai la densità sull'asse y. se non ci fosse solito istrogramma
  geom_histogram(binwidth = 1, fill = datacamp_light_blue) #fill l'ho eguagliato a quel colore per colorare l'istogramma. binwidth è la larghezza dell'istogramma

ggplot(mtcars, aes(mpg, fill = fam)) +  #soita roba (fill è il colore delle barre a seconda delle features)
  # Change the position to identity, with transparency 0.4
  geom_histogram(binwidth = 1, position = "identity",alpha = 0.4)

##ISTROGRAMMI QUANDO hai variabili continue, per valori discreti usa BARPLOT
ggplot(mtcars, aes(fcyl, fill = fam)) +
  # Set the position to "fill"
  geom_bar(position = "fill") #barplot in percentuale, se non specifichi il parametro fill anzichè in percentuale hai il conteggio numerico

ggplot(mtcars, aes(fcyl, fill = fam)) +
  # Change the position to "dodge"
  geom_bar(position = "dodge") #con position = "dodge" mette a confronto le due feature in due barre affincate che una sopra l'altra

ggplot(mtcars, aes(cyl, fill = fam)) +
  # Change position to use the functional form, with width 0.2
  geom_bar(position = position_dodge(width = 0.2),alpha = 0.6)#stessa roba di quello sopra ma le due barre sono sia sovrapposte che affiancate, molto carino questo. alpha regola la trasparenza ricorda

#LINE PLOT
ggplot(economics, aes(date, unemploy)) +
  geom_line() #lineplot semplice

ggplot(fish.tidy, aes(x = Year, y = Capture,color = Species)) + #linee colorate a seconda della specie
  geom_line(aes(group = Species)) #ragruppa i vari sottodataset in sto caso le specie di pesci in un unico plot costituito da lineplot delle varie specie di pesci

