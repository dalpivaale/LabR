### INTRODUCTION
1) geom_point() adds points (as in a scatter plot).
2) geom_smooth() adds a smooth trend curve.

As you saw previously, these are added using the + operator.

ggplot(data, aes(x, y)) + #occhio che devi wrappare come factor la x
  geom_*() #1) fa i punti come plot. 2) una linea col trend

ggplot(mtcars, aes(wt, mpg,col = disp)) +
  geom_point() #il col = disp aggiunge la leggenda dei colori
data("PimaIndiansDiabetes2", package = "mlbench") # you must have the mlbench package installed
dim(PimaIndiansDiabetes2)
head(PimaIndiansDiabetes2)

pima.data <- na.omit(PimaIndiansDiabetes2)
library(ggplot2)

?ggplot(pima.data, aes(mass,glucose)) +
  geom_point()+geom_smooth()
###AESTHETICS
#All about aesthetics: color, shape and size

#In the video you saw 9 visible aesthetics. Let's apply them to a categorical variable - the cylinders in mtcars, cyl.

#These are the aesthetics you can consider within aes() in this chapter: x, y, color, fill, size, alpha, labels and shape.

#One common convention is that you don't name the x and y arguments to aes(), since they almost always come first, but you do name other arguments.

#In the following exercise the fcyl column is categorical. It is cyl transformed into a factor.

ggplot(pima.data, aes(mass,glucose, color = diabetes)) +
  geom_point(shape = 2,size = 2) #coloro i punti in base alla variabile che assegno a color in sto caso coloro i punti in base a diabetes. con size e shape modifichi dimensioni e forma 

ggplot(mtcars, aes(wt, mpg)) +
  # Set the point color and alpha
  geom_point(color = 'red',alpha=0.6) #puoi impostare il colore dei punti, alpha è la trasparenza dei punti

# A hexadecimal color
my_blue <- "#4ABEFF"

# Change the color mapping to a fill mapping
ggplot(mtcars, aes(wt, mpg, fill = fcyl)) +
  # Set point size to 10; shape to 1
  geom_point(color = my_blue, size = 10, shape = 1) #puoi fare + e aggiungere altre caratteristiche al plot con un altro geom_point()


ggplot(mtcars, aes(wt, mpg, color = fcyl)) +
  # Add text layer with label rownames(mtcars) and color red
  geom_text(color = "red",label = rownames(mtcars) ) #label aggiunge le etichette/scritte ad ogni punto del grafico


ggplot(mtcars, aes(mpg, qsec, color = fcyl, shape = fam,size = hp/wt)) +
  geom_point()    #figata ti compare nella legenda che il colore e la forma dei punti nel grafico sono funzione di quelle due features. Size è funzione del reapporto di quelle due features

##Set the position
palette <- c(automatic = "#377EB8", manual = "#E41A1C")


ggplot(mtcars, aes(fcyl, fill = fam)) +
  geom_bar() +
  labs(x = "Number of Cylinders", y = "Count")
scale_fill_manual("Transmission", values = palette)#scale_fill_manual() defines properties of the color scale (i.e. axis). The first argument sets the legend title. values is a named vector of colors to use.