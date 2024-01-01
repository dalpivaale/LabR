#STATISTICS
mean(belgium_consumption$consumption) #caoclo della media e mediana sulla feature consumption di quel dataset
median(belgium_consumption$consumption)

# Calculate mean and median consumption in USA
mean(usa_consumption$consumption)
median(usa_consumption$consumption)


Filter food_consumption for rows with data about Belgium and the USA.
Group the filtered data by country.
Calculate the mean and median of the kilograms of food consumed per person per year in each country. Call these columns mean_consumption and median_consumption.
food_consumption %>%
  # Filter for Belgium and USA      # %>% viene usato per buttare la roba alla sua sinistra nella funzione alla sua destra
  filter(country %in% c("Belgium", "USA")) %>%
  # Group by country                  # The %in% operator in R is used to check if the values of the first argument are present in the second argument and returns a logical  
  group_by(country) %>%
  # Get mean_consumption and median_consumption
  summarize(mean_consumption = mean(consumption),
            median_consumption = median(consumption))


food_consumption %>%  #butta dentro sto dataset al filtro che si becca solo le righe con quella condizione (== "rice)
  # Filter for rice food category
  filter(food_category == "rice") %>%
  # Create histogram of co2_emission      #e poi crea un istogramma di ciò
  ggplot(aes(co2_emission)) +
  geom_histogram()

#VARIANZA,STD,QUARTILI E QUANTILI
# Calculate the quartiles of co2_emission
quantile(food_consumption$co2_emission)

#caga fuori 10 quantili del dataset in input
quantile(food_consumption$co2_emission,probs = seq(0, 1, 0.1))



Calculate the variance and standard deviation of co2_emission for each food_category by grouping by and summarizing variance as var_co2 and standard deviation as sd_co2.
Create a histogram of co2_emission for each food_category using facet_wrap().
# Calculate variance and sd of co2_emission for each food_category
food_consumption %>% 
  group_by(food_category) %>% 
  summarize(var_co2 = var(co2_emission), #summarize varianza e std di co2_emission
            sd_co2 = sqrt(var(co2_emission)))

# Plot food_consumption with co2_emission on x-axis
ggplot(food_consumption,aes(x = co2_emission))+
  # Create a histogram
  geom_histogram()+
  # Create a separate sub-graph for each food_category
  facet_wrap(~food_category) #istogrammi per ogni categoaria di cibo

#CALCOLO QUARTILI
# Compute the first and third quartiles and IQR of total_emission
q1 <- quantile(emissions_by_country$total_emission, 0.25) #primo quartile
q3 <- quantile(emissions_by_country$total_emission, 0.75)#terzo quartile
iqr <- q3 - q1 #distanza interquartile

#definizione di outlier
Outlier: data point that is substantially different from the others
How do we know what a substantial difference is? A data point is an outlier if:
  data < Q1 ??? 1.5 × IQR or
data > Q3 + 1.5 × IQR

# Calculate total co2_emission per country: emissions_by_country
emissions_by_country <- food_consumption %>%
  group_by(country) %>%
  summarize(total_emission = sum(co2_emission))

# Compute the first and third quartiles and IQR of total_emission
q1 <- quantile(emissions_by_country$total_emission, 0.25)
q3 <- quantile(emissions_by_country$total_emission, 0.75)
iqr <- q3 - q1

# Calculate the lower and upper cutoffs for outliers
lower <- q1 - 1.5 * iqr
upper <- q3 + 1.5 * iqr

# Filter emissions_by_country to find outliers
emissions_by_country %>%
  filter(total_emission < lower|total_emission > upper) #filtra gli stati con emissioni < lower e > upper FIGO!!


Count the number of each group_size in restaurant_groups, then add a column called probability that contains the probability of randomly selecting a group of each size. Store this in a new data frame called size_distribution.
# Create probability distribution
size_distribution <- restaurant_groups %>%
  count(group_size) %>%
  mutate(probability = n / sum(n)) #mutate aggiunge al dataframe la colonna probability calcolata in quel modo

##
%>%
mutate(prob = n/sum(n)) #sta funzione aggiunge una colonna al dataset in sto caso "prob" con tale proprietà

##
# Set random seed to 31
set.seed(31) 

# Sample 5 deals without replacement
amir_deals %>%
  sample_n(5) #preleva 5 campioni a caso senza rimetterli nel "mazzo", se vuoi il replacement lo aggiungi nei parametri: replace = TRUE

###DISTRIBUZIONE DI PROBABILITà: asse x valori che la variabule può assumere, asse y la probailità di tali valori: discreta se x può assumere solo valori discreti
# Min and max wait times for back-up that happens every 30 min
min <- 0
max <- 30
# Calculate probability of waiting less than 5 mins
prob_less_than_5 <- punif(5, min , max ) #funzione per calcolare la prob di beccare valori < 5 in sto caso di aspettare meno di 5 minuti (puni: prob uniforme)
prob_less_than_5

# Calculate probability of waiting more than 5 mins
prob_greater_than_5 <-punif(5, min , max , lower.tail = FALSE)
# Calculate probability of waiting 10-20 mins
prob_between_10_and_20 <- punif(20,min,max) -punif(10,min,max)
prob_between_10_and_20

# Generate 1000 wait times between 0 and 30 mins, save in time column
wait_times %>%
  mutate(time = runif(1000, min = 0, max = 30))

#DISTRIBUZIONE BINOMIALE: descrive gli esperimetni con solo due esiti possibili 0 o 1
binom(# of trials, # of coins, # probability of heads/success)

  # Simulate 1 week of 3 deals
  deals <- rbinom(52, 3, 0.3)# caga fuori 1 vettore composto da 52 elementi casuali (i quali possono assumere valori 0 1 2). casuale
  
  dbinom(3, 3, 0.3)#binomiali: caga fuori la prob di ottenere 3 volte testa lanciando 3 volte la moneta
  pbinom(3,3,0.3)#prob P(head <= 3) con 3 lanci. l'ultimo parametro è la prob di ottenere testa ad ogni lancio
  
#DISTRIBUZIONE NORMALE
  # Probability of deal < 7500
  pnorm(7500, mean = 5000, sd = 2000)#ti dà la prob di un valore sotto i 7500 pescando un valore dal dataset
  # Probability of deal > 1000
  pnorm(1000, mean = 5000, sd = 2000,lower.tail = FALSE)# ti dà la prob di un valore sopra i 1000
  # Calculate amount that 75% of deals will be more than
  qnorm(0.75, #contrario
        mean = 5000,
        sd = 2000,
        lower.tail = FALSE) #ti dà il valore del dataset per il quale il 75% del dataset è sopra quel valore: es "180cm è il valore per il quale il 75% della popolazione è sopra di esso"
  #rnorm crea una distribuzione normale con quella media,std con 36 valori a caso
  new_sales <- new_sales %>% 
    mutate(amount = rnorm(36, mean = new_mean, sd = new_sd))

  sample_means <- replicate(1000, sample(data,5, replace =TRUE)%>% mean()) #replicate è una funzione che ripete in sto caso 1000 volte quello che le passi nel secondo parametro

  ##DISTRIBUZIONE DI POISSON, è come la normale ma la curva è scentrata, si sviluppa attorno alla media lambda: Si presta bene col tempo
  #ad esempio, qual'è la probabilità che il numero di adozioni animali questa settimana sia = 5 sapendo che la media settimanale (lamda) è 8?:
  dpois(5,lambda = 8)
  # <= 5 ?
  ppois(5, lamda = 8)  #se vuoi >= metti il parametro lower.tail = FALSE
  
  ##CORRELAZIONE: tra [0,1] oppure [-1,0] se = 1 tutti i punti stanno su una retta, vicino a 0 i punti sono spaiati non c'è una correlazione fra i dati
  # Add a linear trendline to scatterplot
  ggplot(world_happiness, aes(life_exp, happiness_score)) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE) #aggiunge una linea che passa x i punti, se imposta il margine di errore
  
  # Correlation between life_exp and happiness_score. OCCHIO CHE LA CORRELAZIONE SI BASA SU RELAZIONI LINEARI, quindi se x-y hanno una relazione quadratica ad esempio sto metodo fallisce
  cor(world_happiness$life_exp,world_happiness$happiness_score)#calcola la correlazione fra le due variabili
  #Una soluzione è applicare una trasformazione logartimica, o quadratica o 1/x per linearizzare i dati
  # Create log_gdp_per_cap column
  world_happiness <- world_happiness %>%
    mutate(log_gdp_per_cap = log(gdp_per_cap)) #applica una trasformazione logaritmica per linearizzare
  
  # Scatterplot of happiness_score vs. log_gdp_per_cap
  ggplot(world_happiness, aes(log_gdp_per_cap,happiness_score)) +
    geom_point()
  
  # Calculate correlation
  cor(world_happiness$log_gdp_per_cap, world_happiness$happiness_score)
  