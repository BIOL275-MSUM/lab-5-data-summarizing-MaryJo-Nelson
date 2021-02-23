
# Load Packages -----------------------------------------------------------

library(tidyverse)


# Read Data --------------------------------------------------------------

iris
(iris <- as_tibble(iris))
help("iris")


# Question 1 --------------------------------------------------------------
  #Rename each variable so that it is all lower-case and uses an underscore _ instead 
  #of a period . in the name (the recommended coding style in the tidyverse style guide). 
  #Print the resulting table.

(i2 <- rename(iris,
       sepal_length = Sepal.Length,
       sepal_width = Sepal.Width,
       petal_length = Petal.Length,
       petal_width = Petal.Width,
       species = Species))


# Question 2 --------------------------------------------------------------
  #Convert the four numerical variables from mm to cm by multiplying by 10. 
  #Print the resulting table.

(i2_mm <- transmute(i2, 
       sepal_length_mm = sepal_length * 10,
       sepal_width_mm = sepal_width * 10,
       petal_length_mm = petal_length * 10,
       petal_width_mm = petal_width * 10,
       species = species))


# Question 3 --------------------------------------------------------------
  #Calculate sepal area and petal area (area is equal to length multiplied by width). 
  #Print a table with only the variables sepal area, petal area, and species.

(areas <- transmute(i2_mm,
                      sepal_area = sepal_length_mm * sepal_width_mm,
                      petal_area = petal_length_mm * petal_width_mm,
                    species = species))


# Question 4 --------------------------------------------------------------
  #Calculate the following statistics for the entire dataset from the sepal length 
  #variable and print the resulting table:

  #sample size
  #maximum value
  #minimum value
  #range
  #median
  #first quartile (q1)
  #third quartile (q2)
  #inter-quartile range (iqr)

(summary_sepal_length <- summarize(i2_mm,
          sample_size = n(),
          max_value = max(sepal_length_mm),
          min_value = min(sepal_length_mm),
          range = max_value - min_value,
          median = median(sepal_length_mm),
          first_quartile = quantile(sepal_length_mm, .25),   
          third_quartile = quantile(sepal_length_mm, .75),
          inter_quartile_range = IQR(sepal_length_mm)))
          

# Question 5 --------------------------------------------------------------
  # Calculate the following statistics for each species from the petal width variable 
  # and print the resulting table:
  
  #sample size
  #mean
  #standard deviation
  #variance
  #standard error of the mean
  #approximate 95% confidence interval

(species_grouped <- group_by(i2_mm, species))

(summary_petal_width <- summarize(species_grouped,
                                  sample_size = n(),
                                  mean = mean(petal_width_mm),
                                  sd = sd(petal_width_mm),
                                  var = var(petal_width_mm),
                                  sem = sd / sqrt(sample_size),
                                  ci_upper = mean + 2*sem,   
                                  ci_lower = mean - 2*sem))

# Question 6 --------------------------------------------------------------
  #Visualize the relationship between petal length and species using a strip plot.


ggplot(data = i2_mm) +
  geom_jitter(mapping = aes(y = petal_width_mm, x = species), shape = 1,
              alpha = .7) +
  labs(x = "Species", y = "Petal Width (mm)", title = "Petal widths of 150 Iris flowers organized by species (50/species)")


# Question 7 --------------------------------------------------------------
  #Starting with the previous graph, add the mean and 95% confidence interval for each species

ggplot(data = i2_mm) +
  geom_jitter(mapping = aes(y = petal_width_mm, x = species), shape = 1,
              alpha = .7) +
  geom_crossbar(data = summary_petal_width, mapping = aes(x = species, y = mean, 
                                                          ymax = ci_upper, ymin = ci_lower),
                color = "#EDDA74") +
  labs(x = "Species", y = "Petal Width (mm)", title = "Petal widths of 150 Iris flowers organized by species (50/species)",
       subtitle = "*Box plots show mean +/- two standard errors of the mean (roughly 95% confidence interval)")


# Question 8 --------------------------------------------------------------
  #Visualize the relationship between petal length, petal width, and species using a scatterplot. 
  #Map the two numerical variables to the x and y axes and map species to the color and shape aesthetics.

ggplot(data = i2_mm) +
  geom_point(mapping = aes(x = petal_length_mm, y = petal_width_mm, color = species),
               alpha = .4) +
  labs(x = "Petal Length (mm)", y = "Petal Width (mm)", color = "Species", 
       title = "Petal width vs. petal length in 150 Iris flowers, arranged by species")

