###################################
# Zoology 800 Homework 4 Maddy Teter 9-25-25

#Objective 1 

#Install and load palmerpenguins

install.packages("palmerpenguins")
library(palmerpenguins)

View(penguins)


#Create a function to convert a continuous variable into a binary variable

binary <- function(x, breakpoint, low_label = "Low", high_label = "High") {
  ifelse(x < breakpoint, low_label, high_label)
}

#Use median penguin body mass as breakpoint (under 4050 = small, over 4050 = large)
median(penguins$body_mass_g, na.rm = TRUE)

#specifiy what the breakpoint is
breakpoint <- 4050

#Create a new column (size) in df that applies the function and gives binary
#label to all penguins based on body mass 
penguins$size <- binary(penguins$body_mass_g, breakpoint, low_label = "small", 
                              high_label = "large")

# Objective 2  - Generalize your function from Objective 1 to be able to accommodate
#any number of break points. This new function should allow the user to
#specify how many categories the data is broken into, the corresponding
#breakpoints, and the labels for each category

# Calculate various quartiles in body mass data column
quantile(penguins$body_mass_g, c(1/3, 2/3), na.rm = TRUE) #3700 g, 4500 g
min(penguins$body_mass_g, na.rm = TRUE) #2700 g
max(penguins$body_mass_g, na.rm = TRUE) #6300 g

#Create funciton that allows multiple breaks to apply 3 labels (Small, Med, Lg)
multi_cat <- function(x, breakpoints, labels) {
  cut(x,
      breaks = breakpoints,
      labels = labels,
      include.lowest = TRUE,
      right = FALSE)
}

#Input data into function mutli_cat
penguins$size_multi <- multi_cat(penguins$body_mass_g,
                            breakpoints = c(2700, 3700, 4550, 6300),
                            labels = c("small", "medium", "large"))

#See how many penguins fall under each size category 
table(penguins$size_multi, useNA = "ifany")

#Objective 3 - Incorporate species into function from objective 2

#Calculate bp values for each species using quantile function
adelie_bp <- quantile(penguins$body_mass_g [penguins$species == "Adelie"],
                   c(0/3, 1/3, 2/3, 3/3), na.rm = TRUE)

gentoo_bp <- quantile(penguins$body_mass_g [penguins$species == "Gentoo"],
                   c(0/3, 1/3, 2/3, 3/3), na.rm = TRUE)

chinstrap_bp <- quantile(penguins$body_mass_g [penguins$species == "Chinstrap"],
                      c(0/3, 1/3, 2/3, 3/3), na.rm = TRUE)


# Create a list with breakpoints for each species
breakpoints <- list(
  Adelie = c(adelie_bp), #has 4 values (min, 1/3 quartile, 2/3 quartile, and max)
  Gentoo = c(gentoo_bp),
  Chinstrap =c(chinstrap_bp)
)

#Create empty column to place values in for size by species 
penguins$size_species <- factor(NA, levels = c("small", "medium", "large")) 
#assigning column as factor with diff levels, keeping NA if body mass is NA


#Create for loop to return species labels by size for each penguin in dataset
#that is unique to each penguin species breakpoints

for (i in names(breakpoints)) { #i creates iteration for each species
  cuts <- breakpoints[[i]] #creating cut offs for each category of sizes specific to species
  
  mask <- penguins$species == i & !is.na(penguins$body_mass_g) 
  #ensures body mass isn't missing for each for relative to species 
  
  penguins$size_species[mask] <- cut(
    penguins$body_mass_g[mask], #selecting body mass data for certain species only
    breaks = cuts,#defining intervals using species specific cut points
    labels = c("small", "medium", "large"), #naming intervals
    include.lowest = TRUE, #includes lowest value in small category
    right = FALSE
  )
}


#Objective 4 - Create boxplot to visualize size categories 
#created for each species in objective 3

library(ggplot2)
ggplot(penguins, aes(x = size_species, y = body_mass_g, fill = size_species,)) +
  geom_boxplot() +
  facet_wrap(~ species) +
  scale_fill_brewer(palette = "Pastel1") + #automated pastel colors 
  labs(
    title = "Penguin Species Body Mass",
    x = "Size Category (species-specific)",
    y = "Body Mass (g)"
  ) +
  theme_minimal()


