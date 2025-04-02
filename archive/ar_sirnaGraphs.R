library(readxl)
library(tidyverse)
library(janitor)
#^this package has a function called clean_names which I love

dataAR <- read_excel(here("data", "unzipped", "AR_siRNA_speckle-imaging_20240814_PC3-GFP-AR-K22_day1.xlsx"), sheet = "PlateResults", range = "A10:U70", col_names = TRUE) %>%
  clean_names() %>%
  #^this throws up an error but i ignore it <3
  #convert columns to numeric
  mutate(nuclei_number_of_objects = as.numeric(nuclei_number_of_objects),
         nuclei_intensity_nucleus_alexa_488_mean_mean_per_well = as.numeric(nuclei_intensity_nucleus_alexa_488_mean_mean_per_well))
#cut up the "Compound" column into drug and dose
#aaaaand idk how to do that here i go to ask ken

for_plot <- dataAR %>%
  filter(compound != "PC3 GFP-AR K22")
scatterplot <- ggplot(data = for_plot, aes(x = spots_number_of_objects, y = nuclei_intensity_nucleus_alexa_488_mean_mean_per_well)) +
  geom_point() +
  theme_bw() +
  #^i use this because i think the normal r theme is ugly.... the one with the gray background
  ggtitle("# of puncta vs nuclei intensity")

#hmmmm this looks weird
