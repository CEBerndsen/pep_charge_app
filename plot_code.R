# Load packages
library(tidyverse)
library(Peptides)

# In UI side
# peptide sequence 
seq <- "HHHCDEDERRHHCCSSWWWFFIILLGG"

scale <- "Stryer"

# In server side
# make the data frame
df <- tibble("pH" = c(seq(from = 1, to = 14, by = 0.2)))

# calculate the charge
df <- df %>%
  mutate(charge = charge(seq = seq, pH = pH, pKscale = scale))

# calculate the zero value
zero <- pI(seq, pKscale = scale)

# calculate 0.2 * the max value to position text correctly
x20 <- max(df$charge) * 0.15

# make the plot
ggplot(df, aes(x = pH, y = charge)) +
  geom_point(aes(color = charge)) +
  # add crosshairs at the zero value
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = zero) +
  # get the color scale right
  scale_color_gradient2() +
  # make it look pretty
  scale_x_continuous(expand = c(0.01, 0.01)) +
  theme_bw() +
  theme(axis.title = element_text(size = 24, face = "bold", color = "black"),
        axis.text = element_text(size = 20, face = "bold", color = "black"),
        legend.position = "none") +
  # label the axes and the pI value
  labs(x = "pH", y = "Charge", caption = paste("pKa scale used was ", scale)) +
  annotate("text", x =  zero + 1.5, y = x20, label = paste("pI = ", round(zero, 2)), size = 6, color = "black")

pHint <- seq(from = 1, to = 14, by = 1)

# make a table
wide <- df %>% 
  filter(pH %in% pHint) %>%
  spread(., pH, charge, 1:2)
  
