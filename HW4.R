#4
library(ggplot2)
#provided 
cars93 <- MASS::Cars93
ggplot(cars93, aes(x = Price, y = Fuel.tank.capacity)) +
  geom_point(color = "grey60") +
  geom_smooth(se = FALSE, method = "loess", formula = y ~ x, color = "#0072B2") +
  scale_x_continuous(
    name = "price (USD)",
    breaks = c(20, 40, 60),
    labels = c("$20,000", "$40,000", "$60,000")
  ) +
  scale_y_continuous(name = "fuel-tank capacity\n(US gallons)")
#to make it easier
base <- ggplot(cars93, aes(x = Price, y = Fuel.tank.capacity)) +
  geom_point(color = "grey60") +
  scale_x_continuous(
    name = "price (USD)",
    breaks = c(20, 40, 60),
    labels = c("$20,000", "$40,000", "$60,000")
  ) +
  scale_y_continuous(name = "fuel-tank capacity\n(US gallons)")
#(lm)#8fe388
p1 <- base + 
  geom_smooth(se = TRUE, method = "lm", formula = y ~ x, color = "#8fe388") +
  labs(title = "Smoothing Method: lm") +
  theme(plot.title = element_text(size = 14, color = "#8fe388"))

#(glm)#fe8d6d
p2 <- base + 
  geom_smooth(se = TRUE, method = "glm", formula = y ~ x, color = "#fe8d6d") +
  labs(title = "Smoothing Method: glm") +
  theme(plot.title = element_text(size = 14, color = "#fe8d6d"))

#(gam)#7c6bea
p3 <- base + 
  geom_smooth(se = TRUE, method = "gam", formula = y ~ x, color = "#7c6bea") +
  labs(title = "Smoothing Method: gam") +
  theme(plot.title = element_text(size = 14, color = "#7c6bea"))

print(p1)
print(p2)
print(p3)

#5
library(dplyr)
library(tidyr)
load("C:/Users/RJG11/Downloads/preprint_growth.rda") #please change the path if needed
head(preprint_growth)
preprint_growth %>% filter(archive == "bioRxiv") %>%
  filter(count > 0) -> biorxiv_growth
preprints<-preprint_growth %>% filter(archive %in%
                                        c("bioRxiv", "arXiv q-bio", "PeerJ Preprints")) %>%filter(count > 0) %>%
  mutate(archive = factor(archive, levels = c("bioRxiv", "arXiv q-bio", "PeerJ Preprints")))
preprints_final <- filter(preprints, date == ymd("2017-01-01"))
ggplot(preprints) +
  aes(date, count, color = archive, fill = archive) +
  geom_line(size = 1) +
  scale_y_continuous(
    limits = c(0, 600), expand = c(0, 0),
    name = "preprints / month",
    sec.axis = dup_axis( #this part is for the second y axis
      breaks = preprints_final$count, #and we use the counts to position our labels
      labels = c("arXivq-bio", "PeerJPreprints", "bioRxiv"),
      name = NULL)
  ) +
  scale_x_date(name = "year",
               limits = c(min(biorxiv_growth$date), ymd("2017-01-01"))) +
  scale_color_manual(values = c("#0072b2", "#D55E00", "#009e73"),
                     name = NULL) +
  theme(legend.position = "none")

#a
preprint_full <- preprint_growth %>%
  drop_na() %>%
  filter(count > 0 & year(date) > 2004)
#b
subset <- preprint_full %>%
filter(archive %in% c("bioRxiv", "F1000Research"))
#c-f
ggplot(subset, aes(x = date, y = count, color = archive)) +
  geom_line(linewidth = 1) +
  # (c) Manual color selection
  scale_color_manual(values = c("bioRxiv" = "#7c6bea", "F1000Research" = "#fe8d6d")) +
  # (e) Start x-axis from Feb 2014
  scale_x_date(limits = c(ymd("2014-02-01"),ymd("2017-01-01")), name = "Year") +
  # (f) Add Title
  labs(title = "Preprint Counts", y = "Preprints / Month", color = "Archive") +
  # (d) Move legend to the right
  theme(legend.position = "right")


