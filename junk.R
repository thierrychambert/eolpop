library(eolpop)

fatalities_mean = c(0, 5, 10, 15, 20)
fatalities_se = fatalities_mean*0.05

pop_size_mean = 200
pop_size_se = 30
pop_size_type = "Npair"

pop_growth_mean = 1
pop_growth_se = 0.03

survivals <- c(0.5, 0.7, 0.8, 0.95)

fecundities <- c(0, 0, 0.05, 0.55)

DD_params <- list(rMAX = NULL, K = 1200, theta = 1)

time_horzion = 30
coeff_var_environ = 0.10
fatal_constant = "h"

run_test <- run_simul(nsim = 10, cumuated_impacts = FALSE,
          fatalities_mean, fatalities_se, onset_time = NULL,
          pop_size_mean, pop_size_se, pop_size_type,
          pop_growth_mean, pop_growth_se,
          survivals, fecundities, DD_params = DD_params,
          model_demo = NULL, time_horzion, coeff_var_environ, fatal_constant)



run_test
run0 <- run_test

























library(dplyr)
library(ggplot2)

# Get metrics and dimensions
out <- get_metrics(N)$scenario$impact_sc
TH <- dim(N)[2]
nsc <- dim(N)[3]

# Build dataframe
df <- as.data.frame(cbind(year = 1:nrow(out), out[,,1], scenario = 1))
for(j in 2:dim(out)[3]) df <- rbind(df, cbind(year = 1:nrow(out), out[,,j], scenario = j))

## Define Graphic Parameters
size = 1.5

# Plot lines
p <-
  ggplot(data = df, aes(x = year, y = avg)) +
  geom_line(data = filter(df, scenario > 1), size = size, aes(colour = factor(scenario))) +
  geom_line(data = filter(df, scenario == 1), size = size, colour = "black")

# Plot CIs
p <- p + geom_ribbon(data = filter(df, scenario > 1),
                     aes(ymin = uci, ymax = lci, fill = factor(scenario)), linetype = 0, alpha = 0.100)

# Add legend
Legend <- "parc"
nsc <- max(df$scenario)-1
p <- p + labs(x = "Annee", y = "Impact relatif",
              col = "Scenario", fill = "Scenario") +
  scale_color_hue(labels = paste(Legend, 1:nsc), aesthetics = c("colour", "fill"))


















out <- get_metrics(N)$scenario$impact_sc
TH <- dim(N)[2]
nsc <- dim(N)[3]

library(dplyr)

df <- as.data.frame(cbind(year = 1:nrow(out), out[,,1], scenario = 1))
for(j in 2:dim(out)[3]) df <- rbind(df, cbind(year = 1:nrow(out), out[,,j], scenario = j))
dim(df)
class(df)
names(df)

filter(df, scenario == 1)
filter(df, scenario > 1)


library(ggplot2)
library(plotly)

## pars
size = 1.5

# Plot lines
p <-
  ggplot(data = df, aes(x = year, y = avg)) +
  geom_line(data = filter(df, scenario > 1), size = size, aes(colour = factor(scenario))) +
  geom_line(data = filter(df, scenario == 1), size = size, colour = "black")

# Plot CIs
p <- p + geom_ribbon(data = filter(df, scenario > 1),
                     aes(ymin = uci, ymax = lci, fill = factor(scenario)), linetype = 0, alpha = 0.100)

# Add legend
Legend <- "parc"
nsc <- max(df$scenario)-1
p <- p + labs(x = "Annee", y = "Impact relatif",
              col = "Scenario", fill = "Scenario") +
  scale_color_hue(labels = paste(Legend, 1:nsc), aesthetics = c("colour", "fill"))
p

# Plot lines
p <-
  ggplot(data = df, aes(x = year, y = avg)) +
  geom_line(data = filter(df, scenario > 1), size = size, aes(colour = factor(scenario))) +
  geom_line(data = filter(df, scenario == 1), size = size, colour = "black")

# Plot CIs
p <- p + geom_ribbon(data = filter(df, scenario > 1),
                     aes(ymin = uci, ymax = lci, fill = factor(scenario)), linetype = 0, alpha = 0.100)

# Add legend
Legend <- "parc"
nsc <- max(df$scenario)-1
p <- p + labs(x = "Annee", y = "Impact relatif",
              col = "Scenario", fill = "Scenario") +
  scale_color_hue(labels = paste(Legend, 1:nsc)) +
  scale_fill_hue(labels = paste(Legend, 1:nsc))

p
ggplotly(p)

rm(fig)
fig <- ggplotly(p)
fig






#### Plotly
p <-
  ggplot(data = filter(df, scenario > 1), aes(x = year, y = avg)) +
  geom_line(size = size, aes(colour = factor(scenario)))


# Plot CIs
p <- p + geom_ribbon(aes(ymin = uci, ymax = lci, fill = factor(scenario)),
                     linetype = 0, alpha = 0.100)

# Add legend
Legend <- "parc"
nsc <- max(df$scenario)-1
p <- p + labs(x = "Annee", y = "Impact relatif",
              col = "Scenario", fill = "Scenario") +
  scale_color_hue(labels = paste(Legend, 1:nsc), aesthetics = c("colour", "fill"))
p

rm(fig)
fig <- ggplotly(p)
fig


fig <- ggplotly(p)
fig





#### Plotly

p <-
  ggplot(data = df, aes(x = year, y = avg)) +
  geom_line(size = size, aes(colour = factor(scenario)))
p

fig <- ggplotly(p, layerData = 1)
fig


##################


p <- ggplot(data = df, aes(x = year, y = avg, colour = factor(scenario)))
p <- p + geom_line(size = 2)
p

p <- p + geom_ribbon(aes(ymin = uci, ymax = lci, fill = factor(scenario)), linetype = 0, alpha = 0.075)
p


#########
p <- ggplot(data = df, aes(x = year, y = avg, colour = factor(scenario)))
p <- p + geom_line(size = 2)
p
