# preliminaries
library(tidyverse)
theme_set(theme_minimal(16))
library(greta)
library(causact)

# getting the data
allWawaDF = read_csv("https://raw.githubusercontent.com/flyaflya/buad621/master/data/allWawa.csv",
                  col_types = c(col_integer(),
                                col_integer(),
                                col_integer(),
                                col_integer())) 

# create some summary data to include on plot
grpPctDF = allWawaDF %>%
  mutate(storeNum = as_factor(storeNum)) %>%
  group_by(storeNum) %>%
  summarize(maxOff = 0.65 * max(offers),
            maxPct = max(redemptions / offers),
            offers = sum(offers),
            redemptions = sum(redemptions)) %>%
  mutate(pctRedeemed = redemptions/offers) %>%
  mutate(pctRedeemedLabel = 
           paste0("Redemption Pct:\n",
                  round(100*pctRedeemed,1),
                  "%"))    %>%
  arrange(desc(pctRedeemed))

# plotting the data
allWawaDF %>%
  mutate(storeNum = factor(allWawaDF$storeNum,
                           levels = as.character(
                             1:max(allWawaDF$storeNum)
                             ))) %>%
  ggplot(aes(x = distanceKM, #map time to x-axis
             y = offers)) + #map numTrials to y-axis
  geom_col(aes(fill = "offers"),
           width = 0.75) + ## show offers
  geom_col(aes(y = redemptions, 
               fill = "redemptions"),
           width = 0.75) + # show redemptions
  facet_wrap(vars(storeNum), 
             labeller = label_both,
             scales = "free_y") + # one plot per store
  theme_minimal(11) + 
  theme(plot.caption = ## modify caption font and legend pos.
          element_text(size = 9, face = "italic"),
        legend.position = "bottom") +
  labs(x = "x-axis: Customer Distance from Store in KM",
       y = "# of Offers") +
  geom_text(data = grpPctDF, 
            label = grpPctDF$pctRedeemedLabel,
            aes(x = 8, y = maxOff)) + 
  theme(legend.position = c(0.9, 0.07), 
        legend.title = element_blank(), 
        axis.title.x = element_text(hjust = 1)) +
  scale_fill_manual(values = c("#7E9BB5","#FFC20A")) +
  scale_x_continuous(breaks = 2:10) 


## the generative DAG
graph = dag_create() %>%
  dag_node("# Redeemed","k",
           rhs = binomial(n, theta),
           data = allWawaDF$redemptions) %>%
  dag_node("# of Offers","n",
           data = allWawaDF$offers,
           child = "k") %>%
  dag_node("Redemption Probability","theta",
           rhs = 1 / (1 + exp(-y)),
           child = "k") %>%
  dag_node("Linear Predictor","y",
           rhs = alpha + beta*x,
           child = "theta") %>%
  dag_node("Base Succ Prob Param","alpha",
           rhs = student(nu_alpha,mu_alpha,sd_alpha),
           child = "y") %>%
  dag_node("Distance Effect Param","beta",
           rhs = student(nu_beta,mu_beta,sd_beta),
           child = "y")  %>%
  dag_node("Outlier Measure for Base Success","nu_alpha",
           rhs = gamma(2,0.1),
           child = "alpha") %>%
  dag_node("Outlier Measure for Distance Effect","nu_beta",
           rhs = gamma(2,0.1),
           child = "beta")  %>%
  dag_node("Exp Succ Prob Param","mu_alpha",
           rhs = normal(-3,1),
           child = "alpha") %>%
  dag_node("Exp Distance Effect Param","mu_beta",
           rhs = normal(0,3),
           child = "beta") %>%
  dag_node("Inter-store Variation for Base Succ","sd_alpha",
           rhs = uniform(0,2),
           child = "alpha") %>%
  dag_node("Inter-store Variation for Dist Eff","sd_beta",
           rhs = uniform(0,1),
           child = "beta") %>%
  dag_node("DistanceKM","x",
           data = allWawaDF$distanceKM,
           child = "y") %>%
  dag_plate("Store Number","j",
            nodeLabels = c("alpha","beta"),
            data = allWawaDF$storeNum) %>%
  dag_node("Store Number","j",
           data = allWawaDF$storeNum,
           child = "y") %>%
  dag_plate("Observation","i",
            nodeLabels = c("j","k","n","theta","y","x"))
graph %>% dag_render()
graph %>% dag_render(shortLabel = TRUE)

# get the posterior
drawsDF = graph %>% dag_greta()

# compare the store-level parameters
drawsDF %>% 
  select(starts_with("alpha") | starts_with("beta")) %>%
  dagp_plot()

# view the corporation-level parameters
drawsDF %>% 
  select(-(starts_with("alpha") | starts_with("beta"))) %>%
  dagp_plot()


