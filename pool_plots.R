library(tidyverse)

pools_data <- readRDS("~/Downloads/pools_data.RDS")
holders_data <- readRDS("~/Downloads/holders_data.RDS")
rewards_data <- readRDS("~/Downloads/rewards_data.RDS")

## Flatten holders data
have_holders <- which(sapply(holders_data,nrow)>0)
h_names <- pools_data$Name[have_holders]
h_val <- pools_data$Ticket_Val_Base_Tok[have_holders]
h_valusd <- pools_data$Ticket_Val_USD[have_holders]
h_dec <- pools_data$Base_Tok_Decimals[have_holders]
h_holders <- holders_data[have_holders]
holders_flat <- do.call(rbind,mapply(function(x,y,z,w,xx) cbind(x,Pool_Name=y,Value=z,USD=w,Dec=xx),h_holders,h_names,h_val,h_valusd,h_dec,SIMPLIFY=FALSE)) %>%
    as_tibble() %>%
    mutate(across(c(balance, Value, USD, Dec), as.numeric)) %>%
    mutate(Price = USD / Value) %>%
    mutate(Bal = balance / (10^Dec) * Price)




ggplot(holders_flat, aes(x = Bal)) +
    geom_histogram(aes(fill = Pool_Name), bins=100) +
    scale_x_log10(labels = scales::dollar) +
    facet_wrap(~Pool_Name, scales = "free") +
    theme(
        axis.text.x = element_text(angle = 20, hjust = 1),
        legend.position = "off"
    )

my_dollars <- function(x) {
    ifelse(x < .01, paste0("$", x), scales::dollar(x))
}

ggplot(holders_flat, aes(x = Bal)) +
    geom_histogram(aes(fill = Pool_Name), bins=100) +
    scale_x_log10(labels = my_dollars, breaks = c(1e-16, 1e-14, 1e-12, 1e-10, 1e-8, 1e-6, 1e-4, 1e-2, 1, 100, 10000, 1000000, 100000000)) +
    facet_wrap(~Pool_Name, scales = "free") +
    theme(
        axis.text.x = element_text(angle = 20, hjust = 1),
        legend.position = "off"
    )

ggplot(holders_flat, aes(x = Bal)) +
    geom_histogram(aes(fill = Pool_Name), bins=100) +
    scale_x_log10(labels = my_dollars, breaks = c(1e-16, 1e-14, 1e-12, 1e-10, 1e-8, 1e-6, 1e-4, 1e-2, 1, 100, 10000, 1000000, 100000000)) +
    facet_wrap(~Pool_Name, scales = "free_y", ncol = 1) +
    theme(
        axis.text.x = element_text(angle = 20, hjust = 1),
        legend.position = "off"
    )
