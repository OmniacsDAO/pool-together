library(tidyverse)

## Read in data
pools_data <- readRDS("pools_data.RDS")
holders_data <- readRDS("holders_data.RDS")
rewards_data_raw <- readRDS("rewards_data.RDS")

for (i in 1:length(rewards_data_raw)) {
    if (nrow(rewards_data_raw[[i]]) > 0) {
        rewards_data_raw[[i]]$Pool <- pools_data$Name[i]
    }
}

rewards_data <- rewards_data_raw %>%
    bind_rows() %>%
    as_tibble() %>%
    arrange(Awarded_Time) %>%
    mutate(TimeBetween = c(NA, diff(Awarded_Time)))

mean(rewards_data$TimeBetween, na.rm = TRUE)
median(rewards_data$TimeBetween, na.rm = TRUE)

ggplot(rewards_data, aes(x = TimeBetween)) +
    geom_histogram()

ggplot(rewards_data %>% mutate(ID = 1:nrow(.)), aes(x = ID, y = TimeBetween)) +
    geom_point() +
    geom_line()

## Flatten holders data
have_holders <- which(sapply(holders_data,nrow)>0)
h_names <- pools_data$Name[have_holders]
n_tokens <- pools_data$Base_Tok_Name[have_holders]
h_holders <- holders_data[have_holders]


flat <- do.call(rbind,mapply(function(x,y,z) cbind(x,Pool_Name=y, Token_Name=z),h_holders,h_names,n_tokens,SIMPLIFY=FALSE)) %>%
    as_tibble()

more_than_one <- flat %>%
    group_by(id) %>%
    summarise(Pools = length(unique(Pool_Name))) %>%
    filter(Pools > 1)

more_than_one %>% count()

flat %>%
    group_by(id) %>%
    summarise(Pools = length(unique(Pool_Name))) %>%
    filter(Pools == 1) %>%
    count()

flat %>%
    group_by(id) %>%
    summarise(Pools = length(unique(Pool_Name))) %>%
    arrange(desc(Pools))

incidence <- flat %>% select(Address = id, Pool_Name) %>%
    filter(Address %in% more_than_one$id) %>%
    mutate(Count = 1) %>%
    spread(key = Pool_Name, value = Count)
incidence[is.na(incidence)] <- 0

test <- 1 - dist(incidence %>% select(-Address) %>% t, method = "binary", upper = TRUE, diag = TRUE) %>% as.matrix

pools_data %>% group_by(Base_Tok_Name) %>% summarise(Reward = sum(Daily_Reward_Token_Val_USD))

res <- sapply(1:nrow(rewards_data), function(i) {
    x <- rewards_data$Prize_External_ERC20[i]
    do.call(rbind, strsplit(gsub("\\[|\\]", "", strsplit(x, "][", fixed = TRUE)[[1]]), ";")) %>%
        as_tibble() %>%
        mutate(Pool = rewards_data$Pool[i])
}) %>% bind_rows() %>%
    rename(Address = V1, Amount = V2, Name = V3, Ticker = V4) %>%
    mutate(Amount = readr::parse_number(Amount))

final <- res %>% group_by(Pool, Ticker) %>% summarise(Count = n()) %>% spread(key = Ticker, value = Count)
final[is.na(final)] <- 0

write_csv(final, "reward_diversity.csv")

final <- res %>% group_by(Name) %>% summarise(Amount = sum(Amount)) %>% arrange(desc(Amount))

res 
