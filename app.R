library(shiny)
library(shinythemes)
library(tidyverse)
library(DT)
library(httr)
library(jsonlite)
library(lubridate)
library(ghql)
library(thematic)
library(igraph)
library(network)
library(intergraph)
library(GGally)
library(randomcoloR)


thematic_shiny()

addResourcePath("images", "images")

my_dollars <- function(x) {
    ifelse(x < .01, paste0("$", x), scales::dollar(x))
}

ui <- fluidPage(theme = shinytheme("superhero"),

    titlePanel("PoolTogether Interactive Data Analysis Application"),

    sidebarLayout(
        sidebarPanel(width = 3,
                     div(img(src = "images/logo.png"), style = "text-align: center;"),
                     hr(),
                     selectInput("reward_pool", "Select a Pool", choices = NULL),
                     actionButton("scrape", "Scrape New Data", style="color: #fff; background-color: #522EAD; border-color: #522EAD")
        ),

        mainPanel(width = 9,
            tabsetPanel(id = "tabs",
                        
                tabPanel("Data",
                         h4("Pools Data"),
                         DT::dataTableOutput("pools_table"),
                         
                         h4("Rewards Data"),
                         DT::dataTableOutput("rewards_table"),
                         
                         h4("Holders Data"),
                         DT::dataTableOutput("holders_table")
                ),
                tabPanel("Holders",
                         h4("Distribution of Holder Balances across Pools (Fixed Scale)"),
                         plotOutput("holders_dist"),
                         h4("Distribution of Holder Balances across Pools (Free Scale)"),
                         plotOutput("holders_dist_free")
                ),
                tabPanel("Rewards",
                         h4("Average Time between Reward Announcements in Minutes"),
                         h6("A line graph highlighting the increased frequency of lottery drawings"),
                         plotOutput("rewards_time"),
                ),
                tabPanel("Diversity",
                         h4("Reward Pool Diversity"),
                         plotOutput("graph"),
                         DT::dataTableOutput("diversity")
                )
            )
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    
    holders <- reactiveFileReader(intervalMillis = 1000, 
                                  filePath = "data/holders_data.RDS", 
                                  readFunc = readRDS,
                                  session = session)
    
    rewards <- reactiveFileReader(intervalMillis = 1000, 
                                  filePath = "data/rewards_data.RDS", 
                                  readFunc = readRDS,
                                  session = session)
    
    pools <- reactiveFileReader(intervalMillis = 1000, 
                                filePath = "data/pools_data.RDS", 
                                readFunc = readRDS,
                                session = session)
    
    observe({
        choices <- 1:nrow(pools())
        names(choices) <- pools()$Name
        
        updateSelectInput(session, "reward_pool", choices = choices)
    })
    
    output$pools_table <- renderDT({
        my_vals = 1:nrow(pools())
        my_vals2 <- pools()$Name
        my_colors = ifelse(as.character(my_vals) == input$reward_pool, '#cabad1', NA)
        
        return(datatable(iris, style = "bootstrap"))
    })
    
    output$rewards_table <- renderDT({
        return(datatable(rewards()[[as.numeric(input$reward_pool)]],
                         style = "bootstrap", options = list(scrollX = TRUE)))
    })
    
    output$holders_table <- renderDT({
        return(flattened() %>% filter(Pool_Name == unique(Pool_Name)[as.numeric(input$reward_pool)]) %>%
                   select(id, `USD Balance` = Bal) %>%
                   datatable(style = "bootstrap", options = list(scrollX = TRUE)))
    })
    
    observeEvent(input$scrape, {
        withProgress(message = "Scraping newest data, please wait a few seconds...", expr = {
            ## Fetch data
            chain1_data <- GET("https://pooltogether-api.com/pools/1.json")
            chain137_data <- GET("https://pooltogether-api.com/pools/137.json")
            parsed_pools <- c(content(chain1_data,"parsed"),content(chain137_data,"parsed"))
            
            lapply(parsed_pools,function(x) names(x$tokens))
            
            ## Parse Pool
            null_to_na <- function(x) ifelse(is.null(x),NA,x)
            parse_pool <- function(pool)
            {
                data.frame(	
                    Name = pool$name,
                    Symbol = pool$symbol,
                    Address = pool$contract$prizePool$address,
                    Subgraph_Version = pool$contract$subgraphVersion,
                    Num_Winners = pool$config$numberOfWinners,
                    Prize_Freq_Secs = pool$config$prizePeriodSeconds,
                    Pool_Type = pool$prizePool$type,
                    
                    Base_Tok_Address = pool$tokens$underlyingToken$address,
                    Base_Tok_Decimals = pool$tokens$underlyingToken$decimals,
                    Base_Tok_Name = pool$tokens$underlyingToken$name,
                    Base_Tok_Symbol = pool$tokens$underlyingToken$symbol,
                    Base_Tok_Val_USD = pool$tokens$underlyingToken$usd,
                    
                    Ticket_Address = pool$tokens$ticket$address,
                    Ticket_Decimals = pool$tokens$ticket$decimals,
                    Ticket_Name = pool$tokens$ticket$name,
                    Ticket_Symbol = pool$tokens$ticket$symbol,
                    Num_Ticket_Holders = pool$tokens$ticket$numberOfHolders,
                    Ticket_Val_Base_Tok = pool$tokens$ticket$totalSupply,
                    Ticket_Val_USD = pool$tokens$ticket$totalValueUsd,
                    
                    Sponsorship_Address = pool$tokens$sponsorship$address,
                    Sponsorship_Decimals = pool$tokens$sponsorship$decimals,
                    Sponsorship_Name = pool$tokens$sponsorship$name,
                    Sponsorship_Symbol = pool$tokens$sponsorship$symbol,
                    Num_Sponsorship_Holders = pool$tokens$sponsorship$numberOfHolders,
                    Sponsorship_Val_Base_Tok = pool$tokens$sponsorship$totalSupply,
                    Sponsorship_Val_USD = pool$tokens$sponsorship$totalValueUsd,
                    
                    Reserve_Val_Base_Tok = pool$reserve$amount,
                    Reserve_Val_USD = pool$reserve$totalValueUsd,
                    Reserve_Rate = pool$reserve$rate,
                    
                    Daily_Reward = null_to_na(pool$tokenListener$dripRatePerDay),
                    Daily_Reward_Token_Name = null_to_na(pool$tokens$tokenFaucetDripToken$name),
                    Daily_Reward_Token_Decimals = null_to_na(pool$tokens$tokenFaucetDripToken$decimals),
                    Daily_Reward_Token_Symbol = null_to_na(pool$tokens$tokenFaucetDripToken$symbol),
                    Daily_Reward_Token_Val_USD = null_to_na(pool$tokens$tokenFaucetDripToken$usd),
                    Effective_APR = null_to_na(pool$tokenListener$apr),
                    
                    Prize_Current_Index = pool$prize$currentPrizeId,
                    Prize_Current_State = pool$prize$currentState,
                    Prize_Remaining_Secs = strtoi(pool$prize$prizePeriodRemainingSeconds$hex),
                    Prize_Period_Started_At = as_datetime(strtoi(pool$prize$prizePeriodStartedAt$hex)),
                    
                    
                    Prize_Total_USD = pool$prize$totalValueUsd,
                    Prize_Internal = pool$prize$totalInternalAwardsUsd,
                    Prize_External = pool$prize$totalExternalAwardsValueUsd,
                    Prize_External_Yield = null_to_na(pool$prize$yield$comp$totalValueUsd),
                    Prize_External_ERC20 = pool$prize$erc20Awards$totalValueUsd
                    
                )
            }
            pools_data <- do.call(rbind,lapply(parsed_pools,parse_pool))
            saveRDS(pools_data,"data/pools_data.RDS")
            
            
            ####################################################
            ## Collect Past Prize History
            ####################################################
            get_rewards <- function(poolAddress,version)
            {
                grph_url <- paste0("https://api.thegraph.com/subgraphs/name/pooltogether/pooltogether-v",gsub("\\.","_",version))
                con_r <- GraphqlClient$new(grph_url)
                qry_r <- Query$new()
                qry_r$query('prize_data',
                            'query prize_data($poolAddress: String!)
		{
			prizePool(id:$poolAddress)
			{
				prizes(first:1000,orderBy: awardedTimestamp, orderDirection: desc)
				{
					id
					awardedTimestamp
					awardedBlock
					totalTicketSupply
					prizePeriodStartedTimestamp
					lockBlock
					awardedControlledTokens
					{
						id
						amount
						token
						winner
						prize
						{
							id
							prizePool
							{
								underlyingCollateralDecimals
							}
						}
					}
					awardedExternalErc20Tokens
					{
						id
						winner
						address
						balanceAwarded
						name
						symbol
						decimals
					}
					awardedExternalErc721Nfts
					{
						id
						address
						tokenIds
						winner
					}
				}
			}
		}')
                rewards <- fromJSON(con_r$exec(qry_r$queries$prize_data, list(poolAddress = poolAddress)))$data$prizePool$prizes
                rewards_df <- data.frame(
                    Awarded_Block = rewards$awardedBlock,
                    Awarded_Time = as_datetime(as.numeric(rewards$awardedTimestamp)),
                    Prize_Period_Started_At = as_datetime(as.numeric(rewards$prizePeriodStartedTimestamp)),
                    Ticket_Supply = rewards$totalTicketSupply,
                    Prize_External_ERC20 = do.call(c,lapply(rewards$awardedExternalErc20Tokens,function(x) paste0("[",x$address,";",as.numeric(x$balanceAwarded)/(10^as.numeric(x$decimals)),";",x$name,";",x$symbol,"]",collapse=""))),
                    Prize_Internal = do.call(c,lapply(rewards$awardedControlledTokens,function(x) sum(as.numeric(x$amount)/(10^as.numeric(x$prize$prizePool$underlyingCollateralDecimals)))))
                )
                return(rewards_df)
                
            }
            rewards_data <- list()
            for(idx in 1:nrow(pools_data))
            {
                rewards_data[[idx]] <- get_rewards(pools_data$Address[idx],pools_data$Subgraph_Version[idx])
                message(idx)
            }
            saveRDS(rewards_data,"data/rewards_data.RDS")
            ####################################################
            ####################################################
            
            ####################################################
            ## Collect Holder History
            ####################################################
            get_holders <- function(ticketAddress,version)
            {
                grph_url <- paste0("https://api.thegraph.com/subgraphs/name/pooltogether/pooltogether-v",gsub("\\.","_",version))
                con_h <- GraphqlClient$new(grph_url)
                qry_h <- Query$new()
                qry_h$query('holder_data',
                            'query holder_data($ticketAddress: String!,$first: Int!, $skip: Int!)
	{
		controlledTokenBalances(first:$first,skip:$skip,orderBy: balance, orderDirection: desc,where:{controlledToken:$ticketAddress})
		{
			balance
			account
			{
				id
			}
		}
	}')
                first = 1000
                skip = 0
                holders = data.frame()
                while(TRUE)
                {
                    holders_t <- fromJSON(con_h$exec(qry_h$queries$holder_data, list(ticketAddress = ticketAddress,first=first,skip=skip)))$data$controlledTokenBalances
                    holders_t <- data.frame(id = holders_t$account$id,balance = holders_t$balance)
                    if(nrow(holders_t) == 0) break()
                    rownames(holders_t) <- as.character(nrow(holders)+(1:nrow(holders_t)))
                    holders <- do.call(rbind,list(holders,holders_t))
                    if(nrow(holders_t) < 1000) break()
                    skip <- skip + 900
                }
                return(unique(holders))
            }
            holders_data <- list()
            for(idx in 1:nrow(pools_data))
            {
                holders_data[[idx]] <- get_holders(pools_data$Ticket_Address[idx],pools_data$Subgraph_Version[idx])
                message(idx)
            }
            saveRDS(holders_data,"data/holders_data.RDS")
            ####################################################
            ####################################################
        })
    })
    
    flattened <- reactive({
        holders_data <- holders()
        pools_data <- pools()
        
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
        
        return(holders_flat)
    })
    
    output$holders_dist <- renderPlot({
        
        ggplot(flattened(), aes(x = Bal)) +
            geom_histogram(aes(fill = Pool_Name), bins=100) +
            scale_x_log10(labels = my_dollars, breaks = c(1e-16, 1e-14, 1e-12, 1e-10, 1e-8, 1e-6, 1e-4, 1e-2, 1, 100, 10000, 1000000, 100000000)) +
            facet_wrap(~Pool_Name, scales = "free_y") +
            theme(
                axis.text.x = element_text(angle = 20, hjust = 1),
                legend.position = "off"
            )
    })
    
    output$holders_dist_free <- renderPlot({
        
        ggplot(flattened(), aes(x = Bal)) +
            geom_histogram(aes(fill = Pool_Name), bins=100) +
            scale_x_log10(labels = my_dollars, breaks = c(1e-16, 1e-14, 1e-12, 1e-10, 1e-8, 1e-6, 1e-4, 1e-2, 1, 100, 10000, 1000000, 100000000)) +
            facet_wrap(~Pool_Name, scales = "free") +
            theme(
                axis.text.x = element_text(angle = 20, hjust = 1),
                legend.position = "off"
            )
    })
    
    output$rewards_time <- renderPlot({
        rewards_data1 <- rewards() %>%
            bind_rows() %>%
            as_tibble() %>%
            arrange(Awarded_Time) %>%
            mutate(TimeBetween = c(NA, diff(Awarded_Time)))
        
        ggplot(rewards_data1 %>% mutate(ID = 1:nrow(.)), aes(x = ID, y = TimeBetween)) +
            geom_point() +
            geom_line() +
            scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
            labs(
                y = "Time Between Rewards (Minutes)"
            )
    })
    
    incidence <- reactive({
        holders_data <- holders()
        pools_data <- pools()
        
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
        
        incidence <- flat %>% select(Address = id, Pool_Name) %>%
            filter(Address %in% more_than_one$id) %>%
            mutate(Count = 1) %>%
            spread(key = Pool_Name, value = Count)
        incidence[is.na(incidence)] <- 0
        
        return(incidence)
    })
    
    output$diversity <- DT::renderDT({
        test <- 1 - dist(incidence() %>% select(-Address) %>% t, method = "binary", upper = TRUE, diag = TRUE) %>% as.matrix
        
        test %>%
            datatable(style = "bootstrap", options = list(scrollX = TRUE))
    })
    
    output$graph <- renderPlot({
        dist_mat <- incidence()
        sim_mat <- 1 - dist(dist_mat %>% select(-Address) %>% t, method = "binary", upper = TRUE, diag = TRUE) %>% as.matrix
        
        links <- data.frame(source=character(),target=character(),importance=numeric())
        for(idx in 1:nrow(sim_mat))
        {
            temp_df <- data.frame(
                source = rownames(sim_mat)[idx],
                target = colnames(sim_mat),
                importance = round(as.numeric(sim_mat[idx,])*10,2)
            )
            links <- rbind(links,temp_df)
        }
        links <- links[!(links$importance %in% c(0,10)),]
        nodes <- data.frame(
            name = unique(c(links$source,links$target)),
            size = c(170,311,5124,3,1368,13,1037,72,1796),
            color = distinctColorPalette(length(unique(c(links$source,links$target))))
        )
        # nodes$
        sim_net <- graph_from_data_frame(d=links, vertices=nodes, directed=T)
    
        ## Plot
        ggnet2(
            sim_net,
            node.size = "size",
            node.color = "color",
            edge.size="importance",
            edge.color = "grey",
            size.legend=NULL
        ) +  
            geom_text(
                aes(label = label, y = y + 0),
                size=3
            ) 
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
