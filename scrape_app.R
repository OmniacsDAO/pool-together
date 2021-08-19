## Load Required libraries
library(httr)
library(jsonlite)
library(lubridate)
library(ghql)

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
saveRDS(pools_data,"pools_data.RDS")


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
saveRDS(rewards_data,"rewards_data.RDS")
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
saveRDS(holders_data,"holders_data.RDS")
####################################################
####################################################






