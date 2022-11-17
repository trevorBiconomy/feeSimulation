library(shiny)
library(dplyr)
library(tidyr)
library(ggplot2)

# define standard values
# model params
# note, we take averages and variance of daily averages over the last 30 days. 
# This makes sense since for each day, we are assuming our txs are all executed at the averages (gas price and units). 

# sd of daily txs. One number for all chains. On average, it is close to 20% across chains. 
sd_daily_txs_constant <- .20


# min daily txs on all chains
min_daily_txs <- 1000

# sd of avg daily gas price
eth_sd_gas_price_gwei <- 11
polygon_sd_gas_price_gwei <- 204
bnb_sd_gas_price_gwei <- 0.46
op_sd_gas_price_gwei <- 0.326

# premium caps in gwei
eth_gwei_cap <- 10
polygon_gwei_cap <- 40
bnb_gwei_cap <- 2
op_gwei_cap <- .25

# 30 day avg of avg gas used per tx
eth_avg_gas_units_consumed <- 101813
polygon_avg_gas_units_consumed <- 196185
bnb_avg_gas_units_consumed <- 122554
op_avg_gas_units_consumed <- 522806

# sd of avg gas consumed per tx
eth_sd_gas_units_consumed <- 5547
polygon_sd_gas_units_consumed <- 19582
bnb_sd_gas_units_consumed <- 12793
op_sd_gas_units_consumed <-143731

# min gas price & units consumed for each chain
eth_min_gas_price_gwei <- 5
polygon_min_gas_price_gwei <- 15
bnb_min_gas_price_gwei <- 2
op_min_gas_price_gwei <- .1
min_gas_units_consumed <- 21000

# tx counts per account
eth_tx_count_per_account <- 5.98
polygon_tx_count_per_account <- 12.97
bnb_tx_count_per_account <- 8.61
op_tx_count_per_account <- 25.07

# fixed costs
cloud_compute_service_cost <- 3000
engineer_labor_cost <- 60000
blockchain_tx_simulation_cost <- 550
rpc_node_cost <- 300 + 200

# launch period in days
n_beta <- 30
# number of simulations per round
p <- 500
t <- 1:p

ui <- fluidPage(
  
  # Application title
  titlePanel("SDK Revenue - All Chains - 30 days"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      numericInput("base_eth_usd", "ETH/USD Price:", 1300, min = .0001, max = 100000),
      numericInput("base_matic_usd", "MATIC/USD Price:", .95, min = .0001, max = 100000),
      numericInput("base_bnb_usd", "BNB/USD Price:", 300, min = .0001, max = 100000),
      numericInput("eth_avg_gas_price_gwei", "Ethereum Avg Gas price Gwei:", 25, min = 1, max = 100000),
      numericInput("polygon_avg_gas_price_gwei", "Polygon Avg Gas price Gwei:", 252, min = 1, max = 100000),
      numericInput("bnb_avg_gas_price_gwei", "BNB Chain Avg Gas price Gwei:", 7, min = 1, max = 100000),
      numericInput("op_avg_gas_price_gwei", "Optimism Avg Gas price Gwei:", 0.85, min = .0001, max = 100000),
      numericInput("dao_premium", "DAO Premium:", .0075, min = .0001, max = 1),
      numericInput("eth_relayer_premium", "Ethereum Relayer Premium:", .09, min = .0001, max = 1),
      numericInput("polygon_relayer_premium", "Polygon Relayer Premium:", .12, min = .0001, max = 1),
      numericInput("bnb_relayer_premium", "BNB Chain Relayer Premium:", .12, min = .0001, max = 1),
      numericInput("op_relayer_premium", "Optimism Relayer Premium:", .12, min = .0001, max = 1),
      numericInput("daily_tx_baseline", "Daily Tx Baseline:", 100000, min = 2000, 50000000),
      numericInput("eth_tx_share", "Ethereum % Share of Daily Txs:", .05, min = .01, 1),
      numericInput("polygon_tx_share", "Polygon % Share of Daily Txs:", .80, min = .01, .96),
      numericInput("bnb_tx_share", "BNB Chain % Share of Daily Txs:", .10, min = .01, .96),
      numericInput("op_tx_share", "Optimism % Share of Daily Txs:", .05, min = .01, .96),
      width = 3
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("basePlotHist"),
      plotOutput("allPlotHist"),
      plotOutput("allPlotPoly"),
      width = 9
    )
  ),
  p("Stats found here: https://dune.com/trebor_yatska/evm-gas-markets"),
  p(paste0("Premium caps (gwei): Ethereum: ", eth_gwei_cap, 
           "; Polygon ", polygon_gwei_cap,
           "; BNB Chain ", bnb_gwei_cap,
           "; Optimism ", op_gwei_cap)),
  p(paste0("Std. dev. of avg. daily gas price (gwei): Ethereum: ", scales::comma_format()(eth_sd_gas_price_gwei), 
           "; Polygon ", scales::comma_format()(polygon_sd_gas_price_gwei),
           "; BNB Chain ", scales::comma_format(accuracy = .001)(bnb_sd_gas_price_gwei),
           "; Optimism ", scales::comma_format(accuracy = .001)(op_sd_gas_price_gwei))),
  p(paste0("Average gas units consumed per tx: Ethereum ", scales::comma_format()(eth_avg_gas_units_consumed),
           "; Polygon ", scales::comma_format()(polygon_avg_gas_units_consumed), 
           "; BNB Chain ", scales::comma_format()(bnb_avg_gas_units_consumed), 
           "; Optimism ",scales::comma_format()(op_avg_gas_units_consumed))),
  p(paste0("Std. dev. of avg. gas units consumed per tx: Ethereum ", scales::comma_format()(eth_sd_gas_units_consumed),
           "; Polygon ", scales::comma_format()(polygon_sd_gas_units_consumed),
           "; BNB Chain ", scales::comma_format()(bnb_sd_gas_units_consumed),
           "; Optimism ", scales::comma_format()(op_sd_gas_units_consumed))),
  p(paste0("Std. dev. of avg. daily tx volume: all chains ", sd_daily_txs_constant, "%"))
)



# Define server logic required to draw a histogram
server <- function(input, output) {
  
  # mau cost
  mau_cost <- function(mau) {
    # web3auth
    if (mau <= 1000) {
      web3auth_cost <- 0
    } else if(mau > 1000&mau<=10000) {
      web3auth_cost <- 0.031 * mau
    } else if(mau > 10000&mau<=100000) {
      web3auth_cost <- 0.02818 * mau
    } else if(mau > 100000&mau<=500000) {
      web3auth_cost <- 0.01849 * mau
    } else if(mau > 500000&mau<=1000000) {
      web3auth_cost <- 0.0125 * mau
    } else {
      web3auth_cost <- 0.01 * mau
    }
    return (web3auth_cost)
  }
  
  relayer_beta <- function (t, n_beta, mean_daily_txs, sd_daily_txs, 
                            eth_avg_gas_price_gwei, eth_sd_gas_price_gwei,eth_avg_gas_units_consumed, eth_sd_gas_units_consumed, 
                            polygon_avg_gas_price_gwei, polygon_sd_gas_price_gwei,polygon_avg_gas_units_consumed, polygon_sd_gas_units_consumed,
                            bnb_avg_gas_price_gwei, bnb_sd_gas_price_gwei,bnb_avg_gas_units_consumed, bnb_sd_gas_units_consumed,
                            op_avg_gas_price_gwei, op_sd_gas_price_gwei,op_avg_gas_units_consumed, op_sd_gas_units_consumed,
                            eth_min_gas_price_gwei, polygon_min_gas_price_gwei, bnb_min_gas_price_gwei, op_min_gas_price_gwei,
                            min_gas_units_consumed, tx_count_per_account,
                            eth_tx_count_per_account, polygon_tx_count_per_account, bnb_tx_count_per_account, op_tx_count_per_account,
                            eth_tx_share,polygon_tx_share,bnb_tx_share,op_tx_share) {
    # create vectors to hold calcs
    daily_txs <- vector("list",n_beta)
    eth_gas_units_consumed_per_tx <- vector("list",n_beta)
    polygon_gas_units_consumed_per_tx <- vector("list",n_beta)
    bnb_gas_units_consumed_per_tx <- vector("list",n_beta)
    op_gas_units_consumed_per_tx <- vector("list",n_beta)
    eth_gas_price_gwei <- vector("list",n_beta)
    polygon_gas_price_gwei <- vector("list",n_beta)
    bnb_gas_price_gwei <- vector("list",n_beta)
    op_gas_price_gwei <- vector("list",n_beta)
    
    # tx count for the simulated period
    period_tx_count <- 0
    # simulate keeping tx count constant - beta period
    for (tx_day in 1:n_beta) {
      # variable path dependent gas price.
      if (tx_day == 1) {
        eth_gas_price_gwei[[tx_day]] <- max(eth_min_gas_price_gwei,  rnorm(1, eth_avg_gas_price_gwei, eth_sd_gas_price_gwei))
        polygon_gas_price_gwei[[tx_day]] <- max(polygon_min_gas_price_gwei,  rnorm(1, polygon_avg_gas_price_gwei, polygon_sd_gas_price_gwei))
        bnb_gas_price_gwei[[tx_day]] <- max(bnb_min_gas_price_gwei,  rnorm(1, bnb_avg_gas_price_gwei, bnb_sd_gas_price_gwei))
        op_gas_price_gwei[[tx_day]] <- max(op_min_gas_price_gwei,  rnorm(1, op_avg_gas_price_gwei, op_sd_gas_price_gwei))
      } else {
        eth_gas_price_gwei[[tx_day]] <- max(eth_min_gas_price_gwei,  rnorm(1, eth_gas_price_gwei[[tx_day-1]], eth_sd_gas_price_gwei))
        polygon_gas_price_gwei[[tx_day]] <- max(polygon_min_gas_price_gwei,  rnorm(1, polygon_gas_price_gwei[[tx_day-1]], polygon_sd_gas_price_gwei))
        bnb_gas_price_gwei[[tx_day]] <- max(bnb_min_gas_price_gwei,  rnorm(1, bnb_gas_price_gwei[[tx_day-1]], bnb_sd_gas_price_gwei))
        op_gas_price_gwei[[tx_day]] <- max(op_min_gas_price_gwei,  rnorm(1, op_gas_price_gwei[[tx_day-1]], op_sd_gas_price_gwei))
      }
      #now calculate path independent variables
      daily_txs[[tx_day]] <- max(min_daily_txs, rnorm(1, mean_daily_txs, sd_daily_txs))
      eth_gas_units_consumed_per_tx[[tx_day]] <- max(min_gas_units_consumed, rnorm(1, eth_avg_gas_units_consumed, eth_sd_gas_units_consumed))
      polygon_gas_units_consumed_per_tx[[tx_day]] <- max(min_gas_units_consumed, rnorm(1, polygon_avg_gas_units_consumed, polygon_sd_gas_units_consumed))
      bnb_gas_units_consumed_per_tx[[tx_day]] <- max(min_gas_units_consumed, rnorm(1, bnb_avg_gas_units_consumed, bnb_sd_gas_units_consumed))
      op_gas_units_consumed_per_tx[[tx_day]] <- max(min_gas_units_consumed, rnorm(1, op_avg_gas_units_consumed, op_sd_gas_units_consumed))
      
      # keep track of tx count across the period
      period_tx_count <- period_tx_count + daily_txs[[tx_day]]
    }
    
    # calculate mau cost
    mau <- ((period_tx_count*eth_tx_share)/eth_tx_count_per_account) + ((period_tx_count*polygon_tx_share)/polygon_tx_count_per_account) + 
      ((period_tx_count*bnb_tx_share)/bnb_tx_count_per_account) + ((period_tx_count*op_tx_share)/op_tx_count_per_account)
    mau_cost_all <- mau_cost(mau)
    
    # combine lists
    beta_period_results <- c(rep(t,n_beta),rep(mean_daily_txs,n_beta),daily_txs,rep(mau_cost_all,n_beta),
                             eth_gas_units_consumed_per_tx,polygon_gas_units_consumed_per_tx,bnb_gas_units_consumed_per_tx,op_gas_units_consumed_per_tx,
                             eth_gas_price_gwei,polygon_gas_price_gwei,bnb_gas_price_gwei,op_gas_price_gwei)
    dim(beta_period_results) <- c(n_beta,12)
    
    # return combined list
    return(beta_period_results)
  }
  
  get_plot_data <- reactive({
    
    #calculate sd of daily txs
    tx_volume = input$daily_tx_baseline
    sd_daily_txs <- tx_volume * sd_daily_txs_constant
    
    # generate bins based on input$bins from ui.R
    beta_relayer_earnings <- lapply(t, relayer_beta, mean_daily_txs=tx_volume, sd_daily_txs=sd_daily_txs,n_beta=n_beta, 
                                    eth_avg_gas_price_gwei=input$eth_avg_gas_price_gwei, eth_sd_gas_price_gwei=eth_sd_gas_price_gwei,eth_avg_gas_units_consumed=eth_avg_gas_units_consumed,eth_sd_gas_units_consumed=eth_sd_gas_units_consumed, 
                                    polygon_avg_gas_price_gwei=input$polygon_avg_gas_price_gwei, polygon_sd_gas_price_gwei=polygon_sd_gas_price_gwei,polygon_avg_gas_units_consumed=polygon_avg_gas_units_consumed,polygon_sd_gas_units_consumed=polygon_sd_gas_units_consumed,
                                    bnb_avg_gas_price_gwei=input$bnb_avg_gas_price_gwei, bnb_sd_gas_price_gwei=bnb_sd_gas_price_gwei,bnb_avg_gas_units_consumed=bnb_avg_gas_units_consumed,bnb_sd_gas_units_consumed=bnb_sd_gas_units_consumed,
                                    op_avg_gas_price_gwei=input$op_avg_gas_price_gwei, op_sd_gas_price_gwei=op_sd_gas_price_gwei,op_avg_gas_units_consumed=op_avg_gas_units_consumed,op_sd_gas_units_consumed=op_sd_gas_units_consumed,
                                    eth_min_gas_price_gwei=eth_min_gas_price_gwei,polygon_min_gas_price_gwei=polygon_min_gas_price_gwei,
                                    bnb_min_gas_price_gwei=bnb_min_gas_price_gwei,op_min_gas_price_gwei=op_min_gas_price_gwei,
                                    min_gas_units_consumed=min_gas_units_consumed,
                                    eth_tx_count_per_account=eth_tx_count_per_account, polygon_tx_count_per_account=polygon_tx_count_per_account, 
                                    bnb_tx_count_per_account=bnb_tx_count_per_account, op_tx_count_per_account=op_tx_count_per_account,
                                    eth_tx_share=input$eth_tx_share,polygon_tx_share=input$polygon_tx_share,bnb_tx_share=input$bnb_tx_share,
                                    op_tx_share=input$op_tx_share
    )
    
    # # convert list to df
    df <- as.data.frame(array(unlist(do.call(rbind,beta_relayer_earnings)),dim=c(p*n_beta,12)))
    colnames(df) <- c("sim_run","baseline_tx_count","daily_tx_count","mau_cost_usd",
                      "eth_gas_per_tx", "polygon_gas_per_tx", "bnb_gas_per_tx", "op_gas_per_tx", "eth_gwei_price", "polygon_gwei_price",  "bnb_gwei_price",  "op_gwei_price")
    
    # calculated fields
    # add user tx cost usd column and convert revenue columns to usd from native
    df <- df %>%
      mutate(eth_daily_txs = daily_tx_count * input$eth_tx_share, 
             polygon_daily_txs = daily_tx_count * input$polygon_tx_share, 
             bnb_daily_txs = daily_tx_count * input$bnb_tx_share, 
             op_daily_txs = daily_tx_count * input$op_tx_share,
             dly_eth_sdk_revenue_usd = (((eth_daily_txs * eth_gas_per_tx) * pmin((eth_gwei_price * (input$eth_relayer_premium+input$dao_premium)),eth_gwei_cap))/1e9)*input$base_eth_usd,
             dly_eth_relayer_revenue_usd = (input$eth_relayer_premium/(input$eth_relayer_premium+input$dao_premium))*dly_eth_sdk_revenue_usd,
             dly_eth_dao_revenue_usd = (input$dao_premium/(input$eth_relayer_premium+input$dao_premium))*dly_eth_sdk_revenue_usd,
             dly_polygon_sdk_revenue_usd = (((polygon_daily_txs * polygon_gas_per_tx) * pmin((polygon_gwei_price * (input$polygon_relayer_premium+input$dao_premium)),polygon_gwei_cap))/1e9)*input$base_matic_usd,
             dly_polygon_relayer_revenue_usd = (input$polygon_relayer_premium/(input$polygon_relayer_premium+input$dao_premium))*dly_polygon_sdk_revenue_usd,
             dly_polygon_dao_revenue_usd = (input$dao_premium/(input$polygon_relayer_premium+input$dao_premium))*dly_polygon_sdk_revenue_usd,
             dly_bnb_sdk_revenue_usd = (((bnb_daily_txs * bnb_gas_per_tx) * pmin((bnb_gwei_price * (input$bnb_relayer_premium+input$dao_premium)),bnb_gwei_cap))/1e9)*input$base_bnb_usd,
             dly_bnb_relayer_revenue_usd = (input$bnb_relayer_premium/(input$bnb_relayer_premium+input$dao_premium))*dly_bnb_sdk_revenue_usd,
             dly_bnb_dao_revenue_usd = (input$dao_premium/(input$bnb_relayer_premium+input$dao_premium))*dly_bnb_sdk_revenue_usd,
             dly_op_sdk_revenue_usd = (((op_daily_txs * op_gas_per_tx) * pmin((op_gwei_price * (input$op_relayer_premium+input$dao_premium)),op_gwei_cap))/1e9)*input$base_eth_usd,
             dly_op_relayer_revenue_usd = (input$op_relayer_premium/(input$op_relayer_premium+input$dao_premium))*dly_op_sdk_revenue_usd,
             dly_op_dao_revenue_usd = (input$dao_premium/(input$op_relayer_premium+input$dao_premium))*dly_op_sdk_revenue_usd,
             dly_total_sdk_revenue_usd = dly_eth_sdk_revenue_usd + dly_polygon_sdk_revenue_usd + dly_bnb_sdk_revenue_usd + dly_op_sdk_revenue_usd,
             dly_total_relayer_revenue_usd = dly_eth_relayer_revenue_usd + dly_polygon_relayer_revenue_usd + dly_bnb_relayer_revenue_usd + dly_op_relayer_revenue_usd,
             dly_total_dao_revenue_usd = dly_eth_dao_revenue_usd + dly_polygon_dao_revenue_usd + dly_bnb_dao_revenue_usd + dly_op_dao_revenue_usd,
             dly_total_cost_all_items_usd = mau_cost_usd + cloud_compute_service_cost + engineer_labor_cost + blockchain_tx_simulation_cost + rpc_node_cost
      )
    
    # aggregate by sim run
    df_sim_run <- df %>% 
      group_by(sim_run) %>%
      summarise(eth_avg_dly_gas_price = mean(eth_gwei_price),
                polygon_avg_dly_gas_price = mean(polygon_gwei_price),
                bnb_avg_dly_gas_price = mean(bnb_gwei_price),
                op_avg_dly_gas_price = mean(op_gwei_price),
                totl_eth_dao_rev_usd = sum(dly_eth_dao_revenue_usd),
                totl_eth_relayer_rev_usd = sum(dly_eth_relayer_revenue_usd),
                totl_eth_sdk_rev_usd = sum(dly_eth_sdk_revenue_usd),
                totl_polygon_dao_rev_usd = sum(dly_polygon_dao_revenue_usd),
                totl_polygon_relayer_rev_usd = sum(dly_polygon_relayer_revenue_usd),
                totl_polygon_sdk_rev_usd = sum(dly_polygon_sdk_revenue_usd),
                totl_bnb_dao_rev_usd = sum(dly_bnb_dao_revenue_usd),
                totl_bnb_relayer_rev_usd = sum(dly_bnb_relayer_revenue_usd),
                totl_bnb_sdk_rev_usd = sum(dly_bnb_sdk_revenue_usd),
                totl_op_dao_rev_usd = sum(dly_op_dao_revenue_usd),
                totl_op_relayer_rev_usd = sum(dly_op_relayer_revenue_usd),
                totl_op_sdk_rev_usd = sum(dly_op_sdk_revenue_usd),
                total_mo_cost_usd = mean(dly_total_cost_all_items_usd)) %>%
      mutate(totl_sdk_rev_usd = totl_eth_sdk_rev_usd + totl_polygon_sdk_rev_usd + totl_bnb_sdk_rev_usd + totl_op_sdk_rev_usd,
             totl_relayer_rev_usd = totl_eth_relayer_rev_usd + totl_polygon_relayer_rev_usd + totl_bnb_relayer_rev_usd + totl_op_relayer_rev_usd,
             totl_dao_rev_usd = totl_eth_dao_rev_usd + totl_polygon_dao_rev_usd + totl_bnb_dao_rev_usd + totl_op_dao_rev_usd
      )
    
    # pivot longer
    df_sim_run_pivot <- df_sim_run %>%
      transmute(sim_run=sim_run,
                total_mo_cost_usd = total_mo_cost_usd,
                ethereum = totl_eth_sdk_rev_usd,
                polygon = totl_polygon_sdk_rev_usd,
                bnb = totl_bnb_sdk_rev_usd,
                optimism = totl_op_sdk_rev_usd) %>%
      pivot_longer(cols = c(ethereum,polygon,bnb,optimism),
                   names_to = "chain",
                   values_to = "sdk_rev_usd"
      )
    
    # return df
    return(df_sim_run_pivot) 
  })
  
  # base histogram - sum of all chains
  output$basePlotHist <- renderPlot({
    # get pivoted df & calc sums
    df_sums <- get_plot_data() %>% group_by(sim_run) %>% summarise(sdk_rev_usd = sum(sdk_rev_usd),total_mo_cost_usd = mean(total_mo_cost_usd))
    
    ggplot(df_sums, aes(sdk_rev_usd)) +
      geom_histogram() +
      geom_vline(aes(xintercept = total_mo_cost_usd,color='costUSD'),size=1) +
      scale_color_manual(name = paste0(n_beta," day cost"), values = c(costUSD = "red")) +
      scale_x_continuous(labels=scales::dollar_format()) +
      labs(title = paste0("Total SDK Tx Revenue - ", n_beta," days"), x = "total SDK tx revenue usd", y = "number of simulations")
  })
  
  # Total relay revenue hist plot - stacked by chain
  output$allPlotHist <- renderPlot({
    
    ggplot(get_plot_data(), aes(sdk_rev_usd, color = chain)) +
      geom_histogram(position = "identity") +
      scale_x_continuous(labels=scales::dollar_format()) +
      labs(title = paste0("Total SDK Tx Revenue - ", n_beta," days"), x = "total SDK tx revenue usd", y = "number of simulations") +
      facet_wrap(~chain, scales = "free")
    
  })
  
  output$allPlotPoly <- renderPlot({
    ggplot(get_plot_data(), aes(sdk_rev_usd, colour = chain)) +
      geom_freqpoly(binwidth = 500) +
      scale_x_continuous(labels=scales::dollar_format()) +
      labs(title = paste0("SDK Tx Revenue Frequency by Chain - ", n_beta," days"), x = "total SDK tx revenue usd", y = "number of simulations")
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
