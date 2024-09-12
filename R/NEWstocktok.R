library(dplyr)
library(zoo)

# Load required libraries
library(quantmod)

# Download data for SET Index from Yahoo Finance
getSymbols("^SET.BK", from = "2022-05-01", to = "2024-06-01", src = "yahoo")

# Extract closing prices
set_index <- Cl(SET.BK)

# Aggregate to monthly frequency
monthly_data <- to.monthly(set_index, indexAt = "endof", OHLC = FALSE)

# Extract only the Close prices
monthly_close <- monthly_data[, "SET.BK.Close"]

# Save data to CSV file
write.csv(monthly_close, file = "set100_monthly_close.csv")

# Optionally, you can also view the first few rows of the monthly close data
head(monthly_close)



# Load the CSV file
returns_data <- read.csv("return.csv")

# Define NCC test function
ncc_test <- function(Rm, Ri) {
  mu_i <- mean(Ri, na.rm = TRUE)
  mu_m <- mean(Rm, na.rm = TRUE)
  delta_i <- sum((Ri - mu_i)^2, na.rm = TRUE) / max(sum((Rm - mu_m) * (Ri - mu_i), na.rm = TRUE), 1e-10)
  
  if (delta_i >= 1 && delta_i <= 10) {
    for (gamma in 2:10) {
      Rm_gamma <- Rm^(-gamma)
      # Remove NAs before correlation test
      valid_indices <- complete.cases(Rm_gamma, Ri)
      Rm_gamma <- Rm_gamma[valid_indices]
      Ri_valid <- Ri[valid_indices]
      
      if (length(Rm_gamma) > 2) {  # Ensure there are enough data points
        t_test <- cor.test(Rm_gamma, Ri_valid, alternative = "greater")
        if (!is.na(t_test$p.value) && t_test$p.value < 0.05) {
          return(FALSE)
        }
      }
    }
    return(TRUE)
  } else {
    return(FALSE)
  }
}

# Define the list of symbols (94 symbols as provided)
symbols <- c("AAV.return", "ADVANC.return", "AEONTS.return", "AMATA.return", "AOT.return", "AP.return", 
             "AWC.return", "BAM.return", "BANPU.return", "BBL.return", "BCH.return", "BCP.return", 
             "BCPG.return", "BDMS.return", "BEM.return", "BGRIM.return", "BH.return", "BLA.return", 
             "BTS.return", "BYD.return", "CBG.return", "CENTEL.return", "CHG.return", "CK.return", 
             "COM7.return", "CPALL.return", "CPF.return", "CPN.return", "CRC.return", "DELTA.return", 
             "DOHOME.return", "EA.return", "EGCO.return", "ERW.return", "FORTH.return", "GLOBAL.return", 
             "GPSC.return", "GULF.return", "GUNKUL.return", "HANA.return", "HMPRO.return", "ICHI.return", 
             "INTUCH.return", "IRPC.return", "IVL.return", "JMART.return", "JMT.return", "KBANK.return", 
             "KCE.return", "KKP.return", "KTB.return", "KTC.return", "LH.return", "M.return", "MEGA.return", 
             "MINT.return", "MTC.return", "NEX.return", "OR.return", "ORI.return", "OSP.return", 
             "PLANB.return", "PTT.return", "PTTEP.return", "PTTGC.return", "RATCH.return", "RBF.return", 
             "RCL.return", "SAPPE.return", "SAWAD.return", "SCB.return", "SCC.return", "SCGP.return", 
             "SIRI.return", "SISB.return", "SJWD.return", "SNNP.return", "SPALI.return", "SPRC.return", 
             "STA.return", "STGT.return", "TASCO.return", "TCAP.return", "THG.return", "TIDLOR.return", 
             "TISCO.return", "TKN.return", "TOA.return", "TOP.return", "TRUE.return", "TTB.return", 
             "TU.return", "VGI.return", "WHA.return")

# Example usage of the NCC function for stocks in the dataset
results <- list()
market_returns <- returns_data$SET.return

for (symbol in symbols) {
  if (symbol %in% colnames(returns_data)) {
    stock_returns <- returns_data[[symbol]]
    result <- if (ncc_test(market_returns, stock_returns)) "The NCC holds" else "The NCC does not hold"
    results[[symbol]] <- result
    print(paste(symbol, ":", result))
  }
}