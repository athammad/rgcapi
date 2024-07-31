#testing V2
source("./R/GCapiClientv2.R")
################################################################################

IDLOG <- "ahmed.t.hammad@gmail.comhsec"
PSWD <- "Trade123@"
APKEY <- "Ah.Hammad"

client <- GCapiClientV2$new(username = keyring::key_get(service = "fx_sytem", username = "IDLOG"),
                          password = keyring::key_get(service = "fx_sytem", username = "PSWD"),
                          appkey = keyring::key_get(service = "fx_sytem", username = "APKEY"))

account_info <- client$get_account_info()
print(account_info)
client$trading_account_id


