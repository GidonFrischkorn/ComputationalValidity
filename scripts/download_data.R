# Download simulation data from OSF (https://osf.io/6qnrv/)
# Run once from the project root before executing any analysis scripts.
# Requires the osfr package: install.packages("osfr")

library(osfr)

osf_project <- osf_retrieve_node("6qnrv")
osf_data    <- osf_ls_files(osf_project)
osf_data    <- osf_data[osf_data$name == "data", ]
osf_files   <- osf_ls_files(osf_data)

if (!dir.exists("data")) dir.create("data")
osf_download(osf_files, path = "data/", conflicts = "overwrite")

message("Data downloaded to data/")
