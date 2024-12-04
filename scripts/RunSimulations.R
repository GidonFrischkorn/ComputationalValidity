library(here)

SimFiles <- list.files(path = "scripts", pattern = "Simulation_")
for(i in 1:length(SimFiles)){
  source(paste0("scripts/", SimFiles[i]))
}
