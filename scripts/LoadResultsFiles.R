# load required packages
pacman::p_load(here, osfr)

# retrieve OSF Project of the Tutorial Paper
ComputationalValidity_project <- osf_retrieve_node("sjwe2")

# Download rds files in the main repository
ComputationalValidity_project %>%
  osf_ls_files(n_max = Inf, pattern = "rds") %>%
  osf_download(path = here("output"), conflicts = "skip", verbose = FALSE)

# Download folders
ComputationalValidity_project %>%
  osf_ls_files(n_max = Inf, type = "folder") %>%
  osf_download(path = here("output"), conflicts = "skip", verbose = FALSE)

# Print short message about the files being downloaded
print("All missing results files have been downloaded sucesfully!")
