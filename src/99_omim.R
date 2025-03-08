# install.packages("remotes")
# remotes::install_github('davetang/romim')

library(romim)
help(package = "romim")

omim_result <- get_omim(147920, geneMap = TRUE)

# not a real key
set_key('AAAAAAAAAAAAAAAAAAAAAA')


print("awaiting API key")

# https://github.com/davetang/romim?tab=readme-ov-file
