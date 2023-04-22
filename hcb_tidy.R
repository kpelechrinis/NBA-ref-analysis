# ---- start --------------------------------------------------------------

library(tidyverse)

# Create a directory for the data
local_dir     <- "data/"
if (!file.exists(local_dir)) dir.create(local_dir, recursive = T)

# ---- download -----------------------------------------------------------

# Set commit for the particular version of L2M data, 2023-04-20 update
commit <- "f2e89abcbbd1dd6ad0745525bd5405ef31d43432" 

l2m_url <- paste0("https://raw.githubusercontent.com/atlhawksfanatic/L2M/",
                  commit,
                  "/1-tidy/L2M/L2M.csv")
l2m_file <- paste0(local_dir, "L2M_", commit, ".csv")

if (!file.exists(l2m_file)) download.file(l2m_url, l2m_file)

l2m <- read_csv(l2m_file)

# ---- setup --------------------------------------------------------------

# Variables to adjust
szn = 2014:2022
var_group = "call_type" # really should just be call as it's more concise
n = 100
set.seed(324)

# Create event space from L2M sample (ie ignores CNC and maybe more)
l2m_sample <- l2m |>
  # Uncomment for the blank decisions to be INC
  # mutate(decision = ifelse(is.na(decision), "INC", decision)) |>
  filter(decision %in% c("IC", "INC", "CC"),
         season %in% szn)

# Get sample probabilities for the grouping variable
sample_probs <- l2m_sample |>
  group_by(!!rlang::sym(var_group)) |>
  summarise(
    n = n(),
    p1 = sum(decision %in% c("INC"), na.rm = T) / n,
    p2 = sum(decision %in% c("IC", "INC"), na.rm = T) / n
  )

# From the sample, event space results based on home status
sample_t <- l2m_sample |>
  mutate(
    result = case_when(
      committing_side == "home" & decision == "INC" ~ "inch",
      committing_side == "home" & decision == "IC" ~ "icc",
      disadvantaged_side == "home" & decision == "INC" ~ "incd",
      disadvantaged_side == "home" & decision == "IC" ~ "icd",
      T ~ "neither"
    )
  ) |>
  group_by(result) |>
  tally() |>
  pivot_wider(names_from = "result", values_from = "n") |>
  mutate(t_real = (inch + icd) - (incd + icc))

# Merge sample probabilities with the sample itself, easier to sim from
l2m_to_sample <- left_join(l2m_sample, sample_probs)

# For n simulations, take sample dataset and simulate for every event a random
#  number between 0 and 1 to determine if the event falls within the bounds of
#  an incorrect no-call, incorrect call, or correct call.
sample_simmed <- map(1:n, function(x) {
  print(paste0("sampled ", x, " at ", Sys.time()))
  l2m_to_sample |>
    mutate(
      monte = runif(n(), 0, 1),
      result = case_when(
        committing_side == "home" & monte < p1 ~ "inch",
        committing_side == "home" & monte < p2 ~ "icc",
        disadvantaged_side == "home" & monte < p1 ~ "incd",
        disadvantaged_side == "home" & monte < p2 ~ "icd",
        T ~ "neither"
      )
    ) |>
    group_by(result) |>
    tally() |>
    pivot_wider(names_from = "result", values_from = "n") |>
    mutate(t_sim = (inch + icd) - (incd + icc))
}) |>
  bind_rows()

# Summarize simulation
sample_simmed |>
  mutate(t_real = sample_t$t_real) |>
  summarise(
    t_real_mean = mean(t_real),
    t_sim_mean = mean(t_sim),
    effect = t_real_mean - t_sim_mean,
    emp_pval = sum(t_sim >= t_real) / n,
    sims = n
  )
