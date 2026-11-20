library(tidyverse)
tips <- read_csv("tips.csv")

glimpse(tips)

view(tips)


# ----
tips |> 
  mutate(
    table_size = case_when(
      size <= 2 ~ "small",
      size >= 5 ~ "big",
      .default = "medium"
    )
  )


# ----
# How do lunch and dinner compare on average bill, tip and no parties per day? (Mikołaj)


# by time
tips |>
  mutate(
    bill_per_person = total_bill / size,
    tip_per_person = tip / size,
    tip_pct = tip / total_bill * 100,
  ) |>
  group_by(time) |>
  summarise(
    n_parties   = n(),
    avg_bill_pp = round(mean(bill_per_person), 2),
    avg_tip_pp  = round(mean(tip_per_person), 2),
    avg_bill    = round(mean(total_bill), 2),
    median_bill = round(median(total_bill), 2),
    avg_tip     = round(mean(tip), 2),
    median_tip  = round(median(tip), 2),
    avg_tip_pct = round(mean(tip_pct), 2),
  ) |>
  arrange(
    case_when(
      time == "Lunch"  ~ 1,
      time == "Dinner" ~ 2,
      .default = 9
    )
  )

# ----
# Box plots over the day-time of total bill / tip / tip-rate (Mikołaj)
tips |>
  mutate(
    tip_pct = tip / total_bill * 100,
    day  = factor(day,  levels = c("Thur", "Fri", "Sat", "Sun")),
    time = factor(time, levels = c("Lunch", "Dinner"))
  ) |>
  pivot_longer(
    cols = c(tip, tip_pct),
    names_to = "measure",
    values_to = "value"
  ) |>
  mutate(
    measure = factor(
      measure,
      levels = c("tip", "tip_pct"),
      labels = c("Tip ($)", "Tip (% of bill)")
    )
  ) |>
  ggplot(aes(x = day, y = value, fill = time)) +
  geom_boxplot(position = position_dodge2(preserve = "single")) +
  facet_wrap(~ measure, scales = "free_y") +
  labs(
    title = "Tip and tip percentage by day and meal time",
    x = "Day", y = NULL, fill = "Meal time"
  ) +
  theme_minimal()

