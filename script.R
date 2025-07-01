+     select(id, condition, score_meaning) %>%     # note: ‘id’ is the participant ID here
  +     separate_rows(score_meaning, sep = ";") %>%  # one row per selected option
  +     mutate(
    +         score_meaning = str_trim(score_meaning),
    +         is_good       = score_meaning %in% good_opts
    +     ) %>%
  +     group_by(id, condition) %>%
  +     summarise(
    +         n_correct      = sum(is_good, na.rm = TRUE),
    +         total_possible = length(good_opts),
    +         pct_correct    = n_correct / total_possible * 100,
    +         .groups = "drop"
    +     )
> 
  > # 3) Descriptive summary by condition
  > desc_understand <- understand_scores %>%
    +     group_by(condition) %>%
    +     summarise(
      +         N           = n(),
      +         Mean_n      = mean(n_correct),
      +         SD_n        = sd(n_correct),
      +         Mean_pct    = mean(pct_correct),
      +         SD_pct      = sd(pct_correct),
      +         .groups     = "drop"
      +     )
  > 
    > print(desc_understand)
  # A tibble: 3 × 6
  condition     N Mean_n  SD_n Mean_pct SD_pct
  <chr>     <int>  <dbl> <dbl>    <dbl>  <dbl>
    1 c2pa         50   1.06 0.935     17.7   15.6
  2 color        50   1.4  1.05      23.3   17.5
  3 no_color     50   1.28 0.882     21.3   14.7
  > 
    > # 4) One‐way ANOVA on the raw correct‐count
    > aov_u <- aov(n_correct ~ condition, data = understand_scores)
  > summary(aov_u)
  Df Sum Sq Mean Sq F value Pr(>F)
  condition     2   2.97  1.4867    1.62  0.201
  Residuals   147 134.90  0.9177               
  > TukeyHSD(aov_u, "condition")
  Tukey multiple comparisons of means
  95% family-wise confidence level
  
  Fit: aov(formula = n_correct ~ condition, data = understand_scores)
  
  $condition
  diff       lwr      upr     p adj
  color-c2pa      0.34 -0.113631 0.793631 0.1817118
  no_color-c2pa   0.22 -0.233631 0.673631 0.4860742
  no_color-color -0.12 -0.573631 0.333631 0.8059096
  
  > 
    > # 5) Bar‐plot of mean % correct with SD error‐bars
    > ggplot(desc_understand, aes(x = condition, y = Mean_pct, fill = condition)) +
    +     geom_col(show.legend = FALSE, width = 0.6) +
    +     geom_errorbar(aes(ymin = Mean_pct - SD_pct, ymax = Mean_pct + SD_pct),
                        +                   width = 0.2) +
    +     geom_text(aes(label = sprintf("%.1f%%", Mean_pct)), vjust = -0.5) +
    +     scale_y_continuous(expand = expansion(mult = c(0,0.1)), limits = c(0,100)) +
    +     labs(
      +         title = "Actual Label Understanding by Condition",
      +         x     = "Condition",
      +         y     = "Mean % of Correct 'Good' Answers"
      +     ) +
    +     theme_minimal(base_size = 14) +
    +     theme(plot.title = element_text(face = "bold", hjust = 0.5))
  > 
    > library(dplyr)
  > library(purrr)
  > library(tidyr)
  > library(jsonlite)
  > library(ggplot2)
  > 
    > # 1) Parse out the JSON in rounds_clean as before, including the `label_explained` flag
    > safe_parse_article <- function(txt) {
      +     if (is.na(txt) || !nzchar(txt)) return(list(
        +         selected_article_had_label = NA,
        +         label_explained            = NA
        +     ))
      +     tryCatch(
        +         fromJSON(txt, simplifyVector = TRUE)[c("selected_article_had_label","label_explained")],
        +         error = function(e) list(
          +             selected_article_had_label = NA,
          +             label_explained            = NA
          +         )
        +     )
      + }
  > 
    > rounds2 <- rounds_clean %>%
    +     select(id, participant_id, article) %>%
    +     mutate(parsed = map(article, safe_parse_article)) %>%
    +     unnest_wider(parsed) %>%
    +     select(id, participant_id, selected_article_had_label, label_explained)
  > 
    > # 2) Join that back onto mid_df (which has your condition & round_id)
    > mid2 <- mid_df %>%
    +     filter(condition != "nolabel") %>%
    +     left_join(rounds2, by = c("round_id" = "id", "participant_id"))
  > 
    > # 3A) **By‐round** click rate: % of article‐views where the “explain” was clicked
    > round_summary <- mid2 %>%
    +     group_by(condition) %>%
    +     summarise(
      +         views             = n(),
      +         expands           = sum(label_explained, na.rm=TRUE),
      +         pct_expand_views  = expands / views * 100,
      +         .groups = "drop"
      +     )
  > 
    > print(round_summary)
  # A tibble: 3 × 4
  condition views expands pct_expand_views
  <chr>     <int>   <int>            <dbl>
    1 c2pa        150       6                4
  2 color       150       3                2
  3 no_color    150       9                6
  > #> # A tibble: 2 x 4
    > #>   condition views expands pct_expand_views
    > #>   <chr>     <int>   <int>            <dbl>
    > #> 1 color       210      45             21.4
    > #> 2 c2pa        215      98             45.6
    > 
    > # 3B) **By‐participant** ever‐expanded rate: % of people in each cond who clicked at least once
    > participant_summary <- mid2 %>%
    +     group_by(condition, participant_id) %>%
    +     summarise(ever_expanded = any(label_explained, na.rm=TRUE), .groups = "drop") %>%
    +     group_by(condition) %>%
    +     summarise(
      +         n_participants    = n(),
      +         n_ever_expanded   = sum(ever_expanded),
      +         pct_ever_expanded = n_ever_expanded / n_participants * 100,
      +         .groups = "drop"
      +     )
  > 
    > print(participant_summary)
  # A tibble: 3 × 4
  condition n_participants n_ever_expanded pct_ever_expanded
  <chr>              <int>           <int>             <dbl>
    1 c2pa                  50               5                10
  2 color                 50               3                 6
  3 no_color              50               9                18
  > #> # A tibble: 2 x 4
    > #>   condition n_participants n_ever_expanded pct_ever_expanded
    > #>   <chr>              <int>            <int>             <dbl>
    > #> 1 color                70               35              50  
    > #> 2 c2pa                 75               61              81.3
    > 
    > # 4) Bar‐plot of “views‐based” click rate
    > ggplot(round_summary, aes(x = condition, y = pct_expand_views, fill = condition)) +
    +     geom_col(show.legend = FALSE, width = 0.6) +
    +     geom_text(aes(label = sprintf("%.1f%%", pct_expand_views)), vjust = -0.5) +
    +     scale_y_continuous(expand = expansion(c(0, .1)), limits = c(0,100)) +
    +     labs(
      +         title = "Label‐Explanation Clicks per Article View",
      +         subtitle = "% of article‐view events where participants clicked “explain”",
      +         x = "Condition",
      +         y = "% Expand Clicks"
      +     ) +
    +     theme_minimal(base_size = 14) +
    +     theme(plot.title=element_text(face="bold", hjust=0.5))
  > 
    > library(dplyr)
  > library(purrr)
  > library(tidyr)
  > library(jsonlite)
  > library(ggplot2)
  > 
    > # 1) Parse out the JSON in rounds_clean as before, including the `label_explained` flag
    > safe_parse_article <- function(txt) {
      +     if (is.na(txt) || !nzchar(txt)) return(list(
        +         selected_article_had_label = NA,
        +         label_explained            = NA
        +     ))
      +     tryCatch(
        +         fromJSON(txt, simplifyVector = TRUE)[c("selected_article_had_label","label_explained")],
        +         error = function(e) list(
          +             selected_article_had_label = NA,
          +             label_explained            = NA
          +         )
        +     )
      + }
  > 
    > rounds2 <- rounds_clean %>%
    +     select(id, participant_id, article) %>%
    +     mutate(parsed = map(article, safe_parse_article)) %>%
    +     unnest_wider(parsed) %>%
    +     select(id, participant_id, selected_article_had_label, label_explained)
  > 
    > # 2) Join that back onto mid_df (which has your condition & round_id)
    > mid2 <- mid_df %>%
    +     filter(condition != "nolabel") %>%
    +     left_join(rounds2, by = c("round_id" = "id", "participant_id"))
  > 
    > # 3A) **By‐round** click rate: % of article‐views where the “explain” was clicked
    > round_summary <- mid2 %>%
    +     group_by(condition) %>%
    +     summarise(
      +         views             = n(),
      +         expands           = sum(label_explained, na.rm=TRUE),
      +         pct_expand_views  = expands / views * 100,
      +         .groups = "drop"
      +     )
  > 
    > print(round_summary)
  # A tibble: 3 × 4
  condition views expands pct_expand_views
  <chr>     <int>   <int>            <dbl>
    1 c2pa        150       6                4
  2 color       150       3                2
  3 no_color    150       9                6
  > #> # A tibble: 2 x 4
    > #>   condition views expands pct_expand_views
    > #>   <chr>     <int>   <int>            <dbl>
    > #> 1 color       210      45             21.4
    > #> 2 c2pa        215      98             45.6
    > 
    > # 3B) **By‐participant** ever‐expanded rate: % of people in each cond who clicked at least once
    > participant_summary <- mid2 %>%
    +     group_by(condition, participant_id) %>%
    +     summarise(ever_expanded = any(label_explained, na.rm=TRUE), .groups = "drop") %>%
    +     group_by(condition) %>%
    +     summarise(
      +         n_participants    = n(),
      +         n_ever_expanded   = sum(ever_expanded),
      +         pct_ever_expanded = n_ever_expanded / n_participants * 100,
      +         .groups = "drop"
      +     )
  > 
    > print(participant_summary)
  # A tibble: 3 × 4
  condition n_participants n_ever_expanded pct_ever_expanded
  <chr>              <int>           <int>             <dbl>
    1 c2pa                  50               5                10
  2 color                 50               3                 6
  3 no_color              50               9                18
  > #> # A tibble: 2 x 4
    > #>   condition n_participants n_ever_expanded pct_ever_expanded
    > #>   <chr>              <int>            <int>             <dbl>
    > #> 1 color                70               35              50  
    > #> 2 c2pa                 75               61              81.3
    > 
    > # 4) Bar‐plot of “views‐based” click rate
    > ggplot(round_summary, aes(x = condition, y = pct_expand_views, fill = condition)) +
    +     geom_col(show.legend = FALSE, width = 0.6) +
    +     geom_text(aes(label = sprintf("%.1f%%", pct_expand_views)), vjust = -0.5) +
    +     scale_y_continuous(expand = expansion(c(0, .1)), limits = c(0,100)) +
    +     labs(
      +         title = "Label‐Explanation Clicks per Article View",
      +         subtitle = "% of article‐view events where participants clicked “explain”",
      +         x = "Condition",
      +         y = "% Expand Clicks"
      +     ) +
    +     theme_minimal(base_size = 14) +
    +     theme(plot.title=element_text(face="bold", hjust=0.5))
  > 
    > library(dplyr)
  > library(purrr)
  > library(tidyr)
  > library(jsonlite)
  > library(ggplot2)
  > 
    > # 1) Parse out the JSON in rounds_clean as before, including the `label_explained` flag
    > safe_parse_article <- function(txt) {
      +     if (is.na(txt) || !nzchar(txt)) return(list(
        +         selected_article_had_label = NA,
        +         label_explained            = NA
        +     ))
      +     tryCatch(
        +         fromJSON(txt, simplifyVector = TRUE)[c("selected_article_had_label","label_explained")],
        +         error = function(e) list(
          +             selected_article_had_label = NA,
          +             label_explained            = NA
          +         )
        +     )
      + }
  > 
    > rounds2 <- rounds_clean %>%
    +     select(id, participant_id, article) %>%
    +     mutate(parsed = map(article, safe_parse_article)) %>%
    +     unnest_wider(parsed) %>%
    +     select(id, participant_id, selected_article_had_label, label_explained)
  > 
    > # 2) Join that back onto mid_df (which has your condition & round_id)
    > mid2 <- mid_df %>%
    +     filter(condition != "nolabel") %>%
    +     left_join(rounds2, by = c("round_id" = "id", "participant_id"))
  > 
    > # 3A) **By‐round** click rate: % of article‐views where the “explain” was clicked
    > round_summary <- mid2 %>%
    +     group_by(condition) %>%
    +     summarise(
      +         views             = n(),
      +         expands           = sum(label_explained, na.rm=TRUE),
      +         pct_expand_views  = expands / views * 100,
      +         .groups = "drop"
      +     )
  > 
    > print(round_summary)
  # A tibble: 3 × 4
  condition views expands pct_expand_views
  <chr>     <int>   <int>            <dbl>
    1 c2pa        150       6                4
  2 color       150       3                2
  3 no_color    150       9                6
  > #> # A tibble: 2 x 4
    > #>   condition views expands pct_expand_views
    > #>   <chr>     <int>   <int>            <dbl>
    > #> 1 color       210      45             21.4
    > #> 2 c2pa        215      98             45.6
    > 
    > # 3B) **By‐participant** ever‐expanded rate: % of people in each cond who clicked at least once
    > participant_summary <- mid2 %>%
    +     group_by(condition, participant_id) %>%
    +     summarise(ever_expanded = any(label_explained, na.rm=TRUE), .groups = "drop") %>%
    +     group_by(condition) %>%
    +     summarise(
      +         n_participants    = n(),
      +         n_ever_expanded   = sum(ever_expanded),
      +         pct_ever_expanded = n_ever_expanded / n_participants * 100,
      +         .groups = "drop"
      +     )
  > 
    > print(participant_summary)
  # A tibble: 3 × 4
  condition n_participants n_ever_expanded pct_ever_expanded
  <chr>              <int>           <int>             <dbl>
    1 c2pa                  50               5                10
  2 color                 50               3                 6
  3 no_color              50               9                18
  > #> # A tibble: 2 x 4
    > #>   condition n_participants n_ever_expanded pct_ever_expanded
    > #>   <chr>              <int>            <int>             <dbl>
    > #> 1 color                70               35              50  
    > #> 2 c2pa                 75               61              81.3
    > 
    > # 4) Bar‐plot of “views‐based” click rate
    > ggplot(round_summary, aes(x = condition, y = pct_expand_views, fill = condition)) +
    +     geom_col(show.legend = FALSE, width = 0.6) +
    +     geom_text(aes(label = sprintf("%.1f%%", pct_expand_views)), vjust = -0.5) +
    +     scale_y_continuous(expand = expansion(c(0, .1)), limits = c(0,100)) +
    +     labs(
      +         title = "Label Explanation Clicks per Article View",
      +         subtitle = "% of article view events where participants clicked “?”",
      +         x = "Condition",
      +         y = "% Expand Clicks"
      +     ) +
    +     theme_minimal(base_size = 14) +
    +     theme(plot.title=element_text(face="bold", hjust=0.5))
  > 
    > # 0) Load needed packages
    > if (!requireNamespace("dplyr", quietly=TRUE))   install.packages("dplyr")
  > if (!requireNamespace("ggplot2", quietly=TRUE)) install.packages("ggplot2")
  > if (!requireNamespace("ggpubr", quietly=TRUE))  install.packages("ggpubr")
  > 
    > library(dplyr)
  > library(ggplot2)
  > library(ggpubr)
  > 
    > # 1) Prep the data
    > df_fam <- post_df %>%
    +     # keep only folks who actually saw a label and answered understanding
    +     filter(condition   != "nolabel",
                 +            label_present,
                 +            !is.na(understood_label)) %>%
    +     # turn those “Yes/No” familiarity questions into factors
    +     mutate(
      +         fam_trust = factor(familiar_trust_levels, levels = c("No","Yes")),
      +         fam_nutri = factor(familiar_nutriscore,     levels = c("No","Yes"))
      +     )
  > 
    > # 2) Descriptive breakdown
    > desc_trust <- df_fam %>%
    +     group_by(fam_trust) %>%
    +     summarize(
      +         N       = n(),
      +         Mean    = mean(understood_label),
      +         SD      = sd(understood_label),
      +         .groups = "drop"
      +     )
  > 
    > desc_nutri <- df_fam %>%
    +     group_by(fam_nutri) %>%
    +     summarize(
      +         N       = n(),
      +         Mean    = mean(understood_label),
      +         SD      = sd(understood_label),
      +         .groups = "drop"
      +     )
  > 
    > print(desc_trust)
  # A tibble: 3 × 4
  fam_trust     N  Mean    SD
  <fct>     <int> <dbl> <dbl>
    1 No           36  3.47  1.90
  2 Yes          39  4.46  1.50
  3 NA            5  5     1.73
  > print(desc_nutri)
  # A tibble: 3 × 4
  fam_nutri     N  Mean    SD
  <fct>     <int> <dbl> <dbl>
    1 No           22  3.27  1.98
  2 Yes          50  4.28  1.63
  3 NA            8  4.75  1.49
  > 
    > # 3) Simple one‐way ANOVAs
    > aov_trust <- aov(understood_label ~ fam_trust, data = df_fam)
  > cat("\nANOVA: Understood_label ~ fam_trust\n")
  
  ANOVA: Understood_label ~ fam_trust
  > print(summary(aov_trust))
  Df Sum Sq Mean Sq F value Pr(>F)  
  fam_trust    1  18.32  18.322   6.289 0.0144 *
    Residuals   73 212.66   2.913                 
  ---
    Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
  5 observations deleted due to missingness
  > print(TukeyHSD(aov_trust, "fam_trust"))
  Tukey multiple comparisons of means
  95% family-wise confidence level
  
  Fit: aov(formula = understood_label ~ fam_trust, data = df_fam)
  
  $fam_trust
  diff       lwr      upr     p adj
  Yes-No 0.9893162 0.2031039 1.775529 0.0143699
  
  > 
    > aov_nutri <- aov(understood_label ~ fam_nutri, data = df_fam)
  > cat("\nANOVA: Understood_label ~ fam_nutri\n")
  
  ANOVA: Understood_label ~ fam_nutri
  > print(summary(aov_nutri))
  Df Sum Sq Mean Sq F value Pr(>F)  
  fam_nutri    1   15.5  15.501   5.108 0.0269 *
    Residuals   70  212.4   3.035                 
  ---
    Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
  8 observations deleted due to missingness
  > print(TukeyHSD(aov_nutri, "fam_nutri"))
  Tukey multiple comparisons of means
  95% family-wise confidence level
  
  Fit: aov(formula = understood_label ~ fam_nutri, data = df_fam)
  
  $fam_nutri
  diff       lwr      upr     p adj
  Yes-No 1.007273 0.1183519 1.896194 0.0269363
  
  > 
    > # 4) (Optional) Two‐way ANOVA with interaction
    > aov_both <- aov(understood_label ~ fam_trust * fam_nutri, data = df_fam)
  > cat("\nANOVA: Understood_label ~ fam_trust * fam_nutri\n")
  
  ANOVA: Understood_label ~ fam_trust * fam_nutri
  > print(summary(aov_both))
  Df Sum Sq Mean Sq F value Pr(>F)  
  fam_trust            1  17.13  17.131   5.955 0.0174 *
    fam_nutri            1   5.84   5.838   2.029 0.1591  
  fam_trust:fam_nutri  1   5.13   5.127   1.782 0.1865  
  Residuals           65 186.98   2.877                 
  ---
    Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
  11 observations deleted due to missingness
  > 
    > # 5) Visualize with boxplots
    > p1 <- ggplot(df_fam, aes(x = fam_trust, y = understood_label, fill = fam_trust)) +
    +     geom_boxplot() +
    +     geom_jitter(width = 0.2, alpha = 0.5) +
    +     stat_compare_means(method = "anova", label = "p.format", label.y = 7) +
    +     labs(title = "Understanding by Prior Trust‐Label Familiarity",
               +          x = "Familiar with Trust Labels?", y = "Self‐Reported Understanding") +
    +     theme_minimal() + theme(legend.position="none")
  > 
    > p2 <- ggplot(df_fam, aes(x = fam_nutri, y = understood_label, fill = fam_nutri)) +
    +     geom_boxplot() +
    +     geom_jitter(width = 0.2, alpha = 0.5) +
    +     stat_compare_means(method = "anova", label = "p.format", label.y = 7) +
    +     labs(title = "Understanding by Prior Nutri-Score Familiarity",
               +          x = "Familiar with Nutri-Score?", y = NULL) +
    +     theme_minimal() + theme(legend.position="none")
  > 
    > p1 + p2 + plot_layout(ncol=2)
  > 
    > # 0) Load needed packages
    > if (!requireNamespace("dplyr", quietly=TRUE))   install.packages("dplyr")
  > if (!requireNamespace("ggplot2", quietly=TRUE)) install.packages("ggplot2")
  > if (!requireNamespace("ggpubr", quietly=TRUE))  install.packages("ggpubr")
  > 
    > library(dplyr)
  > library(ggplot2)
  > library(ggpubr)
  > 
    > # 1) Prep the data
    > df_fam <- post_df %>%
    +     # keep only folks who actually saw a label and answered understanding
    +     filter(condition   != "nolabel",
                 +            label_present,
                 +            !is.na(understood_label)) %>%
    +     # turn those “Yes/No” familiarity questions into factors
    +     mutate(
      +         fam_trust = factor(familiar_trust_levels, levels = c("No","Yes")),
      +         fam_nutri = factor(familiar_nutriscore,     levels = c("No","Yes"))
      +     )
  > 
    > # 2) Descriptive breakdown
    > desc_trust <- df_fam %>%
    +     group_by(fam_trust) %>%
    +     summarize(
      +         N       = n(),
      +         Mean    = mean(understood_label),
      +         SD      = sd(understood_label),
      +         .groups = "drop"
      +     )
  > 
    > desc_nutri <- df_fam %>%
    +     group_by(fam_nutri) %>%
    +     summarize(
      +         N       = n(),
      +         Mean    = mean(understood_label),
      +         SD      = sd(understood_label),
      +         .groups = "drop"
      +     )
  > 
    > print(desc_trust)
  # A tibble: 3 × 4
  fam_trust     N  Mean    SD
  <fct>     <int> <dbl> <dbl>
    1 No           36  3.47  1.90
  2 Yes          39  4.46  1.50
  3 NA            5  5     1.73
  > print(desc_nutri)
  # A tibble: 3 × 4
  fam_nutri     N  Mean    SD
  <fct>     <int> <dbl> <dbl>
    1 No           22  3.27  1.98
  2 Yes          50  4.28  1.63
  3 NA            8  4.75  1.49
  > 
    > # 3) Simple one‐way ANOVAs
    > aov_trust <- aov(understood_label ~ fam_trust, data = df_fam)
  > cat("\nANOVA: Understood_label ~ fam_trust\n")
  
  ANOVA: Understood_label ~ fam_trust
  > print(summary(aov_trust))
  Df Sum Sq Mean Sq F value Pr(>F)  
  fam_trust    1  18.32  18.322   6.289 0.0144 *
    Residuals   73 212.66   2.913                 
  ---
    Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
  5 observations deleted due to missingness
  > print(TukeyHSD(aov_trust, "fam_trust"))
  Tukey multiple comparisons of means
  95% family-wise confidence level
  
  Fit: aov(formula = understood_label ~ fam_trust, data = df_fam)
  
  $fam_trust
  diff       lwr      upr     p adj
  Yes-No 0.9893162 0.2031039 1.775529 0.0143699
  
  > 
    > aov_nutri <- aov(understood_label ~ fam_nutri, data = df_fam)
  > cat("\nANOVA: Understood_label ~ fam_nutri\n")
  
  ANOVA: Understood_label ~ fam_nutri
  > print(summary(aov_nutri))
  Df Sum Sq Mean Sq F value Pr(>F)  
  fam_nutri    1   15.5  15.501   5.108 0.0269 *
    Residuals   70  212.4   3.035                 
  ---
    Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
  8 observations deleted due to missingness
  > print(TukeyHSD(aov_nutri, "fam_nutri"))
  Tukey multiple comparisons of means
  95% family-wise confidence level
  
  Fit: aov(formula = understood_label ~ fam_nutri, data = df_fam)
  
  $fam_nutri
  diff       lwr      upr     p adj
  Yes-No 1.007273 0.1183519 1.896194 0.0269363
  
  > 
    > # 4) (Optional) Two‐way ANOVA with interaction
    > aov_both <- aov(understood_label ~ fam_trust * fam_nutri, data = df_fam)
  > cat("\nANOVA: Understood_label ~ fam_trust * fam_nutri\n")
  
  ANOVA: Understood_label ~ fam_trust * fam_nutri
  > print(summary(aov_both))
  Df Sum Sq Mean Sq F value Pr(>F)  
  fam_trust            1  17.13  17.131   5.955 0.0174 *
    fam_nutri            1   5.84   5.838   2.029 0.1591  
  fam_trust:fam_nutri  1   5.13   5.127   1.782 0.1865  
  Residuals           65 186.98   2.877                 
  ---
    Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
  11 observations deleted due to missingness
  > 
    > # 5) Visualize with boxplots
    > p1 <- ggplot(df_fam, aes(x = fam_trust, y = understood_label, fill = fam_trust)) +
    +     geom_boxplot() +
    +     geom_jitter(width = 0.2, alpha = 0.5) +
    +     stat_compare_means(method = "anova", label = "p.format", label.y = 7) +
    +     labs(title = "Understanding by Prior Trust Label Familiarity",
               +          x = "Familiar with Trust Labels?", y = "Self Reported Understanding") +
    +     theme_minimal() + theme(legend.position="none")
  > 
    > p2 <- ggplot(df_fam, aes(x = fam_nutri, y = understood_label, fill = fam_nutri)) +
    +     geom_boxplot() +
    +     geom_jitter(width = 0.2, alpha = 0.5) +
    +     stat_compare_means(method = "anova", label = "p.format", label.y = 7) +
    +     labs(title = "Understanding by Prior Nutri-Score Familiarity",
               +          x = "Familiar with Nutri-Score?", y = NULL) +
    +     theme_minimal() + theme(legend.position="none")
  > 
    > p1 + p2 + plot_layout(ncol=2)
  > 
    > library(dplyr)
  > library(ggplot2)
  > library(ggpubr)
  > 
    > # 1) Prep & clean
    > df_fam <- post_df %>%
    +     filter(
      +         condition        != "nolabel",                  # only label‐viewers
      +         label_present,                                  # must have seen a label
      +         !is.na(understood_label),                       # must have answered understanding
      +         familiar_trust_levels  %in% c("Yes","No"),      # drop NA or “–”
      +         familiar_nutriscore    %in% c("Yes","No")       # drop NA or “–”
      +     ) %>%
    +     mutate(
      +         fam_trust = factor(familiar_trust_levels, levels = c("No","Yes")),
      +         fam_nutri = factor(familiar_nutriscore,     levels = c("No","Yes"))
      +     )
  > 
    > # 2) Descriptives
    > df_fam %>% 
    +     group_by(fam_trust) %>% 
    +     summarize(
      +         N    = n(),
      +         Mean = mean(understood_label),
      +         SD   = sd(understood_label),
      +         .groups="drop"
      +     ) %>% print()
  # A tibble: 2 × 4
  fam_trust     N  Mean    SD
  <fct>     <int> <dbl> <dbl>
    1 No           33  3.36  1.93
  2 Yes          36  4.36  1.50
  > 
    > df_fam %>% 
    +     group_by(fam_nutri) %>% 
    +     summarize(
      +         N    = n(),
      +         Mean = mean(understood_label),
      +         SD   = sd(understood_label),
      +         .groups="drop"
      +     ) %>% print()
  # A tibble: 2 × 4
  fam_nutri     N  Mean    SD
  <fct>     <int> <dbl> <dbl>
    1 No           22  3.27  1.98
  2 Yes          47  4.17  1.62
  > 
    > # 3) ANOVA + Tukey for each
    > aov_trust <- aov(understood_label ~ fam_trust, data = df_fam)
  > summary(aov_trust)
  Df Sum Sq Mean Sq F value Pr(>F)  
  fam_trust    1  17.13  17.131   5.798 0.0188 *
    Residuals   67 197.94   2.954                 
  ---
    Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
  > TukeyHSD(aov_trust, "fam_trust")
  Tukey multiple comparisons of means
  95% family-wise confidence level
  
  Fit: aov(formula = understood_label ~ fam_trust, data = df_fam)
  
  $fam_trust
  diff       lwr      upr     p adj
  Yes-No 0.9974747 0.1706563 1.824293 0.0187999
  
  > 
    > aov_nutri <- aov(understood_label ~ fam_nutri, data = df_fam)
  > summary(aov_nutri)
  Df Sum Sq Mean Sq F value Pr(>F)  
  fam_nutri    1  12.07   12.07   3.984   0.05 .
  Residuals   67 203.00    3.03                 
  ---
    Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
  > TukeyHSD(aov_nutri, "fam_nutri")
  Tukey multiple comparisons of means
  95% family-wise confidence level
  
  Fit: aov(formula = understood_label ~ fam_nutri, data = df_fam)
  
  $fam_nutri
  diff           lwr      upr     p adj
  Yes-No 0.8974855 -2.455029e-05 1.794996 0.0500061
  
  > 
    > # 4) Boxplots
    > p1 <- ggplot(df_fam, aes(fam_trust, understood_label, fill=fam_trust)) +
    +     geom_boxplot() + geom_jitter(width=.2, alpha=.4) +
    +     stat_compare_means(method="anova", label="p.format", label.y=7) +
    +     labs(title="Understanding by Prior Trust‐Label Familiarity",
               +          x="Familiar with Trust Labels?", y="Understanding (1–7)") +
    +     theme_minimal() + theme(legend.position="none")
  > 
    > p2 <- ggplot(df_fam, aes(fam_nutri, understood_label, fill=fam_nutri)) +
    +     geom_boxplot() + geom_jitter(width=.2, alpha=.4) +
    +     stat_compare_means(method="anova", label="p.format", label.y=7) +
    +     labs(title="Understanding by Prior Nutri-Score Familiarity",
               +          x="Familiar with Nutri-Score?", y=NULL) +
    +     theme_minimal() + theme(legend.position="none")
  > 
    > library(patchwork)
  > p1 + p2
  > 
    > library(dplyr)
  > library(ggplot2)
  > library(ggpubr)
  > 
    > # 1) Prep & clean
    > df_fam <- post_df %>%
    +     filter(
      +         condition        != "nolabel",                  # only label‐viewers
      +         label_present,                                  # must have seen a label
      +         !is.na(understood_label),                       # must have answered understanding
      +         familiar_trust_levels  %in% c("Yes","No"),      # drop NA or “–”
      +         familiar_nutriscore    %in% c("Yes","No")       # drop NA or “–”
      +     ) %>%
    +     mutate(
      +         fam_trust = factor(familiar_trust_levels, levels = c("No","Yes")),
      +         fam_nutri = factor(familiar_nutriscore,     levels = c("No","Yes"))
      +     )
  > 
    > # 2) Descriptives
    > df_fam %>% 
    +     group_by(fam_trust) %>% 
    +     summarize(
      +         N    = n(),
      +         Mean = mean(understood_label),
      +         SD   = sd(understood_label),
      +         .groups="drop"
      +     ) %>% print()
  # A tibble: 2 × 4
  fam_trust     N  Mean    SD
  <fct>     <int> <dbl> <dbl>
    1 No           33  3.36  1.93
  2 Yes          36  4.36  1.50
  > 
    > df_fam %>% 
    +     group_by(fam_nutri) %>% 
    +     summarize(
      +         N    = n(),
      +         Mean = mean(understood_label),
      +         SD   = sd(understood_label),
      +         .groups="drop"
      +     ) %>% print()
  # A tibble: 2 × 4
  fam_nutri     N  Mean    SD
  <fct>     <int> <dbl> <dbl>
    1 No           22  3.27  1.98
  2 Yes          47  4.17  1.62
  > 
    > # 3) ANOVA + Tukey for each
    > aov_trust <- aov(understood_label ~ fam_trust, data = df_fam)
  > summary(aov_trust)
  Df Sum Sq Mean Sq F value Pr(>F)  
  fam_trust    1  17.13  17.131   5.798 0.0188 *
    Residuals   67 197.94   2.954                 
  ---
    Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
  > TukeyHSD(aov_trust, "fam_trust")
  Tukey multiple comparisons of means
  95% family-wise confidence level
  
  Fit: aov(formula = understood_label ~ fam_trust, data = df_fam)
  
  $fam_trust
  diff       lwr      upr     p adj
  Yes-No 0.9974747 0.1706563 1.824293 0.0187999
  
  > 
    > aov_nutri <- aov(understood_label ~ fam_nutri, data = df_fam)
  > summary(aov_nutri)
  Df Sum Sq Mean Sq F value Pr(>F)  
  fam_nutri    1  12.07   12.07   3.984   0.05 .
  Residuals   67 203.00    3.03                 
  ---
    Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
  > TukeyHSD(aov_nutri, "fam_nutri")
  Tukey multiple comparisons of means
  95% family-wise confidence level
  
  Fit: aov(formula = understood_label ~ fam_nutri, data = df_fam)
  
  $fam_nutri
  diff           lwr      upr     p adj
  Yes-No 0.8974855 -2.455029e-05 1.794996 0.0500061
  
  > 
    > # 4) Boxplots
    > p1 <- ggplot(df_fam, aes(fam_trust, understood_label, fill=fam_trust)) +
    +     geom_boxplot() + geom_jitter(width=.2, alpha=.4) +
    +     stat_compare_means(method="anova", label="p.format", label.y=7) +
    +     labs(title="Understanding by Prior Trust Label Familiarity",
               +          x="Familiar with Trust Labels?", y="Understanding (1–7)") +
    +     theme_minimal() + theme(legend.position="none")
  > 
    > p2 <- ggplot(df_fam, aes(fam_nutri, understood_label, fill=fam_nutri)) +
    +     geom_boxplot() + geom_jitter(width=.2, alpha=.4) +
    +     stat_compare_means(method="anova", label="p.format", label.y=7) +
    +     labs(title="Understanding by Prior Nutri-Score Familiarity",
               +          x="Familiar with Nutri-Score?", y=NULL) +
    +     theme_minimal() + theme(legend.position="none")
  > 
    > library(patchwork)
  > p1 + p2
  > 
    > # 1) Load/install packages
    > if (!requireNamespace("dplyr", quietly=TRUE))    install.packages("dplyr")
  > if (!requireNamespace("tidyr", quietly=TRUE))    install.packages("tidyr")
  > if (!requireNamespace("forcats", quietly=TRUE))  install.packages("forcats")
  > if (!requireNamespace("ggplot2", quietly=TRUE))  install.packages("ggplot2")
  > 
    > library(dplyr)
  > library(tidyr)
  > library(forcats)
  > library(ggplot2)
  > 
    > # 2) Pick off the seven post‐questionnaire columns
    > #    (drop nolabel; you can include it if you want)
    > df_plot <- post_df %>%
    +     filter(condition != "nolabel") %>%
    +     select(
      +         understood_label,     # a) immediately understood
      +         visual_design,        # b) design appealing
      +         decision_support,     # c) useful deciding
      +         info_usefulness,      # d) provides useful info
      +         image_trust,          # e) reassured me
      +         evaluate_trustworthiness, # f) helped evaluate image
      +         more_labels           # g) want more articles labeled
      +     ) %>%
    +     rename(
      +         A = understood_label,
      +         B = visual_design,
      +         C = decision_support,
      +         D = info_usefulness,
      +         E = image_trust,
      +         F = evaluate_trustworthiness,
      +         G = more_labels
      +     )
  > 
    > # 3) Pivot to long form and compute group means +/- 95% CIs
    > stats <- df_plot %>%
    +     pivot_longer(everything(), names_to="Item", values_to="Score") %>%
    +     group_by(Item) %>%
    +     summarise(
      +         M   = mean(Score, na.rm=TRUE),
      +         SEM = sd(Score,   na.rm=TRUE)/sqrt(sum(!is.na(Score))),
      +         CI  = qt(0.975, sum(!is.na(Score))-1) * SEM,
      +         .groups="drop"
      +     ) %>%
    +     mutate(
      +         Label = case_when(
        +             Item=="A" ~ "a. Immediately understood",
        +             Item=="B" ~ "b. Visually appealing",
        +             Item=="C" ~ "c. Useful for choosing article",
        +             Item=="D" ~ "d. Provides useful information",
        +             Item=="E" ~ "e. Reassured trust in image",
        +             Item=="F" ~ "f. Helped evaluate image",
        +             Item=="G" ~ "g. Want more labels"
        +         )
      +     )
  > 
    > # 4) Plot
    > ggplot(stats, aes(x = M, y = fct_reorder(Label, M))) +
    +     geom_col(fill="#4C78A8") +
    +     geom_errorbarh(aes(xmin = M - CI, xmax = M + CI), height=0.2) +
    +     geom_text(aes(label = sprintf("%.2f", M)), 
                    +               hjust = -0.1, size=3) +
    +     scale_x_continuous(limits=c(0,7), breaks=1:7) +
    +     labs(
      +         title = "Post-Questionnaire Ratings (1=low … 7=high)",
      +         x     = "Mean Score",
      +         y     = NULL
      +     ) +
    +     theme_minimal(base_size = 14) +
    +     theme(
      +         plot.title      = element_text(face="bold", hjust=0.5),
      +         panel.grid.major.y = element_blank()
      +     )
  > 
    > library(dplyr)
  > library(tidyr)
  > library(forcats)
  > library(ggplot2)
  > 
    > # 1) Select & rename the seven Likert‐items
    > df_plot <- post_df %>%
    +     filter(condition != "nolabel") %>%
    +     select(
      +         understood_label,         # “I immediately understood what the label represents”
      +         visual_design,            # “I found the design of the label visually appealing”
      +         decision_support,         # “I found the label useful for deciding which news article to read”
      +         info_usefulness,          # “The label provides useful information”
      +         image_trust,              # “The label reassured me that the image was trustworthy…”
      +         evaluate_trustworthiness, # “The label helped me to evaluate the trustworthiness…”
      +         more_labels               # “I would like more articles to show labels like this…”
      +     ) %>%
    +     rename(
      +         `Immediate comprehension`    = understood_label,
      +         `Visual appeal`              = visual_design,
      +         `Decision support`           = decision_support,
      +         `Informational value`        = info_usefulness,
      +         `Reassures image trust`      = image_trust,
      +         `Helps evaluate image trust` = evaluate_trustworthiness,
      +         `Desire for more labels`     = more_labels
      +     )
  > 
    > # 2) Pivot to long form & compute means + 95% CIs
    > stats <- df_plot %>%
    +     pivot_longer(everything(), names_to = "Label", values_to = "Score") %>%
    +     group_by(Label) %>%
    +     summarise(
      +         M   = mean(Score, na.rm = TRUE),
      +         SEM = sd(Score, na.rm = TRUE) / sqrt(sum(!is.na(Score))),
      +         CI  = qt(0.975, sum(!is.na(Score)) - 1) * SEM,
      +         .groups = "drop"
      +     )
  > 
    > # 3) Draw horizontal barplot
    > ggplot(stats, aes(x = M, y = fct_reorder(Label, M))) +
    +     geom_col(fill = "#4C78A8", width = 0.6) +
    +     geom_errorbarh(aes(xmin = M - CI, xmax = M + CI), height = 0.2) +
    +     geom_text(aes(label = sprintf("%.2f", M)), 
                    +               hjust = -0.1, size = 3) +
    +     scale_x_continuous(
      +         limits = c(0, 7), 
      +         breaks = 1:7,
      +         expand = expansion(mult = c(0, .05))
      +     ) +
    +     labs(
      +         title = "Post‐Questionnaire Ratings (1 = Strongly disagree … 7 = Strongly agree)",
      +         x     = "Mean Rating",
      +         y     = NULL
      +     ) +
    +     theme_minimal(base_size = 14) +
    +     theme(
      +         plot.title         = element_text(face = "bold", hjust = 0.5),
      +         panel.grid.major.y = element_blank()
      +     )
  > 
    > library(dplyr)
  > library(tidyr)
  > library(forcats)
  > library(ggplot2)
  > 
    > # 1) Select & rename the seven Likert‐items
    > df_plot <- post_df %>%
    +     filter(condition != "nolabel") %>%
    +     select(
      +         understood_label,         # “I immediately understood what the label represents”
      +         visual_design,            # “I found the design of the label visually appealing”
      +         decision_support,         # “I found the label useful for deciding which news article to read”
      +         info_usefulness,          # “The label provides useful information”
      +         image_trust,              # “The label reassured me that the image was trustworthy…”
      +         evaluate_trustworthiness, # “The label helped me to evaluate the trustworthiness…”
      +         more_labels               # “I would like more articles to show labels like this…”
      +     ) %>%
    +     rename(
      +         `Immediate comprehension`    = understood_label,
      +         `Visual appeal`              = visual_design,
      +         `Decision support`           = decision_support,
      +         `Informational value`        = info_usefulness,
      +         `Reassures image trust`      = image_trust,
      +         `Helps evaluate image trust` = evaluate_trustworthiness,
      +         `Desire for more labels`     = more_labels
      +     )
  > 
    > # 2) Pivot to long form & compute means + 95% CIs
    > stats <- df_plot %>%
    +     pivot_longer(everything(), names_to = "Label", values_to = "Score") %>%
    +     group_by(Label) %>%
    +     summarise(
      +         M   = mean(Score, na.rm = TRUE),
      +         SEM = sd(Score, na.rm = TRUE) / sqrt(sum(!is.na(Score))),
      +         CI  = qt(0.975, sum(!is.na(Score)) - 1) * SEM,
      +         .groups = "drop"
      +     )
  > 
    > # 3) Draw horizontal barplot
    > ggplot(stats, aes(x = M, y = fct_reorder(Label, M))) +
    +     geom_col(fill = "#4C78A8", width = 0.6) +
    +     geom_errorbarh(aes(xmin = M - CI, xmax = M + CI), height = 0.2) +
    +     geom_text(aes(label = sprintf("%.2f", M)), 
                    +               hjust = -0.1, size = 3) +
    +     scale_x_continuous(
      +         limits = c(0, 7), 
      +         breaks = 1:7,
      +         expand = expansion(mult = c(0, .05))
      +     ) +
    +     labs(
      +         title = "Post Questionnaire Ratings (1 = Strongly disagree … 7 = Strongly agree)",
      +         x     = "Mean Rating",
      +         y     = NULL
      +     ) +
    +     theme_minimal(base_size = 14) +
    +     theme(
      +         plot.title         = element_text(face = "bold", hjust = 0.5),
      +         panel.grid.major.y = element_blank()
      +     )