d_21 <- read_csv2("uppg202112island.csv", locale = locale(encoding = "ISO-8859-1")) |> 
    filter(tegund2 == "Launakostnaður") |> 
    group_by(samtala0, samtala1, samtala2, samtala3, ar, arsfjordungur) |> 
    summarise(raun = sum(raun)) |> 
    arrange(desc(raun))


d_20 <- read_csv2("uppg202012island.csv") |> 
    filter(tegund2 == "Launakostnaður") |> 
    group_by(samtala0, samtala1, samtala2, samtala3, ar, arsfjordungur) |> 
    summarise(raun = sum(raun)) |> 
    arrange(desc(raun))


d_19 <- read_csv2("uppgj201912island.csv", locale = locale(encoding = "ISO-8859-1")) |> 
    filter(tegund2 == "Launakostnaður") |> 
    group_by(samtala0, samtala1, samtala2, samtala3, ar, arsfjordungur) |> 
    summarise(raun = sum(raun)) |> 
    arrange(desc(raun))

d_18 <- read_csv2("uppg201812island.csv", locale = locale(encoding = "ISO-8859-1")) |> 
    filter(tegund0 == "Laun og launatengd gjöld") |> 
    group_by(samtala0, samtala1, samtala2, samtala3, ar, arsfjordungur) |> 
    summarise(raun = sum(raun)) |> 
    arrange(desc(raun))



d <- d_21 |> 
    bind_rows(
        d_20,
        d_19,
        d_18
    ) |> 
    ungroup() |> 
    mutate(dags = ymd(str_c(ar, c("01", "04", "07", "10")[arsfjordungur], "-01")))




d |> 
    filter(samtala1 == "Skóla- og frístundasvið") |> 
    group_by(ar, samtala2) |> 
    summarise(raun = sum(raun),
              .groups = "drop") |> 
    mutate(samtala2 = fct_reorder(samtala2, raun)) |> 
    group_by(ar) |> 
    mutate(hlutf = raun / sum(raun)) |> 
    ungroup() |> 
    ggplot(aes(ar, raun)) +
    geom_area(aes(fill = samtala2), position = "stack") +
    scale_fill_brewer(type = "qual", palette = "Set1", direction = -1,
                      guide = guide_legend(reverse = T)) +
    coord_cartesian(expand = F) +
    theme_half_open() +
    theme(legend.position = "bottom") +
    labs(x = NULL, y = NULL, fill = NULL)

d |> 
    filter(samtala1 == "Skóla- og frístundasvið") |> 
    group_by(ar, samtala3) |> 
    summarise(raun = sum(raun),
              .groups = "drop") |> 
    mutate(samtala3 = fct_reorder(samtala3, raun)) |> 
    group_by(ar) |> 
    mutate(hlutf = raun / sum(raun)) |> 
    ungroup() |> 
    ggplot(aes(ar, raun)) +
    geom_area(aes(fill = samtala3), position = "fill") +
    scale_x_continuous(breaks = 2018:2021) +
    scale_y_continuous(labels = label_percent()) +
    scale_fill_brewer(type = "seq", palette = "Paired", direction = -1,
                      guide = guide_legend(reverse = T)) +
    coord_cartesian(expand = F) +
    theme_half_open() +
    theme(legend.position = "bottom",
          plot.margin = margin(t = 5, r = 15, b = 5, l = 5)) +
    labs(x = NULL, y = NULL, fill = NULL,
         title = "Skóla- og frístundasvið")

flokkun_velferdarsvid <- d |> 
    filter(samtala1 == "Velferðarsvið") |> 
    group_by(samtala2) |> 
    summarise(raun = sum(raun),
              .groups = "drop") |> 
    arrange(desc(raun)) |> 
    mutate(nr = row_number(),
           flokkur = ifelse(nr <= 11, samtala2, "Annað")) |> 
    select(flokkur, samtala2)

d |> 
    filter(samtala1 == "Velferðarsvið")

d |> 
    filter(samtala1 == "Velferðarsvið") |> 
    inner_join(flokkun_velferdarsvid, by = "samtala2") |> 
    group_by(ar, flokkur) |> 
    summarise(raun = sum(raun),
              .groups = "drop") |> 
    mutate(flokkur = fct_reorder(flokkur, raun)) |> 
    group_by(ar) |> 
    mutate(hlutf = raun / sum(raun)) |> 
    ungroup() |> 
    ggplot(aes(ar, raun)) +
    geom_col(aes(fill = flokkur), position = "fill", width = 1) +
    scale_fill_brewer(type = "qual", palette = "Paired", direction = -1,
                      guide = guide_legend(reverse = T)) +
    coord_cartesian(expand = F) +
    theme_half_open() +
    theme(legend.position = "bottom") +
    labs(x = NULL, y = NULL, fill = NULL)


d |> 
    filter(samtala1 == "Velferðarsvið",
           samtala2 %in% c("Stjórnsýsla, sérfræðiþjónusta og ráðgjöf",
                           "Skrifstofur")) |> 
    group_by(ar, flokkur = samtala0) |> 
    summarise(raun = sum(raun),
              .groups = "drop") |> 
    mutate(flokkur = fct_reorder(flokkur, raun)) |> 
    group_by(ar) |> 
    mutate(hlutf = raun / sum(raun)) |> 
    ungroup() |> 
    ggplot(aes(ar, raun)) +
    geom_col(aes(fill = flokkur), position = "fill", width = 1) +
    scale_fill_brewer(type = "qual", palette = "Paired", direction = -1,
                      guide = guide_legend(reverse = T)) +
    coord_cartesian(expand = F) +
    theme_half_open() +
    theme(legend.position = "bottom") +
    labs(x = NULL, y = NULL, fill = NULL)

flokkun_samtala1 <- d |> 
    group_by(samtala1) |> 
    summarise(raun = sum(raun),
              .groups = "drop") |> 
    arrange(desc(raun)) |> 
    mutate(nr = row_number(),
           flokkur = ifelse(nr <= 9, samtala1, "Annað")) |> 
    select(flokkur, samtala1)

d |> 
    inner_join(flokkun_samtala1, by = "samtala1") |> 
    group_by(flokkur, ar) |> 
    summarise(raun = sum(raun),
              .groups = "drop") |> 
    mutate(flokkur = fct_reorder(flokkur, raun * (flokkur != "Annað"))) |> 
    ggplot(aes(ar, raun)) +
    geom_col(aes(fill = flokkur), position = "stack", width = 1) +
    scale_fill_brewer(type = "qual", palette = "Paired", direction = -1,
                      guide = guide_legend(reverse = T)) +
    coord_cartesian(expand = F) +
    theme_half_open() +
    theme(legend.position = "bottom") +
    labs(x = NULL, y = NULL, fill = NULL)







d_17 <- read_csv2("rvk201712.csv", locale = locale(encoding = "ISO-8859-1")) |> 
    filter(xtgr1 == "Launakostnaður") |> 
    group_by(samtala1 = xeining5, ar, arsfjordungur = ar_fjordungur) |> 
    summarise(raun = sum(raun)) |> 
    arrange(desc(raun))


d_16 <- read_csv2("rvk2016.csv", locale = locale(encoding = "ISO-8859-1")) |> 
    filter(XTGR1 == "Launakostnaður") |> 
    group_by(samtala1 = XEINING5, ar = AR, arsfjordungur = AR_FJORDUNGUR) |> 
    summarise(raun = sum(RAUN)) |> 
    arrange(desc(raun))


d_15 <- read_csv2("rvk2015.csv", locale = locale(encoding = "ISO-8859-1")) |> 
    filter(XTGR1 == "Launakostnaður") |> 
    group_by(samtala1 = XEINING5, ar = AR, arsfjordungur = AR_FJORDUNGUR) |> 
    summarise(raun = sum(RAUN)) |> 
    arrange(desc(raun))

d_14 <- read_csv2("rvk2014.csv", locale = locale(encoding = "ISO-8859-1")) |> 
    filter(XTGR1 == "Launakostnaður") |> 
    group_by(samtala1 = XEINING5, ar = AR, arsfjordungur = AR_FJORDUNGUR) |> 
    summarise(raun = sum(RAUN)) |> 
    arrange(desc(raun))




d <- d_21 |> 
    bind_rows(
        d_20,
        d_19,
        d_18,
        d_17,
        d_16,
        d_15,
        d_14
    ) |> 
    ungroup() |> 
    mutate(dags = ymd(str_c(ar, c("01", "04", "07", "10")[arsfjordungur], "-01")))

d |> 
    group_by(ar, samtala1) |> 
    summarise(raun = sum(raun)) |> 
    arrange(desc(ar), desc(raun)) |> 
    View()

d |> 
    group_by(ar, samtala1) |> 
    summarise(raun = sum(raun),
              .groups = "drop") |> 
    group_by(ar) |> 
    mutate(hlutf = raun / sum(raun)) |> 
    filter(ar == 2019) |> 
    ggplot() +
    geom_col(aes(hlutf, fct_reorder(samtala1, hlutf))) +
    scale_x_continuous(labels = label_percent()) +
    labs(x = NULL, y = NULL) +
    theme_tufte()


d |> 
    filter(ar >= 2017) |> 
    group_by(ar, samtala1) |> 
    summarise(raun = sum(raun),
              .groups = "drop") |> 
    group_by(ar) |> 
    mutate(hlutf = raun / sum(raun)) |> 
    ungroup() |> 
    ggplot(aes(ar, hlutf)) +
    geom_line() +
    geom_point() +
    scale_x_continuous(breaks = 2014:2021) +
    facet_wrap("samtala1", scales = "free_y", ncol = 4)


d |> 
    distinct(samtala1) |> 
    mutate(svid = case_when(str_detect(samtala1, "Umhverfis") ~ "Umhverfis- og skipulagssvið",
                            samtala1 %in% c("Eignaskrifstofa",
                                            "Verkefnastofa þróunarmála") ~ "Skrifstofa eigna og atvinnuþróunar",
                            TRUE ~ samtala1))


d |> 
    filter(ar >= 2017) |> 
    mutate(svid = case_when(str_detect(samtala1, "Umhverfis") ~ "Umhverfis- og skipulagssvið",
                            samtala1 %in% c("Eignaskrifstofa",
                                            "Verkefnastofa þróunarmála") ~ "Skrifstofa eigna og atvinnuþróunar",
                            samtala1 %in% c("Þjónustu- og nýsköpunarsvið",
                                            "Mannauðs- og starfsþróunarsvið",
                                            "Fjármála- og áhættustýringarsvið") ~ "Skrifstofur miðlægrar stjórnsýslu",
                            TRUE ~ samtala1)) |> 
    group_by(ar, svid) |> 
    summarise(raun = sum(raun),
              .groups = "drop") |> 
    group_by(ar) |> 
    mutate(hlutf = raun / sum(raun)) |> 
    ungroup() |> 
    ggplot(aes(ar, hlutf)) +
    geom_line() +
    scale_x_continuous(breaks = 2014:2021) +
    facet_wrap("svid")



d |> 
    filter(ar >= 2017) |> 
    mutate(svid = case_when(str_detect(samtala1, "Umhverfis") ~ "Umhverfis- og skipulagssvið",
                            samtala1 %in% c("Eignaskrifstofa",
                                            "Verkefnastofa þróunarmála") ~ "Skrifstofa eigna og atvinnuþróunar",
                            samtala1 %in% c("Þjónustu- og nýsköpunarsvið",
                                            "Mannauðs- og starfsþróunarsvið",
                                            "Fjármála- og áhættustýringarsvið") ~ "Skrifstofur miðlægrar stjórnsýslu",
                            TRUE ~ samtala1)) |> 
    group_by(ar, svid) |> 
    summarise(raun = sum(raun),
              .groups = "drop") |> 
    group_by(ar) |> 
    mutate(hlutf = raun / sum(raun)) |> 
    ungroup() |> 
    mutate(svid = fct_reorder(svid, hlutf)) |> 
    ggplot(aes(ar, hlutf)) +
    geom_area(aes(fill = svid), position = "fill") +
    scale_fill_brewer(type = "qual", palette = "Set1", direction = -1,
                      guide = guide_legend(reverse = T)) +
    coord_cartesian(expand = F) +
    theme_half_open() +
    theme(legend.position = "bottom") +
    labs(x = NULL, y = NULL, fill = NULL)


d |> 
    filter(ar >= 2017) |> 
    mutate(svid = case_when(str_detect(samtala1, "Umhverfis") ~ "Umhverfis- og skipulagssvið",
                            samtala1 %in% c("Eignaskrifstofa",
                                            "Verkefnastofa þróunarmála") ~ "Skrifstofa eigna og atvinnuþróunar",
                            samtala1 %in% c("Þjónustu- og nýsköpunarsvið",
                                            "Mannauðs- og starfsþróunarsvið",
                                            "Fjármála- og áhættustýringarsvið") ~ "Skrifstofur miðlægrar stjórnsýslu",
                            TRUE ~ samtala1)) |> 
    group_by(ar, svid) |> 
    summarise(raun = sum(raun),
              .groups = "drop") |> 
    group_by(ar) |> 
    mutate(hlutf = raun / sum(raun)) |> 
    ungroup() |> 
    mutate(svid = fct_reorder(svid, hlutf)) |> 
    ggplot(aes(ar, hlutf)) +
    geom_area(aes(fill = svid), position = "fill") +
    scale_fill_brewer(type = "qual", palette = "Set1", direction = -1,
                      guide = guide_legend(reverse = T)) +
    coord_cartesian(expand = F) +
    theme_half_open() +
    theme(legend.position = "bottom") +
    labs(x = NULL, y = NULL, fill = NULL)
