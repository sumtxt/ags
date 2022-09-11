# Load labels
labs <- read_excel("./data-raw/rawdata/colnames_2019.xlsx")
lab_conv <- labs$new
names(lab_conv) <- labs$old

# Load all Excel sheets
file <- "./data-raw/rawdata/ref-kreise-umrech-2019-1990-2018.xlsx"
sheets <- excel_sheets(file)
tmp <- list()

for (sheet in sheets) {
    cat(sheet, "\n")
    tmp[[sheet]] <- read_xlsx(file, sheet, col_types = "text")
}

# Assign common variable names
tmp <- lapply(tmp, function(x) {
    colnames(x) <- str_replace_all(colnames(x), "\r|\n", "")
    labs_tmp <- labs %>% filter(old %in% colnames(x))
    x <- x %>% rename_at(vars(labs_tmp$old), ~ labs_tmp$new)
})

# Bind and select variables
all <- bind_rows(tmp, .id = "year")
all <- all %>% select(year, ags_xw, 
    pop_conv, size_conv, emp_conv, 
#    pop_old, size_old, soz_old, 
    ags19)

# Format variables and assign types
xd19 <- all %>%
    mutate(
        year = as.integer(str_sub(year, 0, 4)),
        ags_xw = str_pad(ags_xw, 8, "left", 0),
        ags19 = str_pad(ags19, 8, "left", 0),
        ags_xw = str_sub(ags_xw, 0, 5),
        ags19 = str_sub(ags19, 0, 5),
    ) %>%
    mutate_at(vars(year), as.integer) %>%
    mutate_at(vars(
        pop_conv, size_conv, emp_conv,
    ), as.numeric) %>% 
    rename(year_xw=year)

# Add 2019 
xd19_19 <- xd19 %>% 
    select(ags19) %>% 
    distinct() %>% 
    mutate(
        pop_conv=1,
        emp_conv=1, 
        size_conv=1, 
        year_xw=2019, 
        ags_xw=ags19
        )

xd19 <- bind_rows(xd19, xd19_19)
attr(xd19, "id_new") <- "ags19"