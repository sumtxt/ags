# Load labels
labs <- read_excel("./data-raw/rawdata/colnames_2019.xlsx")
labs <- labs %>% 
    mutate(
        old=str_replace_all(old, "Kreisname", "Gemeindename"),
        old=str_replace_all(old, "Kreise ", "Gemeinden "),
        old=str_replace_all(old, "in 1000$", "in 100")
    )
lab_conv <- labs$new
names(lab_conv) <- labs$old

# Load all Excel sheets
files <- c(
    "./data-raw/rawdata/ref-gemeinden-umrech-2019-1990-1999.xlsx",
    "./data-raw/rawdata/ref-gemeinden-umrech-2019-2000-2010.xlsx", 
    "./data-raw/rawdata/ref-gemeinden-umrech-2019-2011-2018.xlsx"
)

tmp <- list()
for(file in files){
    sheets <- excel_sheets(file)
    for (sheet in sheets) {
        cat(sheet, "\n")
        tmp[[sheet]] <- read_xlsx(file, sheet, col_types = "text")
    }
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
xm19 <- all %>%
    mutate(
        year = as.integer(str_sub(year, 0, 4)),
        ags_xw = str_pad(ags_xw, 8, "left", 0),
        ags19 = str_pad(ags19, 8, "left", 0)
    ) %>%
    mutate_at(vars(year), as.integer) %>%
    mutate_at(vars(
        pop_conv, size_conv, emp_conv,
    ), as.numeric) %>% 
    rename(year_xw=year)

# Add 2019 
xm19_19 <- xm19 %>% 
    select(ags19) %>% 
    distinct() %>% 
    mutate(
        pop_conv=1,
        emp_conv=1, 
        size_conv=1, 
        year_xw=2019, 
        ags_xw=ags19
        )

xm19 <- bind_rows(xm19, xm19_19)
attr(xm19, "id_new") <- "ags19"