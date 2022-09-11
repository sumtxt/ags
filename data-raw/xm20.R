# Load labels
labs <- read_excel("./data-raw/rawdata/colnames_2020.xlsx")
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
    "./data-raw/rawdata/ref-gemeinden-umrech-2020-1990-1999.xlsx",
    "./data-raw/rawdata/ref-gemeinden-umrech-2020-2000-2010.xlsx", 
    "./data-raw/rawdata/ref-gemeinden-umrech-2020-2011-2019.xlsx"
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
    ags20)

# Format variables and assign types
xm20 <- all %>%
    mutate(
        year = as.integer(str_sub(year, 0, 4)),
        ags_xw = str_pad(ags_xw, 8, "left", 0),
        ags20 = str_pad(ags20, 8, "left", 0)
    ) %>%
    mutate_at(vars(year), as.integer) %>%
    mutate_at(vars(
        pop_conv, size_conv, emp_conv,
    ), as.numeric) %>% 
    rename(year_xw=year)

# Add 2019 
xm20_20 <- xm20 %>% 
    select(ags20) %>% 
    distinct() %>% 
    mutate(
        pop_conv=1,
        emp_conv=1, 
        size_conv=1, 
        year_xw=2020, 
        ags_xw=ags20
        )

xm20 <- bind_rows(xm20, xm20_20)
attr(xm20, "id_new") <- "ags20"