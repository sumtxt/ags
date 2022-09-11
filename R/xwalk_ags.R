#' Crosswalk Municipality or District Statistics
#'
#' This function constructs time series of counts for Germany's municipalities
#' (Gemeinden) and districts (Kreise).
#'
#' @param data A data frame or a data frame extension (e.g. a tibble).
#'
#' @param ags Name of the character variable (quoted) with municipality
#' AGS (Gemeinden, 8 digits) or district AGS (Kreise, 5 digits).
#'
#' @param time Name of the variable (quoted) identifying the year
#' (YYYY format). Values will be coerced to integers.
#'
#' @param xwalk Name of the crosswalk. The following crosswalks are available:
#' * \code{xd19}, \code{xd20} for district-level data
#'   between 1990-2019/2020.
#' * \code{xm19}, \code{xm20} for municipality-level
#'   data between 1990-2019/2020.
#'
#' @param variables Either a vector of names (quoted) for
#' variables to interpolate or \code{NULL} to disable interpolation and
#' return the data matched with the \code{xwalk}.
#'
#' @param strata Vector of variable names (quoted) or \code{NULL}. See
#' details.
#'
#' @param weight Name of the interpolation weight or \code{NULL}.
#' The following are available:
#'
#' * \code{pop}: Population weights.
#' * \code{size}: Area weights.
#' * \code{emp}: Weights based on the number of employees (1998 onwards).
#'
#' @param fuzzy_time If \code{FALSE} the crosswalk and the \code{data}
#' are matched exactly by \code{ags} and \code{time}. If \code{TRUE}
#' they are matched exactly by \code{ags} and as best as possible on
#' \code{time}. See details below. 
#'
#' @param verbose If \code{TRUE} the function outputs information on
#' the number of matched and unmatched rows.
#'
#' @details
#' This function facilitates the use of crosswalks constructed by
#' the BBSR for municipalities and districts in Germany (Milbert 2010).
#' The crosswalks map one year's set of district/municipality
#' identifiers to later year's identifiers and provide weights
#' to perform area or population weighted interpolation.
#'
#' All data rows with \code{NA}s in either the \code{ags} or \code{time}
#' variable are excluded. The same applies to all rows with a value in
#' \code{ags} or \code{time} that never appears in the crosswalk.
#' 
#' Fuzzy matching uses the absolute difference between the year reported
#' in the data and a crosswalk year. If there is a tie, crosswalk years from
#' before the year reported in the data are preferred.
#'
#' If area or population weighted interpolation is requested (i.e., when
#' \code{variables} are supplied), the combination of the variables set
#' in \code{ags}, \code{time} and \code{strata} need to uniquely
#' identify a row in \code{data}.
#'
#' Caution: Data from \url{https://www.regionalstatistik.de/} \emph{sometimes} includes
#' annual values for merged units (e.g., Städteregion Aachen, 05334)) and
#' for their former parts (Kreis Aachen, 05354 and Stadt Aachen, 05313).
#' When such data is crosswalked with \code{fuzzy_time=TRUE} and
#' interpolated, the final counts will be off by approximately factor 2.
#' The reason is that the final output is the sum of the interpolated counts
#' for the parts and the measured count of the merged unit.
#'
#' @return If interpolation is requested, the crosswalked and interpolated
#' data are returned. If interpolation is not requested, the \code{data} matched
#' with the crosswalk are returned. The following variables are added:
#'
#' * \code{row_id} row number of \code{data} before matching.
#' * \code{ags[*]} the crosswalked AGS.
#' * \code{year_xw} the matched year from the crosswalk.
#' * \code{[*]_conv} the interpolation weight.
#' * \code{diff} the absolute difference between \code{year_xw}
#'   and \code{time}.
#'
#' @references
#' Milbert, Antonia. 2010.
#' \href{https://www.bbsr.bund.de/BBSR/DE/veroeffentlichungen/berichte-kompakt/2010/BK062010.html}{
#' "Gebietsreformen–politische Entscheidungen und Folgen für die Statistik."}
#' BBSR-Berichte kompakt 6/2010. Bundesinsitut für Bau-, Stadt-und Raumfoschung.
#'
#' @examples
#'
#' data(btw_sn)
#'
#' btw_sn_ags20 <- xwalk_ags(
#'     data = btw_sn,
#'     ags = "district",
#'     time = "year",
#'     xwalk = "xd20",
#'     variables = c("voters", "valid"),
#'     weight = "pop"
#' )
#'
#' head(btw_sn_ags20)
#'
#' @import dplyr
#' @importFrom stats na.omit
#' @export
xwalk_ags <- function(data,
                      ags,
                      time,
                      xwalk,
                      variables = NULL,
                      strata = NULL,
                      weight = NULL,
                      fuzzy_time = FALSE,
                      verbose = TRUE) {
    data <- ungroup(data)

    ## ags
    lev <- na.omit(unique(str_length(data[[ags]])))
    if (!is.character(data[[ags]])) {
        stop(ags, " needs to be a character vector.")
    }

    if (length(lev) > 1) {
        stop("All values in ", ags, " need to be of equal length.")
    } else {
        if (grepl("xc", xwalk) & lev != 5) {
            stop("If district crosswalk is request, the values
                in ags need to be of length 5.")
        }
        if (grepl("xm", xwalk) & lev != 8) {
            stop("If municipality crosswalk is request, the values
                in ags need to be of length 8.")
        }
    }

    ## time
    data <- data %>% mutate_at(vars(.data[[time]]), as.integer)

    ## xwalk
    xw <- get(xwalk)
    id_new <- attr(xw, "id_new")

    ## variables, strata, weight
    if (!is.null(variables)) {
        for (var in variables) {
            if (sum(data[[var]] %% 1) != 0) {
                warning("Variable ", var, " seems not to be a count variable.
                The interpolation routine will probably not produce sensible results.")
            }
        }
        aggr <- TRUE
        if (is.null(weight)) stop("If variables are set, weight need to be set.")
        weight <- paste0(weight, "_conv")
    } else {
        aggr <- FALSE
        if (is.null(weight)) weight <- c("pop_conv")
    }

    res <- xwalk(
        xwalk = xw,
        data = data,
        id = c("ags_xw" = ags),
        time = c("year_xw" = time),
        vars = variables,
        weight = weight,
        fun = "sum",
        strata = strata,
        fuzzy = fuzzy_time,
        id_new = id_new,
        aggregate = aggr,
        verbose = verbose
    )

    return(res)
}


#' @importFrom rlang .data
xwalk <- function(xwalk,
                  data,
                  id,
                  time,
                  vars,
                  weight,
                  fun,
                  strata,
                  fuzzy,
                  id_new,
                  aggregate,
                  verbose) {

    ## Checks and Setup
    if (length(id) > 1) {
        stop("id needs to be a (named) vector of length 1.")
    }
    if (length(time) > 1) {
        stop("id needs to be a (named) vector of length 1.")
    }
    if (aggregate == TRUE & !vars_id_obs(data, id, time, strata)) {
        stop("Commbination of id and time does
        not uniquely identify an observation in data.")
    }

    df_i <- unname(id)
    df_t <- unname(time)
    xw_i <- names(id)
    xw_t <- names(time)

    colvars <- c("row_id", id_new, weight)
    v <- colnames(data)
    if (sum(v %in% colvars) > 0) {
        stop("Please rename these variables in data: ", v[v %in% colvars])
    }

    colvars <- c(xw_i, xw_t, weight, id_new)
    v <- colnames(xwalk)
    if (sum(!(colvars %in% v)) > 0) {
        stop("The variable ", v[v %in% colvars], " is not in xwalk.")
    }

    xwalk <- select(
        xwalk,
        .data[[xw_i]],
        .data[[xw_t]],
        .data[[weight]],
        .data[[id_new]]
    )

    xwalk <- na.omit(xwalk)

    ## Set function
    if (fun == "mean") {
        aggfun <- mean_w
    } else if (fun == "sum") {
        aggfun <- sum_w
    } else {
        stop("Aggregation function not recognized.")
    }

    ## Set join vectors
    i_vec <- c(xw_i)
    names(i_vec) <- c(df_i)
    t_vec <- c(xw_t)
    names(t_vec) <- c(df_t)

    ## Prepare data
    data <- ungroup(data) %>%
        mutate(row_id = 1:n())

    if (verbose) {
        n <- nrow(data)
        n_miss <- sum((is.na(data[[df_i]]) |
            is.na(data[[df_t]])))
        n_excl_i <- sum(!(data[[df_i]] %in% xwalk[[xw_i]]))
        n_excl_t <- sum(!(data[[df_t]] %in% xwalk[[xw_t]]))
    }

    ## Exclude obs
    # - any of the key variables missing
    # - unknown AGS
    # - year not part of the crosswalk
    data <- filter(
        data,
        !(is.na(.data[[df_i]]) | is.na(.data[[df_t]])) &
            (.data[[df_i]] %in% xwalk[[xw_i]]) &
            (.data[[df_t]] %in% xwalk[[xw_t]])
    )

    ## Exact matches
    d_exact <- inner_join(data, xwalk, by = c(i_vec, t_vec))
    if (verbose) n_exact <- n_distinct(d_exact[["row_id"]])

    if (fuzzy) {
        ## Fuzzy matches
        d_fuzzy <- data %>%
            anti_join(xwalk, by = c(i_vec, t_vec)) %>%
            left_join(xwalk, by = c(i_vec)) %>%
            mutate(diff = abs_biased(x = .data[[xw_t]], y = .data[[df_t]], bias = .001)) %>%
            group_by(.data$row_id) %>%
            filter(min(diff) == diff) %>%
            ungroup()

        if (verbose) n_fuzzy <- n_distinct(d_fuzzy[["row_id"]])
        if (verbose) n_unmatch <- n - n_fuzzy - n_exact - n_miss - n_excl_i - n_excl_t
    } else {
        if (aggregate == FALSE) {
            d_fuzzy <- data %>%
                anti_join(xwalk, by = c(i_vec, t_vec))
        } else {
            d_fuzzy <- NULL
        }

        if (verbose) n_fuzzy <- NA
        if (verbose) n_unmatch <- n - n_exact - n_miss - n_excl_i - n_excl_t
    }

    ## Output
    if (aggregate) {
        if (is.null(strata)) {
            gr_vars <- c(df_t, id_new)
        } else {
            gr_vars <- c(df_t, id_new, strata)
        }

        df <- bind_rows(d_fuzzy, d_exact) %>%
            group_by_at(gr_vars) %>%
            summarize_at(vars, ~ aggfun(x = ., w = .data[[weight]]))
    } else {
        df <- filter(
            data,
            !(
                !(is.na(.data[[df_i]]) | is.na(.data[[df_t]])) &
                    (.data[[df_i]] %in% xwalk[[xw_i]]) &
                    (.data[[df_t]] %in% xwalk[[xw_t]])
            )
        )
        d_exact <- mutate(d_exact, diff = 0)

        df <- bind_rows(df, d_exact, d_fuzzy)
    }

    if (verbose) {
        cat("\nTotal number of obs:", n, "\n")
        cat("\nExcluded obs:\n")

        excluded <- c(
            "id/time NA" = n_miss,
            "AGS unk" = n_excl_i,
            "Year unk" = n_excl_t
        )

        print(excluded)

        cat("\nMatched obs:\n")

        matched <- c(
            "exact" = n_exact,
            "fuzzy" = n_fuzzy
        )

        print(matched)

        cat("\nUnmatched obs:", n_unmatch, "\n\n")
    }

    return(df)
}
