
rm(list = ls())

# packages ----
library("dplyr")
# library("tidyr")
# library("purrr")
# library("rlang")
library("ggplot2")
# NOTE: The `plm` package needs to be imported explicitly for `pggls()` to work.
library("plm")
# library("Ecdat") # for the Grunfield data set (to verify examples)

# params ----
# NOTE: Use the values in this params list to dictate whether or not output is overwritten.
# This makes it easy to just "source" this file.
params <-
  list(export_data = FALSE,
       export_viz = FALSE,
       do_extra = FALSE,
       try_not_working = FALSE)

path_vars_x <- "State_Level_Data_Tidy_v2.xlsx"
data_vars_x <-
  path_vars_x %>%
  readxl::read_excel() %>%
  filter(var != "AdultReportingBadHealthRate")

pull_distinctly <- function(data, col) {
  col_sym <- rlang::enquo(col)
  data %>%
    distinct(!!col_sym) %>%
    arrange(!!col_sym) %>%
    pull(!!col_sym)
}

vars_x_init <-
  data_vars_x %>%
  pull_distinctly(var)

path_vars_y <- "data_select.csv"
data_vars_y <-
  path_vars_y %>%
  readr::read_csv() %>%
  rename(n = numberofprescriptions, product = productname)
var_y <- "n_log"

# process ----
products <-
  data_vars_y %>%
  pull_distinctly(product)

data_vars_xy <-
  data_vars_y %>%
  left_join(data_vars_x %>% distinct() %>% tidyr::spread(var, value),
            by = c("year", "state"))

data_vars_xy %>% filter(n == 0) %>% count(product, sort = TRUE)
vars_rescale <- c("Population")
vars_log <- c("n", "PerCap", "Population")
# years_filt <- c(2015L, 2017L)
years_filt <- c(2014L:2017L)
data_model <-
  data_vars_xy %>%
  filter(n != 0) %>%
  mutate_at(vars(vars_rescale), funs(. / 1000)) %>%
  mutate_at(vars(c(vars_log)), funs(log = log(.))) %>%
  filter(year %in% years_filt) %>%
  #mutate(YearDummy = if_else(year == 2015, 0, 1))
  mutate(YearDummy1 = if_else(year == 2015, 1, 0)) %>%
  mutate(YearDummy2 = if_else(year == 2016, 1, 0)) %>%
  mutate(YearDummy3 = if_else(year == 2017, 1, 0))
  #mutate(YearTrend = if_else(year == 2015, 1,if_else(year == 2016, 2,if_else(year == 2017, 3, 0))))

data_model %>%
  count(year, sort = TRUE)

viz_data_vars_x <-
  data_vars_xy %>%
  tidyr::gather(metric, value, -year, -state, -product, -n) %>%
  filter(metric != "Population") %>%
  group_by(year, metric) %>%
  summarise_at(vars(value), funs(mean(., na.rm = TRUE))) %>%
  ungroup() %>%
  ggplot(aes(x = year, y = value, fill = metric)) +
  geom_col(position = "dodge") +
  facet_wrap(~metric, scales = "free") +
  theme_minimal() +
  scale_y_continuous(labels = scales::comma) +
  scale_fill_brewer(palette = "Set2") +
  theme(
    legend.position = "none"
  ) +
  labs(
    x = NULL,
    y = NULL,
    title = "Change in Prescription Features Over Time"
  )
viz_data_vars_x

viz_data_vars_y <-
  data_vars_y %>%
  group_by(year, product) %>%
  summarise_at(vars(n), funs(sum(., na.rm = TRUE))) %>%
  ungroup() %>%
  ggplot(aes(x = year, y = n, fill = product)) +
  geom_col(position = "dodge") +
  # facet_wrap(~metric, scales = "free") +
  # teplot::theme_te_dx(base_family = "") +
  theme_minimal() +
  scale_y_continuous(labels = scales::comma) +
  scale_fill_brewer(palette = "Set2") +
  theme(legend.title = element_blank(),
        legend.position = "bottom") +
  labs(x = NULL,
       y = NULL,
       title = "Change in Prescription Count Over Time")
viz_data_vars_y

# plm setup ----
vars_x <-
  vars_x_init %>%
  paste0(c(rep("", 4), rep("_log", 2), "")) %>%
  #c("YearDummy") %>%
  c("YearDummy1") %>%
  c("YearDummy2") %>%
  c("YearDummy3") %>%
  #c("YearTrend") %>%
  setdiff("Population_log")

get_fmla <-
  function(var_y = NULL,
           vars_x = NULL,
           include_intercept = TRUE,
           as_fmla = TRUE) {
    if(!include_intercept) {
      vars_x <- c(vars_x, " 0")
    }
    fmla <- paste0(var_y, " ~ ", paste(vars_x, collapse = " + "))
    if (as_fmla) {
      fmla <- stats::as.formula(fmla)
    }
    fmla
  }

get_plm_model <-
  function(data = NULL,
           value = NULL,
           col_value = "product",
           index = c("state", "year"),
           var_y = NULL,
           vars_x = NULL,
           include_intercept = TRUE,
           func = "plm",
           effect = "individual",
           model = "fd") {

    # NOTE: Separate this out so that this function can be used with "other" data
    # (e.g. the `Ecdata::Grunfeld` data set).
    if(!missing(value)) {
      data <-
        data %>%
        filter(!!rlang::sym(col_value) == value)
    }

    # browser()
    data_pdf <-
      data %>%
      as.data.frame() %>%
      plm::pdata.frame(index = index)

    fmla <-
      get_fmla(
        var_y = var_y,
        vars_x = vars_x,
        include_intercept = include_intercept
      )

    if (func == "plm") {
      ret <-
        plm::plm(fmla, data = data_pdf, effect = effect, model = model)
    } else if (func == "pggls") {

      ret <-
        plm::pggls(fmla, data = data_pdf, effect = effect, model = model)
    } else if(func == "pvcm") {
      ret <-
        plm::pvcm(fmla, data = data_pdf, effect = effect, model = model)

    } else if(func == "pgmm") {
      ret <-
        # plm::pgmm(fmla, data = data_pdf, effect = effect, model = model)
        pgmm(n_log ~ AdultObesityRate + AdultSmokingRate + year, list(year = 1),
                  data = data_pdf,
                  effect = effect,
                  model = model,
              gmm.inst = ~year,
              lag.gmm = list(c(2, 6))
        )

    } else {
      if(func == "pwartest") {
        ret <-
          plm::pwartest(fmla, data = data_pdf)
      } else if(func == "pwfdtest") {
        ret <-
          plm::pwfdtest(fmla, data = data_pdf)
      } else {
        stop("`func` is not value.", call. = FALSE)
      }

      # NOTE: Return early for this function because it is a hypothesis test
      # (which is different from the others).
      return(ret)
    }
    # NOTE: Change the class of the output in order to make the `broom` functions work.
    class(ret) <- c("plm", "panelmodel")
    ret
  }

get_plm_metrics <-
  function(...) {
    get_plm_model(...) %>%
      broom::tidy()
  }

get_plm_summ <-
  function(...) {
    get_plm_model(...) %>%
      broom::glance()
  }

# NOTE: Probably don't need predictions, but it's easy enough to implement.
get_plm_preds <-
  function(...) {
    get_plm_model(...) %>%
      broom::augment()
  }

clean_plm_map_output <-
  function(data = NULL, value = NULL) {
    # NOTE: `round()` in order to suppress super long p.values.
    data %>%
      tibble::as_tibble() %>%
      mutate_if(is.numeric, funs(round(., 6))) %>%
      mutate(product = value) %>%
      select(product, everything())
  }

do_plm_test_prob <-
  function(..., model = "fd") {
    # dots <- list(...)

    # NOTE: The `model` for the `plm()` function may be either `"within"` or `"pooling"`.
    zplm <-
      get_plm_model(
        ...,
        func = "plm",
        model = model
      )

    znp_try <- try({
      znp <-
        get_plm_model(
          ...,
          func = "pvcm",
          model = "within"
        )
    },
    silent = TRUE)
    # browser()
    if (inherits(znp_try, "try-error")) {
      return(warning(sprintf(
        "Error with `plm::pvcm()`. %s", attr(znp_try, "condition")
      ), call. = FALSE))
    }

    plm::pooltest(zplm, znp)
  }

do_plm_test_hausman <-
  function(...) {
    plmw <-
      get_plm_model(
        ...,
        func = "plm",
        model = "within"
      )
    plmr <-
      get_plm_model(
        ...,
        func = "plm",
        model = "random"
      )
    plm::phtest(plmw, plmr)
  }

get_plm_test_hausman_summ <-
  function(...) {
    do_plm_test_hausman(
      ...
    ) %>%
      broom::glance()
  }
do_plm_test_pbg <-
  function(...) {
    fe <-
      get_plm_model(
        ...,
        func = "plm",
        model = "within"
      )
    plm::pbgtest(fe, order = 2)
  }

get_plm_test_pbg_summ <-
  function(...) {
    do_plm_test_pbg(
      ...
    ) %>%
      broom::glance()
  }

# NOTE: Not sure if this is the proper way to invoke `pwartest()`
# based on the example on p. 27 of the pdf.
do_plm_test_pwart <-
  function(...) {
    get_plm_model(
      ...,
      func = "pwartest"
    )
  }

get_plm_test_pwart_summ <-
  function(...) {
    do_plm_test_pwart(
      ...
    ) %>%
      broom::glance()
  }

do_plm_test_pwfd <-
  function(...) {
    get_plm_model(
      ...,
      func = "pwfdtest"
    )
  }

get_plm_test_pwfd_summ <-
  function(...) {
    do_plm_test_pwfd(
      ...
    ) %>%
      broom::glance()
  }

# NOTE: `broom::tidy()` and `broom::glance()` return the same results for hypothesis tests.
get_plm_test_summ <-
  function(..., test = NULL) {
    test_func <- sprintf("get_plm_test_%s_summ", test)
    suppressMessages(do.call(test_func, args = list(...)))
  }

# plm ----
plm_metrics <-
  purrr::map_dfr(
    products,
    ~ get_plm_metrics(
      data_model,
      value = .x,
      var_y = var_y,
      vars_x = vars_x,
      func = "plm"
    ) %>%
      clean_plm_map_output(value = .x)
  )
plm_metrics
plm_metrics %>%
  filter(p.value < 0.05)

plm_summ <-
  purrr::map_dfr(
    products,
    ~ get_plm_summ(
      data_model,
      value = .x,
      var_y = var_y,
      vars_x = vars_x,
      func = "plm"
    ) %>%
      clean_plm_map_output(value = .x)
  )
plm_summ

# pggls ----
pggls_metrics <-
  purrr::map_dfr(
    products,
    ~ get_plm_metrics(
      data_model,
      value = .x,
      var_y = var_y,
      vars_x = vars_x,
      func = "pggls"
    ) %>%
      clean_plm_map_output(value = .x)
  )
pggls_metrics
pggls_metrics %>%
  filter(p.value < 0.05)

# pgmm ----
# NOTE: I couldn't figure this out.
# pgmm_models <-
#   purrr::map_dfr(
#     products,
#     ~ get_plm_model(
#       data_model %>%
#         # select_if(is.numeric) %>%
#         mutate(YearLag = lag(year, 1)),
#       value = .x,
#       var_y = var_y,
#       # vars_x = gsub("YearDummy", "YearLag", vars_x),
#       vars_x = gsub("YearDummy", "year", vars_x),
#       include_intercept = TRUE,
#       func = "pgmm",
#       effect = "twoways",
#       model = "twosteps"
#     )
#   )
# pgmm_models


# NOTE: Do more here if you need to...

# test_prob ----
if(params$try_not_working) {

  # NOTE: Using the pdf example to verify that my function produces the correct output.

  data("Grunfeld", package = "Ecdat")
  Grunfeld <-
    Grunfeld %>%
    tibble::as_tibble()

  # NOTE: This returns the same results as those shown in the pdf for the `Grunfeld` data set.
  # However, it does not work for my data (due to insufficient number of observations,
  # which is probably because there are only 4 records for each of the 50 states.
  # By comparison, there are 20 records for each of the years in the example data set.)
  Grunfeld %>%
    do_plm_test_prob(
      index = c("firm", "year"),
      var_y = "inv",
      vars_x = c("value", "capital"),
      include_intercept = TRUE,
      model = "within"
    )
  Grunfeld %>% count(firm, sort = TRUE)
  Grunfeld %>% count(firm, year, sort = TRUE)

  data_model %>%
    filter(product == products[1]) %>%
    count(product, state, sort = TRUE)
  fmla <- get_fmla(var_y, vars_x, include_intercept = TRUE)

  # NOTE: This will give an error.
  znp <-
    data_model %>%
    filter(product == products[1]) %>%
    select(state, year, everything()) %>%
    pvcm(fmla, data = ., model = "within")

  # NOTE: Again, this will give an error.
  plm_test_prob <-
    purrr::map(
      products,
      ~ do_plm_test_prob(
        data_model,
        value = .x,
        var_y = var_y,
        vars_x = vars_x,
        # model = "within"
        model = "pooling"
      )
    )
  plm_test_prob
}

# test_hausman ----
plm_test_hausman_summ <-
  purrr::map_dfr(
    products,
    ~ get_plm_test_summ(
      data_model,
      value = .x,
      var_y = var_y,
      vars_x = vars_x,
      test = "hausman"
    ) %>%
      clean_plm_map_output(value = .x)
  )
plm_test_hausman_summ

# test_pbg ----
plm_test_pbg_summ <-
  purrr::map_dfr(
    products,
    ~ get_plm_test_summ(
      data_model,
      value = .x,
      var_y = var_y,
      vars_x = vars_x,
      test = "pbg"
    ) %>%
      clean_plm_map_output(value = .x)
  )
plm_test_pbg_summ
knitr::kable(
  plm_test_pbg_summ,
  format = "latex"
)

# test_pwart ----
# NOTE: Don't worry abou the message "Multiple parameters: naming those columns df1, df2".
# This is just an information message from the `broom` package.
# The results are not any different.

# NOTE: Using the pdf example to verify that my function produces the correct output.
# data("EmplUK", package = "plm")
# # EmplUK <-
# #   EmplUK %>%
# #   tibble::as_tibble()
# EmplUK
# EmplUK %>%
#   mutate_at(vars(emp, wage, capital), funs(log = log)) %>%
#   get_plm_test_summ(
#     index = c("firm", "year"),
#     var_y = "emp_log",
#     vars_x = c("wage_log", "capital_log"),
#     include_intercept = TRUE,
#     test = "pwart"
#   )

plm_test_pwart_summ <-
  purrr::map_dfr(
    products,
    ~ get_plm_test_summ(
      data_model,
      value = .x,
      var_y = var_y,
      vars_x = vars_x,
      test = "pwart"
    ) %>%
      clean_plm_map_output(value = .x)
  )
plm_test_pwart_summ

# test_pwfd ----
plm_test_pwfd_summ <-
  purrr::map_dfr(
    products,
    ~ get_plm_test_summ(
      data_model,
      value = .x,
      var_y = var_y,
      vars_x = vars_x,
      test = "pwfd"
    ) %>%
      clean_plm_map_output(value = .x)
  )
plm_test_pwfd_summ
stargazer::stargazer(plm_test_pwfd_summ)
stargazer::stargazer(plm_test_pwfd_summ, type = "html")
plm_test_pwfd_summ %>%
  select(product, p.value) %>%
  tidyr::spread(product, p.value)

# viz_1product ----
if (params$do_extra) {
  years <-
    data_vars_xy %>%
    pull_distinctly(year)
  n_pals <- length(years)
  product_filt <- "MORPHINE S"
  viz_1product_list <-
    purrr::map(
      years,
      # rownames(RColorBrewer::brewer.pal.info[1:n_pals,]),
      ~ statebins::statebins(
        data_vars_xy %>%
          filter(product == product_filt) %>%
          filter(year == .x) %>%
          select(state, value = n),
        # legend_title = paste0("# of Prescriptions, ", .x),
        # breaks = 3,
        labels = "",
        legend_title = .x,
        legend_position = "bottom",
        # legend_position = "none",
        # plot_title = .x,
        # brewer_pal = .y,
        font_size = 3
      )
    )

  ncol <- 2
  viz_1product <-
    do.call(gridExtra::arrangeGrob,
            c(
              viz_1product_list,
              ncol = ncol,
              top = sprintf(
                "# of Prescriptions for %s, Binned by 20%% Percentiles",
                product_filt
              )
            ))
  gridExtra::grid.arrange(viz_1product)
  # dev.off()
  if (params$export_viz) {
    ggsave(
      # viz_1product,
      file = "viz_1product.png",
      # device = "png",
      height = 10,
      width = 10,
      gridExtra::arrangeGrob(grobs = viz_1product_list, ncol = ncol)
    )
  }
}
