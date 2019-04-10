#' Make a matrix of estimates and standard errors
#' 
#' @param est estimates
#' @param se standard errors
#' @param digits digits for rounding
est.se <- function(est, se, digits = 2) {
    est <- formatC(est, format = 'f', flag='0', digits = digits)
    se <- formatC(se, format = 'f', flag='0', digits = digits)
    as.matrix(paste(est, " (", se , ")", sep = ""), ncol = 1)
}

#' Get a summary of the performance and generalizability of multiple metapred fits.
#' 
#' @param models a list of metapred model objects
#' @param perfFUN id or name of performance function.
#' @param model_names names of model fits (make some up yourself)
#' @param stat_used statistics of the MA of performance that are to be used
#' @param digits digits for rounding
get_summary <- function(models, perfFUN = 1, model_names = as.character(seq_along(models)), 
                            stat_used = c("est", "ci.lb", "ci.ub", "pi.lb", "pi.ub"), digits = 2) {
  s <- sapply(lapply(lapply(models, perf, perfFUN = perfFUN), ma), '[', stat_used)
  m <- matrix(nrow = nrow(s), ncol = ncol(s))
  m[] <- unlist(s)
  colnames(m) <- model_names
  rownames(m) <- stat_used
  round(m, digits = digits)
}

#' Make a forest plot
#' 
#' @param m metapred model object
#' @param stat_id id of performance statistic \code{stat}
forest_list <- function(m, stat_id) {
    stat_names_manuscript <- c("MSE", "Calibration Intercept", "Calibration Slope", "AUC")
    stat_used <- c("mse", "bin.cal.int", "cal.slope", "auc")
    xlim_mse <- c(0, .30)
    xlim_int <- c(-2, 2)
    xlim_slo <- c(-1, 2)
    xlim_auc <- c(.4, 1)
    xlims <- list(xlim_mse, xlim_int, xlim_slo, xlim_auc)
    forest(models[[m]], 
           xlim = xlims[[stat_id]], 
           title = LETTERS[m], 
           perfFUN = stat_used[stat_id], 
           xlab = stat_names_manuscript[stat_id],
           sort = "dontsort")
}

#' Make a pdf of a forest plot
#' 
#' Params same as forest_list
pdf_forest <- function(m, stat_id) {
    stat <- c("mse", "int", "slope", "auc")
    cairo_pdf(filename = paste("ignore/figures/", stat[stat_id], "_", LETTERS[m], ".pdf", sep = ""))
    invisible(forest_list(m = m, stat_id = stat_id))
    dev.off() 
}