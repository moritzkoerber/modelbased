estimate_auxilliary <- function(model,
                                type,
                                data = NULL,
                                ci = 0.95,
                                keep_iterations = FALSE,
                                ...) {
  if (missing(type)) {
    stop(insight::format_message(
      "`type` must be specified for estimate_auxilliary()."
    ), call. = FALSE)
  }
  if (! inherits(model, "brmsfit")) {
    type <- match.arg(
      type, choices = c("conditional", "zprob", "zlink", "disp")
    )
  }
  .estimate_predicted(
    model,
    data = data,
    ci = ci,
    keep_iterations = keep_iterations,
    predict = type,
    ...
  )
}