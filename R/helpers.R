coverage <- function(x, truth) mean((truth >= x$conf.low) & (truth <= x$conf.high))
