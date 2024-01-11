#!/usr/bin/env Rscript

library(ggplot2)

intervals = c("off", "manifest_merge", "1", "10", "25", "50")

data <- data.frame(
    rep = integer(),
    interval = integer(),
    type = character(),
    num_reqs = integer(),
    times = integer()
)

for (interval in intervals) {
    for (type in c("scan", "update")) {
        num_reqs <- scan(paste0("trace_", type, "_", interval), what = integer(), quiet = TRUE)
        times <- scan(paste0("runtime_", type, "_", interval), what = integer(), quiet = TRUE)
        data <- rbind(data, data.frame(
            rep = 1:length(num_reqs),
            interval = interval,
            type = type,
            num_reqs = num_reqs,
            times = times
        ))
    }
}

data[["interval"]] <- factor(data[["interval"]], levels = intervals, labels = c(
    "Without rewrite_delete_files",
    "With commit.manifest-merge.enabled",
    "With rewrite_delete_files after each write",
    "With rewrite_delete_files after every 10 writes",
    "With rewrite_delete_files after every 25 writes",
    "With rewrite_delete_files after every 50 writes"
))

data[["type"]] <- factor(data[["type"]], levels = c("scan", "update"), labels = c("Table scan", "Update"))

p1 <- ggplot(data, aes(x = rep, y = num_reqs)) +
    geom_line(aes(color = type, linetype = type)) +
    labs(x = "Repetitions", y = "Number of S3 requests", color = "Type of operation", linetype = "Type of operation") +
    theme(legend.position = "top") +
    facet_wrap(vars(interval), ncol = 2)

ggsave("num_reqs.pdf", plot = p1, height = 10)
ggsave("num_reqs.svg", plot = p1, height = 10)

p2 <- ggplot(data, aes(x = rep, y = times)) +
    geom_line(aes(color = type, linetype = type)) +
    labs(x = "Repetitions", y = "Runtime [ms]", color = "Type of operation", linetype = "Type of operation") +
    theme(legend.position = "top") +
    facet_wrap(vars(interval), ncol = 2)

ggsave("times.pdf", plot = p2, height = 10)
ggsave("times.svg", plot = p2, height = 10)
