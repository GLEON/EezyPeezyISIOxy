#!/usr/bin/env Rscript

# This is a helper script to run the pipeline.
# Choose how to execute the pipeline below.

# targets::tar_make()
targets::tar_make_clustermq(workers = 400, reporter = "summary") # nolint

