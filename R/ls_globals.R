# data.table column-scope symbols used by the ls_* functions; R CMD check
# cannot tell them from global variables.
utils::globalVariables(c(".ls_row", ".ls_n", ".sfrow", "sz", "ridx"))
