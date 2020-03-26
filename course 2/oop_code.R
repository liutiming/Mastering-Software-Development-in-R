library(tidyverse)



make_LD <- function(df) {
  ld <- nest(data, nested = c(visit, room, value, timepoint))
  structure(ld, class = c("LongitudinalData"))
}

print.LongitudinalData <- function(ld) {
  num_subject <- length(unique(ld$id))
  cat("Longitudinal dataset with ", num_subject, " subjects")
}
subject <- function(ld_df, id) UseMethod("subject")

visit <- function(subject, visit_numb) UseMethod("visit")

room <- function(visit, room_name) UseMethod("room")
subject.LongitudinalData <- function(ld_df, id) {
  index <- which(id ==ld_df[["id"]])
  ld = ld_df
  class(ld) <- "data.frame"
  if (length(index) == 0)
    return(NULL)
  structure(list(id = id, data = select(ld, nested) %>% slice(index) %>% pull() %>% extract2(1) ), class = "subject_class")
}

print.subject_class <- function(x) {
  cat("Subject ID:", x[["id"]])
}

# alternative S4: 
# subject <- function(ld, id) UseMethod("subject")
# 
# subject_class <- setClass("subject_class", slot = list(ld = "LongitudinalData", ld_list = "numeric", id = "numeric"))
# 
# print.subject_class <- function(out) {
#   if (out@id %in% out@ld_list) {
#     cat("subject_class ID: ", out@id)
#   } else {
#     return(NULL)
#   }
# }

summary.subject_class <- function(object) {
  df <- object$data
  out <- 
    df %>% 
    group_by(visit, room) %>%
    summarise(value = mean(value)) %>% 
    spread(room, value) %>% 
    as.data.frame
  structure(list(id = object$id, out = out), class = "summary_class")
}

visit.subject_class <- function(subject, visit_num) {
  data <- subject$data %>% 
    filter(visit == visit_num) %>% 
    select(-visit)
  structure(list(id = subject$id,
                 visit_num = visit_num,
                 data = data), class = "visit_class")
}

room.visit_class <- function(visit, room_name) {
  if (!room_name %in% visit[["data"]][["room"]])
    stop("Please provide a room name which was part of the visit")
  data <- visit[["data"]] %>% 
    filter(room == room_name) %>% 
    select(-room)
  structure(list(id = visit[["id"]],
                 visit_num = visit[["visit_num"]],
                 room = room_name,
                 data = data), class = "room_class")
}

print.room_class <- function(x) {
  cat("ID:", x[["id"]], "\n", "Visit:", x[["visit_num"]], "\n", "Room:", x[["room"]])
}

summary.room_class <- function(object) {
  structure(list(id = object$id, out = summary(object$data[["value"]])), class = "summary_class")
}

print.summary_class <- function(x) {
  cat("ID:", x$id, "\n")
  print(x$out)
}
