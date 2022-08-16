#' Turn an R list into an HTML list
#'
#' @param list An R list
#' @param class a class for the list
#' 
#' @return an HTML list
#' @noRd
#' 
#' @examples
#' list_to_li(c("a","b"))
#'
#' @importFrom shiny tags tagAppendAttributes tagList
list_to_li <- function(list, class = NULL){
  if (is.null(class)){
    tagList(
      lapply(
        list, 
        tags$li
      )
    )
  } else {
    res <- lapply(
      list, 
      tags$li
    )
    res <- lapply(
      res, 
      function(x) {
        tagAppendAttributes(
          x, 
          class = class
        )
      }
    )
    tagList(res)
  }
  
}
#' Turn an R list into corresponding HTML paragraph tags
#'
#' @param list an R list
#' @param class a class for the paragraph tags
#' 
#' @return An HTML tag
#' @noRd
#' 
#' @examples 
#' list_to_p(c("This is the first paragraph", "this is the second paragraph"))
#' 
#' @importFrom shiny tags tagAppendAttributes tagList
#' 
list_to_p <- function(list, class = NULL){
  if (is.null(class)){
    tagList(
      lapply(
        list, 
        tags$p
      )
    )
  } else {
    res <- lapply(
      list, 
      tags$p
    )
    res <- lapply(
      res, 
      function(x) { 
        tagAppendAttributes(
          x, 
          class = class
        )
      }
    )
    tagList(res)
  }
  
}

#' @importFrom shiny tags tagAppendAttributes tagList
named_to_li <- function(list, class = NULL){
  if(is.null(class)){
    res <- mapply(
      function(x, y){
        tags$li(
          HTML(
            sprintf("<b>%s:</b> %s", y, x)
          )
        )
      },
      list, 
      names(list), 
      SIMPLIFY = FALSE
    )
    tagList(res)
  } else {
    res <- mapply(
      function(x, y){
        tags$li(
          HTML(
            sprintf("<b>%s:</b> %s", y, x)
          )
        )
      },
      list,
      names(list), 
      SIMPLIFY = FALSE
    )
    res <- lapply(
      res, 
      function(x) {
        tagAppendAttributes(
          x, 
          class = class
        )
      }
    )
    tagList(res)
  }
}

#' Remove a tag attribute
#'
#' @param tag the tag
#' @param ... the attributes to remove
#'
#' @return a new tag
#' @noRd
#' 
#' @examples
#' a <- shiny::tags$p(src = "plop", "pouet")
#' tagRemoveAttributes(a, "src")
tagRemoveAttributes <- function(tag, ...) {
  attrs <- as.character(list(...))
  for (i in seq_along(attrs)) {
    tag$attribs[[ attrs[i] ]] <- NULL
  }
  tag
}

#' Hide or display a tag
#' 
#' @param tag the tag
#' 
#' @return a tag
#' @noRd
#' 
#' @examples
#' ## Hide
#' a <- shiny::tags$p(src = "plop", "pouet")
#' undisplay(a)
#' b <- shiny::actionButton("go_filter", "go")
#' undisplay(b)
#' 
#' @importFrom shiny tagList
undisplay <- function(tag) {
  # if not already hidden
  if (
    !is.null(tag$attribs$style) && 
    !grepl("display:\\s+none", tag$attribs$style)
  ) {
    tag$attribs$style <- paste(
      "display: none;", 
      tag$attribs$style
    )
  } else {
    tag$attribs$style <- "display: none;"
  }
  tag
}

#' @importFrom shiny tagList
display <- function(tag) {
  if (
    !is.null(tag$attribs$style) && 
    grepl("display:\\s+none", tag$attribs$style)
  ) {
    tag$attribs$style <- gsub(
      "(\\s)*display:(\\s)*none(\\s)*(;)*(\\s)*", 
      "", 
      tag$attribs$style
    )
  }
  tag
}

#' Hide an elements by calling jquery hide on it
#' 
#' @param id the id of the element to hide
#' 
#' @noRd
#' 
#' @importFrom shiny tags
jq_hide <- function(id) {
  tags$script(sprintf("$('#%s').hide()", id))
}

#' Add a red star at the end of the text
#'
#' Adds a red star at the end of the text
#' (for example for indicating mandatory fields).
#'
#' @param text the HTLM text to put before the red star
#'
#' @return an html element
#' @noRd
#' 
#' @examples
#' with_red_star("Enter your name here")
#' 
#' @importFrom shiny tags HTML
with_red_star <- function(text) {
  shiny::tags$span(
    HTML(
      paste0(
        text,
        shiny::tags$span(
          style = "color:red", "*"
        )
      )
    )
  )
}



#' Repeat tags$br
#'
#' @param times the number of br to return
#'
#' @return the number of br specified in times
#' @noRd
#' 
#' @examples
#' rep_br(5)
#' 
#' @importFrom shiny HTML
rep_br <- function(times = 1) {
  HTML(rep("<br/>", times = times))
}

#' Create an url
#'
#' @param url the URL
#' @param text the text to display
#'
#' @return an a tag
#' @noRd
#'
#' @examples
#' enurl("https://www.thinkr.fr", "ThinkR")
#' 
#' @importFrom shiny tags
enurl <- function(url, text){
  tags$a(href = url, text)
}

#' Columns wrappers
#' 
#' These are convenient wrappers around 
#' `column(12, ...)`, `column(6, ...)`, `column(4, ...)`...
#' 
#' @noRd
#' 
#' @importFrom shiny column
col_12 <- function(...){
  column(12, ...)
}

#' @importFrom shiny column
col_10 <- function(...){
  column(10, ...)
}

#' @importFrom shiny column
col_8 <- function(...){
  column(8, ...)
}

#' @importFrom shiny column
col_6 <- function(...){
  column(6, ...)
}


#' @importFrom shiny column
col_4 <- function(...){
  column(4, ...)
}


#' @importFrom shiny column
col_3 <- function(...){
  column(3, ...)
}


#' @importFrom shiny column
col_2 <- function(...){
  column(2, ...)
}


#' @importFrom shiny column
col_1 <- function(...){
  column(1, ...)
}



#' Make the current tag behave like an action button
#' 
#' Only works with compatible tags like button or links
#'
#' @param tag Any compatible tag.
#' @param inputId Unique id. This will host the input value to be used
#' on the server side.
#'
#' @return The modified tag with an extra id and the action button class.
#' @noRd
#'
#' @examples
#' if (interactive()) {
#'  library(shiny)
#'  
#'  link <- a(href = "#", "My super link", style = "color: lightblue;") 
#'  
#'  ui <- fluidPage(
#'   make_action_button(link, inputId = "mylink")
#'  )
#'  
#'  server <- function(input, output, session) {
#'    observeEvent(input$mylink, {
#'     showNotification("Pouic!")
#'    })
#'  }
#'  
#'  shinyApp(ui, server)
#'  
#' }
make_action_button <- function(tag, inputId = NULL) {
  # some obvious checks
  if (!inherits(tag, "shiny.tag")) stop("Must provide a shiny tag.")
  if (!is.null(tag$attribs$class)) {
    if (grep("action-button", tag$attribs$class)) {
      stop("tag is already an action button")
    }
  }
  if (is.null(inputId) && is.null(tag$attribs$id)) {
    stop("tag does not have any id. Please use inputId to be able to
           access it on the server side.")
  }
  
  # handle id
  if (!is.null(inputId)) {
    if (!is.null(tag$attribs$id)) {
      warning(
        paste(
          "tag already has an id. Please use input$", 
          tag$attribs$id,
          "to access it from the server side. inputId will be ignored."
        )
      )
    } else {
      tag$attribs$id <- inputId
    }
  } 
  
  # handle class
  if (is.null(tag$attribs$class)) {
    tag$attribs$class <- "action-button"
  } else {
    tag$attribs$class <- paste(tag$attribs$class, "action-button") 
  }
  # return tag
  tag
}

# 添加多组t检验函数
#' @name multGroupTtest
#' @author Xiang LI <lixiang117423@@foxmail.com>
#'
#' @title Multiple Group Student's-test.
#' @description
#' \code{multGroupTtest} Multiple Group Student's-test.
#'
#' @importFrom magrittr %>%
#' @importFrom data.table fread
#' @importFrom stringr str_sub str_split
#' @importFrom dplyr select filter mutate group_by ungroup all_of
#' @importFrom stats t.test
#' @export
#'
#' @return Return a datafram
#'
multGroupTtest <- function(data, group1, group2, CK, value, level) {
  data  %>%
    dplyr::rename(
      first.group = all_of(group1),
      second.group = all_of(group2),
      value = all_of(value)
    ) %>%
    dplyr::mutate(second.group = factor(second.group, levels = c(unique(second.group)))) %>%
    dplyr::mutate(group.temp = paste0(first.group, second.group)) -> df
  
  ttest.results <- NULL
  
  for (i in unique(df$first.group)) {
    df.sub <- df %>% dplyr::filter(first.group == i)
    
    df.sub.ck <- df.sub %>% dplyr::filter(second.group == CK)
    df.sub.2 <- df.sub %>% dplyr::filter(second.group != CK)
    
    df.sub.ck$pvalue.ttest <- ''
    df.sub.ck$signif.ttest <- ''
    
    ttest.results <- rbind(ttest.results, df.sub.ck)
    
    for (j in unique(df.sub.2$second.group)) {
      df.sub.3 <- df.sub.2 %>% dplyr::filter(second.group == j)
      
      df.sub.ck.2 = df.sub.ck %>% dplyr::select(1:(ncol(df.sub.ck)-2))
      
      df.sub.4 <- rbind(df.sub.ck.2, df.sub.3)
      
      if (dim(df.sub.4)[1] == 0) {
        next
      } else if (length(unique(df.sub.4$second.group)) == 1) {
        next
      } else {
        df.sub.4$second.group <- factor(df.sub.4$second.group, levels = unique(df.sub.4$second.group))
        
        fit <- t.test(value ~ second.group, data = df.sub.4, conf.level = level)
        
        pvalue.ttest <- fit$p.value
        
        signif.ttest <- ifelse(pvalue.ttest < 0.001, "***",
                               ifelse(pvalue.ttest < 0.01 & pvalue.ttest > 0.001, "**",
                                      ifelse(pvalue.ttest > 0.05, "NS", "*")
                               )
        )
        
        df.sub.3$pvalue.ttest <- as.character(pvalue.ttest)
        df.sub.3$signif.ttest <- signif.ttest
        
        ttest.results <- rbind(ttest.results, df.sub.3)
      }
    }
  }
  
  ttest.results <- ttest.results %>%
    dplyr::select(first.group, second.group, pvalue.ttest, signif.ttest) %>%
    dplyr::mutate(temp = paste0(first.group, second.group)) %>%
    dplyr::ungroup()
  
  ttest.results <- ttest.results[!duplicated(ttest.results$temp), ] %>%
    dplyr::select(-temp) %>%
    dplyr::select(first.group, second.group, pvalue.ttest, signif.ttest) %>%
    dplyr::mutate(second.group = as.character(second.group))
  
  colnames(ttest.results)[1:2] <- c(group1, group2)
  
  ttest.results <- ttest.results %>%
    dplyr::ungroup()
  
  return(ttest.results)
}


# 批量方差分析函数
#' @name mult.aov
#' @author Xiang LI <lixiang117423@@foxmail.com>
#'
#' @title Multiple Group Anova.
#' @description
#' \code{mult.aov} Multiple Group Anova.
#'
#' @importFrom magrittr %>%
#' @importFrom data.table fread
#' @importFrom stringr str_sub str_split
#' @importFrom dplyr select filter mutate group_by ungroup all_of
#' @importFrom stats aov
#' @importFrom multcomp mcp cld glht
#'
#'
#' @export
#'
#' @return Return a datafram
#'
# 批量Anova
mult.aov <- function(data, group1, group2, value, level) {
  data %>%
    dplyr::rename(
      first.group = all_of(group1),
      second.group = all_of(group2),
      value = all_of(value)
    ) %>%
    dplyr::mutate(second.group = factor(second.group, levels = c(unique(second.group)))) %>%
    dplyr::mutate(group.temp = paste0(first.group, second.group)) -> df
  
  
  aov.results <- NULL
  
  for (i in unique(df$first.group)) {
    df %>%
      dplyr::filter(first.group == i) -> df.temp
    
    fit <- aov(value ~ second.group, data = df.temp)
    
    
    tuk <- glht(fit, linfct = mcp(second.group = "Tukey"))
    sig <- cld(tuk,
               level = ifelse(level != 0.95, level, 0.95),
               decreasing = TRUE
    )[["mcletters"]][["Letters"]] %>%
      as.data.frame()
    
    colnames(sig) <- "signif"
    sig %>%
      dplyr::mutate(
        first.group = i,
        second.group = rownames(.),
        anova.p.value = summary(fit)[[1]][["Pr(>F)"]][1]
      ) %>%
      dplyr::select(first.group, second.group, anova.p.value, signif) -> sig.temp
    
    aov.results <- rbind(aov.results, sig.temp)
  }
  
  aov.results %>%
    dplyr::mutate(group.temp = paste0(first.group, second.group)) %>%
    dplyr::select(group.temp, anova.p.value, signif) -> aov.results
  
  df <- merge(df, aov.results, by = "group.temp", all.x = TRUE) %>%
    dplyr::select(first.group, second.group, value, anova.p.value, signif)
  
  colnames(df)[1:3] <- c(group1, group2, value)
  
  return(df)
}

# 过滤空值
fill_NA <- function(data, value, fill.by) {

  for (i in 1:length(data$Cq)) {
    data$group_fill_NA[i] <- ifelse(data$Cq[i] == "-", "yes",
      ifelse(is.na(data$Cq[i]) == TRUE, "yes", "no")
    )
  }

  df.na <- data %>% dplyr::filter(group_fill_NA == "yes")
  df.nona <- data %>%
    dplyr::filter(group_fill_NA == "no") %>%
    dplyr::mutate(
      Cq = as.numeric(Cq),
      max = max(Cq),
      mean = mean(Cq)
    )
  
  if (dim(df.na)[1] != 0) {
    if (fill.by == "max") {
      df.na$Cq <- mean(df.nona$max)
    } else {
      df.na$Cq <- mean(df.nona$mean)
    }
    
    df.na <- df.na %>% dplyr::select(-group_fill_NA)
    df.nona <- df.nona %>% dplyr::select(-group_fill_NA, -mean, -max)
    
    df <- rbind(df.na, df.nona)
  }else{
    df <- df.nona %>% dplyr::select(-group_fill_NA, -mean, -max)
  }

  return(df)
}

# RqPCR里面的方法
geometric.mean <- function(x) {
  x <- x[!is.na(x)]
  if (any(x < 0)) {
    stop("'x' contains negative value(s)")
  } else {
    return(prod(x)^(1 / length(x)))
  }
}

# 计算稳定值
gene.stable<- function(data, na.rm = TRUE){
  if(!is.data.frame(data) & !is.matrix(data))
    stop("'data' has to of class matrix or data.frame")
  n <- ncol(data)
  if(n == 1) stop("you need at least two genes for this computation")
  M <- numeric(n)
  for(j in 1:n){
    A <- log2(data[,j]/data[,-j])
    if(n > 2)
      M[j] <- mean(apply(A, 2, sd, na.rm = na.rm))
    else
      M[j] <- sd(A, na.rm = na.rm)
  }
  if(is.data.frame(data))
    names(M) <- names(data)
  else
    names(M) <- colnames(data)
  return(M)
}

# fasta转换成df
fasta2df <- function(fasta) {
  
  df.seq <-  fasta %>% na.omit()
  
  seq.num <- c()
  
  for (i in 1:nrow(df.seq)) {
    if (stringr::str_sub(df.seq$V1[i], 1, 1) == ">") {
      seq.num <- c(seq.num, i)
    }
  }
  
  df.temp <- seq.num %>% as.data.frame()
  
  colnames(df.temp)[1] <- "v1"
  
  df.temp <- df.temp %>%
    dplyr::mutate(
      start = v1 + 1,
      end = v1 - 1
    )
  
  seq.info <- as.data.frame(df.seq[df.temp$v1, ]) %>% dplyr::mutate(seq = " ")
  
  for (i in 1:nrow(df.temp)) {
    start <- df.temp$start[i]
    
    if (i == nrow(df.temp)) {
      end <- nrow(df.seq)
    } else {
      end <- df.temp$end[i + 1]
    }
    
    seq.temp <- ""
    
    for (j in start:end) {
      seq.temp <- paste0(seq.temp, df.seq$V1[j])
    }
    seq.info$seq[i] <- seq.temp
  }
  
  colnames(seq.info)[1] <- "id"
  
  return(seq.info)
}

# 从primer3输出结果提取结果
get_res_from_primer3 <- function(data){
  df <- data.table::fread(file = data, header = FALSE, sep = "\n") %>%
    dplyr::mutate(num = "")
  
  res_final <- NULL
  
  cat <- which(df$V1 == "=")
  
  for (i in 1:nrow(df)) {
    df$num[i] <- stringr::str_split(df$V1[i], "_")[[1]][3]
  }
  
  # 按不同基因
  for (i in 1:length(cat)) {
    if (i == 1) {
      df1 <- df[1:(cat[i] - 1), ]
    }else {
      df1 <- df[(cat[i-1] + 1):(cat[i] - 1), ]
    }
    
    
    seq_name <- stringr::str_split(df1$V1[1], "=")[[1]][2]
    seq_input <- stringr::str_split(df1$V1[2], "=")[[1]][2]
    
    
    df2 <- df1[19:nrow(df1), ] %>%
      dplyr::mutate(temp = "")
    
    for (j in 1:nrow(df2)) {
      df2$temp[j] <- ifelse(sjmisc::str_contains(df2$num[j], "="), 1, 0)
    }
    
    for (k in 1:nrow(df2)) {
      df2$num[k] <- ifelse(df2$temp[k] == 1, stringr::str_split(df2$num[k], "=")[[1]][1], df2$num[k])
    }
    
    # 按不同的引物
    for (m in unique(df2$num)) {
      df_temp <- df2 %>% dplyr::filter(num == m)
      
      res_temp <- matrix(ncol = 16, nrow = 2)
      colnames(res_temp) <- c(
        "seq-name", "seq", "primer_num", "PENALTY", "COMPL_ANY_TH", "COMPL_END_TH",
        "PRODUCT_SIZE", "PRODUCT_TM", "primer-seq", "P-PENALTY", "TM",
        "GC", "SELF_ANY_TH", "SELF_END_TH", "HAIRPIN_TH", "END_STABILITY"
      )
      
      res_temp[, 1] <- seq_name
      res_temp[, 2] <- seq_input
      res_temp[1, 3] <- paste0(seq_name, "_", as.character(as.numeric(m) + 1), "_F")
      res_temp[2, 3] <- paste0(seq_name, "_", as.character(as.numeric(m) + 1), "_R")
      res_temp[, 4] <- stringr::str_split(df_temp$V1[1], "=")[[1]][2]
      res_temp[, 5] <- stringr::str_split(df_temp$V1[20], "=")[[1]][2]
      res_temp[, 6] <- stringr::str_split(df_temp$V1[21], "=")[[1]][2]
      res_temp[, 7] <- stringr::str_split(df_temp$V1[22], "=")[[1]][2]
      res_temp[, 8] <- stringr::str_split(df_temp$V1[23], "=")[[1]][2]
      
      res_temp[1, 9] <- stringr::str_split(df_temp$V1[4], "=")[[1]][2] %>% toupper()
      res_temp[2, 9] <- stringr::str_split(df_temp$V1[5], "=")[[1]][2] %>% toupper()
      
      res_temp[1, 10] <- stringr::str_split(df_temp$V1[2], "=")[[1]][2]
      res_temp[2, 10] <- stringr::str_split(df_temp$V1[3], "=")[[1]][2]
      
      res_temp[1, 11] <- stringr::str_split(df_temp$V1[8], "=")[[1]][2]
      res_temp[2, 11] <- stringr::str_split(df_temp$V1[9], "=")[[1]][2]
      
      res_temp[1, 12] <- stringr::str_split(df_temp$V1[10], "=")[[1]][2]
      res_temp[2, 12] <- stringr::str_split(df_temp$V1[11], "=")[[1]][2]
      
      res_temp[1, 13] <- stringr::str_split(df_temp$V1[12], "=")[[1]][2]
      res_temp[2, 13] <- stringr::str_split(df_temp$V1[13], "=")[[1]][2]
      
      res_temp[1, 14] <- stringr::str_split(df_temp$V1[14], "=")[[1]][2]
      res_temp[2, 14] <- stringr::str_split(df_temp$V1[15], "=")[[1]][2]
      
      res_temp[1, 15] <- stringr::str_split(df_temp$V1[16], "=")[[1]][2]
      res_temp[2, 15] <- stringr::str_split(df_temp$V1[17], "=")[[1]][2]
      
      res_temp[1, 16] <- stringr::str_split(df_temp$V1[18], "=")[[1]][2]
      res_temp[2, 16] <- stringr::str_split(df_temp$V1[19], "=")[[1]][2]
      
      res_final <- rbind(res_final, as.data.frame(res_temp))
    }
  }
  return(res_final)
}
# UNCOMMENT AND USE 
# 
# usethis::use_package("markdown")
# usethis::use_package("rmarkdown")
#   
# To use this part of the UI
#   
#' #' Include Content From a File
#' #' 
#' #' Load rendered RMarkdown from a file and turn into HTML.
#' #' 
#' #' @rdname includeRMarkdown
#' #' @export
#' #' 
#' #' @importFrom rmarkdown render
#' #' @importFrom markdown markdownToHTML
#' #' @importFrom shiny HTML
#' includeRMarkdown <- function(path){
#'   
#'   md <- tempfile(fileext = '.md')
#'   
#'   on.exit(unlink(md),add = TRUE)
#'   
#'   rmarkdown::render(
#'     path,
#'     output_format = 'md_document',
#'     output_dir = tempdir(),
#'     output_file = md,quiet = TRUE
#'     )
#'   
#'   html <- markdown::markdownToHTML(md, fragment.only = TRUE)
#'   
#'   Encoding(html) <- "UTF-8"
#'   
#'   return(HTML(html))
#' }
