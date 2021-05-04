namesOrLabels <- function(x) {
    sapply(seq_along(x), function(i) {
        if (!is.null(attr(x[[i]], "label"))) {
            attr(x[[i]], "label")
        } else {
            names(x)[i]
        }
    })
}

render.value <- function(x, .default="") {
    suppressWarnings({
        if (is.null(x) || length(x) == 0) {
            .default
        } else {
            as.character(x)
        }
    })
}

render.count <- function(x, .default="") {
    suppressWarnings({
        if (is.null(x) || length(x) == 0) {
            .default
        } else {
            as.character(length(x))
        }
    })
}

render.npct <- function(x, pct, .default="") {
    suppressWarnings({
        if (is.null(x) || length(x) == 0) {
            .default
        } else {
            sprintf("%d (%0.1f%%)", x, pct)
        }
    })
}


#' Formatted tables the easy way
#'
#' \code{ttt} stands for ``The Table Tool'' (or, if you prefer, ``Tables! Tables! Tables!'').
#' It allows you to creates formatted HTML tables of in a flexible and
#' convenient way.
#'
#' @param x An object.
#' @param data A data.frame.
#' @param formula A three-part formula of the form \code{v ~ r1 + r2 ~
#' c1 + c2} where \code{v} specifies a column of values, while \code{r1,
#' r2} specify row variables and \code{c1, c2} column variables for
#' splitting the values.
#' @param rowvars A list of row variables for splitting the data.
#' @param colvars A list of column variables for splitting the data.
#' @param render A function to render the contents of each cell to character data.
#' @param lab Specify the contents of an extra table cell spanning
#' over all column labels.
#' @param caption A character string to be added as a caption to the table.
#' The default is to omit the caption.
#' @param footnote A character string to be added as a footnote to the table.
#' The default is to omit the footnote.
#' @param expand.along Specify the direction to expand the table when render
#' returns a (named) vector.
#' @param text A character matrix containing the textual content of each table cell.
#' @param drop If \code{TRUE} (the default), rows and columns with zero counts
#' will be dropped.
#' @param collapse.cells If \code{TRUE} (the default), row/column header cells
#' will be collapsed (merged) where appropriate.
#' @param html.class A character matrix with the same dimentions as \code{text}
#' specifying a class attribute for the corresponding \code{<td>} element.
#' @param row.names If \code{TRUE} (the default), row names will be shown in the
#' first column of the table. Set to \code{FALSE} to suppress row names.
#' Only applies when displaying whole \code{data.frame}.
#' @param ... Additional arguments passed to \code{render}.
#'
#' @return A \code{character} which contains an HTML table fragment. It has
#' additional class attributes that cause it to be displayed in a browser in an
#' interactive context, and rendered as HTML in a \code{knitr} context.
#'
#' @examples
#' # mtcars examples
#' ttt(mtcars)
#' ttt(mtcars, mpg ~ gear | cyl, lab="Cylinders")
#' ttt(mpg ~ gear | cyl, data=mtcars, lab="Cylinders")
#' ttt(rownames(mtcars) ~ gear | cyl, data=mtcars,
#'   render=paste, collapse="<br/>", lab="Cylinders")
#'
#' # OrchardSprays examples
#' ttt(head(OrchardSprays, 12))
#' ttt(head(OrchardSprays, 12), row.names=FALSE)
#' ttt(treatment ~ rowpos | colpos, data=OrchardSprays, lab="colpos")
#' ttt(paste(treatment, decrease, sep="<br/>") ~ rowpos | colpos, data=OrchardSprays, lab="colpos")
#'
#' rndr.meansd <- function(x) formatC(c(Mean=mean(x), SD=sd(x)), digits=3)
#' ttt(decrease ~ treatment, data=OrchardSprays, render=rndr.meansd, expand.along="rows")
#' ttt(decrease ~ treatment, data=OrchardSprays, render=rndr.meansd, expand.along="columns")
#'
#' # ToothGrowth examples
#' ttt(len ~ dose | supp, data=ToothGrowth, lab="Mean (SD)",
#'   render=function(x) sprintf("%0.3g (%0.3g)", mean(x), sd(x)))
#'
#' ttt(len ~ dose | supp, data=ToothGrowth, lab="Supplement Type",
#'   render=rndr.meansd)
#'
#' ttt(len ~ dose | supp, data=ToothGrowth, lab="Supplement Type",
#'   render=rndr.meansd, expand.along="columns")
#'
#' @keywords utilities
#' @export
ttt <- function(x, ...) {
    UseMethod("ttt")
}

#' @describeIn ttt The \code{data.frame} method.
#' @export
#' @importFrom stats formula model.frame na.pass
#' @importFrom Formula Formula model.part
ttt.data.frame <- function(x, formula, ..., render, lab, caption, footnote,
    expand.along=c("rows", "columns"), drop=c("both", "rows", "columns", "none"),
    collapse.cells=TRUE, row.names=T) {

    if (missing(formula)) {
        value <- unlist(as.list(format(x)))
        eg <- expand.grid(rownames(x), colnames(x))
        rowvars <- eg[, 1, drop=F]
        colvars <- eg[, 2, drop=F]
        if (missing(lab) || is.null(lab)) {
            names(rowvars) <- " " # Avoid displaying anything in the row label header
        } else {
            names(rowvars) <- lab # In this case use lab for row label header instead
        }
        lab <- list() # Special value
        attr(lab, ".suppressrowlabels") <- !row.names

        ttt.numeric(value, rowvars, colvars, render=render, lab=lab, caption=caption, footnote=footnote,
            expand.along=expand.along, drop=drop, collapse.cells=collapse.cells, ...)
    } else {
        ttt.formula(formula, x, ..., render=render, lab=lab, caption=caption, footnote=footnote,
            expand.along=expand.along, drop=drop, collapse.cells=collapse.cells)
    }
}

#' @describeIn ttt The \code{formula} method.
#' @export
#' @importFrom stats formula model.frame na.pass
#' @importFrom Formula Formula model.part
ttt.formula <- function(x, data, ..., render, lab, caption, footnote,
    expand.along=c("rows", "columns"), drop=c("both", "rows", "columns", "none"),
    collapse.cells=TRUE) {

    dummy <- NULL
    if (is.character(x[[2]])) {
        dummy <- rep(1, nrow(data))
        if (as.character(x[[2]]) == "1") {
            attr(dummy, "label") <- "Count"
        } else {
            attr(dummy, "label") <- x[[2]]
        }
        x[[2]] <- NULL
    }
    f <- Formula(x)
    m <- model.frame(f, data=data, na.action=na.pass)
    if (is.null(dummy)) {
        x <- model.part(f, data=m, lhs=1, drop=T)
        xname <- as.character(f[[2]])
    } else {
        x <- dummy
        xname <- "dummy"
    }
    rowvars <- model.part(f, data=m, rhs=1, drop=F)
    if (ncol(rowvars) == 0) {
        rowvars <- NULL
        if (missing(lab) || is.null(lab)) {
            lab <- list() # Special value
        }
        attr(lab, ".suppressrowlabels") <- TRUE
    }
    if (length(f)[2] > 1) {
        colvars <- rev(model.part(f, data=m, rhs=2, drop=F))
    } else {
        colvars <- data.frame(rep(xname, nrow(m)))
        names(colvars) <- xname
        if (!is.null(xlabel <- attr(x, "label"))) {
            colvars[,1] <- xlabel
        }
        if (missing(lab)) {
            lab <- NULL
        }
    }

    ttt.numeric(x, rowvars, colvars, render=render, lab=lab, caption=caption, footnote=footnote,
        expand.along=expand.along, drop=drop, collapse.cells=collapse.cells, ...)
}

#' @describeIn ttt The \code{numeric} method.
#' @export
#' @importFrom stats setNames ftable
ttt.numeric <- function(x, rowvars, colvars, ..., render, lab, caption, footnote,
    expand.along=c("rows", "columns"), drop=c("both", "rows", "columns", "none"),
    collapse.cells=TRUE) {

    statslab <- names(expand.along)
    if (is.null(statslab)) {
        statslab <- "Statistic"
    }
    expand.along <- match.arg(expand.along)

    tab <- table(c(rev(rowvars), rev(colvars)), dnn=c(rev(names(rowvars)), rev(names(colvars))))
    if (is.null(rowvars) || length(rowvars) == 0) {
        counts <- tab
        class(counts) <- "ftable"
        dim(counts) <- c(1, length(counts))
        attr(counts, "row.vars") <- list()
        attr(counts, "col.vars") <- dimnames(tab)
    } else {
        counts <- ftable(table(c(rev(rowvars), rev(colvars))), row.vars=names(rowvars), col.vars=names(colvars))
    }
    if (missing(lab)) {
        #lab <- names(colvars)[1]
        lab <- NULL
    }
    if (missing(render)) {
        if (all(counts <= 1)) {
            render <- render.value
        } else {
            render <- render.count
        }
    }
    if (expand.along == "rows") {
        text <- lapply(split(unname(x), c(rev(rowvars), rev(colvars))), render, ...)
    } else {
        text <- lapply(split(unname(x), c(rev(colvars), rev(rowvars))), render, ...)
    }
    get.html.class <- function(y) {
        z <- attr(y, which="html.class", exact=TRUE)
        if (is.null(z)) z <- ""
        rep(z, length.out=length(y))
    }
    html.class <- lapply(text, get.html.class)

    stats <- names(text[[1]])
    nstats <- length(stats)
    text <- unlist(text)
    html.class <- unlist(html.class)
    if (expand.along != "rows") {
        text <- matrix(text, nrow=nrow(counts), byrow=T)
        html.class <- matrix(html.class, nrow=nrow(counts), byrow=T)
    }

    a <- attributes(counts)
    names(a$row.vars) <- namesOrLabels(rowvars)
    names(a$col.vars) <- namesOrLabels(colvars)
    if (nstats > 0) {
        if (expand.along == "rows") {
            counts <- counts[rep(seq_len(nrow(counts)), each=nstats),, drop=F]
            a$row.vars <- c(a$row.vars, setNames(list(stats), statslab))
            if (missing(lab) || is.null(lab)) {
                lab <- list() # Special value
            }
            attr(lab, ".suppressrowlabels") <- FALSE
        } else {
            counts <- counts[,rep(seq_len(ncol(counts)), each=nstats), drop=F]
            a$col.vars <- c(a$col.vars, setNames(list(stats), statslab))
        }
        counts[is.na(text)] <- 0
    }
    a$dim <- dim(counts)
    attributes(counts) <- a
    attributes(text) <- a
    attributes(html.class) <- a

    ttt.ftable(counts, text=text, lab=lab, caption=caption, footnote=footnote, drop=drop, collapse.cells=collapse.cells, html.class=html.class)
}

#' @describeIn ttt The \code{ftable} method.
#' @export
#' @importFrom stats ftable
ttt.ftable <- function(x, text=matrix(as.character(x), nrow(x)), ..., lab, caption, footnote,
    drop=c("both", "rows", "columns", "none"), collapse.cells=TRUE, html.class=NULL) {

    .ttt.ftable.internal(
        x              = x,
        text           = text,
        lab            = lab,
        caption        = caption,
        footnote       = footnote,
        drop           = drop,
        collapse.cells = collapse.cells,
        html.class     = html.class)
}

.ttt.ftable.internal <- function(x, text=matrix(as.character(x), nrow(x)), lab, caption, footnote,
    drop=c("both", "rows", "columns", "none"), collapse.cells=TRUE, html.class=NULL, .suppressrowlabels=F) {

    if (!inherits(x, "ftable")) stop("'x' must be an \"ftable\" object")
    if (!all.equal(dim(x), dim(text))) stop("'x' and 'text' must be have the same dimensions")
    if (!is.null(html.class) && !all.equal(dim(x), dim(html.class))) stop("'x' and 'html.class' must be have the same dimensions")

    drop <- match.arg(drop)

    xrv <- attr(x, "row.vars")
    xcv <- attr(x, "col.vars")

    rlab <- rev(expand.grid(rev(xrv), stringsAsFactors=F))
    clab <- rev(expand.grid(rev(xcv), stringsAsFactors=F))

    zr <- apply(x, 1, sum) == 0
    zc <- apply(x, 2, sum) == 0

    if (is.null(html.class)) {
        hcls <- rep("", prod(dim(x)))
        dim(hcls) <- dim(x)
    } else {
        hcls <- html.class
    }

    if (drop == "both") {
        text <- text[!zr, !zc, drop=F]
        hcls <- hcls[!zr, !zc, drop=F]
        rlab <- rlab[!zr, , drop=F]
        clab <- clab[!zc, , drop=F]
    } else if (drop == "rows") {
        text <- text[!zr, ]
        hcls <- hcls[!zr, ]
        rlab <- rlab[!zr, ]
    } else if (drop == "columns") {
        text <- text[, !zc]
        hcls <- hcls[, !zc]
        clab <- clab[, !zc]
    }

    collapseLabels <- function(lab) {
        res <- lapply(seq_along(lab), function(i) {
            z <- lab[,i]
            z2 <- apply(lab[,1:i, drop=F], 1, paste0, collapse=".")
            n <- length(z)
            z[c(FALSE, z2[-1] == z2[-n])] <- ""
            z
        })
        attributes(res) <- attributes(lab)
        res
    }

    if (collapse.cells) {
        rlab <- collapseLabels(rlab)
        clab <- collapseLabels(clab)
    }

    makeRowLabelTags <- function(rlab) {
        lapply(seq_along(rlab), function(i) {
            z <- rlab[,i]
            ind <- z != ""
            span <- table(cumsum(ind))
            sp <- ifelse(span > 1, sprintf(" rowspan=\"%d\"", span), "")
            cl <- sprintf(" class=\"Rttt-rl Rttt-rl-lvl%d\"", length(rlab) - i + 1)
            td <- "td"
            tags <- paste0("<", td, sp, cl, ">", z[ind], "</", td, ">\n")
            z[ind] <- tags
            z
        })
    }

    makeColLabelTags <- function(clab) {
        lapply(seq_along(clab), function(i) {
            z <- clab[,i]
            ind <- z != ""
            span <- table(cumsum(ind))
            sp <- ifelse(span > 1, sprintf(" colspan=\"%d\"", span), "")
            cl <- sprintf(" class=\"Rttt-cl Rttt-cl-lvl%d\"", length(clab) - i + 1)
            td <- "th"
            tags <- paste0("<", td, sp, cl, ">", z[ind], "</", td, ">\n")
            z[ind] <- tags
            z
        })
    }

    rltags <- makeRowLabelTags(rlab)
    cltags <- makeColLabelTags(clab)

    makeRowLabelHeadTags <- function(rhlab, span) {
        sp <- if (span > 1) sprintf(" rowspan=\"%d\"", span) else ""
        cl <- " class=\"Rttt-rlh\""
        td <- "th"
        tags <- paste0("<", td, sp, cl, ">", rhlab, "</", td, ">\n")
        tags
    }


    if (!missing(lab) && !is.null(lab)) {
        .suppressrowlabels <- attr(lab, ".suppressrowlabels")
        if (is.null(.suppressrowlabels)) {
            .suppressrowlabels <- FALSE
        }
        if (length(lab) > 0) {
            span <- ncol(text)
            sp <- if (span > 1) sprintf(" colspan=\"%d\"", span) else ""
            cl <- " class=\"Rttt-lab\""
            td <- "th"
            tags <- paste0("<", td, sp, cl, ">", lab, "</", td, ">\n")
            cltags <- c(tags, cltags)
        }
    }

    rlhtags <- makeRowLabelHeadTags(names(xrv), length(cltags))

    thead <- lapply(seq_along(cltags), function(i) {
        tags <- cltags[[i]]
        if (i == 1 && !.suppressrowlabels) {
            for (j in rev(seq_along(rlhtags))) {
                tags <- c(rlhtags[j], tags)
            }
        }
        paste0("<tr>\n", paste0(tags, collapse=""), "</tr>\n", collapse="")
    })

    dat <- as.matrix(text)
    ncolumns <- ncol(dat) + length(rltags)

    hcls <- as.character(hcls)
    hcls <- ifelse(is.na(hcls) | hcls=="", "",
        paste0(" class=\"", gsub(".", "-", make.names(hcls), fixed=TRUE), "\""))
    dim(hcls) <- dim(dat)

    tbody <- lapply(seq_len(nrow(dat)), function(i) {
        td <- "td"
        tags <- paste0("<", td, hcls[i,], ">", dat[i,], "</", td, ">\n")
        if (!.suppressrowlabels) {
            for (j in rev(seq_along(rltags))) {
                tags <- c(rltags[[j]][i], tags)
            }
        }
        paste0("<tr>\n", paste0(tags, collapse=""), "</tr>\n", collapse="")
    })

    if (!missing(caption) && !is.null(caption)) {
        caption <- sprintf('<caption>%s</caption>\n', caption)
    } else {
        caption <- ""
    }

    if (!missing(footnote) && !is.null(footnote)) {
        footnote <- sprintf('<p>%s</p>\n', footnote)
        footnote <- paste0(footnote, collapse="\n")
        tfoot <- sprintf('<tfoot><tr><td colspan="%d">%s</td></tr></tfoot>\n', ncolumns, footnote)
    } else {
        tfoot <- ""
    }

    x <- paste0(
        sprintf('\n<table>\n%s<thead>\n', caption),
        paste0(thead, collapse=""),
        sprintf('</thead>\n%s<tbody>\n', tfoot),
        paste0(tbody, collapse=""),
        "</tbody>\n</table>\n")

    structure(x, class=c("ttt", "html", "character"), html=TRUE)
}

#' Print \code{ttt} object
#'
#' @param x An object returned by \code{\link{ttt}}.
#' @param ... Further arguments passed on to other \code{print} methods.
#' @return Returns \code{x} invisibly.
#' @details In an interactive context, the rendered table will be displayed in
#' a web browser. Otherwise, the HTML code will be printed as text.
#' @export
print.ttt <- function(x, ...) {
    if (interactive()) {
        z <- htmltools::HTML(x)
        default.style <- htmltools::htmlDependency("ttt", "1.0",
            src=system.file(package="ttt", "ttt_defaults_1.0"),
            stylesheet="ttt_defaults.css")
        z <- htmltools::div(class="Rttt", default.style, z)
        z <- htmltools::browsable(z)
        print(z, ...) # Calls htmltools:::print.html(z, ...)
    } else {
        cat(x)
    }
    invisible(x)
}


#' Method for printing in a \code{knitr} context
#'
#' @param x An object returned by \code{\link{ttt}}.
#' @param ... Further arguments passed on to \code{knitr::knit_print}.
#' @importFrom knitr knit_print
#' @export
knit_print.ttt <- function(x, ...) {
    knit_to_html <-
        !is.null(knitr::opts_knit$get("rmarkdown.pandoc.to")) &&
        grepl("^html", knitr::opts_knit$get("rmarkdown.pandoc.to"))

    if (knit_to_html) {
        z <- htmltools::HTML(x)
        default.style <- htmltools::htmlDependency("ttt", "1.0",
            src=system.file(package="ttt", "ttt_defaults_1.0"),
            stylesheet="ttt_defaults.css")
        z <- htmltools::div(class="Rttt", default.style, z)
        knitr::knit_print(z, ...)
    } else {
        knitr::knit_print(as.character(x), ...)
    }
}

