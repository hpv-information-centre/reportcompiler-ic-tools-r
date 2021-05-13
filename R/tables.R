#' Generates HPV Information Centre referenced table structure.
#'
#' Generates a new dataframe with the markers corresponding to the defined
#' references (sources, notes, ...), alongside a list of the markers' meaning.
#'
#'  @param data.dict List returned by the IC data fetcher
#'  @param selected.columns Column names to be selected
#'  from the original dataframe
#'  @param column.names List with the column names of the selected
#'  dataframe columns
#'  @param row.id.column Column name that will contain the marks for row
#'  references
#'  @param format Format of the returned dataframe
#'  @param collapse.refs Whether markers should be collapsed when
#'  appropriate (e.g. all cells of a column to the column header)
#'
#' @return List with three components: data, columns and references,
#' where data is the original dataframe with the necessary reference markers,
#' columns is the list with the table columns as will be displayed and
#' references is a nested structure: for each type (sources, notes, ...)
#' there is a list of dictionaries with each ('marker' key) and associated
#' reference ('text' key).
#'
#' @examples
#' # TODO
#'
#' @export
generate.table.data <- function(data.dict,
                                selected.columns=NULL,
                                column.names=NULL,
                                row.id.column=NULL,
                                format='latex',
                                collapse.refs=TRUE) {
  data <- data.dict$data

  if (is.null(selected.columns)) {selected.columns <- names(data)}
  if (is.null(column.names)) {column.names <- names(data)}
  if (is.null(row.id.column)) {row.id.column <- names(data)[1]}

  if (length(column.names) != length(selected.columns))
    stop('column.names must have the same lengths as the number of columns in data.dict')

  if (!(all(selected.columns %in% names(data))))
    stop('Selected columns must be included in the original dataframe')

  ref.type.markers <- list(
    sources=source.markers(),
    notes=note.markers(),
    methods=method.markers(),
    years=year.markers()
  )

  stringsAsFactorsDefault <- getOption('stringsAsFactors')
  options(stringsAsFactors = F)

  marker.data <- data.frame(row.names=row.names(data))
  for(col in names(data)) {
    marker.data[,col] <- NA
    for(index in row.names(marker.data)) {
      marker.data[[index, col]] <- list(value=data.dict$data[[index, col]])
      marker.data[[index, col]]$markers <- character(0)
    }
  }
  row.id.column <- names(marker.data)[names(marker.data) == row.id.column]

  table.footer <- list(
    sources=data.frame(marker=character(0), text=character(0)),
    notes=data.frame(marker=character(0), text=character(0)),
    methods=data.frame(marker=character(0), text=character(0)),
    years=data.frame(marker=character(0), text=character(0))
  )

  column.markers <- vector('list', length(selected.columns))
  names(column.markers) <- selected.columns

  for(ref.type in names(ref.type.markers)) {
    ref.generator <- ref.type.markers[[ref.type]]
    ref.data <- data.dict[[ref.type]]
    table.footer[[ref.type]] <- .build.global.refs(
                                  ref.data$global,
                                  table.footer,
                                  ref.generator,
                                  ref.type
                                )
    column.ref.data <- .build.column.refs(
      ref.data$column,
      table.footer,
      ref.generator, ref.type,
      marker.data,
      selected.columns,
      column.names
    )
    table.footer[[ref.type]] <- column.ref.data$table.footer
    .column.markers <- column.ref.data$markers

    for(col in names(column.markers)) {
      if (!is.null(.column.markers[[col]]))
        column.markers[[col]] <- c(column.markers[[col]], .column.markers[[col]])
    }
    row.ref.data <- .build.row.refs(
                      ref.data$row,
                      table.footer,
                      ref.generator,
                      ref.type,
                      marker.data,
                      row.id.column
                    )
    marker.data <- row.ref.data$marker.data
    table.footer[[ref.type]] <- row.ref.data$table.footer

    cell.ref.data <- .build.cell.refs(
                        ref.data$cell,
                        table.footer,
                        ref.generator,
                        ref.type,
                        marker.data
                      )
    marker.data <- cell.ref.data$marker.data
    table.footer[[ref.type]] <- cell.ref.data$table.footer
  }

  column.info <- list()
  for(col in names(column.markers)) {
    column.info <- append(column.info, list(list(value=col, markers=column.markers[[col]])))
  }

  marker.data <- marker.data[selected.columns]
  names(marker.data) <- column.names

  if (collapse.refs)
    .collapse.common.refs(marker.data, column.info)

  table.footer$date <- data.dict$date
  for(ref.type in names(ref.type.markers)) {
    table.footer[[ref.type]] <- apply(table.footer[[ref.type]], 1, function(row) {
      return(list(marker=row['marker'], text=row['text']))
    })
  }

  options(stringsAsFactors = stringsAsFactorsDefault)

  info.dict <- list(
    table=marker.data,
    columns=column.info,
    footer=table.footer,
    markers=ref.type.markers
  )
  return(info.dict)
}

.collapse.common.refs <- function(table, columns) {
  for(i in seq_along(names(table))) {
    col <- names(table)[i]
    markers <- table['markers',col]
    col.markers <- c()
    for(cell.markers in markers)
      for(marker in cell.markers) {
        col.markers <- c(col.markers, marker)
      }
    col.markers <- col.markers[!duplicated(col.markers)]
    for(marker in col.markers) {
      if (all(marker %in% table[,col])) {
        columns[i]['markers'] <- c(columns[i]['markers'], marker)
        for(cell in table[col]) {
          cell['markers'] <- cell['markers'][cell['markers'] != marker]
        }
      }
    }
  }
}

.build.global.refs <- function(ref.data,
                               table.footer,
                               markers,
                               ref.type) {
  for(ref in ref.data$text) {
    if (length(table.footer[[ref.type]][table.footer[[ref.type]][,'text']==ref]) == 0) {
      table.footer[[ref.type]] <- rbind(table.footer[[ref.type]], list(marker='', text=ref))
    }
  }
  return(table.footer[[ref.type]])
}

.build.column.refs <- function(ref.data,
                               table.footer,
                               markers,
                               ref.type,
                               marker.data,
                               selected.columns,
                               column.names) {
  column.markers <- list()
  if (!is.null(ref.data) && length(ref.data) > 0) {
	  for(i in seq_along(selected.columns)) {
	    column <- selected.columns[i]
	    refs <- ref.data[ref.data$column == column, 'text']
	    col.markers <- c()
	    for(ref in refs) {
	      if (nrow(table.footer[[ref.type]][table.footer[[ref.type]][,'text']==ref,]) == 0) {
		tryCatch(marker <- iterators::nextElem(markers), error=function(e) stop(paste0('No more "', ref.type, '" markers are available.')))
		table.footer[[ref.type]] <- rbind(table.footer[[ref.type]], list(marker=marker, text=ref))
	      } else {
		marker <- table.footer[[ref.type]][table.footer[[ref.type]][,'text']==ref,'marker'][[1]]
	      }
	      if (!(marker %in% col.markers)) col.markers <- c(col.markers, marker)
	    }
	    column.markers[[column]] <- c(column.markers[[column]], col.markers)
	  }
  }
  return(list(
    table.footer=table.footer[[ref.type]],
    markers=column.markers
  ))
}

.build.row.refs <- function(ref.data,
                            table.footer,
                            markers,
                            ref.type,
                            marker.data,
                            row.id.column) {
  if (!is.null(ref.data) && length(ref.data) > 0) {
	  for(row.index in row.names(marker.data)) {
	    refs <- ref.data[ref.data$row == row.index, 'text']
	    for(ref in refs) {
	      row.markers <- marker.data[[row.index, row.id.column]]$markers
	      if (nrow(table.footer[[ref.type]][table.footer[[ref.type]][,'text']==ref,]) == 0) {
		tryCatch(marker <- iterators::nextElem(markers), error=function(e) stop(paste0('No more "', ref.type, '" markers are available.')))
		table.footer[[ref.type]] <- rbind(table.footer[[ref.type]], list(marker=marker, text=ref))
	      } else {
		marker <- table.footer[[ref.type]][table.footer[[ref.type]][,'text']==ref,'marker'][[1]]
	      }
	      if (!(marker %in% row.markers))
		marker.data[[row.index, row.id.column]]$markers <- c(row.markers, marker)
	    }
	  }
  }
  return(list(
    marker.data=marker.data,
    table.footer=table.footer[[ref.type]]
  ))
}

.build.cell.refs <- function(ref.data,
                             table.footer,
                             markers,
                             ref.type,
                             marker.data) {
  if (!is.null(ref.data) && length(ref.data) > 0) {
	  for(row.index in row.names(marker.data)) {
	    for(column in names(marker.data)) {
	      refs <- ref.data[ref.data$row == row.index && ref.data$column == column, 'text']
	      for(ref in refs) {
		if (nrow(table.footer[[ref.type]][table.footer[[ref.type]][,'text']==ref,]) == 0) {
		  tryCatch(marker <- iterators::nextElem(markers), error=function(e) stop(paste0('No more "', ref.type, '" markers are available.')))
		  table.footer[[ref.type]] <- rbind(table.footer[[ref.type]], list(marker=marker, text=ref))
		} else {
		  marker <- table.footer[[ref.type]][table.footer[[ref.type]][,'text']==ref,'marker'][[1]]
		}
		if (!(marker %in% marker.data[[row.index, column]]))
		  marker.data[[row.index, column]]$markers <- c(marker.data[[row.index, column]]$markers, marker)
	      }
	    }
	  }
  }
  return(list(
    marker.data=marker.data,
    table.footer=table.footer[[ref.type]]
  ))
}

#' Generates iterator for source markers
#'
#' Generates an iterator for successive source markers to be used as footnotes.
#'
#' @return Iterator for source markers
#'
#' @examples
#' markers <- source.markers()
#' iterators::nextElem(markers)
#' # [1] '1'
#' iterators::nextElem(markers)
#' # [1] '2'
#'
#' @export
source.markers <- function() {
  values <- as.character(seq(1,50))
  return(iterators::iter(values))
}


#' Generates iterator for note markers
#'
#' Generates an iterator for successive note markers to be used as footnotes.
#'
#' @return Iterator for note markers
#'
#' @examples
#' markers <- note.markers()
#' iterators::nextElem(markers)
#' # [1] 'a'
#' iterators::nextElem(markers)
#' # [1] 'b'
#'
#' @export
note.markers <- function() {
  values <- c(letters, LETTERS)
  return(iterators::iter(values))
}


#' Generates iterator for method markers
#'
#' Generates an iterator for successive method markers to be used as footnotes.
#' Currently these markers are formatted as LaTeX.
#'
#' @return Iterator for method markers
#'
#' @examples
#' markers <- method.markers()
#' iterators::nextElem(markers)
#' # [1] '\\alpha'
#' iterators::nextElem(markers)
#' # [1] '\\beta'
#'
#' @export
method.markers <- function() {
  values <- c(
    '\\alpha', '\\beta', '\\gamma', '\\delta', '\\epsilon', '\\zeta',
    '\\eta', '\\theta', '\\iota', '\\kappa', '\\lambda', '\\mu', '\\nu',
    '\\omicron', '\\pi', '\\rho', '\\sigma', '\\tau', '\\upsilon', '\\phi',
    '\\chi', '\\psi', '\\omega', '\\Gamma', '\\Delta', '\\Theta',
    '\\Lambda', '\\Pi', '\\Sigma', '\\Upsilon', '\\Phi', '\\Psi', '\\Omega'
  )
  return(iterators::iter(values))
}


#' Generates iterator for year markers
#'
#' Generates an iterator for successive year markers to be used as footnotes.
#' Currently these markers are formatted as LaTeX.
#'
#' @return Iterator for year markers
#'
#' @examples
#' markers <- year.markers()
#' iterators::nextElem(markers)
#' # [1] '\\Diamond'
#' iterators::nextElem(markers)
#' # [1] '\\triangle'
#'
#' @export
year.markers <- function() {
  values <- c(
    '\\Diamond', '\\triangle', '\\nabla', '\\S', '\\bigstar', '\\aleph',
    '\\infty', '\\Join', '\\natural', '\\mho', '\\emptyset', '\\partial',
    '\\textdollar', '\\triangleright', '\\triangleleft', '\\bullet',
    '\\star', '\\dagger', '\\ddagger', '\\oplus', '\\ominus', '\\otimes',
    '\\Box'
  )
  return(iterators::iter(values))
}
















