generate_context <- function(doc_var, data, metadata) {
    library(forestplot)
    library(stringr)

    selected.columns <- c('study', 'n')
    row.id.column <- selected.columns[1]
    column.names <- c('Study', 'N')
    row.id.column.name <- column.names[1]

    prev.ci.name <- 'prev.ci'
    generate.prev.ci <- function(df) {
      return(paste(df$prevalence, df$ci))
    }

    img_width <- 6
    img_height <- 5.5
    img_width_report <- 1
    caption <- 'Sample forest plot'

    figure_name <- metadata$fragment_name
    image_path <- file.path(metadata$fig_path, figure_name)



    pdf(paste0(image_path,'.pdf'),onefile=F,
        #family=grafic$Font,
        height=img_height,width=img_width);

    full.data <- data$forest_data
    datos <- full.data$data
    datos[,prev.ci.name] <- generate.prev.ci(datos)

    if(datos$study[1] == '-9999' || nrow(datos) == 0)
    {
        df <- data.frame(study='No data available', n='', prevalence='', ci='')
        dfCI <- data.frame(cci=c(NA,0),lci=c(NA,0),uci=c(NA,0))

        xticks <- seq(0, 100, 10)
        xtlab <- paste0(xticks,'%')
        attr(xticks, "labels") <- xtlab

        tg <- fpTxtGp(ticks = gpar(cex=.75), label=gpar(cex=.75))

        forestplot(
            rbind(
              column.names,
              matrix(c(df$study, df$n, paste(df$prevalence, df$ci)), ncol=3, byrow=F)
            ),
            is.summary = c(T, rep(F, nrow(df))),
            align = c('l','c','c','c'),
            mean = dfCI$cci,
            lower = dfCI$lci,
            upper = dfCI$uci,
            boxsize = 0,
            #lineheight = unit(.6,'inch'),
            graph.pos=1,
            xticks=xticks,
            txt_gp=tg
        )
    } else {
        datos[datos==-9999]<-NA

        ranges <- t(as.data.frame(str_extract_all(datos$ci, '[0-9.]+')))
        lci <- as.numeric(ranges[,1])
        uci <- as.numeric(ranges[,2])
        if (any(lci > uci))
            stop('Ranges are not valid')

        datos$lci <- lci
        datos$uci <- uci
        datos$cci <- datos$prevalence
        datos <- datos[order(as.numeric(datos$n),decreasing=T),];

        boxSizes <- c(0, (as.numeric(datos$n) / max(as.numeric(datos$n)) * .35) + 0.05)

        datos$n <- format(as.numeric(datos$n), big.mark=',')

        table.data <- data.frame(datos$study,datos$n,paste(datos$prevalence, datos$ci), stringsAsFactors=F)
        table.data <- rbind(c('Study', 'N', '% (95% CI)'), table.data)
        table.data <- lapply(split(t(table.data),seq_along(table.data[1,])),as.list)

        refs <- generate.table.data(full.data,
                                    selected.columns=selected.columns,
                                    column.names=column.names,
                                    row.id.column=row.id.column)

        for(i in 2:length(table.data[[1]])) {
            if (length(refs$table[[i-1,row.id.column.name]]$markers) > 0) {
              markers <- paste0(refs$table[[i-1,row.id.column.name]]$markers, collapse = ',')
              table.data[[1]][[i]] <- as.expression(substitute(x^y,list(x=table.data[[1]][[i]],y=markers)))
            }
        }

        table.cis <- data.frame(mean=datos$cci, lower=datos$lci, upper=datos$uci)
        table.cis[] <- lapply(table.cis, as.numeric)
        table.cis <- rbind(c(NA, NA, NA), table.cis)

        minX <- min(datos$lci,na.rm=T)
        maxX <- max(datos$uci,na.rm=T)
        minX <- floor(minX/10) * 10
        maxX <- ceiling(maxX/10) * 10
        if (minX == maxX) {
            if (minX != 0) minX <- maxX - 10
            else maxX <- 10
        }
        xticks <- seq(minX, maxX, 10)
        xtlab <- paste0(xticks,'%')
        attr(xticks, "labels") <- xtlab

        tg <- fpTxtGp(ticks = gpar(cex=.75), label=gpar(cex=.75))

        forestplot(
            table.data,
            is.summary = c(T, rep(F, nrow(datos))),
            align = c('l','c','c'),
            mean=c(NA, datos$prevalence),
            lower=c(NA, datos$lci),
            upper=c(NA, datos$uci),
            boxsize=boxSizes,
            ci.vertices=T,
            #lineheight = unit(.6,'inch'),
            graph.pos=1,
            grid=T,
            xticks=xticks,
            colgap = unit(.01,'npc'),
            zero=xticks[1],
            txt_gp = tg,
            col=fpColors(box="black",line="black")
        )
    }
    dev.off();
    return(list(
        image_path=image_path,
        footer=list(sources=list(),
                    notes=list(),
                    methods=list(),
                    years=list(),
                    date=data$forest_data$date),
        image_width=img_width_report,
        caption=caption
    ))
}



data <- list()
data$data <- data.frame(iso=c('ESP', 'ESP', 'ESP'),
                        study=c('Alemany 2015', 'Castellsagué 2010', 'Sanjosé 2012'),
                        n=c('100', '200', '300'),
                        prevalence=c('30', '40', '60'),
                        ci=c('[25-36]', '[12.3-65.2]', '[53-80]'))
data$sources <- list(
  global=data.frame(
    text=c('Global source')
  ),
  row=data.frame(
  ),
  column=data.frame(
  ),
  cell=data.frame()
)
data$notes <- list(
  global=data.frame(),
  row=data.frame(
    row=c(1,1),
    text=c('Note 1', 'Note 2')
  ),
  column=data.frame(
  ),
  cell=data.frame(
  )
)
data$methods <- list(
  global=data.frame(),
  row=data.frame(),
  column=data.frame(),
  cell=data.frame(
  )
)
data$years <- list(
  global=data.frame(),
  row=data.frame(),
  column=data.frame(),
  cell=data.frame()
)
data$dates <- data.frame(date_accessed='2019-01-01', date_closing='2019-01-02', date_publication='2019-01-03', date_delivery='2019-01-04')

ctx <- generate_context(list(iso='ESP'), list(forest_data=data), list(fragment_name='test', fig_path='/home/david/Desktop/'))

