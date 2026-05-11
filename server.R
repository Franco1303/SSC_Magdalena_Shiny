# ─────────────────────────────────────────
# server.R — Lógica reactiva (v2)
# CSS Magdalena EDA — Shiny
# ─────────────────────────────────────────

server <- function(input, output, session) {
  
  # ═══════════════════════════════════════
  # DATASET FILTRADO
  # ═══════════════════════════════════════
  dff <- reactive({
    req(input$km_filter)
    df %>% filter(km %in% input$km_filter)
  })
  
  output$km_filter_count <- renderText({
    paste0(nrow(dff())," observaciones · ",length(input$km_filter)," estación(es)")
  })
  
  # ═══════════════════════════════════════
  # CONTEXTO
  # ═══════════════════════════════════════
  output$map_estaciones <- renderPlotly({
    lats   <- c(11.10354,11.102,11.09628,11.09018,11.0755,11.0585,11.0428,
                11.0249,11.0011327,10.9919,10.9782,10.9637,10.9546)
    lons   <- c(-74.8516,-74.8513,-74.8497,-74.8492,-74.8456,-74.8387,-74.8195,
                -74.7911,-74.7660752,-74.7611,-74.7579,-74.7569,-74.7562)
    labels <- c("Km 0 +250","Km 0 +500","Km 1","Km 1+900","Km 3+500",
                "Km 5+500","Km 7+900","Km 11+200","Km 14+800",
                "Km 17+600","Km 18+200","Km 19+800","Km 19+940")
    plot_ly(lat=lats,lon=lons,type="scattermapbox",mode="markers+lines",
            text=labels,hoverinfo="text",
            marker=list(size=12,color=COLOR_ACCENT),
            line=list(color=COLOR_ACCENT)) %>%
      layout(mapbox=list(style="carto-positron",
                         center=list(lat=11.05,lon=-74.82),zoom=11.5),
             margin=list(l=0,r=0,t=0,b=0),paper_bgcolor=COLOR_CARD)
  })
  
  output$map_calamar <- renderPlotly({
    plot_ly(lat=c(10.2422934,10.30),lon=c(-74.9138168,-74.95),
            type="scattermapbox",mode="markers",
            text=c("Calamar, IDEAM (20037020)","INKORA K-7 (29037360)"),hoverinfo="text",
            marker=list(size=14,color=COLOR_ACCENT)) %>%
      layout(mapbox=list(style="carto-positron",
                         center=list(lat=10.2422934,lon=-74.9138168),zoom=9),
             margin=list(l=0,r=0,t=0,b=0),paper_bgcolor=COLOR_CARD)
  })
  
  output$km_badges <- renderUI({
    div(style="display:flex;gap:12px;flex-wrap:wrap;",
        lapply(sort(unique(df$km)), function(k) {
          col <- km_color(k)
          div(style=paste0(
            "background:",COLOR_CARD,";border-radius:10px;padding:16px 24px;",
            "box-shadow:0 1px 4px rgba(0,0,0,0.07);border:1px solid ",COLOR_BORDER,
            ";border-top:3px solid ",col,";"),
            div(paste0("Km ",k), style=paste0("font-weight:700;color:",col,";font-size:16px;")),
            div(paste0(nrow(df[df$km==k,])," obs."),
                style=paste0("font-size:13px;color:",COLOR_MUTED,";"))
          )
        })
    )
  })
  
  # ═══════════════════════════════════════
  # MARCO — Bands visualizer
  # ═══════════════════════════════════════
  selected_band <- reactiveVal(NULL)
  
  output$bands_viz <- renderUI({
    MIN_NM <- 400; MAX_NM <- 2400
    nm_pct <- function(nm) (nm - MIN_NM) / (MAX_NM - MIN_NM) * 100
    
    bars <- lapply(seq_along(BANDS_S2), function(i) {
      b    <- BANDS_S2[[i]]
      left <- nm_pct(b$lambda - 10)
      wid  <- max(nm_pct(20), 0.8)
      ht   <- switch(b$res, "10 m"=100, "20 m"=76, "60 m"=52, 76)
      div(
        style=paste0("position:absolute;left:",left,"%;width:",wid,
                     "%;bottom:24px;height:",ht,"px;background:",b$color,
                     ";opacity:0.82;border-radius:4px;cursor:pointer;border:1.5px solid transparent;"),
        onclick=paste0("Shiny.setInputValue('selected_band_idx',",i,",{priority:'event'})"),
        div(as.character(b$lambda),
            style="position:absolute;top:-18px;left:50%;transform:translateX(-50%);font-size:9px;color:#6b7280;white-space:nowrap;")
      )
    })
    
    tagList(
      div(style="position:relative;width:100%;height:160px;",
          div(style="position:absolute;top:0;left:0;width:100%;height:100%;", bars),
          div(style="position:absolute;bottom:0;left:0;width:100%;height:20px;",
              lapply(c(400,600,800,1000,1400,1800,2200), function(nm)
                tags$span(as.character(nm), style=paste0(
                  "position:absolute;left:",(nm-400)/2000*100,"%;",
                  "transform:translateX(-50%);font-size:10px;color:",COLOR_MUTED,";"
                ))
              )
          )
      )
    )
  })
  
  output$band_info_ui <- renderUI({
    idx <- input$selected_band_idx
    if (is.null(idx)) {
      return(div(style=paste0("background:",COLOR_BG,";border-radius:8px;padding:14px 18px;",
                              "margin-top:12px;border:0.5px solid ",COLOR_BORDER,";min-height:70px;"),
                 tags$p("Selecciona una banda del espectro",
                        style=paste0("font-size:13px;color:",COLOR_MUTED,";margin:0;"))))
    }
    b <- BANDS_S2[[idx]]
    res_colors <- list("10 m"=list(bg="#E6F1FB",col="#0C447C"),
                       "20 m"=list(bg="#E1F5EE",col="#085041"),
                       "60 m"=list(bg="#FAEEDA",col="#633806"))
    rc <- res_colors[[b$res]]
    div(style=paste0("background:",COLOR_BG,";border-radius:8px;padding:14px 18px;",
                     "margin-top:12px;border:0.5px solid ",COLOR_BORDER,";min-height:70px;"),
        div(style="display:flex;align-items:center;gap:10px;margin-bottom:8px;",
            div(style=paste0("width:12px;height:12px;border-radius:3px;background:",b$color,";flex-shrink:0;")),
            tags$span(b$name, style=paste0("font-size:14px;font-weight:600;color:",COLOR_TEXT,";")),
            tags$span(b$res, style=paste0("font-size:11px;font-weight:500;padding:2px 8px;border-radius:6px;",
                                          "background:",rc$bg,";color:",rc$col,";")),
            tags$span(paste0("λ = ",b$lambda," nm"),
                      style=paste0("font-size:12px;color:",COLOR_MUTED,";margin-left:auto;"))
        ),
        tags$p(b$desc, style=paste0("font-size:13px;color:",COLOR_MUTED,";margin:0;line-height:1.6;"))
    )
  })
  
  # ── Fórmulas navegables ──
  formula_idx <- reactiveVal(1)
  
  observeEvent(input$formula_prev, {
    formula_idx(max(1, formula_idx()-1))
  })
  observeEvent(input$formula_next, {
    formula_idx(min(length(FORMULAS), formula_idx()+1))
  })
  
  output$formula_tag <- renderUI({
    f <- FORMULAS[[formula_idx()]]
    tags$span(f$tag, style="font-size:11px;font-weight:500;padding:3px 10px;border-radius:6px;background:#E6F1FB;color:#0C447C;")
  })
  
  output$formula_content <- renderUI({
    f <- FORMULAS[[formula_idx()]]
    tagList(
      tags$p(f$title, style=paste0("font-size:15px;font-weight:500;color:",COLOR_TEXT,";margin:0 0 4px;")),
      tags$p(f$sub,   style=paste0("font-size:12px;color:",COLOR_MUTED,";margin:0 0 4px;")),
      div(f$formula,  class="formula-card", style="margin:16px 0;"),
      tags$p(f$desc,  style=paste0("font-size:13px;color:",COLOR_MUTED,";margin:0;line-height:1.7;"))
    )
  })
  
  output$formula_counter <- renderText({
    paste0(formula_idx()," / ",length(FORMULAS))
  })
  
  # ═══════════════════════════════════════
  # EDA — Estadísticas
  # ═══════════════════════════════════════
  stats_group <- reactiveVal("bandas")
  
  observeEvent(input$pill_bandas,  stats_group("bandas"))
  observeEvent(input$pill_indices, stats_group("indices"))
  observeEvent(input$pill_ssc,     stats_group("ssc"))
  
  observe({
    vars <- switch(stats_group(), bandas=BANDAS, indices=INDICES, ssc=SSCS)
    updateSelectInput(session,"stats_var", choices=vars, selected=vars[1])
  })
  
  output$stats_table <- DT::renderDataTable({
    d   <- dff(); var <- input$stats_var
    req(var %in% names(d))
    x <- d[[var]]
    data.frame(
      Variable  = var,
      Media     = round(mean(x,na.rm=T),4),
      `Desv.Est`= round(sd(x,na.rm=T),4),
      Min       = round(min(x,na.rm=T),4),
      Mediana   = round(median(x,na.rm=T),4),
      Max       = round(max(x,na.rm=T),4),
      check.names=FALSE
    ) %>%
      DT::datatable(options=list(dom='t',paging=FALSE,ordering=FALSE),
                    rownames=FALSE, class="compact")
  })
  
  # ═══════════════════════════════════════
  # EDA — Perfiles
  # ═══════════════════════════════════════
  observe({
    fechas <- sort(unique(df_profiles$fecha))
    updateSelectInput(session, "profile_fecha",
                      choices  = setNames(as.character(fechas), format(fechas, "%d/%m/%Y")),
                      selected = as.character(fechas[1]))
  })
  
  observe({
    req(input$profile_fecha)
    sub <- df_profiles %>% filter(fecha == as.Date(input$profile_fecha))
    kms <- sort(unique(sub$km))
    updateSelectInput(session, "profile_km",
                      choices=setNames(kms, paste0("Km ",kms)), selected=kms[1])
  })
  
  observe({
    req(input$profile_fecha, input$profile_km)
    sub <- df_profiles %>%
      filter(fecha == as.Date(input$profile_fecha), km == as.integer(input$profile_km))
    pms <- sort(unique(sub$pm))
    updateSelectInput(session, "profile_pm",
                      choices=setNames(pms, paste0("+",pms," m")), selected=pms[1])
  })
  
  profile_data <- reactive({
    req(input$profile_fecha, input$profile_km, input$profile_pm)
    df_profiles %>%
      filter(fecha == as.Date(input$profile_fecha),
             km    == as.integer(input$profile_km),
             pm    == as.integer(input$profile_pm)) %>%
      mutate(
        depth = as.numeric(depth),
        ssc   = as.numeric(ssc)
      ) %>%
      arrange(desc(depth))
  })
  
  output$profile_plot <- renderPlotly({
    sub <- profile_data()
    req(nrow(sub) > 0)
    col <- unname(KM_COLORS[as.character(input$profile_km)])
    if (is.na(col)) col <- COLOR_ACCENT
    fecha_l <- format(as.Date(input$profile_fecha), "%d/%m/%Y")
    
    fig <- plot_ly(sub, x=~ssc, y=~depth, type="scatter", mode="lines+markers",
                   line=list(color=col, width=2.5),
                   marker=list(size=6, color=col, line=list(width=1,color="white")),
                   hovertemplate="Prof: %{y:.2f} m<br>SSC: %{x:.1f} mg/L<extra></extra>") %>%
      layout(
        title=list(text=paste0("Perfil SSC — Km ",input$profile_km,
                               ", +",input$profile_pm," m | ",fecha_l),
                   font=list(family="'Merriweather',serif",size=14,color=COLOR_TEXT), x=0.5),
        xaxis=list(title="SSC (mg/L)", showgrid=TRUE, gridcolor=COLOR_BORDER),
        yaxis=list(title="Profundidad (m)", autorange="reversed",
                   showgrid=TRUE, gridcolor=COLOR_BORDER),
        paper_bgcolor=COLOR_CARD, plot_bgcolor=COLOR_BG,
        font=list(family="'Lato',sans-serif",size=12,color=COLOR_TEXT),
        margin=list(l=60,r=30,t=50,b=50),
        hovermode="y unified",
        shapes=list(
          list(type="line",x0=min(sub$ssc)*0.95,x1=max(sub$ssc)*1.05,
               y0=4,y1=4,line=list(color="gray",width=1,dash="dash")),
          list(type="line",x0=min(sub$ssc)*0.95,x1=max(sub$ssc)*1.05,
               y0=7,y1=7,line=list(color="gray",width=1,dash="dot"))
        )
      )
    fig
  })
  
  output$profile_stats_ui <- renderUI({
    sub <- profile_data()
    req(nrow(sub) > 0)
    col <- unname(KM_COLORS[as.character(input$profile_km)])
    if (is.na(col)) col <- COLOR_ACCENT
    tot  <- round(mean(sub$ssc, na.rm=T), 1)
    m4   <- round(mean(sub$ssc[sub$depth<=4], na.rm=T), 1)
    m7   <- round(mean(sub$ssc[sub$depth<=7], na.rm=T), 1)
    n    <- nrow(sub)
    make_stat <- function(label, val, color) {
      div(style=paste0("background:",COLOR_CARD,";border-radius:10px;padding:12px 20px;",
                       "box-shadow:0 1px 4px rgba(0,0,0,0.07);border:1px solid ",COLOR_BORDER,";"),
          div(label, style=paste0("font-size:11px;color:",COLOR_MUTED,
                                  ";text-transform:uppercase;letter-spacing:0.05em;")),
          div(paste0(val," mg/L"), style=paste0("font-size:18px;font-weight:700;color:",color,
                                                ";font-family:'Merriweather',serif;"))
      )
    }
    div(style="display:flex;gap:12px;flex-wrap:wrap;margin-bottom:12px;",
        make_stat("Promedio total", tot, COLOR_ACCENT),
        make_stat("Promedio 0–4 m", m4,  col),
        make_stat("Promedio 0–7 m", m7,  col),
        div(style=paste0("background:",COLOR_CARD,";border-radius:10px;padding:12px 20px;",
                         "box-shadow:0 1px 4px rgba(0,0,0,0.07);border:1px solid ",COLOR_BORDER,";"),
            div("N mediciones", style=paste0("font-size:11px;color:",COLOR_MUTED,
                                             ";text-transform:uppercase;letter-spacing:0.05em;")),
            div(as.character(n), style=paste0("font-size:18px;font-weight:700;color:",COLOR_MUTED,
                                              ";font-family:'Merriweather',serif;"))
        )
    )
  })
  
  # ═══════════════════════════════════════
  # EDA — Distribución
  # ═══════════════════════════════════════
  output$dist_plot <- renderPlotly({
    d <- dff(); var <- input$dist_variable; req(var %in% names(d))
    kms <- sort(unique(d$km))
    fig <- plot_ly()
    for (k in kms) {
      sub <- d %>% filter(km==k)
      fig <- fig %>%
        add_histogram(x=sub[[var]],name=paste0("Km ",k),marker=list(color=COLOR_ACCENT,opacity=0.75),
                      xaxis="x",yaxis="y",legendgroup=paste0("km",k)) %>%
        add_boxplot(y=sub[[var]],name=paste0("Km ",k),marker=list(color=COLOR_ACCENT),
                    line=list(color=COLOR_ACCENT),boxmean=TRUE,showlegend=FALSE,
                    xaxis="x2",yaxis="y2",legendgroup=paste0("km",k))
    }
    fig %>% layout(barmode="overlay",
                   grid=list(rows=1,columns=2,pattern="independent"),
                   xaxis=list(domain=c(0,0.48),showgrid=FALSE),
                   yaxis=list(gridcolor=COLOR_BORDER),
                   xaxis2=list(domain=c(0.52,1),showgrid=FALSE),
                   yaxis2=list(gridcolor=COLOR_BORDER),
                   paper_bgcolor=COLOR_CARD,plot_bgcolor=COLOR_BG,
                   font=list(family="'Lato',sans-serif",size=12,color=COLOR_TEXT),
                   legend=list(orientation="h",y=-0.15),
                   margin=list(l=40,r=20,t=40,b=60),
                   annotations=list(
                     list(text="Histograma por Km",x=0.24,y=1.05,xref="paper",yref="paper",showarrow=FALSE),
                     list(text="Boxplot por Km",x=0.76,y=1.05,xref="paper",yref="paper",showarrow=FALSE)
                   ))
  })
  
  # ═══════════════════════════════════════
  # EDA — Series de tiempo
  # ═══════════════════════════════════════
  output$ts_plot <- renderPlotly({
    d <- dff(); var <- input$ts_variable; req(var %in% names(d))
    fig <- plot_ly()
    for (k in sort(unique(d$km))) {
      sub <- d %>% filter(km==k) %>% arrange(reflectance_date); col <- km_color(k)
      fig <- fig %>% add_trace(x=sub$reflectance_date,y=sub[[var]],type="scatter",mode="lines+markers",
                               name=paste0("Km ",k),line=list(color=col,width=2),marker=list(size=7,color=col))
    }
    fig %>% layout(xaxis=list(title="Fecha",showgrid=FALSE),
                   yaxis=list(title=var,gridcolor=COLOR_BORDER),
                   paper_bgcolor=COLOR_CARD,plot_bgcolor=COLOR_BG,
                   font=list(family="'Lato',sans-serif",size=12,color=COLOR_TEXT),
                   legend=list(orientation="h",y=-0.2),margin=list(l=50,r=20,t=20,b=60))
  })
  
  # ═══════════════════════════════════════
  # EDA — Scatter (lineal y potencial)
  # ═══════════════════════════════════════
  scatter_results <- reactive({
    d <- dff(); xvar <- input$scatter_x; yvar <- input$scatter_y
    req(xvar %in% names(d), yvar %in% names(d))
    x <- d[[xvar]]; y_raw <- d[[yvar]]
    y <- if (input$scatter_transform=="log") log(y_raw) else y_raw
    
    if (input$scatter_ajuste=="lineal") {
      cc  <- cor.test(x,y); r2 <- cc$estimate^2; p <- cc$p.value
      fit <- lm(y~x); xl <- seq(min(x,na.rm=T),max(x,na.rm=T),length.out=200)
      yl  <- coef(fit)[1]+coef(fit)[2]*xl
      eq  <- paste0("y = ",round(coef(fit)[2],4),"x + ",round(coef(fit)[1],4))
      list(x=x,y=y,xl=xl,yl=yl,r2=r2,p=p,eq=eq)
    } else {
      ok  <- x>0 & y_raw>0 & !is.na(x) & !is.na(y_raw)
      xf  <- x[ok]; yf <- y_raw[ok]
      lx  <- log(xf); ly <- log(yf)
      cc  <- cor.test(lx,ly); r2 <- cc$estimate^2; p <- cc$p.value
      fit <- lm(ly~lx); b_exp <- coef(fit)[2]; a <- exp(coef(fit)[1])
      xl  <- seq(min(xf),max(xf),length.out=200)
      yl  <- a*(xl^b_exp)
      if (input$scatter_transform=="log") yl <- log(yl)
      eq  <- paste0("y = ",round(a,4),"x^",round(b_exp,4))
      list(x=x[ok],y=if(input$scatter_transform=="log") log(y_raw[ok]) else y_raw[ok],
           xl=xl,yl=yl,r2=r2,p=p,eq=eq)
    }
  })
  
  output$scatter_plot <- renderPlotly({
    res  <- scatter_results(); d <- dff()
    xvar <- input$scatter_x;   yvar <- input$scatter_y
    y_label <- if(input$scatter_transform=="log") paste0("ln(",yvar,")") else paste0(yvar," (mg/L)")
    fig <- plot_ly()
    if (input$scatter_color=="km") {
      for (k in sort(unique(d$km))) {
        sub <- d %>% filter(km==k)
        y_sub <- if(input$scatter_transform=="log") log(sub[[yvar]]) else sub[[yvar]]
        col   <- km_color(k)
        fig   <- fig %>% add_markers(x=sub[[xvar]],y=y_sub,name=paste0("Km ",k),
                                     marker=list(size=9,color=col,line=list(width=1,color="white")))
      }
    } else if (input$scatter_color=="CSS") {
      fig <- fig %>% add_markers(x=d[[xvar]],y=res$y,name="Datos",
                                 marker=list(size=9,color=d[[yvar]],colorscale="Inferno",
                                             showscale=TRUE,colorbar=list(title="CSS"),
                                             line=list(width=1,color="white")))
    } else {
      fig <- fig %>% add_markers(x=res$x,y=res$y,name="Datos",
                                 marker=list(size=9,color=COLOR_ACCENT,line=list(width=1,color="white")))
    }
    fig %>%
      add_lines(x=res$xl,y=res$yl,name="Regresión",
                line=list(color="#c0392b",width=2,dash="dash")) %>%
      layout(xaxis=list(title=xvar,showgrid=FALSE),
             yaxis=list(title=y_label,gridcolor=COLOR_BORDER),
             paper_bgcolor=COLOR_CARD,plot_bgcolor=COLOR_BG,
             font=list(family="'Lato',sans-serif",size=12,color=COLOR_TEXT),
             legend=list(orientation="h",y=-0.2),margin=list(l=50,r=20,t=20,b=60))
  })
  
  output$scatter_stats_ui <- renderUI({
    res <- scatter_results(); d <- dff()
    p_text <- if(res$p<0.0001) "< 0.0001" else round(res$p,4)
    mk <- function(txt,col=COLOR_TEXT,bold=FALSE)
      tags$span(txt,style=paste0("font-family:monospace;font-size:13px;color:",col,";",
                                 if(bold)"font-weight:700;" else "",
                                 "background:",COLOR_ACCENT,"18;padding:4px 10px;border-radius:4px;"))
    div(style="display:flex;gap:24px;flex-wrap:wrap;margin-top:8px;",
        mk(res$eq), mk(paste0("R² = ",round(res$r2,3)),COLOR_ACCENT,TRUE),
        mk(paste0("p = ",p_text)),
        tags$span(paste0("n = ",nrow(d)),
                  style=paste0("font-family:monospace;font-size:13px;color:",COLOR_MUTED,
                               ";background:",COLOR_BORDER,";padding:4px 10px;border-radius:4px;")))
  })
  
  # ═══════════════════════════════════════
  # EDA — Ranking correlaciones
  # ═══════════════════════════════════════
  output$corrbar_plot <- renderPlotly({
    d <- dff(); req("SSC" %in% names(d))
    cols   <- intersect(c(BANDAS,INDICES),names(d))
    y_css  <- if(input$corrbar_transform=="log") log(d$SSC) else d$SSC
    y_label<- if(input$corrbar_transform=="log") "ln(CSS)" else "CSS"
    res <- do.call(rbind, lapply(cols, function(col) {
      x <- d[[col]]; ok <- !is.na(x)&!is.na(y_css)
      if(sum(ok)<3) return(NULL)
      cc <- cor.test(x[ok],y_css[ok])
      data.frame(variable=col,r=cc$estimate,r_abs=abs(cc$estimate),p=cc$p.value)
    }))
    req(!is.null(res), nrow(res)>0)
    res    <- res[order(res$r_abs),]
    colors <- ifelse(res$r>=0,"#2eaa6b","#c0392b")
    p_lbl  <- ifelse(res$p<0.001,"<0.001",round(res$p,3))
    plot_ly(res,x=~r,y=~variable,type="bar",orientation="h",
            marker=list(color=colors),
            text=paste0("r=",round(res$r,3),"  p=",p_lbl),
            textposition="outside",
            hovertemplate="%{y}<br>r = %{x:.3f}<extra></extra>") %>%
      layout(xaxis=list(title=paste0("Correlación de Pearson con ",y_label),
                        range=c(-1.2,1.2),showgrid=TRUE,gridcolor=COLOR_BORDER,zeroline=FALSE),
             yaxis=list(showgrid=FALSE),
             shapes=list(
               list(type="line",x0=0,x1=0,y0=0,y1=1,yref="paper",
                    line=list(color=COLOR_TEXT,width=1,dash="dash")),
               list(type="line",x0=0.7,x1=0.7,y0=0,y1=1,yref="paper",
                    line=list(color="#2eaa6b",width=1,dash="dot")),
               list(type="line",x0=-0.7,x1=-0.7,y0=0,y1=1,yref="paper",
                    line=list(color="#c0392b",width=1,dash="dot"))
             ),
             paper_bgcolor=COLOR_CARD,plot_bgcolor=COLOR_BG,
             font=list(family="'Lato',sans-serif",size=12,color=COLOR_TEXT),
             margin=list(l=80,r=140,t=30,b=50),showlegend=FALSE)
  })
  
  # ═══════════════════════════════════════
  # EDA — Firmas espectrales
  # ═══════════════════════════════════════
  observe({
    d <- dff(); kms <- sort(unique(d$km))
    updateSelectInput(session,"spec_km",
                      choices=c("Todas"="all",setNames(kms,paste0("Km ",kms))), selected="all")
  })
  
  output$spec_plot <- renderPlotly({
    d   <- dff()
    sub <- if(input$spec_km=="all") d else d %>% filter(km==as.integer(input$spec_km))
    sub <- sub %>% arrange(SSC) %>% as.data.frame()
    req(nrow(sub)>0, all(WL_NAMES %in% names(sub)))
    ssc_min <- min(sub$SSC,na.rm=T); ssc_max <- max(sub$SSC,na.rm=T)
    ssc_col <- function(ssc) {
      t <- (ssc-ssc_min)/(ssc_max-ssc_min+1e-9)
      sprintf("rgb(255,%d,0)",as.integer(165*(1-t)))
    }
    wl_vis  <- WL_REAL[1:9]; band_vis <- WL_NAMES[1:9]
    s1_fict <- c(0,150); s2_fict <- c(170,320)
    all_refl <- unlist(lapply(WL_NAMES, function(b) sub[[b]]))
    y_min <- max(0,min(all_refl,na.rm=T)*0.90); y_max <- max(all_refl,na.rm=T)*1.08
    fig <- plot_ly() %>% layout(
      xaxis=list(domain=c(0,0.72),title="Longitud de onda (nm)",range=c(400,950),showgrid=FALSE),
      xaxis2=list(domain=c(0.76,1),showgrid=FALSE,range=c(0,320),
                  tickvals=c(64,114,232,282),ticktext=c("1614","1650","2202","2250")),
      yaxis=list(title="Reflectancia (sr⁻¹)",gridcolor=COLOR_BORDER,range=c(y_min,y_max)),
      yaxis2=list(anchor="x2",gridcolor=COLOR_BORDER,showgrid=TRUE,range=c(y_min,y_max)),
      paper_bgcolor=COLOR_CARD,plot_bgcolor=COLOR_BG,
      font=list(family="'Lato',sans-serif",size=12,color=COLOR_TEXT),
      margin=list(l=60,r=100,t=40,b=50),hovermode="closest",
      shapes=c(
        list(list(type="rect",x0=458,x1=523,y0=0,y1=1,yref="paper",fillcolor="rgba(32,32,229,0.12)",line=list(width=0),xref="x")),
        list(list(type="rect",x0=543,x1=578,y0=0,y1=1,yref="paper",fillcolor="rgba(0,200,0,0.12)",line=list(width=0),xref="x")),
        list(list(type="rect",x0=650,x1=680,y0=0,y1=1,yref="paper",fillcolor="rgba(228,0,0,0.12)",line=list(width=0),xref="x")),
        list(list(type="rect",x0=785,x1=900,y0=0,y1=1,yref="paper",fillcolor="rgba(230,192,4,0.12)",line=list(width=0),xref="x")),
        list(list(type="rect",x0=s1_fict[1],x1=s1_fict[2],y0=0,y1=1,yref="paper",fillcolor="rgba(139,69,19,0.12)",line=list(width=0),xref="x2")),
        list(list(type="rect",x0=s2_fict[1],x1=s2_fict[2],y0=0,y1=1,yref="paper",fillcolor="rgba(139,69,19,0.12)",line=list(width=0),xref="x2")),
        list(list(type="line",x0=160,x1=160,y0=0,y1=1,yref="paper",line=list(color="gray",width=1,dash="dash"),xref="x2"))
      )
    )
    for (i in seq_len(nrow(sub))) {
      row   <- sub[i,]
      col   <- ssc_col(row$SSC)
      dstr  <- format(as.Date(row$reflectance_date),"%Y-%m-%d")
      hover <- paste0("SSC: ",round(row$SSC,1)," mg/L<br>Fecha: ",dstr,"<br>Km: ",row$km)
      rv    <- as.numeric(row[band_vis])
      fig   <- fig %>% add_trace(x=wl_vis,y=rv,xaxis="x",yaxis="y",type="scatter",mode="lines+markers",
                                 line=list(color=col,width=2),
                                 marker=list(size=7,color=col,line=list(width=0.5,color="white")),
                                 hovertemplate=paste0(hover,"<extra></extra>"),showlegend=FALSE)
      for (si in 10:11) {
        wf <- to_fict(WL_REAL[si])
        if (!is.na(wf))
          fig <- fig %>% add_trace(x=wf,y=as.numeric(row[WL_NAMES[si]]),xaxis="x2",yaxis="y2",
                                   type="scatter",mode="markers",
                                   marker=list(size=9,color=col,line=list(width=0.5,color="white")),
                                   hovertemplate=paste0(WL_NAMES[si]," (",WL_REAL[si]," nm)<br>",hover,"<extra></extra>"),
                                   showlegend=FALSE)
      }
    }
    fig
  })
  
  # ═══════════════════════════════════════
  # EDA — Hidrología
  # ═══════════════════════════════════════
  hf <- reactive({
    y0 <- input$hydro_years[1]; y1 <- input$hydro_years[2]
    d0 <- as.POSIXct(paste0(y0,"-01-01")); d1 <- as.POSIXct(paste0(y1,"-12-31"))
    list(
      Q   = Q_cal   %>% filter(Fecha>=d0,Fecha<=d1),
      TSS = TSS_cal %>% filter(Fecha>=d0,Fecha<=d1),
      Qbq = Q_baq   %>% filter(Fecha>=d0,Fecha<=d1),
      mer = df_hydro%>% filter(Fecha>=d0,Fecha<=d1)
    )
  })
  
  output$hydro_ts_plot <- renderPlotly({
    h <- hf(); mer <- h$mer %>% drop_na(Q_calamar,TSS_calamar)
    fig <- plot_ly()
    if(nrow(h$Q)>0)
      fig <- fig %>% add_lines(data=h$Q,x=~Fecha,y=~Q_calamar,name="Q Calamar",
                               line=list(color=COLOR_ACCENT,width=1.5),yaxis="y")
    if(nrow(h$Qbq)>0) {
      fig <- fig %>%
        add_trace(data=h$Qbq,x=~Fecha,y=~Q_barranquilla,type="scatter",mode="markers+lines",
                  name="Q Barranquilla",line=list(color="#e07b2a",width=2),marker=list(size=7),yaxis="y")
      if(nrow(mer)>0)
        fig <- fig %>% add_lines(data=mer,x=~Fecha,y=~Q_sinincora,name="Q Cal - Q Incora",
                                 line=list(color="red",width=2.5),yaxis="y")
    }
    if(nrow(h$TSS)>0)
      fig <- fig %>% add_lines(data=h$TSS,x=~Fecha,y=~TSS_calamar,name="TSS Calamar",
                               line=list(color="#c0392b",width=1.5),yaxis="y2")
    if(nrow(mer)>0)
      fig <- fig %>% add_lines(data=mer,x=~Fecha,y=~ssc_derived,name="SSC Derivado",
                               line=list(color="#4bb929",width=1.5),yaxis="y3")
    fig %>% layout(
      yaxis =list(title="Caudal (m³/s)",domain=c(0.67,1),showgrid=TRUE,gridcolor=COLOR_BORDER),
      yaxis2=list(title="TSS (Kt/día)",domain=c(0.34,0.65),showgrid=TRUE,gridcolor=COLOR_BORDER),
      yaxis3=list(title="SSC (mg/L)",domain=c(0,0.32),showgrid=TRUE,gridcolor=COLOR_BORDER),
      xaxis=list(showgrid=FALSE,anchor="y3"),
      paper_bgcolor=COLOR_CARD,plot_bgcolor=COLOR_BG,
      font=list(family="'Lato',sans-serif",size=12,color=COLOR_TEXT),
      legend=list(orientation="h",y=-0.08),
      margin=list(l=60,r=20,t=40,b=50)
    )
  })
  
  output$hydro_ts_stats <- renderUI({
    h <- hf(); mer <- h$mer %>% drop_na(Q_calamar,TSS_calamar)
    items <- list(
      list("Q Calamar",    if(nrow(h$Q)>0)  h$Q$Q_calamar     else numeric(0),"m³/s"),
      list("TSS Calamar",  if(nrow(h$TSS)>0) h$TSS$TSS_calamar else numeric(0),"Kt/día"),
      list("Q Barranquilla",if(nrow(h$Qbq)>0) h$Qbq$Q_barranquilla else numeric(0),"m³/s"),
      list("SSC Derivado", if(nrow(mer)>0)  mer$ssc_derived   else numeric(0),"mg/L")
    )
    div(style="margin-top:16px;",
        lapply(items, function(x) {
          if(length(x[[2]])==0 || all(is.na(x[[2]]))) return(NULL)
          div(style=paste0("background:",COLOR_CARD,";border-radius:10px;padding:12px 20px;",
                           "box-shadow:0 1px 4px rgba(0,0,0,0.07);border:1px solid ",COLOR_BORDER,";margin-bottom:8px;"),
              div(x[[1]],style=paste0("font-size:11px;color:",COLOR_MUTED,";text-transform:uppercase;")),
              div(style="display:flex;gap:16px;margin-top:4px;",
                  tags$span(paste0("Media: ",round(mean(x[[2]],na.rm=T),1)," ",x[[3]]),style=paste0("font-size:13px;color:",COLOR_TEXT,";")),
                  tags$span(paste0("Mín: ",round(min(x[[2]],na.rm=T),1)),style=paste0("font-size:13px;color:",COLOR_MUTED,";")),
                  tags$span(paste0("Máx: ",round(max(x[[2]],na.rm=T),1)),style=paste0("font-size:13px;color:",COLOR_MUTED,";"))
              )
          )
        })
    )
  })
  
  output$hydro_seas_plot <- renderPlotly({
    h <- hf()
    req(nrow(h$Q)>0 || nrow(h$TSS)>0)
    Qm  <- h$Q   %>% mutate(mes=month(Fecha))
    Tm  <- h$TSS %>% mutate(mes=month(Fecha))
    fig <- plot_ly()
    for (m in 1:12) {
      qv <- Qm$Q_calamar[Qm$mes==m]; tv <- Tm$TSS_calamar[Tm$mes==m]
      if(length(qv)>0) fig <- fig %>% add_boxplot(y=qv,x=MESES[m],showlegend=FALSE,
                                                  marker=list(color=COLOR_ACCENT,opacity=0.6),line=list(color=COLOR_ACCENT),xaxis="x",yaxis="y")
      if(length(tv)>0) fig <- fig %>% add_boxplot(y=tv,x=MESES[m],showlegend=FALSE,
                                                  marker=list(color="#c0392b",opacity=0.6),line=list(color="#c0392b"),xaxis="x2",yaxis="y2")
    }
    fig %>% layout(
      grid=list(rows=1,columns=2,pattern="independent"),
      xaxis=list(domain=c(0,0.47),categoryorder="array",categoryarray=MESES,showgrid=FALSE),
      yaxis=list(title="Q Calamar (m³/s)",gridcolor=COLOR_BORDER),
      xaxis2=list(domain=c(0.53,1),categoryorder="array",categoryarray=MESES,showgrid=FALSE),
      yaxis2=list(title="TSS Calamar (Kt/día)",gridcolor=COLOR_BORDER),
      paper_bgcolor=COLOR_CARD,plot_bgcolor=COLOR_BG,
      font=list(family="'Lato',sans-serif",size=12,color=COLOR_TEXT),
      margin=list(l=60,r=20,t=40,b=60)
    )
  })
  
  output$hydro_qtss_plot <- renderPlotly({
    mg <- hf()$mer %>% drop_na(Q_calamar,TSS_calamar); req(nrow(mg)>0)
    x <- mg$Q_calamar; y <- mg$TSS_calamar
    cc <- cor.test(x,y); r2 <- cc$estimate^2; p <- cc$p.value
    fit<- lm(y~x); xl <- seq(min(x),max(x),length.out=300)
    plot_ly() %>%
      add_markers(x=x,y=y,marker=list(size=5,color=COLOR_ACCENT,opacity=0.5),
                  hovertemplate="Q: %{x:.0f}<br>TSS: %{y:.1f}<extra></extra>") %>%
      add_lines(x=xl,y=coef(fit)[1]+coef(fit)[2]*xl,name="Regresión",
                line=list(color="#c0392b",width=2,dash="dash")) %>%
      layout(xaxis=list(title="Q Calamar (m³/s)",showgrid=FALSE),
             yaxis=list(title="TSS Calamar (Kt/día)",gridcolor=COLOR_BORDER),
             paper_bgcolor=COLOR_CARD,plot_bgcolor=COLOR_BG,
             font=list(family="'Lato',sans-serif",size=12,color=COLOR_TEXT),
             margin=list(l=60,r=20,t=20,b=50),
             annotations=list(list(text=paste0("R²=",round(r2,3),"  n=",nrow(mg)),
                                   x=0.95,y=0.05,xref="paper",yref="paper",showarrow=FALSE,
                                   font=list(size=12,color=COLOR_ACCENT))))
  })
  
  output$hydro_qtss_stats <- renderUI({
    mg <- hf()$mer %>% drop_na(Q_calamar,TSS_calamar); req(nrow(mg)>0)
    cc <- cor.test(mg$Q_calamar,mg$TSS_calamar)
    r2 <- cc$estimate^2; p <- cc$p.value
    fit<- lm(TSS_calamar~Q_calamar,data=mg)
    p_text <- if(p<0.0001)"< 0.0001" else round(p,4)
    mk <- function(txt,col=COLOR_TEXT,bold=FALSE)
      tags$span(txt,style=paste0("font-family:monospace;font-size:13px;color:",col,";",
                                 if(bold)"font-weight:700;",
                                 "background:",COLOR_ACCENT,"18;padding:4px 10px;border-radius:4px;"))
    div(style="display:flex;gap:16px;flex-wrap:wrap;margin-top:8px;",
        mk(paste0("y = ",round(coef(fit)[2],4),"x + ",round(coef(fit)[1],2))),
        mk(paste0("R² = ",round(r2,3)),COLOR_ACCENT,TRUE),
        mk(paste0("p = ",p_text)),
        tags$span(paste0("n = ",nrow(mg)),style=paste0("font-family:monospace;font-size:13px;color:",COLOR_MUTED,
                                                       ";background:",COLOR_BORDER,";padding:4px 10px;border-radius:4px;")))
  })
  
  output$hydro_qq_plot <- renderPlotly({
    h <- hf(); Qf <- h$Q; QGf <- h$Qbq
    mg <- inner_join(Qf,QGf,by="Fecha") %>% drop_na()
    fig <- plot_ly()
    if(nrow(Qf)>0)  fig <- fig %>% add_lines(data=Qf,x=~Fecha,y=~Q_calamar,name="Q Calamar",
                                             line=list(color=COLOR_ACCENT,width=1.5),xaxis="x",yaxis="y")
    if(nrow(QGf)>0) fig <- fig %>% add_trace(data=QGf,x=~Fecha,y=~Q_barranquilla,type="scatter",
                                             mode="markers+lines",name="Q Barranquilla",
                                             line=list(color="#e07b2a",width=2),marker=list(size=7),xaxis="x",yaxis="y")
    if(nrow(mg)>0) {
      cc <- cor.test(mg$Q_calamar,mg$Q_barranquilla)
      fit<- lm(Q_barranquilla~Q_calamar,data=mg)
      xl <- seq(min(mg$Q_calamar),max(mg$Q_calamar),length.out=200)
      fig <- fig %>%
        add_markers(x=mg$Q_calamar,y=mg$Q_barranquilla,name="Coincidentes",
                    marker=list(size=8,color=COLOR_ACCENT,line=list(width=1,color="white")),
                    xaxis="x2",yaxis="y2") %>%
        add_lines(x=xl,y=coef(fit)[1]+coef(fit)[2]*xl,showlegend=FALSE,
                  line=list(color="#c0392b",width=2,dash="dash"),xaxis="x2",yaxis="y2")
    }
    fig %>% layout(
      grid=list(rows=1,columns=2,pattern="independent"),
      xaxis=list(domain=c(0,0.47),showgrid=FALSE),
      yaxis=list(title="Caudal (m³/s)",gridcolor=COLOR_BORDER),
      xaxis2=list(domain=c(0.53,1),title="Q Calamar (m³/s)",showgrid=FALSE),
      yaxis2=list(title="Q Barranquilla (m³/s)",gridcolor=COLOR_BORDER),
      paper_bgcolor=COLOR_CARD,plot_bgcolor=COLOR_BG,
      font=list(family="'Lato',sans-serif",size=12,color=COLOR_TEXT),
      legend=list(orientation="h",y=-0.12),margin=list(l=60,r=20,t=40,b=70)
    )
  })
  
  output$hydro_qincora_plot <- renderPlotly({
    h <- hf(); mer <- h$mer %>% drop_na(Q_calamar,Q_sinincora)
    fig <- plot_ly()
    if(nrow(h$Q)>0)
      fig <- fig %>% add_lines(data=h$Q,x=~Fecha,y=~Q_calamar,name="Q Calamar",
                               line=list(color=COLOR_ACCENT,width=1.5))
    if(nrow(mer)>0)
      fig <- fig %>% add_lines(data=mer,x=~Fecha,y=~Q_sinincora,name="Q Cal − Q Incora",
                               line=list(color="red",width=2))
    fig %>% layout(
      xaxis=list(showgrid=FALSE,title="Fecha"),
      yaxis=list(title="Caudal (m³/s)",gridcolor=COLOR_BORDER),
      paper_bgcolor=COLOR_CARD,plot_bgcolor=COLOR_BG,
      font=list(family="'Lato',sans-serif",size=12,color=COLOR_TEXT),
      legend=list(orientation="h",y=-0.1),margin=list(l=60,r=20,t=20,b=60)
    )
  })
  
  output$hydro_tss_plot <- renderPlotly({
    h <- hf()
    fig <- plot_ly()
    if(nrow(h$TSS)>0)
      fig <- fig %>% add_lines(data=h$TSS,x=~Fecha,y=~TSS_calamar,name="TSS Calamar",
                               line=list(color=COLOR_ACCENT,width=1.5))
    if(nrow(df_tss_baq)>0)
      fig <- fig %>% add_markers(data=df_tss_baq,x=~Fecha,y=~TSS_barranquilla,name="TSS Km19 (est.)",
                                 marker=list(size=9,color="#e07b2a",line=list(width=1,color="white")))
    fig %>% layout(
      xaxis=list(showgrid=FALSE,title="Fecha"),
      yaxis=list(title="TSS (Kt/día)",gridcolor=COLOR_BORDER),
      paper_bgcolor=COLOR_CARD,plot_bgcolor=COLOR_BG,
      font=list(family="'Lato',sans-serif",size=12,color=COLOR_TEXT),
      legend=list(orientation="h",y=-0.1),margin=list(l=60,r=20,t=20,b=60)
    )
  })
  
  output$hydro_tss_note <- renderUI({
    div("⚠ El TSS de Barranquilla es una estimación puntual basada en CSS superficial del Km 19 y
      el caudal de Barranquilla. Solo coincide con fechas de campañas que tienen imagen Sentinel-2.",
        style=paste0("font-size:12px;color:",COLOR_MUTED,";font-style:italic;margin-top:8px;",
                     "padding:8px 16px;background-color:",COLOR_ACCENT,"08;border-radius:6px;"))
  })
  
  # ═══════════════════════════════════════
  # EDA — Correlación (heatmap)
  # ═══════════════════════════════════════
  output$corr_plot <- renderPlotly({
    d  <- dff(); cols <- intersect(c(BANDAS,INDICES,"SSC"),names(d))
    dc <- d[,cols]
    if(input$corr_transform=="log") dc$SSC <- log(dc$SSC)
    labels <- ifelse(cols=="SSC",ifelse(input$corr_transform=="log","ln(CSS)","CSS"),cols)
    cm <- cor(dc,use="pairwise.complete.obs")
    plot_ly(z=cm,x=labels,y=labels,type="heatmap",colorscale="RdBu",zmid=0,zmin=-1,zmax=1,
            text=round(cm,2),texttemplate="%{text}",textfont=list(size=10)) %>%
      layout(xaxis=list(tickangle=-45),paper_bgcolor=COLOR_CARD,plot_bgcolor=COLOR_CARD,
             font=list(family="'Lato',sans-serif",size=11,color=COLOR_TEXT),
             margin=list(l=80,r=20,t=20,b=80))
  })
  
  # ═══════════════════════════════════════
  # EDA — Mapa de calor
  # ═══════════════════════════════════════
  output$heatmap_plot <- renderPlotly({
    d   <- dff(); var <- input$heatmap_var
    req(var %in% names(d))
    d$fecha <- as.Date(d$reflectance_date)
    pivot <- d %>%
      group_by(km, fecha) %>%
      summarise(val=mean(.data[[var]],na.rm=T), .groups="drop") %>%
      complete(km, fecha) %>%
      pivot_wider(names_from=fecha, values_from=val) %>%
      arrange(desc(km))
    kms_ord <- pivot$km
    mat     <- as.matrix(pivot[,-1])
    dates   <- colnames(pivot)[-1]
    plot_ly(z=mat, x=dates, y=paste0("Km ",kms_ord),
            type="heatmap", colorscale="YlOrRd",
            hovertemplate=paste0("Fecha: %{x}<br>%{y}<br>",var,": %{z:.1f}<extra></extra>"),
            xgap=2, ygap=2) %>%
      layout(xaxis=list(title="Fecha de imagen",tickangle=-45,showgrid=FALSE),
             yaxis=list(showgrid=FALSE),
             paper_bgcolor=COLOR_CARD, plot_bgcolor=COLOR_CARD,
             font=list(family="'Lato',sans-serif",size=12,color=COLOR_TEXT),
             margin=list(l=80,r=60,t=20,b=80))
  })
  
  # ═══════════════════════════════════════
  # EDA — Climatograma
  # ═══════════════════════════════════════
  output$climo_plot <- renderPlotly({
    d <- dff(); req("SSC" %in% names(d))
    d$mes <- month(d$reflectance_date)
    fig   <- plot_ly()
    for (m in 1:12) {
      sub <- d %>% filter(mes==m); if(nrow(sub)==0) next
      fig <- fig %>% add_boxplot(y=sub$SSC,x=rep(MESES[m],nrow(sub)),showlegend=FALSE,
                                 marker=list(color=COLOR_ACCENT,opacity=0.4,size=5),line=list(color=COLOR_ACCENT),
                                 boxmean=TRUE,hoverinfo="skip")
    }
    for (k in sort(unique(d$km))) {
      sub <- d %>% filter(km==k); col <- km_color(k)
      fig <- fig %>% add_markers(x=MESES[sub$mes],y=sub$SSC,name=paste0("Km ",k),
                                 marker=list(size=8,color=col,line=list(width=1,color="white"),opacity=0.85),
                                 hovertemplate=paste0("Km ",k,"<br>Mes: %{x}<br>CSS: %{y:.1f} mg/L<extra></extra>"))
    }
    means <- d %>% group_by(mes) %>% summarise(m=mean(SSC,na.rm=T),.groups="drop")
    fig <- fig %>% add_trace(x=MESES[means$mes],y=means$m,type="scatter",mode="lines+markers",
                             name="Media mensual",line=list(color=COLOR_TEXT,width=2,dash="dash"),
                             marker=list(size=7,color=COLOR_TEXT))
    fig %>% layout(
      xaxis=list(title="Mes",categoryorder="array",categoryarray=MESES,showgrid=FALSE),
      yaxis=list(title="CSS (mg/L)",gridcolor=COLOR_BORDER),
      paper_bgcolor=COLOR_CARD,plot_bgcolor=COLOR_BG,
      font=list(family="'Lato',sans-serif",size=12,color=COLOR_TEXT),
      legend=list(orientation="h",y=-0.2),margin=list(l=60,r=20,t=20,b=80),
      boxmode="overlay"
    )
  })
  
  # ═══════════════════════════════════════
  # HELPERS
  # ═══════════════════════════════════════
  `%||%` <- function(a,b) if (!is.null(a) && length(a)>0) a else b
  
  # Obtener key segura del modelo (siempre devuelve una key válida)
  sel_mdl <- reactive({
    v <- input$model_selector %||% "lineal"
    if (!v %in% names(MODEL_LABELS)) "lineal" else v
  })
  
  # Obtener resultado del modelo seleccionado desde pkl
  model_res <- reactive({
    req(PKL_OK)
    mdl <- sel_mdl()
    tryCatch({
      # reticulate puede devolver dict Python; convertir a lista R si hace falta
      res_list <- PKL_RESULTS[["results"]]
      if (is.null(res_list)) return(NULL)
      r <- res_list[[mdl]]
      if (is.null(r)) {
        # Intentar con py_to_r por si es objeto Python puro
        r <- tryCatch(reticulate::py_to_r(res_list)[[mdl]], error=function(e) NULL)
      }
      r
    }, error=function(e) NULL)
  })
  
  # Helper: extraer vector numérico de lista Python/R
  get_vec <- function(x) tryCatch(as.numeric(unlist(x)), error=function(e) numeric(0))
  
  # Color del modelo seleccionado
  sel_col <- reactive({
    v <- MODEL_COLS[sel_mdl()]
    if (is.na(v) || length(v)==0) COLOR_ACCENT else unname(v)
  })
  
  # ═══════════════════════════════════════
  # MODELO
  # ═══════════════════════════════════════
  output$model_header_chips <- renderUI({
    # Usa df_calib si existe, si no usa df (dataset principal)
    d_ref <- if (is.data.frame(df_calib) && nrow(df_calib)>0) df_calib else df
    n   <- nrow(d_ref)
    rng <- paste0(floor(min(d_ref$SSC,na.rm=TRUE)),"–",
                  ceiling(max(d_ref$SSC,na.rm=TRUE))," mg/L")
    div(style="display:flex;gap:12px;flex-wrap:wrap;",
        metric_chip_ui("Dataset (n)", as.character(n), COLOR_ACCENT),
        metric_chip_ui("Variable predictora", "NIR (sr\u207b\u00b9)", ACCENT2),
        metric_chip_ui("Rango SSC", rng, ACCENT3))
  })
  
  output$model_eq_box <- renderUI({
    if (!PKL_OK) return(div("Sin modelos — ejecuta train_models.py primero",
                            style=paste0("color:",COLOR_MUTED,";font-size:13px;")))
    res <- model_res()
    if (is.null(res)) return(div("Modelo no disponible",style=paste0("color:",COLOR_MUTED,";")))
    eq  <- tryCatch(as.character(res$equation), error=function(e) "–")
    div(class="formula-card",
        tags$span("f(x)  =  ", style=paste0("color:",COLOR_MUTED,";font-size:13px;")),
        tags$span(eq, style=paste0("color:",COLOR_ACCENT,";font-family:monospace;font-size:14px;")))
  })
  
  output$model_metrics_row <- renderUI({
    if (!PKL_OK) return(NULL)
    res <- model_res()
    if (is.null(res)) return(NULL)
    col <- sel_col()
    safe <- function(x, fmt="%.3f") tryCatch(sprintf(fmt, as.numeric(x)), error=function(e) "–")
    n_tr <- tryCatch(as.character(PKL_RESULTS$n_train), error=function(e) "–")
    div(style="display:flex;gap:12px;flex-wrap:wrap;margin-bottom:24px;",
        metric_chip_ui("R² Cal",   safe(res$cal_r2),         col),
        metric_chip_ui("RMSE Cal", safe(res$cal_rmse,"%.1f"), col, "mg/L"),
        metric_chip_ui("MAPE Cal", safe(res$cal_mape,"%.1f"), col, "%"),
        metric_chip_ui("Bias",     safe(res$cal_bias,"%+.1f"),col, "mg/L"),
        metric_chip_ui("n",        n_tr, COLOR_MUTED))
  })
  
  output$model_1to1 <- renderPlotly({
    empty <- plot_ly() %>% layout(paper_bgcolor=COLOR_CARD,plot_bgcolor=COLOR_BG,
                                  annotations=list(list(text=if(!PKL_OK)"Ejecuta train_models.py" else "Selecciona un modelo",
                                                        x=0.5,y=0.5,xref="paper",yref="paper",showarrow=FALSE,
                                                        font=list(color=COLOR_MUTED,size=13))))
    if (!PKL_OK) return(empty)
    res <- model_res(); if(is.null(res)) return(empty)
    
    y_true <- get_vec(PKL_RESULTS$y_true)
    yhat   <- get_vec(res$yhat_cal)
    kms_v  <- get_vec(PKL_RESULTS$kms)
    if(length(y_true)==0 || length(yhat)==0) return(empty)
    
    lims <- c(min(y_true,yhat)*0.95, max(y_true,yhat)*1.05)
    fig <- plot_ly() %>%
      add_lines(x=lims,y=lims,showlegend=TRUE,name="1:1",
                line=list(color=COLOR_MUTED,dash="dash",width=1.5))
    for (km in sort(unique(kms_v))) {
      mask <- kms_v==km; col <- km_color(km)
      fig <- fig %>% add_markers(
        x=y_true[mask], y=yhat[mask], name=paste0("Km ",as.integer(km)),
        marker=list(size=10,color=col,line=list(width=1.5,color="#f4f7fb"),opacity=0.9),
        hovertemplate="SSC_med: %{x:.1f}<br>SSC_mod: %{y:.1f}<extra></extra>")
    }
    fig %>% layout(
      xaxis=list(title="SSC medido (mg/L)",showgrid=TRUE,gridcolor=COLOR_BORDER),
      yaxis=list(title="SSC modelado (mg/L)",gridcolor=COLOR_BORDER),
      paper_bgcolor=COLOR_CARD, plot_bgcolor=COLOR_BG,
      font=list(family="'Lato',sans-serif",size=12,color=COLOR_TEXT),
      legend=list(orientation="h",y=-0.22), margin=list(l=56,r=20,t=30,b=70))
  })
  
  output$model_residuals <- renderPlotly({
    empty <- plot_ly() %>% layout(paper_bgcolor=COLOR_CARD,plot_bgcolor=COLOR_BG)
    if (!PKL_OK) return(empty)
    res <- model_res(); if(is.null(res)) return(empty)
    
    y_true  <- get_vec(PKL_RESULTS$y_true)
    yhat    <- get_vec(res$yhat_cal)
    kms_v   <- get_vec(PKL_RESULTS$kms)
    if(length(y_true)==0) return(empty)
    residuals <- y_true - yhat
    
    fig <- plot_ly()
    for (km in sort(unique(kms_v))) {
      mask <- kms_v==km; col <- km_color(km); idx <- which(mask)-1
      fig <- fig %>% add_bars(x=idx, y=residuals[mask],
                              name=paste0("Km ",as.integer(km)),
                              marker=list(color=col,opacity=0.85))
    }
    fig %>% add_lines(x=c(0,length(residuals)-1),y=c(0,0),showlegend=FALSE,
                      line=list(color=COLOR_MUTED,dash="dash",width=1.5)) %>%
      layout(barmode="overlay",
             xaxis=list(title="Observación",showgrid=FALSE),
             yaxis=list(title="Residuo SSC_med − SSC_mod (mg/L)",gridcolor=COLOR_BORDER),
             paper_bgcolor=COLOR_CARD,plot_bgcolor=COLOR_BG,
             font=list(family="'Lato',sans-serif",size=12,color=COLOR_TEXT),
             legend=list(orientation="h",y=-0.22),margin=list(l=56,r=20,t=30,b=70))
  })
  
  output$model_ratio_km <- renderPlotly({
    empty <- plot_ly() %>% layout(paper_bgcolor=COLOR_CARD,plot_bgcolor=COLOR_BG)
    if (!PKL_OK) return(empty)
    res <- model_res(); if(is.null(res)) return(empty)
    
    y_true <- get_vec(PKL_RESULTS$y_true)
    yhat   <- get_vec(res$yhat_cal)
    kms_v  <- get_vec(PKL_RESULTS$kms)
    if(length(y_true)==0) return(empty)
    ratio  <- y_true / ifelse(yhat==0, NA_real_, yhat)
    
    fig <- plot_ly()
    for (km in sort(unique(kms_v))) {
      mask <- kms_v==km; col <- km_color(km)
      rv <- ratio[mask]; n <- sum(mask)
      kl <- paste0("Km ",as.integer(km))
      med <- median(rv,na.rm=TRUE)
      p25 <- quantile(rv,.25,na.rm=TRUE); p75 <- quantile(rv,.75,na.rm=TRUE)
      fig <- fig %>%
        add_markers(x=rep(kl,n), y=rv, showlegend=FALSE,
                    marker=list(size=9,color="#4c8bc7",opacity=0.55)) %>%
        add_lines(x=c(kl,kl), y=c(p25,p75), showlegend=FALSE,
                  line=list(color=col,width=6)) %>%
        add_markers(x=kl, y=med, showlegend=FALSE,
                    marker=list(size=14,color=col,line=list(width=2,color="#f4f7fb")),
                    hovertemplate=paste0(kl,"<br>n=",n,"<br>Med: ",round(med,2),"<extra></extra>"))
    }
    # bandas ±20% y ±30%
    n_km <- length(unique(kms_v))
    fig %>% layout(
      shapes=list(
        list(type="rect",x0=-0.5,x1=n_km-0.5,y0=0.8,y1=1.2,
             fillcolor="rgba(46,170,107,0.08)",line=list(width=0),xref="x",yref="y"),
        list(type="rect",x0=-0.5,x1=n_km-0.5,y0=0.7,y1=1.3,
             fillcolor="rgba(230,180,0,0.05)",line=list(width=0),xref="x",yref="y"),
        list(type="line",x0=-0.5,x1=n_km-0.5,y0=1,y1=1,
             line=list(color=COLOR_MUTED,dash="dash",width=1.5),xref="x",yref="y")),
      xaxis=list(title="Estación",showgrid=FALSE),
      yaxis=list(title="SSC_med / SSC_mod",gridcolor=COLOR_BORDER),
      paper_bgcolor=COLOR_CARD,plot_bgcolor=COLOR_BG,
      font=list(family="'Lato',sans-serif",size=12,color=COLOR_TEXT),
      margin=list(l=60,r=20,t=30,b=60))
  })
  
  output$loocv_metrics_row <- renderUI({
    if (!PKL_OK) return(NULL)
    res <- model_res(); if(is.null(res)) return(NULL)
    col  <- "#2eaa6b"
    safe <- function(x, fmt="%.3f") tryCatch(sprintf(fmt, as.numeric(x)), error=function(e) "–")
    div(style="display:flex;gap:12px;flex-wrap:wrap;margin-bottom:16px;",
        metric_chip_ui("R² LOOCV",   safe(res$loo_r2),         col),
        metric_chip_ui("RMSE LOOCV", safe(res$loo_rmse,"%.1f"), col, "mg/L"),
        metric_chip_ui("MAPE LOOCV", safe(res$loo_mape,"%.1f"), col, "%"),
        metric_chip_ui("Bias LOOCV", safe(res$loo_bias,"%+.1f"),col, "mg/L"))
  })
  
  output$loocv_1to1 <- renderPlotly({
    empty <- plot_ly() %>% layout(paper_bgcolor=COLOR_CARD,plot_bgcolor=COLOR_BG)
    if (!PKL_OK) return(empty)
    res <- model_res(); if(is.null(res)) return(empty)
    
    y_true <- get_vec(PKL_RESULTS$y_true)
    yloo   <- get_vec(res$yhat_loo)
    if(length(y_true)==0) return(empty)
    lims <- c(min(y_true,yloo)*0.95, max(y_true,yloo)*1.05)
    
    plot_ly() %>%
      add_lines(x=lims,y=lims,showlegend=FALSE,
                line=list(color=COLOR_MUTED,dash="dash",width=1.5)) %>%
      add_markers(x=y_true,y=yloo,name="LOOCV",
                  marker=list(size=10,color="#2eaa6b",line=list(width=1.5,color="#f4f7fb"),opacity=0.9),
                  hovertemplate="SSC_med: %{x:.1f}<br>SSC_loo: %{y:.1f}<extra></extra>") %>%
      layout(xaxis=list(title="SSC medido (mg/L)",showgrid=TRUE,gridcolor=COLOR_BORDER),
             yaxis=list(title="SSC predicho LOOCV (mg/L)",gridcolor=COLOR_BORDER),
             paper_bgcolor=COLOR_CARD,plot_bgcolor=COLOR_BG,
             font=list(family="'Lato',sans-serif",size=12,color=COLOR_TEXT),
             margin=list(l=56,r=20,t=30,b=70))
  })
  
  output$loocv_error_dist <- renderPlotly({
    empty <- plot_ly() %>% layout(paper_bgcolor=COLOR_CARD,plot_bgcolor=COLOR_BG)
    if (!PKL_OK) return(empty)
    res <- model_res(); if(is.null(res)) return(empty)
    
    y_true <- get_vec(PKL_RESULTS$y_true)
    yloo   <- get_vec(res$yhat_loo)
    if(length(y_true)==0) return(empty)
    errors <- y_true - yloo
    
    plot_ly() %>%
      add_histogram(x=errors, nbinsx=12,
                    marker=list(color="#2eaa6b",opacity=0.75,line=list(color=COLOR_BORDER,width=1)),
                    name="Error LOOCV") %>%
      add_lines(x=c(0,0),y=c(0,max(tabulate(cut(errors,12)))+2),showlegend=FALSE,
                line=list(color=COLOR_MUTED,dash="dash",width=1.5)) %>%
      layout(xaxis=list(title="Error SSC_med − SSC_loo (mg/L)",showgrid=FALSE),
             yaxis=list(title="Frecuencia",gridcolor=COLOR_BORDER),
             paper_bgcolor=COLOR_CARD,plot_bgcolor=COLOR_BG,
             font=list(family="'Lato',sans-serif",size=12,color=COLOR_TEXT),
             margin=list(l=56,r=20,t=30,b=70))
  })
  
  output$model_comparison <- renderPlotly({
    empty <- plot_ly() %>% layout(paper_bgcolor=COLOR_CARD,plot_bgcolor=COLOR_BG)
    if (!PKL_OK) return(empty)
    res_list <- tryCatch(PKL_RESULTS$results, error=function(e) NULL)
    if (is.null(res_list)) return(empty)
    
    fig <- plot_ly()
    for (k in names(MODEL_LABELS)) {
      r <- tryCatch(res_list[[k]], error=function(e) NULL)
      if (is.null(r)) next
      col <- unname(MODEL_COLS[k])
      if (is.na(col)) col <- COLOR_ACCENT
      fig <- fig %>% add_bars(
        name = MODEL_LABELS[[k]],
        x    = c("R² Cal","R² LOOCV","RMSE Cal (÷500)","RMSE LOOCV (÷500)"),
        y    = c(as.numeric(r$cal_r2), as.numeric(r$loo_r2),
                 as.numeric(r$cal_rmse)/500, as.numeric(r$loo_rmse)/500),
        marker=list(color=col,opacity=0.85,line=list(color=COLOR_BORDER,width=1)))
    }
    fig %>% layout(barmode="group",
                   xaxis=list(showgrid=FALSE),
                   yaxis=list(title="Valor (RMSE normalizado)",gridcolor=COLOR_BORDER),
                   paper_bgcolor=COLOR_CARD,plot_bgcolor=COLOR_BG,
                   font=list(family="'Lato',sans-serif",size=12,color=COLOR_TEXT),
                   legend=list(orientation="h",y=-0.22),margin=list(l=56,r=20,t=30,b=70))
  })
  
  # ═══════════════════════════════════════
  # APLICACIÓN
  # ═══════════════════════════════════════
  output$app_model_badge_ui <- renderUI({
    mdl <- sel_mdl()
    lbl <- unname(MODEL_LABELS[mdl])
    col <- unname(MODEL_COLS[mdl])
    if (is.na(col)) col <- COLOR_ACCENT
    tags$span(lbl, style=paste0(
      "font-size:13px;font-weight:700;color:",col,";",
      "background-color:",col,"20;padding:4px 14px;",
      "border-radius:20px;border:1px solid ",col,"55;"))
  })
  
  observe({
    # Poblar selector de estaciones desde coords (siempre disponible)
    ests <- if (VS_HAS_COORDS) sort(unique(df_vs_coords$estacion)) else integer(0)
    if (length(ests)==0) return()
    updateSelectInput(session,"app_estaciones",
                      choices=c("Todas"="all", setNames(ests, paste0("E",ests))), selected="all")
  })
  
  vs_filtered <- reactive({
    if (!VS_HAS_SERIES) return(data.frame())
    y0  <- input$app_year_slider[1]; y1 <- input$app_year_slider[2]
    d   <- df_vs_global %>%
      filter(!is.na(fecha), year(fecha)>=y0, year(fecha)<=y1)
    sel <- input$app_estaciones
    if (!is.null(sel) && !"all" %in% sel)
      d <- d %>% filter(estacion %in% sel[sel!="all"])
    d
  })
  
  calamar_monthly <- reactive({
    if (input$app_show_calamar != "si") return(NULL)
    y0 <- input$app_year_slider[1]; y1 <- input$app_year_slider[2]
    tryCatch({
      mg <- Q_cal %>% inner_join(TSS_cal, by="Fecha") %>%
        drop_na(Q_calamar, TSS_calamar) %>%
        filter(year(Fecha)>=y0, year(Fecha)<=y1) %>%
        mutate(ssc_cal = ((TSS_calamar*(1e6/86400)) / Q_calamar) * (1e6/1e3),
               mes     = as.Date(format(Fecha,"%Y-%m-01")))
      mg %>% group_by(mes) %>% summarise(ssc_cal=mean(ssc_cal,na.rm=TRUE),.groups="drop")
    }, error=function(e) NULL)
  })
  
  # ── Estaciones virtuales (mapa) ──
  output$app_estmap_content <- renderUI({
    if (!VS_HAS_COORDS)
      return(div("Estaciones_virtuales.csv no encontrado.",
                 style=paste0("color:",COLOR_MUTED,";font-size:13px;padding:20px;")))
    
    n_est <- nrow(df_vs_coords)
    tagList(
      tags$p(paste0("Ubicación de las ", n_est,
                    " estaciones virtuales distribuidas a lo largo del tramo Km 11–19 del río Magdalena."),
             style=paste0("font-size:14px;color:",COLOR_MUTED,";margin:16px 0 12px;")),
      if (!VS_HAS_SERIES)
        div(style=paste0("background:#fff8e1;border-left:4px solid #f59e0b;",
                         "padding:10px 16px;border-radius:4px;font-size:13px;",
                         "color:#92400e;margin-bottom:12px;"),
            "⚠ series_estaciones_virtuales.xlsx no encontrado — solo se muestra el mapa de ubicación. ",
            "Las series de SSC estarán disponibles cuando se cargue ese archivo.")
      else NULL,
      plotlyOutput("app_estmap_plot", height="540px"),
      div(style="display:flex;gap:12px;flex-wrap:wrap;margin-top:16px;",
          metric_chip_ui("Estaciones virtuales", as.character(n_est), ACCENT2),
          metric_chip_ui("Rango lat",
                         paste0(round(min(df_vs_coords$lat,na.rm=TRUE),4),"° – ",
                                round(max(df_vs_coords$lat,na.rm=TRUE),4),"°"), COLOR_MUTED),
          metric_chip_ui("Rango lon",
                         paste0(round(min(df_vs_coords$lon,na.rm=TRUE),4),"° – ",
                                round(max(df_vs_coords$lon,na.rm=TRUE),4),"°"), COLOR_MUTED)
      )
    )
  })
  
  output$app_estmap_plot <- renderPlotly({
    req(VS_HAS_COORDS)
    locs <- df_vs_coords %>% arrange(estacion)
    
    # Estaciones de campo (de df original) para overlay
    sampling_lats <- c(11.10354,11.102,11.09628,11.09018,11.0755,11.0585,
                       11.0428,11.0249,11.0011327,10.9919,10.9782,10.9637,10.9546)
    sampling_lons <- c(-74.8516,-74.8513,-74.8497,-74.8492,-74.8456,-74.8387,
                       -74.8195,-74.7911,-74.7660752,-74.7611,-74.7579,-74.7569,-74.7562)
    sampling_names<- c("Km 0","Km 1","Km 3","Km 5","Km 7","Km 11",
                       "Km 14","Km 17","Km 18","Km 19a","Km 19b","Km 19c","Km 19d")
    
    plot_ly() %>%
      add_trace(lat=locs$lat, lon=locs$lon, type="scattermapbox", mode="markers+text",
                text=paste0("E",locs$estacion), textposition="top right",
                marker=list(size=10, color=ACCENT2, opacity=0.9),
                hovertemplate="<b>E%{text}</b><br>Lat: %{lat:.5f}<br>Lon: %{lon:.5f}<extra></extra>",
                name="Estaciones virtuales") %>%
      add_trace(lat=sampling_lats, lon=sampling_lons, type="scattermapbox", mode="markers+text",
                text=sampling_names, textposition="top right",
                marker=list(size=14, color=COLOR_ACCENT, opacity=1, symbol="circle"),
                textfont=list(size=9, color=COLOR_TEXT),
                hovertemplate="<b>%{text}</b><br>Lat: %{lat:.5f}<br>Lon: %{lon:.5f}<extra></extra>",
                name="Estaciones de campo") %>%
      layout(
        mapbox=list(style="carto-positron",
                    center=list(lat=mean(c(locs$lat,sampling_lats)),
                                lon=mean(c(locs$lon,sampling_lons))), zoom=11),
        margin=list(l=0,r=0,t=0,b=0), paper_bgcolor=COLOR_CARD,
        legend=list(orientation="v", x=0.01, y=0.99,
                    bgcolor="rgba(255,255,255,0.85)",
                    bordercolor=COLOR_BORDER, borderwidth=1))
  })
  
  # ── Serie anual ──
  output$app_anual_content <- renderUI({
    d <- vs_filtered()
    if (!is.data.frame(d) || nrow(d)==0)
      return(div("Sin datos para el período seleccionado.",
                 style=paste0("color:",COLOR_MUTED,";padding:20px;")))
    plotlyOutput("app_anual_plot", height="440px")
  })
  
  output$app_anual_plot <- renderPlotly({
    d <- vs_filtered(); req(is.data.frame(d), nrow(d)>0, "SSC" %in% names(d))
    y0 <- input$app_year_slider[1]; y1 <- input$app_year_slider[2]
    
    annual <- d %>% mutate(year=year(fecha)) %>%
      group_by(year) %>%
      summarise(m=mean(SSC,na.rm=TRUE), s=sd(SSC,na.rm=TRUE), .groups="drop")
    
    fig <- plot_ly()
    if (nrow(annual)>1)
      fig <- fig %>% add_ribbons(data=annual, x=~year, ymin=~(m-s), ymax=~(m+s),
                                 fillcolor="rgba(26,107,154,0.12)", line=list(color="rgba(0,0,0,0)"), showlegend=FALSE)
    fig <- fig %>% add_trace(data=annual, x=~year, y=~m,
                             type="scatter", mode="lines+markers",
                             name="Estaciones virtuales (media ± SD)",
                             line=list(color=COLOR_ACCENT,width=2.5), marker=list(size=9,color=COLOR_ACCENT),
                             hovertemplate="Año: %{x}<br>SSC: %{y:.1f} mg/L<extra></extra>")
    
    if (nrow(annual)>2) {
      fit <- lm(m~year, data=annual); m_t <- coef(fit)[2]
      xl  <- range(annual$year)
      fig <- fig %>% add_lines(x=xl, y=coef(fit)[1]+coef(fit)[2]*xl,
                               name=sprintf("Tendencia (%+.1f mg/L/año)", m_t),
                               line=list(color=COLOR_ACCENT,dash="dash",width=1.5))
    }
    
    cal <- calamar_monthly()
    if (!is.null(cal) && nrow(cal)>0) {
      cal_a <- cal %>% mutate(year=year(mes)) %>%
        group_by(year) %>% summarise(ssc=mean(ssc_cal,na.rm=TRUE),.groups="drop")
      fig <- fig %>% add_trace(data=cal_a, x=~year, y=~ssc,
                               type="scatter", mode="lines+markers", name="Calamar (IDEAM)",
                               line=list(color="#c0392b",width=2.5), marker=list(size=9,symbol="square"))
    }
    
    fig %>% layout(
      title=list(text=paste0("Serie anual SSC (",y0,"–",y1,")"),
                 font=list(color=COLOR_TEXT,size=14)),
      xaxis=list(title="Año",showgrid=TRUE,gridcolor=COLOR_BORDER),
      yaxis=list(title="SSC (mg/L)",gridcolor=COLOR_BORDER),
      paper_bgcolor=COLOR_CARD,plot_bgcolor=COLOR_BG,
      font=list(family="'Lato',sans-serif",size=12,color=COLOR_TEXT),
      legend=list(orientation="h",y=-0.2),margin=list(l=60,r=20,t=50,b=80))
  })
  
  # ── Serie mensual ──
  output$app_mensual_content <- renderUI({
    d <- vs_filtered()
    if (!is.data.frame(d) || nrow(d)==0)
      return(div("Sin datos.",style=paste0("color:",COLOR_MUTED,";padding:20px;")))
    plotlyOutput("app_mensual_plot", height="460px")
  })
  
  output$app_mensual_plot <- renderPlotly({
    d <- vs_filtered(); req(is.data.frame(d), nrow(d)>0)
    ma_win <- input$app_ma_window
    d$mes  <- as.Date(format(d$fecha,"%Y-%m-01"))
    monthly <- d %>% group_by(estacion,mes) %>%
      summarise(SSC=mean(SSC,na.rm=TRUE),.groups="drop")
    ests   <- sort(unique(monthly$estacion))
    n_col  <- max(3, min(length(ests),8))
    cols_e <- RColorBrewer::brewer.pal(n_col,"Set2")
    ecol   <- function(e) cols_e[(which(ests==e)-1)%%length(cols_e)+1]
    
    fig <- plot_ly()
    for (est in ests) {
      sub <- monthly %>% filter(estacion==est) %>% arrange(mes)
      col <- ecol(est)
      if (ma_win>1) {
        y_ma <- stats::filter(sub$SSC, rep(1/ma_win,ma_win), sides=2)
        fig <- fig %>%
          add_lines(x=sub$mes, y=sub$SSC, showlegend=FALSE,
                    line=list(color=col,width=1,dash="dot"), opacity=0.35) %>%
          add_trace(x=sub$mes, y=y_ma, type="scatter", mode="lines+markers",
                    name=paste0("E",est," MA",ma_win,"m"),
                    line=list(color=col,width=2.5), marker=list(size=5,color=col))
      } else {
        fig <- fig %>% add_trace(x=sub$mes, y=sub$SSC,
                                 type="scatter", mode="lines+markers", name=paste0("E",est),
                                 line=list(color=col,width=2), marker=list(size=6,color=col))
      }
    }
    
    cal <- calamar_monthly()
    if (!is.null(cal) && nrow(cal)>0) {
      if (ma_win>1) {
        y_ma <- stats::filter(cal$ssc_cal,rep(1/ma_win,ma_win),sides=2)
        fig <- fig %>%
          add_lines(x=cal$mes, y=cal$ssc_cal, showlegend=FALSE,
                    line=list(color="#c0392b",dash="dot",width=1), opacity=0.35) %>%
          add_lines(x=cal$mes, y=y_ma,
                    name=paste0("Calamar MA",ma_win,"m"), line=list(color="#c0392b",width=2.5))
      } else {
        fig <- fig %>% add_lines(x=cal$mes, y=cal$ssc_cal,
                                 name="Calamar (IDEAM)", line=list(color="#c0392b",dash="dot",width=2))
      }
    }
    
    fig %>% layout(
      xaxis=list(title="Fecha",showgrid=TRUE,gridcolor=COLOR_BORDER),
      yaxis=list(title="SSC (mg/L)",gridcolor=COLOR_BORDER),
      paper_bgcolor=COLOR_CARD,plot_bgcolor=COLOR_BG,
      font=list(family="'Lato',sans-serif",size=12,color=COLOR_TEXT),
      legend=list(orientation="h",y=-0.22),margin=list(l=60,r=20,t=30,b=80))
  })
  
  # ── Media móvil espacial ──
  output$app_movil_content <- renderUI({
    d <- vs_filtered()
    if (!is.data.frame(d) || nrow(d)==0)
      return(div("Sin datos.",style=paste0("color:",COLOR_MUTED,";padding:20px;")))
    plotlyOutput("app_movil_plot",height="460px")
  })
  
  output$app_movil_plot <- renderPlotly({
    d <- vs_filtered(); req(is.data.frame(d),nrow(d)>0)
    d$mes   <- as.Date(format(d$fecha,"%Y-%m-01"))
    win     <- input$app_window
    all_e   <- sort(unique(d$estacion))
    monthly <- d %>% group_by(estacion,mes) %>%
      summarise(SSC=mean(SSC,na.rm=TRUE),.groups="drop")
    n_col  <- max(3, min(length(all_e),8))
    cols_e <- RColorBrewer::brewer.pal(n_col,"Set2")
    ecol   <- function(e) cols_e[(which(all_e==e)-1)%%length(cols_e)+1]
    
    fig <- plot_ly()
    for (est in all_e) {
      idx       <- which(all_e==est)
      neighbors <- all_e[max(1,idx-win%/%2):min(length(all_e),idx+win%/%2)]
      mm <- monthly %>% filter(estacion %in% neighbors) %>%
        group_by(mes) %>% summarise(SSC=mean(SSC,na.rm=TRUE),.groups="drop") %>% arrange(mes)
      col <- ecol(est)
      fig <- fig %>% add_trace(x=mm$mes, y=mm$SSC, type="scatter", mode="lines+markers",
                               name=paste0("E",est," (MM esp.)"),
                               line=list(color=col,width=2), marker=list(size=5,color=col))
    }
    fig %>% layout(
      xaxis=list(title="Fecha",showgrid=TRUE,gridcolor=COLOR_BORDER),
      yaxis=list(title="SSC (mg/L)",gridcolor=COLOR_BORDER),
      paper_bgcolor=COLOR_CARD,plot_bgcolor=COLOR_BG,
      font=list(family="'Lato',sans-serif",size=12,color=COLOR_TEXT),
      legend=list(orientation="h",y=-0.22),margin=list(l=60,r=20,t=30,b=80))
  })
  
  # ── Ciclo multianual ──
  output$app_ciclo_content <- renderUI({
    d <- vs_filtered()
    if (!is.data.frame(d) || nrow(d)==0)
      return(div("Sin datos.",style=paste0("color:",COLOR_MUTED,";padding:20px;")))
    plotlyOutput("app_ciclo_plot",height="460px")
  })
  
  output$app_ciclo_plot <- renderPlotly({
    d <- vs_filtered(); req(is.data.frame(d),nrow(d)>0)
    climo <- d %>% mutate(mes_num=month(fecha)) %>%
      group_by(mes_num) %>%
      summarise(m=mean(SSC,na.rm=TRUE),s=sd(SSC,na.rm=TRUE),.groups="drop")
    
    plot_ly() %>%
      add_ribbons(data=climo,x=~mes_num,ymin=~(m-s),ymax=~(m+s),
                  fillcolor="rgba(26,107,154,0.12)",line=list(color="rgba(0,0,0,0)"),showlegend=FALSE) %>%
      add_trace(data=climo,x=~mes_num,y=~m,type="scatter",mode="lines+markers",
                name="Media mensual",
                line=list(color=COLOR_ACCENT,width=2.5),marker=list(size=9,color=COLOR_ACCENT)) %>%
      layout(
        xaxis=list(title="Mes",tickvals=1:12,ticktext=MESES,showgrid=FALSE),
        yaxis=list(title="SSC (mg/L)",gridcolor=COLOR_BORDER),
        paper_bgcolor=COLOR_CARD,plot_bgcolor=COLOR_BG,
        font=list(family="'Lato',sans-serif",size=12,color=COLOR_TEXT),
        margin=list(l=60,r=20,t=30,b=60))
  })
  
  # ── Mapa SSC — navegación por carpeta ──────────────────────────────────────
  #
  # Constantes (ajusta si tu carpeta tiene otro nombre o banda)
  CARPETA_TIF  <- "fotografias_sentinel"
  BANDA_NIR    <- 8L
  FACTOR_ESCALA <- 10000
  
  # Helper: lista TIFs ordenados por fecha extraída del nombre
  # Patrón esperado: S2_YYYY-MM-DD_*.tif
  listar_tifs <- function() {
    archivos <- list.files(CARPETA_TIF, pattern="\\.(tif|tiff)$",
                           full.names=TRUE, ignore.case=TRUE)
    if (length(archivos) == 0) return(data.frame(path=character(), label=character(),
                                                  fecha=as.Date(character()), stringsAsFactors=FALSE))
    fechas <- regmatches(basename(archivos),
                         regexpr("\\d{4}-\\d{2}-\\d{2}", basename(archivos)))
    fechas_d <- suppressWarnings(as.Date(fechas, format="%Y-%m-%d"))
    labels <- ifelse(
      !is.na(fechas_d),
      format(fechas_d, "%d %b %Y"),   # ej. "27 Ene 2018"
      basename(archivos)
    )
    df_t <- data.frame(path=archivos, label=labels, fecha=fechas_d, stringsAsFactors=FALSE)
    df_t[order(df_t$fecha, na.last=TRUE), ]
  }
  
  # Índice reactivo de la imagen activa (0-based internamente, 1-based para mostrar)
  img_idx <- reactiveVal(1L)
  
  observeEvent(input$btn_mapa_prev, {
    tifs <- listar_tifs()
    if (nrow(tifs) == 0) return()
    img_idx(((img_idx() - 2L) %% nrow(tifs)) + 1L)   # circular
  })
  
  observeEvent(input$btn_mapa_next, {
    tifs <- listar_tifs()
    if (nrow(tifs) == 0) return()
    img_idx((img_idx() %% nrow(tifs)) + 1L)            # circular
  })
  
  # UI del mapa (botones + disclaimer + plotly)
  output$app_mapa_content <- renderUI({
    tifs  <- listar_tifs()
    n     <- nrow(tifs)
    idx   <- img_idx()
    label <- if (n > 0) tifs$label[idx] else "—"
    
    card(
      section_title("Mapa de SSC modelada",
                    "Navega entre las imágenes Sentinel-2 para visualizar la SSC modelada"),
      
      # ── Controles de navegación ──
      div(style="display:flex;gap:12px;align-items:center;margin-bottom:16px;flex-wrap:wrap;",
        actionButton("btn_mapa_prev", "◀ Anterior",
                     style=paste0("padding:8px 18px;font-size:13px;font-weight:600;",
                                  "border-radius:8px;border:1px solid ",COLOR_BORDER,";",
                                  "background:",COLOR_CARD,";color:",COLOR_TEXT,";")),
        div(style=paste0("flex:1;text-align:center;font-size:15px;font-weight:700;",
                         "color:",COLOR_ACCENT,";min-width:160px;"),
            label),
        actionButton("btn_mapa_next", "Siguiente ▶",
                     style=paste0("padding:8px 18px;font-size:13px;font-weight:600;",
                                  "border-radius:8px;border:1px solid ",COLOR_BORDER,";",
                                  "background:",COLOR_CARD,";color:",COLOR_TEXT,";")),
        div(style=paste0("font-size:12px;color:",COLOR_MUTED,";min-width:50px;text-align:right;"),
            if (n > 0) paste0(idx, " / ", n) else "Sin imágenes")
      ),
      
      # ── Disclaimer ──
      div(style=paste0(
        "background:",COLOR_ACCENT,"12;border:1px solid ",COLOR_ACCENT,"40;",
        "border-radius:8px;padding:12px 18px;margin-bottom:16px;",
        "display:flex;gap:10px;align-items:flex-start;"),
        tags$span("💡", style="font-size:16px;flex-shrink:0;margin-top:1px;"),
        div(
          tags$span("Imágenes disponibles: ",
                    style=paste0("font-weight:600;color:",COLOR_ACCENT,";font-size:13px;")),
          tags$span(paste0(
            "Las imágenes Sentinel-2 están en la carpeta '", CARPETA_TIF, "/'. ",
            "Corresponden al tramo Km 11–19 del río Magdalena. ",
            "Usa los botones para navegar entre fechas."),
            style=paste0("font-size:13px;color:",COLOR_TEXT,";line-height:1.6;"))
        )
      ),
      
      # ── Barra de estado ──
      uiOutput("mapa_output_ui"),
      
      # ── Mapa ──
      plotlyOutput("mapa_ssc", height="560px")
    )
  })
  
  output$mapa_output_ui <- renderUI({
    tifs <- listar_tifs()
    msg  <- if (nrow(tifs) == 0)
      paste0("⚠️ No se encontraron imágenes TIF en '", CARPETA_TIF, "/'.")
    else
      "Procesando imagen…"
    div(style=paste0("min-height:40px;border-radius:8px;padding:10px 14px;",
                     "background:",COLOR_BG,";border:1px solid ",COLOR_BORDER,";",
                     "font-size:13px;color:",COLOR_MUTED,";margin-bottom:16px;"),
        msg)
  })
  
  output$mapa_ssc <- renderPlotly({
    # Re-ejecuta cuando cambia el índice
    idx  <- img_idx()
    tifs <- listar_tifs()
    
    fig_vacia <- function(msg="Sin imagen") {
      plot_ly() %>% layout(
        paper_bgcolor=COLOR_CARD, height=560,
        mapbox=list(style="carto-positron",
                    center=list(lat=11.0, lon=-74.85), zoom=11),
        margin=list(l=0,r=0,t=0,b=0),
        annotations=list(list(text=msg, x=0.5, y=0.5,
                              xref="paper", yref="paper", showarrow=FALSE,
                              font=list(size=13, color=COLOR_MUTED)))
      )
    }
    
    if (nrow(tifs) == 0) return(fig_vacia(paste0("No hay TIFs en '", CARPETA_TIF, "/'")))
    
    ruta  <- tifs$path[idx]
    label <- tifs$label[idx]
    
    tryCatch({
      library(terra)
      r     <- rast(ruta)
      nb    <- BANDA_NIR
      sf    <- FACTOR_ESCALA
      nir_r <- r[[nb]] / sf
      
      # Máscara agua con NDWI (banda 3 = green)
      if (nlyr(r) >= 3) {
        green_r <- r[[3]] / sf
        ndwi_r  <- (green_r - nir_r) / (green_r + nir_r + 1e-9)
        nir_r[ndwi_r <= 0] <- NA
      }
      
      # Modelo SSC = m*NIR + b
      if (!is.data.frame(df_calib) || nrow(df_calib) == 0) {
        m_c <- 1; b_c <- 0
      } else {
        fit <- lm(SSC ~ NIR, data=df_calib)
        m_c <- coef(fit)[2]; b_c <- coef(fit)[1]
      }
      ssc_r <- m_c * nir_r + b_c
      ssc_r[ssc_r < 0] <- NA
      
      # Downsample
      ssc_df <- as.data.frame(ssc_r, xy=TRUE, na.rm=TRUE)
      names(ssc_df) <- c("lon","lat","ssc")
      step   <- max(1L, as.integer(nrow(ssc_df) / 4000))
      ssc_df <- ssc_df[seq(1, nrow(ssc_df), by=step), ]
      
      p10 <- quantile(ssc_df$ssc, 0.10, na.rm=TRUE)
      p90 <- quantile(ssc_df$ssc, 0.90, na.rm=TRUE)
      
      # Actualizar barra de estado
      output$mapa_output_ui <- renderUI({
        div(style=paste0("min-height:40px;border-radius:8px;padding:10px 14px;",
                         "background:",COLOR_BG,";border:1px solid ",COLOR_BORDER,";",
                         "font-size:13px;color:",COLOR_MUTED,";margin-bottom:16px;"),
            paste0("✅ ", label, " — banda NIR ", nb, " | escala 1/", sf,
                   " | SSC: ", round(min(ssc_df$ssc,na.rm=TRUE)),
                   "–", round(max(ssc_df$ssc,na.rm=TRUE)), " mg/L",
                   " | píxeles agua: ", format(nrow(ssc_df), big.mark=",")))
      })
      
      plot_ly(ssc_df, lat=~lat, lon=~lon, type="scattermapbox", mode="markers",
              marker=list(size=4, color=~ssc, colorscale="YlOrRd",
                          cmin=p10, cmax=p90,
                          colorbar=list(title=list(text="SSC (mg/L)"), thickness=16),
                          opacity=0.85),
              hovertemplate="Lon: %{lon:.4f}<br>Lat: %{lat:.4f}<br>SSC: %{marker.color:.0f} mg/L<extra></extra>") %>%
        layout(
          mapbox=list(style="carto-positron",
                      center=list(lat=mean(ssc_df$lat), lon=mean(ssc_df$lon)), zoom=12),
          margin=list(l=0,r=0,t=40,b=0), paper_bgcolor=COLOR_CARD,
          font=list(family="'Lato',sans-serif", size=12, color=COLOR_TEXT),
          title=list(text=paste0("SSC modelada (mg/L) — Río Magdalena · ", label),
                     font=list(color=COLOR_TEXT, size=14))
        )
    }, error=function(e) {
      fig_vacia(paste0("Error: ", e$message))
    })
  })
  
  output$app_content <- renderUI(NULL)
  
}