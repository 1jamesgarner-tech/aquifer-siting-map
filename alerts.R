## ============================================================================
## Aquifer Siting -- LIVE listing alerts
##   RentCast active listings (radius queries over parcel clusters)
##     -> point-in-polygon vs the 67k shortlist parcels
##     -> NEW matches: email both recipients + ntfy push
##     -> ALL current matches: rebuild listings.html (browsable, in the map site)
##
## Secrets come from ENVIRONMENT VARIABLES (never hard-coded / committed):
##   RENTCAST_KEY  GMAIL_USER  GMAIL_APP_PW  EMAIL_TO (comma-sep)  NTFY_TOPIC
## MODE env: "preview" (default; no send, just builds listings.html + counts)
##           "send"    (emails + pushes NEW matches, records them as seen)
## ============================================================================
suppressMessages(library(sf)); sf::sf_use_s2(FALSE); options(warn=1)
say <- function(...) cat(format(Sys.time(),"%H:%M:%S"), sprintf(...), "\n")
`%||%` <- function(a,b) if (is.null(a)||length(a)==0||(length(a)==1&&is.na(a))) b else a
genv <- function(k, d="") { v <- Sys.getenv(k); if (nzchar(v)) v else d }

## Paths default to your local folders, but the cloud job overrides them via env
## (AQ_TARGETS/AQ_SITE/AQ_SEEN/AQ_STATE) so the SAME script runs in both places.
CFG <- list(
  targets   = genv("AQ_TARGETS","C:/Users/1jame/OneDrive/Desktop/Real Estate Maps/siting_index/shortlist_parcels_enriched.gpkg"),
  site_dir  = genv("AQ_SITE","C:/Users/1jame/OneDrive/Desktop/Real Estate Maps/siting_map"),     # listings.html + for_sale.geojson go here
  seen_csv  = genv("AQ_SEEN","C:/Users/1jame/OneDrive/Desktop/Real Estate Maps/siting_alerts/seen_listings.csv"),
  state_csv = genv("AQ_STATE","C:/Users/1jame/OneDrive/Desktop/Real Estate Maps/siting_alerts/market_state.csv"),
  key       = genv("RENTCAST_KEY"),
  gmail_user= genv("GMAIL_USER"),
  gmail_pw  = gsub(" ","", genv("GMAIL_APP_PW")),
  email_to  = strsplit(genv("EMAIL_TO","jggarner@umass.edu,linneawinter2604@gmail.com"), "\\s*,\\s*")[[1]],
  ntfy      = genv("NTFY_TOPIC","tangoats-fwuhfwuh"),
  n_regions = 7, radius_cap = 12   # miles
)
MODE <- genv("MODE","preview")

## ---- targets + polling regions (k-means clusters of parcel centroids) -------
load_targets <- function() {
  t <- sf::st_read(CFG$targets, quiet=TRUE)
  if (is.na(sf::st_crs(t)) || sf::st_crs(t)$epsg != 4326) t <- sf::st_transform(t, 4326)
  t
}
poll_regions <- function(targets) {
  ctr <- suppressWarnings(sf::st_coordinates(sf::st_centroid(sf::st_geometry(targets))))
  set.seed(1); km <- kmeans(ctr, centers=min(CFG$n_regions, nrow(unique(ctr))), iter.max=50)
  do.call(rbind, lapply(seq_len(nrow(km$centers)), function(i){
    cl <- ctr[km$cluster==i,,drop=FALSE]; cen <- km$centers[i,]
    ## radius (miles) to cover the cluster, capped
    d <- sqrt(((cl[,1]-cen[1])*53)^2 + ((cl[,2]-cen[2])*69)^2)   # deg->mi approx
    data.frame(lon=cen[1], lat=cen[2], radius=min(CFG$radius_cap, max(3, ceiling(quantile(d,.95)))))
  }))
}

## ---- RentCast collector (radius queries) -----------------------------------
zillow_url <- function(addr) paste0("https://www.zillow.com/homes/",
  utils::URLencode(gsub("\\s+","-", gsub(",","", addr)), reserved=TRUE), "_rb/")
collect_rentcast <- function(regions) {
  if (CFG$key=="") { say("no RENTCAST_KEY -> skipping live pull"); return(NULL) }
  rows <- list()
  for (i in seq_len(nrow(regions))) {
    r <- regions[i,]
    url <- sprintf("https://api.rentcast.io/v1/listings/sale?latitude=%.5f&longitude=%.5f&radius=%d&status=Active&limit=500",
                   r$lat, r$lon, r$radius)
    tmp <- tempfile(fileext=".json")
    ok <- suppressWarnings(tryCatch(system2("curl", c("-s","-H",
            shQuote(paste0("X-Api-Key: ",CFG$key)), shQuote(url), "-o", shQuote(tmp)), stdout=TRUE), error=function(e) NULL))
    js <- tryCatch(jsonlite::fromJSON(tmp, simplifyVector=FALSE), error=function(e) NULL); unlink(tmp)
    if (is.null(js) || (length(js)==1 && !is.null(js$status))) next     # error object
    for (L in js) rows[[length(rows)+1]] <- data.frame(
      id=L$id %||% L$formattedAddress, address=L$formattedAddress %||% "",
      price=L$price %||% NA, beds=L$bedrooms %||% NA, baths=L$bathrooms %||% NA,
      sqft=L$squareFootage %||% NA, lot_acres=round((L$lotSize %||% NA)/43560,2),
      year=L$yearBuilt %||% NA, dom=L$daysOnMarket %||% NA, hoa=(L$hoa$fee) %||% NA,
      lon=L$longitude %||% NA, lat=L$latitude %||% NA, listed=substr(L$listedDate %||% "",1,10),
      mls=L$mlsNumber %||% "", agent=(L$listingAgent$name) %||% "", agent_ph=(L$listingAgent$phone) %||% "",
      url=zillow_url(L$formattedAddress %||% ""), stringsAsFactors=FALSE)
  }
  out <- if (length(rows)) unique(do.call(rbind, rows)) else NULL
  if (!is.null(out)) out <- out[!is.na(out$lon) & !is.na(out$lat) & !duplicated(out$id),]
  out
}

## ---- match: which shortlist parcel does each listing fall on? ---------------
match_listings <- function(listings, targets) {
  ## canonical empty result so callers can always rely on nrow()/[ , ] working
  empty <- data.frame(id=character(0), address=character(0), price=character(0), dom=character(0),
                      lon=numeric(0), lat=numeric(0), p_pid=character(0), p_town=character(0),
                      p_acres=character(0), p_assessed_total=character(0), p_suit_overlap_acres=character(0),
                      p_aquifer_class=character(0), p_aquifer_gpm=character(0), p_owner=character(0),
                      stringsAsFactors=FALSE)
  if (is.null(listings) || nrow(listings)==0) return(empty)
  pts <- sf::st_as_sf(listings, coords=c("lon","lat"), crs=4326, remove=FALSE)
  hit <- suppressWarnings(sf::st_intersects(pts, targets)); keep <- lengths(hit)>0
  if (!any(keep)) return(empty)
  idx <- vapply(hit[keep], function(x) x[1], integer(1))
  pcols <- intersect(c("pid","town","acres","assessed_total","owner","aquifer_class","aquifer_gpm","suit_overlap_acres","flood_ok"), names(targets))
  pa <- sf::st_drop_geometry(targets)[idx, pcols, drop=FALSE]; names(pa) <- paste0("p_", names(pa))
  cbind(listings[keep,,drop=FALSE], pa)
}

## ---- dedupe -----------------------------------------------------------------
seen_ids <- function() if (file.exists(CFG$seen_csv))
  tryCatch(as.character(read.csv(CFG$seen_csv,stringsAsFactors=FALSE)$id), error=function(e) character(0)) else character(0)
mark_seen <- function(ids){ prev <- if(file.exists(CFG$seen_csv)) read.csv(CFG$seen_csv,stringsAsFactors=FALSE) else data.frame(id=character(0),ts=character(0))
  write.csv(unique(rbind(prev, data.frame(id=as.character(ids), ts=format(Sys.time())))), CFG$seen_csv, row.names=FALSE) }

## ---- formatting -------------------------------------------------------------
mny <- function(v) if(is.na(v)||v=="") "—" else paste0("$", format(as.numeric(v), big.mark=",", scientific=FALSE))
g <- function(r,x){ v<-r[[x]]; if(is.null(v)||length(v)==0||is.na(v)) "" else as.character(v) }
flood_label <- function(fok){ fok <- suppressWarnings(as.numeric(fok))
  if (is.na(fok)) "" else if (fok>=1000) "Higher ground (1,000-yr + HAND&ge;10 m)" else if (fok>=500) "500-yr (0.2%) floodplain clear" else "100-yr floodplain clear" }
card <- function(r) {
  flood <- flood_label(g(r,"p_flood_ok"))
  hoav  <- suppressWarnings(as.numeric(g(r,"hoa")))
  hoa   <- if (!is.na(hoav) && hoav>0)
             sprintf('<div style="color:#b00020;font-weight:700">&#9888; HOA fee reported: $%s/mo</div>', format(hoav, big.mark=","))
           else '<div style="color:#2c6e49">No HOA reported</div>'
  sprintf('
  <div style="font-family:Segoe UI,Arial;border:1px solid #d9c98a;border-radius:10px;padding:14px 16px;margin:10px 0;max-width:560px">
   <div style="font-size:16px;font-weight:700">%s</div>
   <div style="font-size:20px;font-weight:700;color:#2c6e49">%s</div>
   <div style="margin:4px 0">%s bd / %s ba · %s sqft · lot %s ac · listed %s%s</div>
   %s
   <hr style="border:none;border-top:1px solid #eee">
   <div style="font-weight:700;color:#7a6f4a">On shortlist parcel %s · %s</div>
   <div>%s ac parcel · assessed %s · %s ac on suitable ground</div>
   <div>Aquifer: %s%s &nbsp; · &nbsp; Owner: %s</div>
   %s%s%s
  </div>',
  g(r,"address"), mny(g(r,"price")), g(r,"beds"), g(r,"baths"), g(r,"sqft"), g(r,"lot_acres"), g(r,"listed"),
  ifelse(g(r,"dom")!="", paste0(" · ",g(r,"dom")," days"),""),
  hoa,
  g(r,"p_pid"), g(r,"p_town"), g(r,"p_acres"), mny(g(r,"p_assessed_total")), g(r,"p_suit_overlap_acres"),
  g(r,"p_aquifer_class"), ifelse(g(r,"p_aquifer_gpm")!="", paste0(" (~",g(r,"p_aquifer_gpm")," gpm)"),""), g(r,"p_owner"),
  ifelse(flood!="", sprintf('<div style="color:#1d4ed8">Flood clearance: %s</div>', flood),""),
  ifelse(g(r,"agent")!="", sprintf('<div style="color:#555">Agent: %s %s</div>',g(r,"agent"),g(r,"agent_ph")),""),
  sprintf('<a href="%s" style="display:inline-block;margin-top:6px;font-weight:600">View listing &rarr;</a>', g(r,"url")))
}

## ---- channels ---------------------------------------------------------------
send_email <- function(html, subject){
  if (CFG$gmail_user=="" || CFG$gmail_pw=="") { say("  email: no creds -> skip"); return(invisible()) }
  eml <- tempfile(fileext=".txt")
  writeLines(c(sprintf("From: Aquifer Alerts <%s>", CFG$gmail_user),
               sprintf("To: %s", paste(CFG$email_to, collapse=", ")),
               sprintf("Subject: %s", subject),
               "Content-Type: text/html; charset=utf-8", "", html), eml)
  args <- c("-s","--url","smtps://smtp.gmail.com:465","--ssl-reqd",
            "--mail-from", shQuote(CFG$gmail_user),
            unlist(lapply(CFG$email_to, function(x) c("--mail-rcpt", shQuote(x)))),
            "--user", shQuote(paste0(CFG$gmail_user,":",CFG$gmail_pw)),
            "--upload-file", shQuote(eml))
  system2("curl", args, stdout=TRUE, stderr=TRUE); unlink(eml); say("  email sent to %s", paste(CFG$email_to,collapse=", "))
}
push_ntfy <- function(title, msg){
  if (CFG$ntfy=="") return(invisible())
  system2("curl", c("-s","-H", shQuote(paste0("Title: ",title)), "-H","Tags: house",
          "-d", shQuote(msg), shQuote(paste0("https://ntfy.sh/", CFG$ntfy))), stdout=TRUE)
}

## ---- listings.html (all current matches, browsable, lives in the map site) --
write_listings_page <- function(m){
  body <- if (nrow(m)==0) "<p>No active listings currently fall on a shortlist parcel. This page refreshes each run.</p>" else
          paste(vapply(seq_len(nrow(m)), function(i) card(m[i,]), character(1)), collapse="\n")
  html <- sprintf('<!DOCTYPE html><html><head><meta charset="utf-8"><meta name="viewport" content="width=device-width,initial-scale=1">
   <title>Available parcels for sale</title></head><body style="font-family:Segoe UI,Arial;max-width:620px;margin:18px auto;padding:0 12px">
   <h1 style="font-size:20px">Aquifer-shortlist properties for sale</h1>
   <p style="color:#666">%d active listing(s) on land that passed the aquifer/flood/terrain filters. Updated %s.
   <a href="index.html">&larr; back to map</a></p>%s
   <p style="color:#999;font-size:12px;margin-top:24px">Listings via RentCast. Assessed/aquifer data from your siting index. Verify everything at the source.</p>
   </body></html>', nrow(m), format(Sys.Date()), body)
  writeLines(html, file.path(CFG$site_dir, "listings.html")); say("  wrote listings.html (%d matches)", nrow(m))
}

## ---- market-state tracking -> for_sale.geojson (the colored map overlay) ----
## new       = on market <= 14 days            -> green
## lingering = on market  > 14 days            -> sunflower yellow
## gone      = was matched before, now absent  -> black (kept 1 day, i.e. dropped at next run)
update_market_state <- function(m, targets){
  today <- as.Date(Sys.time())
  cols <- c("id","pid","address","price","dom","first_seen","last_seen","status","gone_date","hoa")
  st <- if (file.exists(CFG$state_csv)) read.csv(CFG$state_csv, stringsAsFactors=FALSE, colClasses="character") else
        setNames(data.frame(matrix("", 0, length(cols)), stringsAsFactors=FALSE), cols)
  if (!"hoa" %in% names(st)) st$hoa <- ""                 # migrate older state files
  cur <- as.character(m$id)
  hoa_c <- function(i) { v <- suppressWarnings(as.numeric(m$hoa[i])); if (is.na(v)) "" else as.character(v) }
  for (i in seq_len(nrow(m))) {
    id <- as.character(m$id[i]); dom <- suppressWarnings(as.numeric(m$dom[i]))
    stt <- if (!is.na(dom) && dom > 14) "lingering" else "new"
    j <- which(st$id == id)
    if (length(j)) { st$last_seen[j]<-as.character(today); st$dom[j]<-as.character(m$dom[i]); st$price[j]<-as.character(m$price[i])
                     st$status[j]<-stt; st$gone_date[j]<-""; st$pid[j]<-as.character(m$p_pid[i]); st$hoa[j]<-hoa_c(i) }
    else st <- rbind(st, setNames(data.frame(id, as.character(m$p_pid[i]), as.character(m$address[i]),
                     as.character(m$price[i]), as.character(m$dom[i]), as.character(today), as.character(today),
                     stt, "", hoa_c(i), stringsAsFactors=FALSE), cols))
  }
  gi <- which(!(st$id %in% cur) & st$status != "gone")        # newly disappeared -> gone
  if (length(gi)) { st$status[gi] <- "gone"; st$gone_date[gi] <- as.character(today) }
  keep <- is.na(st$gone_date) | st$gone_date=="" | (as.numeric(today - as.Date(st$gone_date)) <= 1)
  st <- st[keep, ]
  write.csv(st, CFG$state_csv, row.names=FALSE)
  ## build for_sale.geojson (one feature per parcel, joined to its polygon)
  st2 <- st[!duplicated(st$pid) & st$pid %in% targets$pid, ]
  if (nrow(st2)==0) { say("  for_sale.geojson: 0 parcels"); return(invisible()) }
  geo <- targets[match(st2$pid, targets$pid), ]
  fs <- sf::st_sf(status=st2$status, address=st2$address, price=st2$price, dom=st2$dom, pid=st2$pid,
                  hoa=ifelse(is.na(st2$hoa),"",st2$hoa), geometry=sf::st_geometry(geo))
  sf::st_write(fs, file.path(CFG$site_dir,"for_sale.geojson"), delete_dsn=TRUE, quiet=TRUE,
               layer_options=c("RFC7946=YES","COORDINATE_PRECISION=6"))
  say("  for_sale.geojson: %d parcels (new=%d lingering=%d gone=%d)", nrow(fs),
      sum(fs$status=="new"), sum(fs$status=="lingering"), sum(fs$status=="gone"))
}

## ---- run --------------------------------------------------------------------
run <- function(){
  tg <- load_targets(); say("targets: %d parcels", nrow(tg))
  reg <- poll_regions(tg); say("polling %d regions (radius %s mi)", nrow(reg), paste(reg$radius,collapse="/"))
  L <- collect_rentcast(reg); say("listings pulled: %d", if(is.null(L)) 0 else nrow(L))
  m <- match_listings(L, tg); say("on shortlist parcels: %d", nrow(m))
  write_listings_page(m)                       # always refresh the browsable page
  update_market_state(m, tg)                   # refresh for_sale.geojson (map overlay)
  if (MODE=="digest") {                        # one-off: email current listings, don't touch seen
    n <- min(as.integer(genv("DIGEST_N","40")), nrow(m))
    top <- m[order(-suppressWarnings(as.numeric(m$p_suit_overlap_acres))), , drop=FALSE][seq_len(n), ]
    html <- sprintf('<html><body style="font-family:Segoe UI,Arial"><h2>%d aquifer-shortlist properties currently for sale</h2>
      <p>Showing the %d with the most suitable ground (full list in listings.html). Each is on land that passed the
      aquifer + flood + terrain filters.</p>%s</body></html>',
      nrow(m), n, paste(vapply(seq_len(nrow(top)), function(i) card(top[i,]), character(1)), collapse=""))
    send_email(html, sprintf("%d aquifer-shortlist properties for sale (top %d shown)", nrow(m), n))
    push_ntfy("Climate Resilient Parcel For Sale!", sprintf("%d properties currently for sale on your shortlist parcels.", nrow(m)))
    say("DONE (digest: emailed top %d of %d)", n, nrow(m)); return(invisible(m))
  }
  prior <- seen_ids(); is_first <- length(prior)==0
  new <- m[!(as.character(m$id) %in% prior), , drop=FALSE]
  ## never alert on listings with a reported HOA fee (they still appear, marked, on the listings page/map)
  if (nrow(new)>0) {
    hoa_n <- suppressWarnings(as.numeric(new$hoa)); drop_hoa <- !is.na(hoa_n) & hoa_n > 0
    if (any(drop_hoa)) say("  skipping %d new listing(s) with a reported HOA fee", sum(drop_hoa))
    new <- new[!drop_hoa, , drop=FALSE]
  }
  say("NEW matches: %d (first_run=%s)", nrow(new), is_first)
  if (MODE != "send") { if (nrow(new)>0) say("  (preview: %d would alert)", nrow(new)); say("DONE (mode=%s)", MODE); return(invisible(m)) }
  if (nrow(new)==0) { say("DONE -- nothing new"); return(invisible(m)) }
  if (is_first) {
    ## FIRST RUN: announce + show a few highlights + seed everything (no 293-flood)
    ord <- order(-suppressWarnings(as.numeric(m$p_suit_overlap_acres)))
    top <- m[ord, ][seq_len(min(10, nrow(m))), ]
    html <- sprintf('<html><body style="font-family:Segoe UI,Arial"><h2>Your aquifer listing alerts are live.</h2>
      <p><b>%d</b> properties are currently for sale on your shortlist parcels. Below are the 10 with the most
      suitable ground; the full list is in <b>listings.html</b>. From now on you\'ll get an alert the moment a
      <i>new</i> one appears.</p>%s</body></html>',
      nrow(m), paste(vapply(seq_len(nrow(top)), function(i) card(top[i,]), character(1)), collapse=""))
    send_email(html, sprintf("Aquifer alerts live - %d properties currently for sale", nrow(m)))
    push_ntfy("Climate Resilient Parcel For Sale!",
              sprintf("Alerts are live: %d properties currently match your shortlist parcels. Browse the listings page.", nrow(m)))
    mark_seen(m$id)
  } else {
    ## INCREMENTAL: full alert for each genuinely new listing (push capped at 5)
    send_email(sprintf('<html><body><h2>%d new aquifer-shortlist listing(s)</h2>%s</body></html>', nrow(new),
               paste(vapply(seq_len(nrow(new)), function(i) card(new[i,]), character(1)), collapse="")),
               sprintf("%d new aquifer-shortlist listing(s)", nrow(new)))
    if (nrow(new) <= 5) for (i in seq_len(nrow(new)))
        push_ntfy("Climate Resilient Parcel For Sale!", sprintf("%s - %s on %s ac (parcel %s)",
          g(new[i,],"address"), mny(g(new[i,],"price")), g(new[i,],"p_acres"), g(new[i,],"p_pid")))
    else push_ntfy("Climate Resilient Parcel For Sale!",
          sprintf("%d new properties on your shortlist parcels - see your email / listings page.", nrow(new)))
    mark_seen(new$id)
  }
  say("DONE (mode=send)"); invisible(m)
}
run()
