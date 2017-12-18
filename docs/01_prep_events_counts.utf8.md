
<!-- rnb-text-begin -->

---
title: "01 Prep Events Counts"
author: "Rex W. Douglass and Kristen Harkness"
date: "12/9/2017"
output:
  html_notebook:
    toc: yes
    toc_float: yes
  html_document:
    toc: yes
editor_options:
  chunk_output_type: inline
---
<style>
    body .main-container {
        max-width: 100%;
    }
</style>

This is the entry point for the paper "Measuring the Landscape of Civil War." 

In this file, a raw csv file of the events dataset created for the Mau Mau rebellion is loaded and processed.


# Load Library


<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->



<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->



# Load Events Data


<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxuXG5ldmVudHMgPC0gcHJlcF9ldmVudHMoZnJvbXNjcmF0Y2ggPSBGKVxuXG5gYGAifQ== -->

```r

events <- prep_events(fromscratch = F)

```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->


# Dates
Basic cleaning. Generaly format is DD.MM.YYYY Sometimes multiple days are included by DD1/DD2/MM/YY. Somtimes year is YY or YYYY. 
-Your plots seem to suggest that there are a number of typos in the dates. All dates should range between 1951-1961.


<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxuXG4jcF9sb2FkKGRhdGUpXG5ldmVudHMgPC0gZXZlbnRzICU+JVxuICAgICAgICAgIG11dGF0ZShldmVudF9kYXRlPXN0cmluZ3I6OnN0cl9yZXBsYWNlX2FsbChldmVudF9kYXRlLFwiW1s6ZGlnaXQ6XV0rL1wiLCBcIlwiKSkgJT4lICNzdHJpcCBvZmYgZXh0cmEgZGF5IGF0IHRoZSBmcm9udCAwMS8wMi4xMi4xOTUwXG4gICAgICAgICAgbXV0YXRlKGV2ZW50X2RhdGU9c3RyaW5ncjo6c3RyX3JlcGxhY2VfYWxsKGV2ZW50X2RhdGUsXCJcXFxcLlwiLCBcIi9cIikpICU+JSAjQ29udmVydCBwZXJpb2RzIHRvIHNsYXNoZXNcbiAgICAgICAgICBtdXRhdGUoZXZlbnRfZGF0ZT10cmltd3MoZXZlbnRfZGF0ZSkpICU+JSAjdHJpbSB3aGl0ZXNwYWNlXG4gICAgICAgICAgbXV0YXRlKGV2ZW50X2RhdGU9c3RyaW5ncjo6c3RyX3JlcGxhY2VfYWxsKGV2ZW50X2RhdGUsXCIvNTJcIiwgXCIvMTk1MlwiKSkgJT4lICNjb252ZXJ0IDIgZGlnaXQgeWVhcnMgdG8gNCBkaWdpdCB5ZWFyc1xuICAgICAgICAgIG11dGF0ZShldmVudF9kYXRlPXN0cmluZ3I6OnN0cl9yZXBsYWNlX2FsbChldmVudF9kYXRlLFwiLzUzXCIsIFwiLzE5NTNcIikpICU+JVxuICAgICAgICAgIG11dGF0ZShldmVudF9kYXRlPXN0cmluZ3I6OnN0cl9yZXBsYWNlX2FsbChldmVudF9kYXRlLFwiLzU0XCIsIFwiLzE5NTRcIikpICU+JVxuICAgICAgICAgIG11dGF0ZShldmVudF9kYXRlPXN0cmluZ3I6OnN0cl9yZXBsYWNlX2FsbChldmVudF9kYXRlLFwiLzU1XCIsIFwiLzE5NTVcIikpICU+JVxuICAgICAgICAgIG11dGF0ZShldmVudF9kYXRlPXN0cmluZ3I6OnN0cl9yZXBsYWNlX2FsbChldmVudF9kYXRlLFwiLzU2XCIsIFwiLzE5NTZcIikpICU+JVxuICAgICAgICAgIG11dGF0ZShldmVudF9kYXRlPXN0cmluZ3I6OnN0cl9yZXBsYWNlX2FsbChldmVudF9kYXRlLFwiLzE5NTI0XCIsIFwiLzE5NTRcIikpICU+JSAjY2xlYW4gdHlwb1xuICAgICAgICAgICBtdXRhdGUoZXZlbnRfZGF0ZT0gbHVicmlkYXRlOjpkbXkoZXZlbnRfZGF0ZSkgKSAjRmVlZCB0byBsdWJyaWRhdGVcbiAgXG5ldmVudHMgJT4lIGZpbHRlcihpcy5uYShldmVudF9kYXRlX2NsZWFuKSkgJT4lIGRwbHlyOjpzZWxlY3Qoc3RhcnRzX3dpdGgoXCJldmVudF9kYXRlXCIpKSAlPiUgZGlzdGluY3QoKSAlPiUgcHJpbnQobj00MCkgI3Zpc3VhbGl6ZSBlcnJvcnNcblxuZXZlbnRzJGV2ZW50X2RhdGVfY2xlYW5feWVhciA8LSB5ZWFyKGV2ZW50cyRldmVudF9kYXRlX2NsZWFuKVxuZXZlbnRzJGV2ZW50X2RhdGVfY2xlYW5feWVhciAlPiUgamFuaXRvcjo6dGFieWwoKSAlPiUgcm91bmQoMylcblxuYGBgIn0= -->

```r

#p_load(date)
events <- events %>%
          mutate(event_date=stringr::str_replace_all(event_date,"[[:digit:]]+/", "")) %>% #strip off extra day at the front 01/02.12.1950
          mutate(event_date=stringr::str_replace_all(event_date,"\\.", "/")) %>% #Convert periods to slashes
          mutate(event_date=trimws(event_date)) %>% #trim whitespace
          mutate(event_date=stringr::str_replace_all(event_date,"/52", "/1952")) %>% #convert 2 digit years to 4 digit years
          mutate(event_date=stringr::str_replace_all(event_date,"/53", "/1953")) %>%
          mutate(event_date=stringr::str_replace_all(event_date,"/54", "/1954")) %>%
          mutate(event_date=stringr::str_replace_all(event_date,"/55", "/1955")) %>%
          mutate(event_date=stringr::str_replace_all(event_date,"/56", "/1956")) %>%
          mutate(event_date=stringr::str_replace_all(event_date,"/19524", "/1954")) %>% #clean typo
           mutate(event_date= lubridate::dmy(event_date) ) #Feed to lubridate
  
events %>% filter(is.na(event_date_clean)) %>% dplyr::select(starts_with("event_date")) %>% distinct() %>% print(n=40) #visualize errors

events$event_date_clean_year <- year(events$event_date_clean)
events$event_date_clean_year %>% janitor::tabyl() %>% round(3)

```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->


How often are event dates missing?


<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxudGFibGUoaXMubmEoZXZlbnRzJGV2ZW50X2RhdGUpKVxuYGBgIn0= -->

```r
table(is.na(events$event_date))
```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->


The documents also have dates, sometimes spanning a period of time. Can use that to nail down missing dates.


<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxuXG4oZXZlbnRzJGRvY3VtZW50X2RhdGVfdHlwZSA8LSBldmVudHMkZG9jdW1lbnRfZGF0ZSAlPiUgXG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgIHRvbG93ZXIoKSAlPiUgXG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgIG1vc2FpYzo6ZGVyaXZlZEZhY3RvcihcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIFwidW5rbm93blwiID0gVCxcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIFwibWlzc2luZ1wiICAgICA9IHN0cmluZ3I6OnN0cl9kZXRlY3QoLixcIm9ic2N1cmVkfG1pc3Npbmd8aWxsZWdpYmxlfHh4fERvY3VtZW50IG1pc3NpbmdcIiksXG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICBcIm9uIHRoZVwiICAgICAgPSBzdHJpbmdyOjpzdHJfZGV0ZWN0KC4sXCJvbiB0aGVcIiksXG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICBcInRvXCIgICAgICAgICAgPSBzdHJpbmdyOjpzdHJfZGV0ZWN0KC4sXCIgdG9cIiksXG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICBcImZvclwiICAgICAgICAgPSBzdHJpbmdyOjpzdHJfZGV0ZWN0KC4sXCJGb3IgXCIpLFxuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgXCJ3ZWVrXCIgICAgICAgID0gc3RyaW5ncjo6c3RyX2RldGVjdCguLFwid2Vla1wiKSxcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIFwid2VlayBlbmRpbmdcIiA9IHN0cmluZ3I6OnN0cl9kZXRlY3QoLixcIndlZWsgZW5kaW5nXCIpLFxuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgXCJwZXJpb2RcIiAgICAgID0gc3RyaW5ncjo6c3RyX2RldGVjdCguLFwicGVyaW9kXCIpLFxuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgXCJmb3J0bmlnaHRcIiAgID0gc3RyaW5ncjo6c3RyX2RldGVjdCguLFwiZm9ydG5pZ2h0XCIpLFxuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgXCJlbmRpbmdcIiAgICAgID0gc3RyaW5ncjo6c3RyX2RldGVjdCguLFwiZW5kaW5nXCIpLFxuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgLm1ldGhvZCA9IFwibGFzdFwiLFxuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgLmRlZmF1bHQgPSBcInVua25vd25cIlxuICAgICAgICAgICAgICAgICAgICAgICAgICAgICkgXG4gKSAlPiUgamFuaXRvcjo6dGFieWwoKSBcblxuXG5ldmVudHMkZG9jdW1lbnRfZGF0ZV9jbGVhbiA8LSBldmVudHMkZG9jdW1lbnRfZGF0ZSAlPiUgdG9sb3dlcigpICU+JSBcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgc3RyaW5ncjo6c3RyX3JlcGxhY2VfYWxsKFwiRm9ydG5pZ2h0IEVuZGVkIHxwZXJpb2R8d2VlayBlbmRpbmd8Zm9yIHx0aGUgfGZvcnRuaWdodCB8ZW5kaW5nIHx3ZWVrIHxGcm9tIHxvbiBcIixcIlwiKSAlPiVcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgc3RyaW5ncjo6c3RyX3JlcGxhY2VfYWxsKFwiW0RpZ2l0c10qdGh8W0RpZ2l0c10qc3R8W0RpZ2l0c10qcmR8W0RpZ2l0c10qbmRcIixcIlwiKVxuXG5ldmVudHMgPC0gZXZlbnRzICU+JSBcbiAgICAgICAgIGRwbHlyOjpzZWxlY3QoLW9uZV9vZihcImRvY3VtZW50X2RhdGVfMVwiLFwiZG9jdW1lbnRfZGF0ZV8yXCIpKSAlPiUgICNzZXBhcmF0ZSB3aWxsIGNvbnRpbnVlIHRvIGFkZCBjb2x1bW5zIGV2ZXJ5IHRpbWUgaXRzIHJ1blxuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgdGlkeXIgOjpzZXBhcmF0ZShjb2w9ZG9jdW1lbnRfZGF0ZV9jbGVhbixcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICBpbnRvPWMoXCJkb2N1bWVudF9kYXRlXzFcIixcImRvY3VtZW50X2RhdGVfMlwiKSxcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICBzZXAgPSBcIiB0b3x0byB8VG8gfCAtIFwiLCByZW1vdmU9RiwgZXh0cmE9XCJkcm9wXCIsIGZpbGw9XCJyaWdodFwiKVxuXG5ldmVudHMkZG9jdW1lbnRfZGF0ZV9jbGVhbl8xIDwtIGV2ZW50cyRkb2N1bWVudF9kYXRlXzEgJT4lIFxuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgc3RyaW5ncjo6c3RyX3JlcGxhY2VfYWxsKFwiW1s6ZGlnaXQ6XV0rL1wiLCBcIlwiKSAgICU+JSAjc3RyaXAgb2ZmIGV4dHJhIGRheSBhdCB0aGUgZnJvbnQgMDEvMDIuMTIuMTk1MFxuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgc3RyaW5ncjo6c3RyX3JlcGxhY2VfYWxsKFwiXFxcXC5cIiwgXCIvXCIpICAgICAgICAgICAgICU+JSAjQ29udmVydCBwZXJpb2RzIHRvIHNsYXNoZXNcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIHRyaW13cygpICU+JSAgICAgICAgICAgICAgICAgICAgICAgICAgICBcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIGx1YnJpZGF0ZTo6ZG15KClcblxuZXZlbnRzJGRvY3VtZW50X2RhdGVfY2xlYW5fMiA8LSBldmVudHMkZG9jdW1lbnRfZGF0ZV8yICU+JSBcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIHN0cmluZ3I6OnN0cl9yZXBsYWNlX2FsbChcIltbOmRpZ2l0Ol1dKy9cIiwgXCJcIikgICAlPiUgI3N0cmlwIG9mZiBleHRyYSBkYXkgYXQgdGhlIGZyb250IDAxLzAyLjEyLjE5NTBcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIHN0cmluZ3I6OnN0cl9yZXBsYWNlX2FsbChcIlxcXFwuXCIsIFwiL1wiKSAgICAgICAgICAgICAlPiUgI0NvbnZlcnQgcGVyaW9kcyB0byBzbGFzaGVzXG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICB0cmltd3MoKSAlPiUgICAgICAgICAgICAgICAgICAgICAgICAgICAgXG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICBsdWJyaWRhdGU6OmRteSgpICAgIFxuXG5ldmVudHMgJT4lIGZpbHRlcihpcy5uYShkb2N1bWVudF9kYXRlX2NsZWFuXzEpKSAlPiUgZHBseXI6OnNlbGVjdChzdGFydHNfd2l0aChcImRvY3VtZW50X2RhdGVcIikpICU+JSBkaXN0aW5jdCgpICU+JSBwcmludChuPTQwKSAjdmlzdWFsaXplIGVycm9yc1xuXG5cbmV2ZW50cyRkb2N1bWVudF9kYXRlX2Jlc3RfZGF0ZSA8LSBldmVudHMkZG9jdW1lbnRfZGF0ZV9jbGVhbl8yXG5jb25kaXRpb24gPC0gaXMubmEoZXZlbnRzJGRvY3VtZW50X2RhdGVfYmVzdF9kYXRlKVxuZXZlbnRzJGRvY3VtZW50X2RhdGVfYmVzdF9kYXRlW2NvbmRpdGlvbl0gPC0gZXZlbnRzJGRvY3VtZW50X2RhdGVfY2xlYW5fMVtjb25kaXRpb25dXG4oZXZlbnRzJGRvY3VtZW50X2RhdGVfYmVzdF95ZWFyIDwtIHllYXIoZXZlbnRzJGRvY3VtZW50X2RhdGVfYmVzdF9kYXRlKSkgJT4lIGphbml0b3I6OnRhYnlsKCkgJT4lIHJvdW5kKDMpXG5cbmBgYCJ9 -->

```r

(events$document_date_type <- events$document_date %>% 
                             tolower() %>% 
                             mosaic::derivedFactor(
                                          "unknown" = T,
                                          "missing"     = stringr::str_detect(.,"obscured|missing|illegible|xx|Document missing"),
                                          "on the"      = stringr::str_detect(.,"on the"),
                                          "to"          = stringr::str_detect(.," to"),
                                          "for"         = stringr::str_detect(.,"For "),
                                          "week"        = stringr::str_detect(.,"week"),
                                          "week ending" = stringr::str_detect(.,"week ending"),
                                          "period"      = stringr::str_detect(.,"period"),
                                          "fortnight"   = stringr::str_detect(.,"fortnight"),
                                          "ending"      = stringr::str_detect(.,"ending"),
                                          .method = "last",
                                          .default = "unknown"
                            ) 
 ) %>% janitor::tabyl() 


events$document_date_clean <- events$document_date %>% tolower() %>% 
                             stringr::str_replace_all("Fortnight Ended |period|week ending|for |the |fortnight |ending |week |From |on ","") %>%
                             stringr::str_replace_all("[Digits]*th|[Digits]*st|[Digits]*rd|[Digits]*nd","")

events <- events %>% 
         dplyr::select(-one_of("document_date_1","document_date_2")) %>%  #separate will continue to add columns every time its run
                              tidyr ::separate(col=document_date_clean,
                                        into=c("document_date_1","document_date_2"),
                                        sep = " to|to |To | - ", remove=F, extra="drop", fill="right")

events$document_date_clean_1 <- events$document_date_1 %>% 
                                 stringr::str_replace_all("[[:digit:]]+/", "")   %>% #strip off extra day at the front 01/02.12.1950
                                 stringr::str_replace_all("\\.", "/")             %>% #Convert periods to slashes
                                 trimws() %>%                            
                                 lubridate::dmy()

events$document_date_clean_2 <- events$document_date_2 %>% 
                                 stringr::str_replace_all("[[:digit:]]+/", "")   %>% #strip off extra day at the front 01/02.12.1950
                                 stringr::str_replace_all("\\.", "/")             %>% #Convert periods to slashes
                                 trimws() %>%                            
                                 lubridate::dmy()    

events %>% filter(is.na(document_date_clean_1)) %>% dplyr::select(starts_with("document_date")) %>% distinct() %>% print(n=40) #visualize errors


events$document_date_best_date <- events$document_date_clean_2
condition <- is.na(events$document_date_best_date)
events$document_date_best_date[condition] <- events$document_date_clean_1[condition]
(events$document_date_best_year <- year(events$document_date_best_date)) %>% janitor::tabyl() %>% round(3)

```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->


Only 666 missing from the document date

# Type of Event

Heads up, some of these event types in the codebook don't exist in the data. If a category has zero results, it's not a bug, just codebook needs to be updated.


<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxuXG5jYXQoXCJcXDAxNFwiKVxuI3BfbG9hZChjYXIsIHN0cmluZ2ksIHN0cmluZ3IsIHh0YWJsZSwgU25vd2JhbGxDKVxuZXZlbnRzJHR5cGVfY2xlYW4gPC0gc3RyaW5ncjo6c3RyX3RyaW0oc3RyaW5naTo6c3RyaV90cmFuc190b3RpdGxlKGV2ZW50cyR0eXBlKSlcblxuZXZlbnRzJHR5cGVfY2xlYW5fYWdnbG93IDwtIGV2ZW50cyR0eXBlX2NsZWFuICU+JVxuICBzdHJpbmdyOjpzdHJfdHJpbSgpICU+JVxuICB0b2xvd2VyKCkgJT4lXG4gIGNhcjo6cmVjb2RlKFwiXG4gICAgICAgICAgICAgJ2Rlc2VydGlvbic9J2Rlc2VydGlvbic7XG4gICAgICAgICAgICAgJ2VzY2FwZSc9J2VzY2FwZSc7XG4gICAgICAgICAgICAgYygnYWJkdWN0aW9uJywna2lkbmFwcGluZycsJ2tpZG5hcCcsJ2tpdG5hcCcsJ2tpbmRuYXAnKT0nYWJkdWN0aW9uJztcbiAgICAgICAgICAgICBjKCdhc3NhdWx0JywnYXR0YWNrJywnYXNzYXVsdGVkJywnYXNzYXVsdHMnLCdhc3N1YWx0JywnYXNzdWx0Jyk9J2Fzc2F1bHQnO1xuICAgICAgICAgICAgIGMoJ211cmRlcicsJ2VsaW1pbmF0aW9uJywna2lkbmFwIC8gbXVyZGVyJywnJyk9J211cmRlcic7XG4gICAgICAgICAgICAgYygnYXJzb24nLCdidXJuJyk9J2Fyc29uJztcbiAgICAgICAgICAgICBjKCdzbGFzaGVkJywnc3RhbXBlZGUnKT0nY2F0dGxlIHNsYXNoaW5nJztcbiAgICAgICAgICAgICAndmFuZGFsaXNtJz0ndmFuZGFsaXNtJztcbiAgICAgICAgICAgICBjKCd0aGVmdCcsJ3RoZWZ0cycsJ3RoZXQnLCdtaXNzaW5nJywnbG9zdCcsJ2VudHJ5Jyk9J3RoZWZ0JztcbiAgICAgICAgICAgICBjKCdjb25maXNjYXRlJywnc2VudGVuY2VkJyk9J3B1bmlzaG1lbnQnO1xuICAgICAgICAgICAgIGMoJ2NhcHR1cmUnLCdjYXB0dXJlZCcpPSdyZWJlbCBjYXB0dXJlJztcbiAgICAgICAgICAgICBjKCdvYXRoJywnb2F0aGluZycsJ3JlY3J1aXRtZW50JywncmVjcnVpdGVkJyk9J29hdGhpbmcnO1xuICAgICAgICAgICAgIGMoJ2NvbnRhY3QnLCdjYW9udGFjdCcsJ2NvbnRhY3RzJywnZHJvdmUgb2ZmJywnZHJpdmUgb2ZmJywnZHJvdmUgIG9mZicsXG4gICAgICAgICAgICAgICdjaGFzZWQgb2ZmJywnYnJva2UgdXAgb2F0aGluZycsJ2FtYnVzaCcpPSdjb250YWN0JztcbiAgICAgICAgICAgICBjKCdwYXRyb2wnLCdwb2xpY2UgYW5kIGtwciBwYXRyb2wnLCdzd2VlcCcpPSdwYXRyb2wnO1xuICAgICAgICAgICAgIGMoJ3NjcmVlbmluZycsJ3NyZWVuaW5nJyk9J3NjcmVlbmluZyc7XG4gICAgICAgICAgICAgYygndHlwZScpPSd1bmNsYXNzaWZpZWQnXG4gICAgICAgICAgICAgXCIpXG5cbmV2ZW50cyR0eXBlX2NsZWFuX2FnZ2xvdyAlPiVcbiAgamFuaXRvcjo6dGFieWwoc29ydCA9IFRSVUUpICU+JVxuICBqYW5pdG9yOjphZG9ybl9jcm9zc3RhYiguLGRpZ2l0cyA9IDEpXG5cblxuYGBgIn0= -->

```r

cat("\014")
#p_load(car, stringi, stringr, xtable, SnowballC)
events$type_clean <- stringr::str_trim(stringi::stri_trans_totitle(events$type))

events$type_clean_agglow <- events$type_clean %>%
  stringr::str_trim() %>%
  tolower() %>%
  car::recode("
             'desertion'='desertion';
             'escape'='escape';
             c('abduction','kidnapping','kidnap','kitnap','kindnap')='abduction';
             c('assault','attack','assaulted','assaults','assualt','assult')='assault';
             c('murder','elimination','kidnap / murder','')='murder';
             c('arson','burn')='arson';
             c('slashed','stampede')='cattle slashing';
             'vandalism'='vandalism';
             c('theft','thefts','thet','missing','lost','entry')='theft';
             c('confiscate','sentenced')='punishment';
             c('capture','captured')='rebel capture';
             c('oath','oathing','recruitment','recruited')='oathing';
             c('contact','caontact','contacts','drove off','drive off','drove  off',
              'chased off','broke up oathing','ambush')='contact';
             c('patrol','police and kpr patrol','sweep')='patrol';
             c('screening','sreening')='screening';
             c('type')='unclassified'
             ")

events$type_clean_agglow %>%
  janitor::tabyl(sort = TRUE) %>%
  janitor::adorn_crosstab(.,digits = 1)

```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->


## Collapse Event Types


<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxuXG4oZXZlbnRzJHR5cGVfY2xlYW5fYWdnbWVkIDwtIGNhcjo6cmVjb2RlKGV2ZW50cyR0eXBlX2NsZWFuX2FnZ2xvdywgXCJcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIGMoJ2FiZHVjdGlvbicsJ2Fzc2F1bHQnLCdtdXJkZXInKT0ncGh5c2ljYWwgdmlvbGVuY2UnO1xuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgYygndmFuZGFsaXNtJywnYXJzb24nLCdjYXR0bGUgc2xhc2hpbmcnKT0ncHJvcGVydHkgZGVzdHJ1Y3Rpb24nO1xuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgYygndGhlZnQnKT0ndGhlZnQnO1xuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgYygnY29udGFjdCcsJ3NjcmVlbmluZycsJ3NyZWVuaW5nJywncGF0cm9sJywncHVuaXNobWVudCcpPSdzZWN1cml0eSBvcGVyYXRpb25zJztcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIGMoJ2Rlc2VydGlvbicsJ2VzY2FwZScsJ3VuY2xhc3NpZmllZCcpPSd1bmNsYXNzaWZpZWQnO1xuICAgICAgICAgICAgICAgICAgICAgICAgICAgIFwiKSkgJT4lXG4gIGphbml0b3I6OnRhYnlsKHNvcnQgPSBUUlVFKSAlPiVcbiAgamFuaXRvcjo6YWRvcm5fY3Jvc3N0YWIoZGlnaXRzID0gMSlcbmBgYCJ9 -->

```r

(events$type_clean_aggmed <- car::recode(events$type_clean_agglow, "
                                 c('abduction','assault','murder')='physical violence';
                                 c('vandalism','arson','cattle slashing')='property destruction';
                                 c('theft')='theft';
                                 c('contact','screening','sreening','patrol','punishment')='security operations';
                                 c('desertion','escape','unclassified')='unclassified';
                            ")) %>%
  janitor::tabyl(sort = TRUE) %>%
  janitor::adorn_crosstab(digits = 1)
```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->



<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxuXG5ldmVudHMkdHlwZV9jbGVhbl9hZ2doaWdoIDwtIGNhcjo6cmVjb2RlKGV2ZW50cyR0eXBlX2NsZWFuX2FnZ21lZCwgXCJcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIGMoJ29hdGhpbmcnLCdwaHlzaWNhbCB2aW9sZW5jZScsJ3Byb3BlcnR5IGRlc3RydWN0aW9uJywndGhlZnQnKT0ncmViZWwgYWN0aXZpdHknO1xuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgYygncmViZWwgY2FwdHVyZScsJ3NlY3VyaXR5IG9wZXJhdGlvbnMnKT0nZ292ZXJubWVudCBhY3Rpdml0eSc7XG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgXCIpXG5cbmV2ZW50cyR0eXBlX2NsZWFuX2FnZ2hpZ2ggJT4lXG4gIGphbml0b3I6OnRhYnlsKHNvcnQgPSBUUlVFKSAlPiVcbiAgamFuaXRvcjo6YWRvcm5fY3Jvc3N0YWIoZGlnaXRzID0gMSlcblxuYGBgIn0= -->

```r

events$type_clean_agghigh <- car::recode(events$type_clean_aggmed, "
                                 c('oathing','physical violence','property destruction','theft')='rebel activity';
                                 c('rebel capture','security operations')='government activity';
                            ")

events$type_clean_agghigh %>%
  janitor::tabyl(sort = TRUE) %>%
  janitor::adorn_crosstab(digits = 1)

```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->





# Initiator of Event

Collapsed Initators to just Rebels, Government, and Civilians


<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxuXG5jYXQoXCJcXDAxNFwiKVxuXG5pbml0aWF0b3JfdGFyZ2V0X21hc3Rlcl9jbGVhbiA8LSBcIlxuYygnYW1tdW5pdGlvbicpPSAnYW1tdW5pdGlvbicgO1xuXG5jKCdleHBsb3NpdmVzJywgJ2dlbGlnbml0ZScpPSAnZXhwbG9zaXZlcycgO1xuXG5jKCdhcm1zJywgJ2ZpcmVhcm0nLCAnZ3VuJywgJ3Bpc3RvbCcsICdyaWZsZScsXG4nYW1tdW5pdGlvbicsICdyaWZpbGUnLCAnc2hvdGd1bicsICd2ZXJleSBwaXN0b2wnKT0gJ2ZpcmVhcm1zJyA7XG5cbmMoJ2F4ZScsJ3NjYWJiYXJkJywnd2VhcG9ucycpPSAnb3RoZXIgd2VhcG9ucycgO1xuXG5jKCdjb3VuY2lsbG9yJywgJ2Rpc3RyaWN0IGNvbW1pc3Npb25lcicsICdkaXN0cmljdCBvZmZpY2VyJywgJ2ZvcmVzdCByYW5nZXInLCAnZ2FtZSByYW5nZXInLCBcbidnYW1lIHdhcmRlbicsICdnb3Zlcm5tZW50Jyxcbidnb3Zlcm5tZW50IGVtcGxveWVlcycsICdwb3J0IGF1dGhvcml0eScsICdwdWJsaWMgd29ya3MgZGVwYXJ0bWVudCcsICdzY3JlZW5pbmcgdGVhbScgLCAnZG8nLCBcbidnb3Zybm1lbnQnLCAnd2FrYW1iYSBzY3JlZW5pbmcgdGVhbScsXG4nZG8gbXVudWdhJywnYWZyaWNhbiBkbycsJ2RjbWVydScsICdjb2xvbmlhbCBhdXRob3JpdGllcycgLCdnb3Z0ZW1wbG95ZWUnXG4pPSAnY29sb25pYWwgYXV0aG9yaXRpZXMnIDtcblxuYygnY2hpZWYnLCAnZWxkZXJzJywgJ2hlYWRtYW4nICwgJ2NoaWVmIGNob3N0cmFtJywnY2hpZWYgZWxpdWQnLCAnY2hpZWZcXFxcJ3Mgc2VudHJ5J1xuKT0gJ3RyaWJhbCBhdXRob3JpdGllcycgO1xuXG5jKCdidWlsZGluZ3MnLCAnY2F0dGxlIGRpcCcsICdkdWthJywgJ2Zhcm1zJyxcbidnYXJhZ2UnLCAnaG9tZXMnLCdodXRzJywgJ2hvdGVsJywgJ2xhbmQgcm92ZXInLCAnbG9ycnknLCAnbWFya2V0JywgJ29mZmljZScsICdveGNhcnQnLCAncHJvcGVydHknLCBcbidwdW1wIGhvdXNlJywgJ3Nhd21pbGwnLCAnc2hvcHMnLCAnc3RvcmVzJywgXG4ndHJhY3RvcicsICd2ZWhpY2xlJywgJ3dpbmRtaWxsJyAsICdidWxsb2NrXFxcXCdzIGZhcm0nLCdjYXR0bGUgYm9tYScsJ2NvZmZlIHRyZWVzJywnY29mZmVlIHRyZWVzJyxcbidjdXRob3VzZScsJ2RhaXJ5IGZhcm0nLCdkaXAnLCdob3VzZScsJ2hvdXNlaG9sZCcsXG4naG91c2VzJywnaHV0JywnaW5zdHJ1bWVudCcsJ2xhYm91ciBjYW1wIHBvc3QnLCdsYWJvdXIgaHV0cycsJ2xvcnJpZXMnLCdsdWNlcm5lIHNoZWRzJywnbWFpemUgc2hhbWJhJyxcbidtaWxrIGZhY3RvcnknLCdwaWcgc3R5JywncHJpdmF0ZSBwcm9wZXJ0eScsXG4ncHJvcGVydHkgb2YgY2l2aWxpYW5zJywnc2hvcCcsJ3N0b3JlJywndGhpa2EgZmlzaGluZyBjYW1wJywndmVoaWNsZXMnKT0gJ3ByaXZhdGUgcHJvcGVydHknO1xuXG5jKCdjYXNoJywgJ2Z1bmRzJywgJ21vbmV5JyAsICdjb25kdWN0b3JcXFxcJ3MgdGFraW5ncydcbik9ICdjYXNoJztcblxuYygnYmFuYW5hJywgJ2JhcmxleScsICdicmFuJywgJ2NhYmJhZ2UnLCAnY29mZmVlJywgJ2Nvcm4nLCAnY3JlYW0nLCAnY3JvcHMnLCAnZGFpcnknLCAnZm9vZCcsIFxuJ2ZydWl0JywgJ2dyYWluJywgJ2hvbmV5JywgJ21haXplJywgXG4nbWVhdCcsICdtaWxrJywgJ29hdHMnLCAncG9zaG8nLCAncG90YXRvZXMnLCAnc3VnYXInLCAndmVnZXRhYmxlJywgJ3doZWF0Jyxcbidmb29kJywnZm9vZCBldGMnLCdmb29kIHN0b3JlJywnZm9vZCBzdG9yZXMnLCdmb29kc3R1ZmZzJywnZnJ1aXRzJywnZ3JhaW5zJyxcbidncmFpbnMrY2xvdGggK21vbmV5JywnZ3JlZW4gbWFpemUgY29icycsJ3BvdGF0bycsJ3BvdGF0byBzdG9yZScsXG4ncG90YXRvcycsJ3NraW1tZWQgbWlsaycsJ3N1Z2FyIGNhbmUnLCdzdWdhciBtYWl6ZScsJ3ZlZ2V0YWJsZXMnLCd2ZWdpdGFibGUgZ2FyZGVuJyxcbid2ZWdpdGFibGVzJywnd2hlYXQgYmFncycsJ3doZWF0IHN0b3JlJywnd2hlZXQnLCd3aGlza3knXG4pPSAnZm9vZCc7XG5cbmMoJ2JlYXN0JywgJ2NhdHRsZScsICdjb3cnLCAnaGVyZCcsICdsaXZlc3RvY2snLCAncGlnJywgJ3NoZWVwJywgJ3N0ZWVyJywgJ3N0b2NrJyxcbidhbmltYWwnLCAnYnVsbHMnLCdjYWxmJywnY2FsdmVzJywnY2hpY2tlbicsJ2Nvd3MnLCdkb25rZXknLCdnb2F0JywnZ29hdHMnLFxuJ2hlYWQgb2YgY2F0dGxlJywnaGVhZCBvZiBjb3cnLCdoZWFkIG9mIHNoZWVwJywnaGVpZmVyJywnaGVpZmVycycsXG4nbGFtYicsJ2xpdmUgc3RvY2snLCdsaXZlc3RvY2snLCdsaXZlc3RvY2tzJywnbWFzYWkgaGVyZCcsJ21pbGsgY293Jywnb3gnLCdveCBjYXJ0JyxcbidveGVuJywncmFtJywncmVkIHBvbGwgY2F0dGxlJywnc2hlZScsJ3NoZWVwIG9yIG94Jywnc3RlZXJzJywnc3RvY2tzJ1xuKT0gJ2xpdmVzdG9jayc7XG5cbmMoJ21lZGljYWwgc3VwcGxpZXMnLCAnbWVkaWNpbmUnLCAnbSZiIHRhYmxldHMnLCAnbWVkaWNpbmVzJyk9ICdtZWRpY2luZSc7XG5cbmMoJ2JhZ3MnLCAnYmVkZGluZycsICdibGFua2V0cycsICdib29rcycsICdjaGFyY29hbCcsICdjbG90aCcsICdjbG90aGluZycsIFxuJ2Nvb2tpbmcgdXRlbnNpbHMnLCAnY3V0bGVyeScsICdlcXVpcG1lbnQnLCAnZmFybSBpbXBsZW1lbnRzJywgXG4naG91c2Vob2xkIGl0ZW1zJywnaW5zdHJ1bWVudHMnLCAnaXJvbicsICdwYWlscycsJ3BldHJvbCcsICdwcm92aXNpb25zJyxcbidvaWwnLCAnc2Fja3MnLCAnc3VwcGxpZXMnLCAndGFycGF1bGluJywgJ3RoYXRjaCcsICd0aW1iZXInLCBcbid0b2JhY2NvJywgJ3Rvb2xzJywgJ3VuaWZvcm1zJywgJ3dpcmUnLCAnd2lyZWxlc3Mgc2V0JywgJ3doaXNrZXknLFxuJ2FydGljbGVzJywnYmFnJywnYmF0dGVyeScsJ2J1Y2tldCcsJ2NpZ2EnLCdjaWdhcmV0dGVzJywnY2xvdGhlcycsXG4nY2xvdGhpbmcgZXRjJywnY2xvdGhzJywnZGFpcnkgaXRlbScsJ2RhaXJ5IHJlY29yZCBib29rJywnZ29vZHMnLFxuJ21hdGVyaWFsJywnb2lsK3RpbnMnLCdwcm92aXNpb252JywncmFpbHdheSB1bmlmb3JtcycsJ3N1cHBsaWVzJyxcbid0YXJwYXVsaWFuJywndHlwZXdyaXRlcicsJ3YtIGRyaXZlIGJlbHRzJywgJ2d1bm55IGJhZ3MnXG4pPSAnc3VwcGxpZXMnO1xuXG5jKCdjaHVyY2gnKT0gJ2NodXJjaCc7XG5cbmMoJ2FpcnN0cmlwJywgJ2JyaWRnZXMnLCAnaGFsZiBidWlsdCB2aWxsYWdlJywgJ3JvYWRzJywgJ3RyZW5jaGVzJywgJ3dhdGVyIHRhbmsnLFxuJ2JyaWRnZScsICdicmlkZ2UgYnJva2VuJywgJ2JyaWRnZSBkYW1hZ2VkJywgJ2luZnJhc3RydWN0dXJlJywgJ21pbHQgcHJvcGVydHknLCBcbidtaWx0cHJvcGVydHknLCAncHJpc29uIGNhbXAnLCdzdG4gZGFtYWdlZCdcbik9ICdpbmZyYXN0cnVjdHVyZSc7XG5cbmMoJ3NjaG9vbCcsICdzY2hvb2wnLCdzY2hvb2wgYnVpbGRpbmcnLCdzY2hvb2wgaG91c2UnLCdzY2hvb2wgcHJvcGVydHknLCdzY2hvb2xzJyk9ICdzY2hvb2wnO1xuXG5jKCdiZycsJ2tnJywnZWcnLCAnZ3VhcmQnLCdlbWJ1IGd1YXJkJywgJ2Zhcm0gZ3VhcmQnLCAnZm9yZXN0IGd1YXJkJywgJ2hvbWUgZ3VhcmQnLFxuJ2lrYW5kaW5lIGd1YXJkJywgJ2thdGhhbmp1cmUgZ3VhcmQnLCAna2lqYWJlIGd1YXJkJyxcbidraWt1eXUgZ3VhcmQnLCAnbWFzYWkgZ3VhcmQnLCAnbWVydSBndWFyZCcsICduYW5kaSBndWFyZCcsICdua3VidSBndWFyZCcsXG4nc3RvY2sgZ3VhcmQnLCAndGlnb25pIGd1YXJkJywndHAgYW5kIGVnIHBhdHJvbCcsJ2hnJywndHAgcGF0cm9sJywnaG9tZSBndWFyZCBwYXRyb2wnLFxuJ20nLCAnbS9nJywnbS9nIHBhdHJvbCcsJ2cnLFxuJ2thdGhhbmp1cmUgaGcnLCdrIGcnLCAnbmcnLFxuJ2VnIHBhdHJvbCcsICdoZyBjYW1wJywnaGcgbGVhZGVyJywnaGcgcGF0cm9sJywnaGcgcG9zdCcsJ2hvbWUnLCdob21lIGd1YXJkJywna2cgcG9zdCdcbik9ICdob21lIGd1YXJkJztcblxuYygnYXJhYiBjb21iYXQnICwgJ2FyYWIgY29tYmF0IHVuaXQnKT0gJ2FyYWIgY29tYmF0IHVuaXRzJztcblxuYygnYXNpYW4gY29tYmF0JywgJ2FzaWFuIGNvbWJhdCB1bml0JywgJ2FzaWFuIGNvbWJhdCB0ZWFtJywgJ3NlY29uZCBhc2lhbiBjb21iYXQgdW5pdCcgKT0gJ2FzaWFuIGNvbWJhdCB1bml0cyc7XG5cbmMoJzMga2FyJywgJzQga2FyJywgJzUga2FyJywgJzYga2FyJywgJzcga2FyJywgJzIzIGthcicsICcyNiBrYXInLCdrLmEucicsJ2sucC5yJywnay5hLnIuJyxcbic1dGggay5hLnInLCc1a2FyJywnNSBrLmEucicsJzR0aCBrYXInLCdrYXInICkgPSAnS2luZ3MgQWZyaWNhbiBSaWZsZXMnO1xuXG5jKCdkZXZvbnNoaXJlIHJlZ2ltZW50JywnZGV2b25zJywgJ2ZpZWxkIGludGVsbGlnZW5jZSBhc3Npc3RhbnQnLCAnZmllbGQgaW50ZWxsaWdlbmNlIG9mZmljZXInLFxuJ2ZpbycsICdnbG91Y2VzdGVyc2hpcmUgcmVnaW1lbnQnLCAnZ2xvc3RlcnMnLCAnbGFuY2FzaGlyZSBmdXNpbGllcnMnLCAna2luZ1xcXFwncyBzaHJvcHNoaXJlIGxpZ2h0IGluZmFudHJ5Jyxcbidyb3lhbCBlYXN0IGtlbnQgcmVnaW1lbnQnLCAnYnVmZnMnLCAncm95YWwgZnVzaWxpZXJzJywgJ3JveWFsIGhpZ2hsYW5kIHJlZ2ltZW50JywnYmxhY2sgd2F0Y2gnLFxuJ3dhdGNoJywgJ3JveWFsIGlubmlza2lsbGluZyBmdXNpbGllcnMnLCAncm95YWwgaXJpc2ggZnVzaWxpZXJzJywgJ3JveWFsIG5vcnRodW1iZXJsYW5kIGZ1c2lsaWVycycsXG4ncm5mJywncG9saWNlIGFuZCBtaWxpdGFyeScsICdhcm15JyAsICdsYW5jYXNoaXJlIGZ1c2lsbGllcnMnLCAnc3AgY29tcGFueSAxIHJveWFsIGlubmlza3MnLFxuJzEgcm5mJywgJ3JpZicsICdrc2xpJywgJ2lubmlza2lsbGluZ3MnLCAnZmlhJywnMSBnbG9zdGVycycsICcxIGJ3JywgJzEgYnVmZnMnLCBcbidcXFwiYVxcXCIgY29tcGFueSAxIHJveWFsIGlubmlza3MnLFxuJ1xcXCJhXFxcIiBjb21wYW55JywgJ3JveWFsIGZ1c2lsZXJzJywgJ29mIGRldm9ucycsJ29mIDEgZ2xvc3RlcnMnLCAnbGFuYyBmdXMnLCAnZnVzaWxpZXJzJyxcbidmaW8ga3J1Z2VyJywnZmlvcycsJ2EgY28gZGV2b24nLCc0IHBsYXRvb24gc3VwcG9ydCBjb21wYW55JyxcbidcXFwiY1xcXCIgY29tcGFueTEgcm95YWwgaW5uaXNrcycsJzYgcGxhdG9vbnNwIGNvbXBhbnkgMSByb3lhbCBpbm5pc2tzJywnMSBsZicsXG4nXFxcImNcXFwiIGNvbXBhbnknLFxuJ1xcXCJkXFxcIiBjb21wYW55JywnXFxcImFcXFwiJywnXFxcImFcXFwiIGNvbXBhbnkgYncnLCdidWZmcyBhbWJ1c2gnLCdkIGNvbXBhbnknLCdkXFxcXCcgZm9yY2UnLCdkZXZlbnMnLFxuJ2MgY29tcGFueScsJ1xcXCJkXFxcIiBmb3JjZScsXG4nYXJteSBvZmZpY2VyJyxcbidicml0aXNoIGFybXkgb2ZmaWNlcicsXG4nYnJpdGlzaCBtaWxpdGFyeScsXG4nYnVmZnMgcGF0cm9sJyxcbidldXJvcGVhbiBvZmZpY2VyJyxcbidldXJvcGVhbiBzb2xkaWVycycsXG4nZ2xvc3RlciBwYXRyb2wnXG4pPSAnYnJpdGlzaCBtaWxpdGFyeSc7XG5cblxuYygna2VueWEgcmVnaW1lbnQnLCdjYXB0YWluIGZvbGxpb3R04oCZcyB0ZWFtJyAsICdrcicsICdrZW5yZWcnLCAna2VucmVnZycsJ2tlbnlhIHJlZ2ltZW50IHNlcmdlYW50JyxcbidrZW55YSByZWd0Jywna2VuaXlhIHJlZ2ltZW50Jywna2VueWEgcmVnaW1lbnQgcHJpdmF0ZScpPSAna2VueWEgcmVnaW1lbnQnO1xuXG5jKCdjYXB0YWluJywgJ2NvbXBhbnknLCAnbWlsaXRhcnknLCAnYXJteScsICdtaWxpdGFyeSBwcm9wZXJ0eScsICdwbGF0b29uJywgJ3NlY3VyaXR5IGZvcmNlcycsXG4nc2VjdXJpdHkgZm9yY2UnLCAnY295JywgJ3N0cmlraW5nIGZvcmNlJyAsJ3NlbnRyeScsXG4nbWlsaXRhcnkgKGdlbmVyaWMpJywgJ25vbiBjb21taXNzaW9uZWQgb2ZmaWNlcnMnLCAncGF0cm9sJywgJ3NlbnRyaWUnLCAnc2d0IHdoaXRlJ1xuKT0gJ21pbGl0YXJ5IChnZW5lcmljKSc7XG5cbmMoJ3BzZXVkbyBnYW5nJywgJ3BzZXVkbyB0ZWFtJywgJ3Ryb2phbicsICdwc3VlZG8gZ2FuZ3MnLCAndHJvamFuIHRlYW0nICwgJ3RyYWNrZXIgZ3JvdXAnLFxuJ3BzZXVkbyB0ZWFtcycpPSAncHN1ZWRvIGdhbmdzJztcblxuYygncmFmJywgJ2JvbWJlcnMnLCAnYWlyIHN0cmlrZScsICdoYXJ2YXJkcycsICdyYWYgbGluY29sbnMnLCdmbHlpbmcgc3F1YXJkJyk9J3JveWFsIGFpciBmb3JjZSc7XG5cbmMoJ2dlbmVyYWwgc2VydmljZSB1bml0JywgJ2dzdScgKT0gJ3BhcmFtaWxpdGFyeSc7XG5cbmMoJ2NpZCcpPSdjaWQnO1xuXG5jKCdrZW55YSBwb2xpY2UnLCAna3AnICwgJ2twIGNvbnN0YWJsZXNcXFxcJyBxdWFydGVycycsICdrcGEnXG4pPSAna2VueWEgcG9saWNlJztcblxuYygna2VueWEgcG9saWNlIHJlc2VydmUnLCAna3ByJywgJ2twciBvZmZpY2VycycsICdyZXNlcnZlIHBvbGljZSBvZmZpY2VyJywgJ3JwbycgLCBcbidycG9zJywgJ3BvbGljZSBhbmQgay5wLnInKT0gJ2tlbnlhIHBvbGljZSByZXNlcnZlJztcblxuYygnY29uc3RhYmxlJywgJ3BvbGljZScsICdwb2xjZScsJ3BvbGljeSBwYXJ0eScpPSAncG9saWNlIChnZW5lcmljKSc7XG5cbmMoJ3JhaWx3YXkgcG9saWNlJyApPSAncmFpbHdheSBwb2xpY2UnO1xuXG5jKCdzcGVjaWFsIGJyYW5jaCcsICdibHVlIGRvY3RvciB0ZWFtJywgJ3NwZWNpYWwgYnJhbmNoIHRlYW0nLCAnc2Igb2ZmaWNlcnMnICk9ICdzcGVjaWFsIGJyYW5jaCc7XG5cbmMoJ2dpdGh1bXUgcG9saWNlJywgJ21hc2FpIHNwZWNpYWwgY29uc3RhYmxlJywgJ3RyaWJhbCBwb2xpY2UnLCAndHAnICwgJ3RwZWcnLFxuJ2FmcmljYW4gY29uc3RhYmxlJywgJ2FmcmljYW4gY29zdGFibGUnLCAnYWZyaWNhbiBzcGVjaWFsIGNvbnN0YWJsZScsICd0cmliYWwgcG9saWNlJ1xuKT0gJ3RyaWJhbCBwb2xpY2UnO1xuXG5jKCd0cmliYWwgcG9saWNlIHJlc2VydmUnLCAndHByJykgPSAndHJpYmFsIHBvbGljZSByZXNlcnZlJztcblxuYygnbWFueWF0dGEnLCAnZmlzaGluZyBjYW1wJywgJ3N1YmxvY2F0aW9uJywgJ3ZpbGxhZ2UnLCAnY2FtcCcgLCAndmlsbGFnZXMnKT0gJ2NvbW11bml0aWVzJztcblxuYygnZGV0YWluZWVzJywgJ3ByaXNvbmVyJywgJ3ByaXNvbmVycydcbik9ICdkZXRhaW5lZXMnO1xuXG5jKCdiYW5kaXRzJywgJ2Zvb2QgZm9yYWdlcnMnLCAnZ2FuZ3MnLCAnZ2FuZycsICdraWFtYSBraWEgbXVpbmdpJyAsICdra20nLCAna29tZXJlcmEnICwgJ21hdSBtYXUnLCAnb2F0aCBhZG1pbmlzdHJhdG9yJywgJ3Bhc3NpdmUgd2luZycsXG4ncmViZWxzJywgJ3N1c3BlY3RzJywgJ3RlcnJvcmlzdHMnLCd0ZXJyb3Jvc3RzJywndGVycm9yaXN0JywgJ2d1bm1hbicsICd0ZXJvcmlzdCcsICdndW5tZW4nLFxuJ3Jlc2lzdGFuY2UgZ3JvdXAnLCdyZXNpc3RhbmNlIGdyb3VwcycsICdvYXRoIGFkbWluaXN0cmF0ZXInLCdvYXRoIGFkbWluaXN0cmF0b3JzJywncGFzc2l2ZSB3aW5nIG1lbWJlcnMnLCdyZXNpc3RhbmNlJywnc3VzcGVjdCcsXG4nc3VzcGVjdGVkIGluc3VyZ2VudHMnLCd0ZXJyb2lzdCcsJ3RlcnJvaXN0cycsJ3RlcnJvc3QnKSA9ICdzdXNwZWN0ZWQgaW5zdXJnZW50cyc7XG5cbmMoJ2FmcmljYW5zJywgJ2NoaWxkcmVuJywgJ2NpdmlsaWFuJywnY2l2aWxpYW5zJywgJ2RyaXZlcicsICdlbXBsb3llZXMnLCAnZXZhbmdlbGlzdCcsIFxuJ2ZhbWlseScsICdmYXJtIGJveXMnLCAnZ2lybHMnLCAnaW5mb3JtZXInLFxuJ2tpa3V5dScsICdsYWJvcm91cicsICdsb3lhbGlzdCcsICdtYXNhaScsICdtZW4nLCAnbWlzc2lvbiBzdGFmZicsICdvd25lcicsICdwYXNzZW5nZXJzJyxcbidwZW9wbGUnLCAgJ3R1Z2VuIHRyaWJlc21lbicgLCAnc3RyYW5nZXInLCAnc2lraCcsXG4naGVyZCBib3lzJywgJ2lzaW9sbyBnYW1lIHNjb3V0cycsICdmYXJtIGxhYm91cicsICdmYXJtZXInLCAnZXVyb3BlYW4nLCAnZW1wbG95ZXInLFxuJ2VtcGxveWVlJywgJ2NpdmlsYW4nLCdzaG9wa2VlcGVyJyAsICdzdHVkZW50cycsICd0ZWFjaGVycycsXG4ndHVya2FuYScsICd2aWdpbGFudGVzJywgJ3dvbWVuJywgJ3dvcmtlcnMnLCd2aWxsYWdlcnMnLCAgJ2xhYm91cicsICdsb2NhbCBsYWJvdXInLFxuJ2tpa3V5dXMnLCAnZW1idScsICd0aXJpa2kgaG91c2Vib3knLCAnc2FtYnVydScsICdtYW5hZ2VyJywgJ3dvbWFuJyxcbid2ZXRvZmZpY2VyJywgJ21yaGlnZ2lucycsICdtYXNhaSBwYXJ0eScsJ2t1cmlhIHRyaWJlc21lbicsJ21hbmFnZXIgb2YgYWtpcmEgZXN0YXRlcycsXG4na3VyaWEgdHJpYmVzbWVuJywnY2hzdGVwaGVuJywnYWZyaWNhbicsXG4nY2F0aG9saWMgbWlzc29uIHN0YWZmJywgJ2FmcmljYW4gc3RhZmYnLCAnYXNpYW4gd29tZW4nLCAnYnVzIGNvbmR1Y3RvcicsICdjaGlsZCcsXG4nY2l2aWxpYW4oZm9vZCBjYXJyaWVycyknLCAnY2l2aWxpYW4oc2Nob29sbWFzdGVyKScsICdjaXZpbGlhbnMnLFxuJ2NpdmlsaW9uJywgJ2NvbW1pdHRlZScsICdjb21taXR0ZWUgbWVtYmVyJywgICdjb3VyaWVyJywnZWxkZXInLCdlbWJ1IHRyYWN0b3IgZHJpdmVyJyxcbidlbXBsb3llZXMgb2YgY2x1YicsJ2VuZ2luZSBib3knLCdnaXJsJywnZ29sZiBjbHViIHN0YWZmJywnaGlzIG93biBodXQnLFxuJ2hvdGVsIGtlZXBlcicsJ2hvdXNlYm95JywnaWxsZWdhbCByZXNpZGVudHMnLCdpbmRpYW4nLCdpbnRlcnByZXRlcicsJ2tlbScsJ2tpa2l5dScsXG4na2lrdXl1IGFzc2Vzc29yJywna2lrdXl1IGZhbWlsaWVzJywna2lrdXl1IGhvdXNlYm95Jywna2lrdXl1IGxhYm91cmVyJywna2lreXUnLFxuJ2tpcnVhIHZpbGxhZ2UnLCdsYWJvdXIgbGluZScsJ2xhYm91ciBsaW5lcycsJ2xhYm91cmVyJywnbGFib3VyZXJzJyxcbidsYWJvdXJlcycsJ2xhYm91cmxpbmUnLCdsYWJvdXJzJywnbWFsZXMnLCdtYW4nLCdtYXJhZ29saScsJ21hcmFnb2xpIGxhYm91cmVyJyxcbidtYXNhaSBlbGRlcnMnLCdtYXNhaSB0cmliZXNtYW4nLCdtZW1iZXJzIG9mIHRoZSB0aGlrYSBjb21taXR0ZWUnLFxuJ21uYSBzZWN0aW9uIGxlYWRlcnMnLCdtdW5pY2lwYWwgaW5zcGVjdG9ycycsJ25vbiBraWt1eXUgZW1wbG95ZWVzJywncGVyc29uJyxcbidwcm9zdGl0dXRlcycsJ3B1cmtlIG1hc2FpJywncHdkIGVtcGxveWVlJywncmFpbHdheSBlbXBsb3llZXMnLFxuJ3NjaG9vbCBtYXN0ZXInLCdzY2hvb2wgdGVhY2hlcicsJ3Npc3RlcnMgY29tbWl0dGVlJywnc29tYWxpJywnc3RhZmYnLCdzdHJhbmdlcnMnLFxuJ3RheGkgZHJpdmVycycsJ3RlYWNoZXInLCd0cmVhc3VyZXJzJyxcbidoZWFkbWFuXFxcXCdzIHNvbicsJ25vcnRvbiB0cmFpbGxcXFxcJ3MgbGFib3VyJywnZ29yZG9uXFxcXCdzIGxhYm91cicsICdmb29kIGNhcnJpZXJzJ1xuKSA9ICdjaXZpbGlhbnMnO1xuXG5jKCcnKT1OQVxuXG5cIlxuXG5yZWdleCA8LSBcIlxcXFwufHBhdHJvbHxbMS05XVxcXFxzKnJkfFsxLTldXFxcXHMqdGhcIiAjIHdpdGggcmVnZXggc3RhcnQgdHJ5aW5nIHRvIGdldCBtb3JlIG9mIHRoZXNlIHRvIGF1dG9tYXRpY2FsbHkgbWFwIGluc3RlYWQgb2YgZ2VuZXJhdGluZyBsb3RzIG9mIGhhbmQgY29kaW5nc1xuZXZlbnRzJGluaXRpYXRvcl9jbGVhbiA8LSBldmVudHMkaW5pdGlhdG9yICU+JSBzdHJpbmdyOjpzdHJfdHJpbSgpICU+JSBnc3ViKHJlZ2V4LCBcIlwiLCAuLCBpZ25vcmUuY2FzZSA9VClcblxuZXZlbnRzIDwtIGV2ZW50cyAlPiVcbiAgZHBseXI6OnNlbGVjdCgtb25lX29mKFwiaW5pdGlhdG9yX2NsZWFuXzFcIiwgXCJpbml0aWF0b3JfY2xlYW5fMlwiLCBcImluaXRpYXRvcl9jbGVhbl8zXCIpKSAlPiUgIyBzZXBhcmF0ZSB3aWxsIGNvbnRpbnVlIHRvIGFkZCBjb2x1bW5zIGV2ZXJ5IHRpbWUgaXRzIHJ1blxuICB0aWR5ciA6OnNlcGFyYXRlKFxuICAgIGNvbCA9IGluaXRpYXRvcl9jbGVhbixcbiAgICBpbnRvID0gYyhcImluaXRpYXRvcl9jbGVhbl8xXCIsIFwiaW5pdGlhdG9yX2NsZWFuXzJcIiwgXCJpbml0aWF0b3JfY2xlYW5fM1wiKSxcbiAgICBzZXAgPSBcImFuZHxcXFxcXFxcXHwvfFxcXFwmfCxcIiwgcmVtb3ZlID0gRiwgZXh0cmEgPSBcImRyb3BcIiwgZmlsbCA9IFwicmlnaHRcIlxuICApXG5cbmV2ZW50cyA8LSBldmVudHMgJT4lXG4gIG11dGF0ZV9hdCh2YXJzKHN0YXJ0c193aXRoKFwiaW5pdGlhdG9yX2NsZWFuX1wiKSksIGZ1bnMoZ3N1YihcIi4qcG9saWNlLipcIiwgXCJwb2xpY2VcIiwgLiwgaWdub3JlLmNhc2UgPVQpKSkgJT4lXG4gIG11dGF0ZV9hdCh2YXJzKHN0YXJ0c193aXRoKFwiaW5pdGlhdG9yX2NsZWFuX1wiKSksIGZ1bnMoZ3N1YihcIi4qZ3VhcmQuKlwiLCBcImd1YXJkXCIsIC4sIGlnbm9yZS5jYXNlID1UKSkpICU+JVxuICBtdXRhdGVfYXQodmFycyhzdGFydHNfd2l0aChcImluaXRpYXRvcl9jbGVhbl9cIikpLCBmdW5zKGdzdWIoXCIuKnRlcnJvci4qfC4qbWF1IG1hdS4qfC4qZ2FuZy4qXCIsIFwidGVycm9yaXN0XCIsIC4sIGlnbm9yZS5jYXNlID1UKSkpICU+JVxuICBtdXRhdGVfYXQodmFycyhzdGFydHNfd2l0aChcImluaXRpYXRvcl9jbGVhbl9cIikpLCBmdW5zKGdzdWIoXCIuKmtwci4qfC4qayBwIHIuKlwiLCBcImtwclwiLCAuLCBpZ25vcmUuY2FzZSA9VCkpKSAlPiVcbiAgbXV0YXRlX2F0KHZhcnMoc3RhcnRzX3dpdGgoXCJpbml0aWF0b3JfY2xlYW5fXCIpKSwgZnVucyhnc3ViKFwiLiprYXIuKnwuKmsgYSByLipcIiwgXCJrYXJcIiwgLiwgaWdub3JlLmNhc2UgPVQpKSkgJT4lXG4gIG11dGF0ZV9hdCh2YXJzKHN0YXJ0c193aXRoKFwiaW5pdGlhdG9yX2NsZWFuX1wiKSksIGZ1bnMoZ3N1YihcIi4qY295LipcIiwgXCJjb3lcIiwgLiwgaWdub3JlLmNhc2UgPVQpKSkgJT4lXG4gIG11dGF0ZV9hdCh2YXJzKHN0YXJ0c193aXRoKFwiaW5pdGlhdG9yX2NsZWFuX1wiKSksIGZ1bnMoZ3N1YihcIi4qZ3N1LipcIiwgXCJnc3VcIiwgLiwgaWdub3JlLmNhc2UgPVQpKSkgJT4lXG4gIG11dGF0ZV9hdCh2YXJzKHN0YXJ0c193aXRoKFwiaW5pdGlhdG9yX2NsZWFuX1wiKSksIGZ1bnMoZ3N1YihcIi4qd2F0Y2guKlwiLCBcIndhdGNoXCIsIC4sIGlnbm9yZS5jYXNlID1UKSkpICU+JVxuICBtdXRhdGVfYXQodmFycyhzdGFydHNfd2l0aChcImluaXRpYXRvcl9jbGVhbl9cIikpLCBmdW5zKHRyaW13cyguKSkpXG5cbmV2ZW50cyA8LSBldmVudHMgJT4lXG4gIG11dGF0ZShpbml0aWF0b3JfY2xlYW5fMV9hZ2dsb3cgPSBjYXI6OnJlY29kZShpbml0aWF0b3JfY2xlYW5fMSwgaW5pdGlhdG9yX3RhcmdldF9tYXN0ZXJfY2xlYW4pKSAlPiVcbiAgbXV0YXRlKGluaXRpYXRvcl9jbGVhbl8yX2FnZ2xvdyA9IGNhcjo6cmVjb2RlKGluaXRpYXRvcl9jbGVhbl8yLCBpbml0aWF0b3JfdGFyZ2V0X21hc3Rlcl9jbGVhbikpICU+JVxuICBtdXRhdGUoaW5pdGlhdG9yX2NsZWFuXzNfYWdnbG93ID0gY2FyOjpyZWNvZGUoaW5pdGlhdG9yX2NsZWFuXzMsIGluaXRpYXRvcl90YXJnZXRfbWFzdGVyX2NsZWFuKSlcblxuIyBzb3J0KHRhYmxlKGV2ZW50cyRpbml0aWF0b3JfY2xlYW5fMV9hZ2dsb3cpKVxuXG5sb3dsZXZlbGFnZyA8LSBjKFxuICBcImFyYWIgY29tYmF0IHVuaXRzXCIsIFwiY2lkXCIsIFwicHN1ZWRvIGdhbmdzXCIsIFwiYXNpYW4gY29tYmF0IHVuaXRzXCIsIFwic3BlY2lhbCBicmFuY2hcIixcbiAgXCJ0cmliYWwgYXV0aG9yaXRpZXNcIiwgXCJ0cmliYWwgcG9saWNlIHJlc2VydmVcIiwgXCJyb3lhbCBhaXIgZm9yY2VcIixcbiAgXCJwYXJhbWlsaXRhcnlcIiwgXCJrZW55YSByZWdpbWVudFwiLCBcInRyaWJhbCBwb2xpY2VcIiwgXCJrZW55YSBwb2xpY2UgcmVzZXJ2ZVwiLCBcImtlbnlhIHBvbGljZVwiLFxuICBcImJyaXRpc2ggbWlsaXRhcnlcIiwgXCJjaXZpbGlhbnNcIiwgXCJLaW5ncyBBZnJpY2FuIFJpZmxlc1wiLCBcIm1pbGl0YXJ5IChnZW5lcmljKVwiLCBcInBvbGljZSAoZ2VuZXJpYylcIixcbiAgXCJyYWlsd2F5IHBvbGljZVwiLCBcImhvbWUgZ3VhcmRcIiwgXCJjb2xvbmlhbCBhdXRob3JpdGllc1wiLCBcInN1c3BlY3RlZCBpbnN1cmdlbnRzXCJcbilcblxuIyBldmVudHMgPC0gZXZlbnRzICU+JVxuIyBtdXRhdGUoaW5pdGlhdG9yX2NsZWFuXzFfYWdnbG93PWlmZWxzZShpbml0aWF0b3JfY2xlYW5fMV9hZ2dsb3cgICVpbiUgbG93bGV2ZWxhZ2cgJiAhaXMubmEoaW5pdGlhdG9yX2NsZWFuXzFfYWdnbG93KSxpbml0aWF0b3JfY2xlYW5fMV9hZ2dsb3csIFwidW5jYXRlZ29yaXplZFwiKSkgJT4lIG11dGF0ZShpbml0aWF0b3JfY2xlYW5fMl9hZ2dsb3c9aWZlbHNlKGluaXRpYXRvcl9jbGVhbl8yX2FnZ2xvdyAgJWluJSBsb3dsZXZlbGFnZyAmICFpcy5uYShpbml0aWF0b3JfY2xlYW5fMl9hZ2dsb3cpLGluaXRpYXRvcl9jbGVhbl8yX2FnZ2xvdywgXCJ1bmNhdGVnb3JpemVkXCIpKSAlPiUgbXV0YXRlKGluaXRpYXRvcl9jbGVhbl8zX2FnZ2xvdz1pZmVsc2UoaW5pdGlhdG9yX2NsZWFuXzNfYWdnbG93ICAlaW4lIGxvd2xldmVsYWdnICYgIWlzLm5hKGluaXRpYXRvcl9jbGVhbl8zX2FnZ2xvdyksaW5pdGlhdG9yX2NsZWFuXzNfYWdnbG93LCBcInVuY2F0ZWdvcml6ZWRcIikpXG5cbiMgdGFibGUoZXZlbnRzJGluaXRpYXRvcl9jbGVhbl8xX2FnZ2xvdywgdXNlTkE9XCJhbHdheXNcIilcbmBgYCJ9 -->

```r

cat("\014")

initiator_target_master_clean <- "
c('ammunition')= 'ammunition' ;

c('explosives', 'gelignite')= 'explosives' ;

c('arms', 'firearm', 'gun', 'pistol', 'rifle',
'ammunition', 'rifile', 'shotgun', 'verey pistol')= 'firearms' ;

c('axe','scabbard','weapons')= 'other weapons' ;

c('councillor', 'district commissioner', 'district officer', 'forest ranger', 'game ranger', 
'game warden', 'government',
'government employees', 'port authority', 'public works department', 'screening team' , 'do', 
'govrnment', 'wakamba screening team',
'do munuga','african do','dcmeru', 'colonial authorities' ,'govtemployee'
)= 'colonial authorities' ;

c('chief', 'elders', 'headman' , 'chief chostram','chief eliud', 'chief\\'s sentry'
)= 'tribal authorities' ;

c('buildings', 'cattle dip', 'duka', 'farms',
'garage', 'homes','huts', 'hotel', 'land rover', 'lorry', 'market', 'office', 'oxcart', 'property', 
'pump house', 'sawmill', 'shops', 'stores', 
'tractor', 'vehicle', 'windmill' , 'bullock\\'s farm','cattle boma','coffe trees','coffee trees',
'cuthouse','dairy farm','dip','house','household',
'houses','hut','instrument','labour camp post','labour huts','lorries','lucerne sheds','maize shamba',
'milk factory','pig sty','private property',
'property of civilians','shop','store','thika fishing camp','vehicles')= 'private property';

c('cash', 'funds', 'money' , 'conductor\\'s takings'
)= 'cash';

c('banana', 'barley', 'bran', 'cabbage', 'coffee', 'corn', 'cream', 'crops', 'dairy', 'food', 
'fruit', 'grain', 'honey', 'maize', 
'meat', 'milk', 'oats', 'posho', 'potatoes', 'sugar', 'vegetable', 'wheat',
'food','food etc','food store','food stores','foodstuffs','fruits','grains',
'grains+cloth +money','green maize cobs','potato','potato store',
'potatos','skimmed milk','sugar cane','sugar maize','vegetables','vegitable garden',
'vegitables','wheat bags','wheat store','wheet','whisky'
)= 'food';

c('beast', 'cattle', 'cow', 'herd', 'livestock', 'pig', 'sheep', 'steer', 'stock',
'animal', 'bulls','calf','calves','chicken','cows','donkey','goat','goats',
'head of cattle','head of cow','head of sheep','heifer','heifers',
'lamb','live stock','livestock','livestocks','masai herd','milk cow','ox','ox cart',
'oxen','ram','red poll cattle','shee','sheep or ox','steers','stocks'
)= 'livestock';

c('medical supplies', 'medicine', 'm&b tablets', 'medicines')= 'medicine';

c('bags', 'bedding', 'blankets', 'books', 'charcoal', 'cloth', 'clothing', 
'cooking utensils', 'cutlery', 'equipment', 'farm implements', 
'household items','instruments', 'iron', 'pails','petrol', 'provisions',
'oil', 'sacks', 'supplies', 'tarpaulin', 'thatch', 'timber', 
'tobacco', 'tools', 'uniforms', 'wire', 'wireless set', 'whiskey',
'articles','bag','battery','bucket','ciga','cigarettes','clothes',
'clothing etc','cloths','dairy item','dairy record book','goods',
'material','oil+tins','provisionv','railway uniforms','supplies',
'tarpaulian','typewriter','v- drive belts', 'gunny bags'
)= 'supplies';

c('church')= 'church';

c('airstrip', 'bridges', 'half built village', 'roads', 'trenches', 'water tank',
'bridge', 'bridge broken', 'bridge damaged', 'infrastructure', 'milt property', 
'miltproperty', 'prison camp','stn damaged'
)= 'infrastructure';

c('school', 'school','school building','school house','school property','schools')= 'school';

c('bg','kg','eg', 'guard','embu guard', 'farm guard', 'forest guard', 'home guard',
'ikandine guard', 'kathanjure guard', 'kijabe guard',
'kikuyu guard', 'masai guard', 'meru guard', 'nandi guard', 'nkubu guard',
'stock guard', 'tigoni guard','tp and eg patrol','hg','tp patrol','home guard patrol',
'm', 'm/g','m/g patrol','g',
'kathanjure hg','k g', 'ng',
'eg patrol', 'hg camp','hg leader','hg patrol','hg post','home','home guard','kg post'
)= 'home guard';

c('arab combat' , 'arab combat unit')= 'arab combat units';

c('asian combat', 'asian combat unit', 'asian combat team', 'second asian combat unit' )= 'asian combat units';

c('3 kar', '4 kar', '5 kar', '6 kar', '7 kar', '23 kar', '26 kar','k.a.r','k.p.r','k.a.r.',
'5th k.a.r','5kar','5 k.a.r','4th kar','kar' ) = 'Kings African Rifles';

c('devonshire regiment','devons', 'field intelligence assistant', 'field intelligence officer',
'fio', 'gloucestershire regiment', 'glosters', 'lancashire fusiliers', 'king\\'s shropshire light infantry',
'royal east kent regiment', 'buffs', 'royal fusiliers', 'royal highland regiment','black watch',
'watch', 'royal inniskilling fusiliers', 'royal irish fusiliers', 'royal northumberland fusiliers',
'rnf','police and military', 'army' , 'lancashire fusilliers', 'sp company 1 royal innisks',
'1 rnf', 'rif', 'ksli', 'inniskillings', 'fia','1 glosters', '1 bw', '1 buffs', 
'\"a\" company 1 royal innisks',
'\"a\" company', 'royal fusilers', 'of devons','of 1 glosters', 'lanc fus', 'fusiliers',
'fio kruger','fios','a co devon','4 platoon support company',
'\"c\" company1 royal innisks','6 platoonsp company 1 royal innisks','1 lf',
'\"c\" company',
'\"d\" company','\"a\"','\"a\" company bw','buffs ambush','d company','d\\' force','devens',
'c company','\"d\" force',
'army officer',
'british army officer',
'british military',
'buffs patrol',
'european officer',
'european soldiers',
'gloster patrol'
)= 'british military';


c('kenya regiment','captain folliott’s team' , 'kr', 'kenreg', 'kenregg','kenya regiment sergeant',
'kenya regt','keniya regiment','kenya regiment private')= 'kenya regiment';

c('captain', 'company', 'military', 'army', 'military property', 'platoon', 'security forces',
'security force', 'coy', 'striking force' ,'sentry',
'military (generic)', 'non commissioned officers', 'patrol', 'sentrie', 'sgt white'
)= 'military (generic)';

c('pseudo gang', 'pseudo team', 'trojan', 'psuedo gangs', 'trojan team' , 'tracker group',
'pseudo teams')= 'psuedo gangs';

c('raf', 'bombers', 'air strike', 'harvards', 'raf lincolns','flying squard')='royal air force';

c('general service unit', 'gsu' )= 'paramilitary';

c('cid')='cid';

c('kenya police', 'kp' , 'kp constables\\' quarters', 'kpa'
)= 'kenya police';

c('kenya police reserve', 'kpr', 'kpr officers', 'reserve police officer', 'rpo' , 
'rpos', 'police and k.p.r')= 'kenya police reserve';

c('constable', 'police', 'polce','policy party')= 'police (generic)';

c('railway police' )= 'railway police';

c('special branch', 'blue doctor team', 'special branch team', 'sb officers' )= 'special branch';

c('githumu police', 'masai special constable', 'tribal police', 'tp' , 'tpeg',
'african constable', 'african costable', 'african special constable', 'tribal police'
)= 'tribal police';

c('tribal police reserve', 'tpr') = 'tribal police reserve';

c('manyatta', 'fishing camp', 'sublocation', 'village', 'camp' , 'villages')= 'communities';

c('detainees', 'prisoner', 'prisoners'
)= 'detainees';

c('bandits', 'food foragers', 'gangs', 'gang', 'kiama kia muingi' , 'kkm', 'komerera' , 'mau mau', 'oath administrator', 'passive wing',
'rebels', 'suspects', 'terrorists','terrorosts','terrorist', 'gunman', 'terorist', 'gunmen',
'resistance group','resistance groups', 'oath administrater','oath administrators','passive wing members','resistance','suspect',
'suspected insurgents','terroist','terroists','terrost') = 'suspected insurgents';

c('africans', 'children', 'civilian','civilians', 'driver', 'employees', 'evangelist', 
'family', 'farm boys', 'girls', 'informer',
'kikuyu', 'laborour', 'loyalist', 'masai', 'men', 'mission staff', 'owner', 'passengers',
'people',  'tugen tribesmen' , 'stranger', 'sikh',
'herd boys', 'isiolo game scouts', 'farm labour', 'farmer', 'european', 'employer',
'employee', 'civilan','shopkeeper' , 'students', 'teachers',
'turkana', 'vigilantes', 'women', 'workers','villagers',  'labour', 'local labour',
'kikuyus', 'embu', 'tiriki houseboy', 'samburu', 'manager', 'woman',
'vetofficer', 'mrhiggins', 'masai party','kuria tribesmen','manager of akira estates',
'kuria tribesmen','chstephen','african',
'catholic misson staff', 'african staff', 'asian women', 'bus conductor', 'child',
'civilian(food carriers)', 'civilian(schoolmaster)', 'civilians',
'civilion', 'committee', 'committee member',  'courier','elder','embu tractor driver',
'employees of club','engine boy','girl','golf club staff','his own hut',
'hotel keeper','houseboy','illegal residents','indian','interpreter','kem','kikiyu',
'kikuyu assessor','kikuyu families','kikuyu houseboy','kikuyu labourer','kikyu',
'kirua village','labour line','labour lines','labourer','labourers',
'laboures','labourline','labours','males','man','maragoli','maragoli labourer',
'masai elders','masai tribesman','members of the thika committee',
'mna section leaders','municipal inspectors','non kikuyu employees','person',
'prostitutes','purke masai','pwd employee','railway employees',
'school master','school teacher','sisters committee','somali','staff','strangers',
'taxi drivers','teacher','treasurers',
'headman\\'s son','norton traill\\'s labour','gordon\\'s labour', 'food carriers'
) = 'civilians';

c('')=NA

"

regex <- "\\.|patrol|[1-9]\\s*rd|[1-9]\\s*th" # with regex start trying to get more of these to automatically map instead of generating lots of hand codings
events$initiator_clean <- events$initiator %>% stringr::str_trim() %>% gsub(regex, "", ., ignore.case =T)

events <- events %>%
  dplyr::select(-one_of("initiator_clean_1", "initiator_clean_2", "initiator_clean_3")) %>% # separate will continue to add columns every time its run
  tidyr ::separate(
    col = initiator_clean,
    into = c("initiator_clean_1", "initiator_clean_2", "initiator_clean_3"),
    sep = "and|\\\\|/|\\&|,", remove = F, extra = "drop", fill = "right"
  )

events <- events %>%
  mutate_at(vars(starts_with("initiator_clean_")), funs(gsub(".*police.*", "police", ., ignore.case =T))) %>%
  mutate_at(vars(starts_with("initiator_clean_")), funs(gsub(".*guard.*", "guard", ., ignore.case =T))) %>%
  mutate_at(vars(starts_with("initiator_clean_")), funs(gsub(".*terror.*|.*mau mau.*|.*gang.*", "terrorist", ., ignore.case =T))) %>%
  mutate_at(vars(starts_with("initiator_clean_")), funs(gsub(".*kpr.*|.*k p r.*", "kpr", ., ignore.case =T))) %>%
  mutate_at(vars(starts_with("initiator_clean_")), funs(gsub(".*kar.*|.*k a r.*", "kar", ., ignore.case =T))) %>%
  mutate_at(vars(starts_with("initiator_clean_")), funs(gsub(".*coy.*", "coy", ., ignore.case =T))) %>%
  mutate_at(vars(starts_with("initiator_clean_")), funs(gsub(".*gsu.*", "gsu", ., ignore.case =T))) %>%
  mutate_at(vars(starts_with("initiator_clean_")), funs(gsub(".*watch.*", "watch", ., ignore.case =T))) %>%
  mutate_at(vars(starts_with("initiator_clean_")), funs(trimws(.)))

events <- events %>%
  mutate(initiator_clean_1_agglow = car::recode(initiator_clean_1, initiator_target_master_clean)) %>%
  mutate(initiator_clean_2_agglow = car::recode(initiator_clean_2, initiator_target_master_clean)) %>%
  mutate(initiator_clean_3_agglow = car::recode(initiator_clean_3, initiator_target_master_clean))

# sort(table(events$initiator_clean_1_agglow))

lowlevelagg <- c(
  "arab combat units", "cid", "psuedo gangs", "asian combat units", "special branch",
  "tribal authorities", "tribal police reserve", "royal air force",
  "paramilitary", "kenya regiment", "tribal police", "kenya police reserve", "kenya police",
  "british military", "civilians", "Kings African Rifles", "military (generic)", "police (generic)",
  "railway police", "home guard", "colonial authorities", "suspected insurgents"
)

# events <- events %>%
# mutate(initiator_clean_1_agglow=ifelse(initiator_clean_1_agglow  %in% lowlevelagg & !is.na(initiator_clean_1_agglow),initiator_clean_1_agglow, "uncategorized")) %>% mutate(initiator_clean_2_agglow=ifelse(initiator_clean_2_agglow  %in% lowlevelagg & !is.na(initiator_clean_2_agglow),initiator_clean_2_agglow, "uncategorized")) %>% mutate(initiator_clean_3_agglow=ifelse(initiator_clean_3_agglow  %in% lowlevelagg & !is.na(initiator_clean_3_agglow),initiator_clean_3_agglow, "uncategorized"))

# table(events$initiator_clean_1_agglow, useNA="always")
```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->



<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxuXG5ldmVudHNbLCBjKFwiaW5pdGlhdG9yX2NsZWFuXzFfYWdnbWVkXCIsIFwiaW5pdGlhdG9yX2NsZWFuXzJfYWdnbWVkXCIsIFwiaW5pdGlhdG9yX2NsZWFuXzNfYWdnbWVkXCIpXSA8LVxuICBldmVudHNbLCBjKFwiaW5pdGlhdG9yX2NsZWFuXzFfYWdnbG93XCIsIFwiaW5pdGlhdG9yX2NsZWFuXzJfYWdnbG93XCIsIFwiaW5pdGlhdG9yX2NsZWFuXzNfYWdnbG93XCIpXVxuZXZlbnRzIDwtIGV2ZW50cyAlPiVcbiAgbXV0YXRlX2F0KFxuICAgIHZhcnMoc3RhcnRzX3dpdGgoXCJpbml0aWF0b3JfY2xlYW5fMV9hZ2dtZWR8aW5pdGlhdG9yX2NsZWFuXzJfYWdnbWVkfGluaXRpYXRvcl9jbGVhbl8zX2FnZ21lZFwiKSksXG4gICAgLmZ1bnMgPSBmdW5zKGNhcjo6cmVjb2RlKFwiXG4gICAgIGMoJ2NpZCcsJ2tlbnlhIHBvbGljZSByZXNlcnZlJywna2VueWEgcG9saWNlJywncG9saWNlIChnZW5lcmljKScsJ3JhaWx3YXkgcG9saWNlJywnc3BlY2lhbCBicmFuY2gnLFxuJ3RyaWJhbCBwb2xpY2UnLCd0cmliYWwgcG9saWNlIHJlc2VydmUnKSA9ICdwb2xpY2UnO1xuICAgICBjKCdhcmFiIGNvbWJhdCB1bml0cycsJ2FzaWFuIGNvbWJhdCB1bml0cycsJ2JyaXRpc2ggbWlsaXRhcnknLCdLaW5ncyBBZnJpY2FuIFJpZmxlcycsXG4na2VueWEgcmVnaW1lbnQnLCdtaWxpdGFyeSAoZ2VuZXJpYyknLCdwc3VlZG8gZ2FuZ3MnLCdyb3lhbCBhaXIgZm9yY2UnKSA9ICdtaWxpdGFyeSc7IFxuICAgICBjKCdjb2xvbmlhbCBhdXRob3JpdGllcycsICd0cmliYWwgYXV0aG9yaXRpZXMnKT0nY2l2aWwgYXV0aG9yaXRpZXMnXG4gICAgICAgICAgXCIpKVxuICApXG5cbmV2ZW50cyRpbml0aWF0b3JfY2xlYW5fMl9hZ2dtZWQgJT4lXG4gIGphbml0b3I6OnRhYnlsKHNvcnQgPSBUUlVFKSAlPiVcbiAgamFuaXRvcjo6YWRvcm5fY3Jvc3N0YWIoZGlnaXRzID0gMSlcbmBgYCJ9 -->

```r

events[, c("initiator_clean_1_aggmed", "initiator_clean_2_aggmed", "initiator_clean_3_aggmed")] <-
  events[, c("initiator_clean_1_agglow", "initiator_clean_2_agglow", "initiator_clean_3_agglow")]
events <- events %>%
  mutate_at(
    vars(starts_with("initiator_clean_1_aggmed|initiator_clean_2_aggmed|initiator_clean_3_aggmed")),
    .funs = funs(car::recode("
     c('cid','kenya police reserve','kenya police','police (generic)','railway police','special branch',
'tribal police','tribal police reserve') = 'police';
     c('arab combat units','asian combat units','british military','Kings African Rifles',
'kenya regiment','military (generic)','psuedo gangs','royal air force') = 'military'; 
     c('colonial authorities', 'tribal authorities')='civil authorities'
          "))
  )

events$initiator_clean_2_aggmed %>%
  janitor::tabyl(sort = TRUE) %>%
  janitor::adorn_crosstab(digits = 1)
```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->



<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxuXG5ldmVudHNbLCBjKFwiaW5pdGlhdG9yX2NsZWFuXzFfYWdnaGlnaFwiLCBcImluaXRpYXRvcl9jbGVhbl8yX2FnZ2hpZ2hcIiwgXCJpbml0aWF0b3JfY2xlYW5fM19hZ2doaWdoXCIpXSA8LVxuICBldmVudHNbLCBjKFwiaW5pdGlhdG9yX2NsZWFuXzFfYWdnbWVkXCIsIFwiaW5pdGlhdG9yX2NsZWFuXzJfYWdnbWVkXCIsIFwiaW5pdGlhdG9yX2NsZWFuXzNfYWdnbWVkXCIpXVxuZXZlbnRzIDwtIGV2ZW50cyAlPiVcbiAgbXV0YXRlX2F0KFxuICAgIHZhcnMoc3RhcnRzX3dpdGgoXCJpbml0aWF0b3JfY2xlYW5fMV9hZ2doaWdofGluaXRpYXRvcl9jbGVhbl8yX2FnZ2hpZ2h8aW5pdGlhdG9yX2NsZWFuXzNfYWdnaGlnaFwiKSksXG4gICAgLmZ1bnMgPSBmdW5zKGNhcjo6cmVjb2RlKFwiXG4gICAgICAgICAgICAgICAgICBjKCdjaXZpbCBhdXRob3JpdGllcycsICdob21lIGd1YXJkJywgJ21pbGl0YXJ5JywgJ3BvbGljZScsICdwYXJhbWlsaXRhcnknKSA9J2dvdmVybm1lbnQnO1xuICAgICAgICAgICAgICAgICAgYygnc3VzcGVjdGVkIGluc3VyZ2VudHMnKSA9J3JlYmVscyc7XG4gICAgICAgICAgXCIpKVxuICApXG5cbmV2ZW50cyRpbml0aWF0b3JfY2xlYW5fM19hZ2doaWdoICU+JVxuICBqYW5pdG9yOjp0YWJ5bChzb3J0ID0gVFJVRSkgJT4lXG4gIGphbml0b3I6OmFkb3JuX2Nyb3NzdGFiKGRpZ2l0cyA9IDEpXG5gYGAifQ== -->

```r

events[, c("initiator_clean_1_agghigh", "initiator_clean_2_agghigh", "initiator_clean_3_agghigh")] <-
  events[, c("initiator_clean_1_aggmed", "initiator_clean_2_aggmed", "initiator_clean_3_aggmed")]
events <- events %>%
  mutate_at(
    vars(starts_with("initiator_clean_1_agghigh|initiator_clean_2_agghigh|initiator_clean_3_agghigh")),
    .funs = funs(car::recode("
                  c('civil authorities', 'home guard', 'military', 'police', 'paramilitary') ='government';
                  c('suspected insurgents') ='rebels';
          "))
  )

events$initiator_clean_3_agghigh %>%
  janitor::tabyl(sort = TRUE) %>%
  janitor::adorn_crosstab(digits = 1)
```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->


# Target of Event


<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxuXG5yZWdleCA8LSBcIlxcXFwufHBhdHJvbHxbMS05XVxcXFxzKnJkfFsxLTldXFxcXHMqdGhcIiAjIHdpdGggcmVnZXggc3RhcnQgdHJ5aW5nIHRvIGdldCBtb3JlIG9mIHRoZXNlIHRvIGF1dG9tYXRpY2FsbHkgbWFwIGluc3RlYWQgb2YgZ2VuZXJhdGluZyBsb3RzIG9mIGhhbmQgY29kaW5nc1xuZXZlbnRzJHRhcmdldF9jbGVhbiA8LSBldmVudHMkaW5pdGlhdG9yICU+JSBzdHJpbmdyOjpzdHJfdHJpbSgpICU+JSB0b2xvd2VyKCkgJT4lIGdzdWIocmVnZXgsIFwiXCIsIC4pXG5cbmV2ZW50cyA8LSBldmVudHMgJT4lXG4gIGRwbHlyOjpzZWxlY3QoLW9uZV9vZihcInRhcmdldF9jbGVhbl8xXCIsIFwidGFyZ2V0X2NsZWFuXzJcIiwgXCJ0YXJnZXRfY2xlYW5fM1wiKSkgJT4lICMgc2VwYXJhdGUgd2lsbCBjb250aW51ZSB0byBhZGQgY29sdW1ucyBldmVyeSB0aW1lIGl0cyBydW5cbiAgdGlkeXIgOjpzZXBhcmF0ZShcbiAgICBjb2wgPSBpbml0aWF0b3JfY2xlYW4sXG4gICAgaW50byA9IGMoXCJ0YXJnZXRfY2xlYW5fMVwiLCBcInRhcmdldF9jbGVhbl8yXCIsIFwidGFyZ2V0X2NsZWFuXzNcIiksXG4gICAgc2VwID0gXCJhbmR8XFxcXFxcXFx8L3xcXFxcJnwsXCIsIHJlbW92ZSA9IEYsIGV4dHJhID0gXCJkcm9wXCIsIGZpbGwgPSBcInJpZ2h0XCJcbiAgKVxuXG5ldmVudHMgPC0gZXZlbnRzICU+JSBcbiAgbXV0YXRlX2F0KHZhcnMoc3RhcnRzX3dpdGgoXCJ0YXJnZXRfY2xlYW5fXCIpKSwgZnVucyhnc3ViKFwiLip0ZXJyb3IuKnwuKmVyb3JpLip8LiplcnJvcmlzKnwuKm1hdSBtYXUuKnwuKmdhbmcuKlwiLCBcInRlcnJvcmlzdFwiLCAuLCBpZ25vcmUuY2FzZSA9VCkpICApICAlPiVcbiAgbXV0YXRlX2F0KHZhcnMoc3RhcnRzX3dpdGgoXCJ0YXJnZXRfY2xlYW5fXCIpKSwgZnVucyhnc3ViKFwiLipwb2xpY2UuKlwiLCBcInBvbGljZVwiLCAuLCBpZ25vcmUuY2FzZSA9VCkpKSAlPiVcbiAgbXV0YXRlX2F0KHZhcnMoc3RhcnRzX3dpdGgoXCJ0YXJnZXRfY2xlYW5fXCIpKSwgZnVucyhnc3ViKFwiLipndWFyZC4qXCIsIFwiZ3VhcmRcIiwgLiwgaWdub3JlLmNhc2UgPVQpKSkgJT4lXG4gIG11dGF0ZV9hdCh2YXJzKHN0YXJ0c193aXRoKFwidGFyZ2V0X2NsZWFuX1wiKSksIGZ1bnMoZ3N1YihcIi4qa3ByLip8LiprIHAgci4qXCIsIFwia3ByXCIsIC4sIGlnbm9yZS5jYXNlID1UKSkpICU+JVxuICBtdXRhdGVfYXQodmFycyhzdGFydHNfd2l0aChcInRhcmdldF9jbGVhbl9cIikpLCBmdW5zKGdzdWIoXCIuKmthci4qfC4qayBhIHIuKlwiLCBcImthclwiLCAuLCBpZ25vcmUuY2FzZSA9VCkpKSAlPiVcbiAgbXV0YXRlX2F0KHZhcnMoc3RhcnRzX3dpdGgoXCJ0YXJnZXRfY2xlYW5fXCIpKSwgZnVucyhnc3ViKFwiLipjb3kuKlwiLCBcImNveVwiLCAuLCBpZ25vcmUuY2FzZSA9VCkpKSAlPiVcbiAgbXV0YXRlX2F0KHZhcnMoc3RhcnRzX3dpdGgoXCJ0YXJnZXRfY2xlYW5fXCIpKSwgZnVucyhnc3ViKFwiLipnc3UuKlwiLCBcImdzdVwiLCAuLCBpZ25vcmUuY2FzZSA9VCkpKSAlPiVcbiAgbXV0YXRlX2F0KHZhcnMoc3RhcnRzX3dpdGgoXCJ0YXJnZXRfY2xlYW5fXCIpKSwgZnVucyhnc3ViKFwiLip3YXRjaC4qXCIsIFwid2F0Y2hcIiwgLiwgaWdub3JlLmNhc2UgPVQpKSkgJT4lXG4gIG11dGF0ZV9hdCh2YXJzKHN0YXJ0c193aXRoKFwidGFyZ2V0X2NsZWFuX1wiKSksIGZ1bnModHJpbXdzKC4pKSlcblxuZXZlbnRzJHRhcmdldF9jbGVhbl8xICU+JVxuICBqYW5pdG9yOjp0YWJ5bChzb3J0ID0gVFJVRSkgJT4lXG4gIGphbml0b3I6OmFkb3JuX2Nyb3NzdGFiKGRpZ2l0cyA9IDEpXG5gYGAifQ== -->

```r

regex <- "\\.|patrol|[1-9]\\s*rd|[1-9]\\s*th" # with regex start trying to get more of these to automatically map instead of generating lots of hand codings
events$target_clean <- events$initiator %>% stringr::str_trim() %>% tolower() %>% gsub(regex, "", .)

events <- events %>%
  dplyr::select(-one_of("target_clean_1", "target_clean_2", "target_clean_3")) %>% # separate will continue to add columns every time its run
  tidyr ::separate(
    col = initiator_clean,
    into = c("target_clean_1", "target_clean_2", "target_clean_3"),
    sep = "and|\\\\|/|\\&|,", remove = F, extra = "drop", fill = "right"
  )

events <- events %>% 
  mutate_at(vars(starts_with("target_clean_")), funs(gsub(".*terror.*|.*erori.*|.*erroris*|.*mau mau.*|.*gang.*", "terrorist", ., ignore.case =T))  )  %>%
  mutate_at(vars(starts_with("target_clean_")), funs(gsub(".*police.*", "police", ., ignore.case =T))) %>%
  mutate_at(vars(starts_with("target_clean_")), funs(gsub(".*guard.*", "guard", ., ignore.case =T))) %>%
  mutate_at(vars(starts_with("target_clean_")), funs(gsub(".*kpr.*|.*k p r.*", "kpr", ., ignore.case =T))) %>%
  mutate_at(vars(starts_with("target_clean_")), funs(gsub(".*kar.*|.*k a r.*", "kar", ., ignore.case =T))) %>%
  mutate_at(vars(starts_with("target_clean_")), funs(gsub(".*coy.*", "coy", ., ignore.case =T))) %>%
  mutate_at(vars(starts_with("target_clean_")), funs(gsub(".*gsu.*", "gsu", ., ignore.case =T))) %>%
  mutate_at(vars(starts_with("target_clean_")), funs(gsub(".*watch.*", "watch", ., ignore.case =T))) %>%
  mutate_at(vars(starts_with("target_clean_")), funs(trimws(.)))

events$target_clean_1 %>%
  janitor::tabyl(sort = TRUE) %>%
  janitor::adorn_crosstab(digits = 1)
```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->



<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxuZXZlbnRzIDwtIGV2ZW50cyAlPiVcbiAgbXV0YXRlKHRhcmdldF9jbGVhbl8xX2FnZ2xvdyA9IGNhcjo6cmVjb2RlKHRhcmdldF9jbGVhbl8xLCBpbml0aWF0b3JfdGFyZ2V0X21hc3Rlcl9jbGVhbikpICU+JVxuICBtdXRhdGUodGFyZ2V0X2NsZWFuXzJfYWdnbG93ID0gY2FyOjpyZWNvZGUodGFyZ2V0X2NsZWFuXzIsIGluaXRpYXRvcl90YXJnZXRfbWFzdGVyX2NsZWFuKSkgJT4lXG4gIG11dGF0ZSh0YXJnZXRfY2xlYW5fM19hZ2dsb3cgPSBjYXI6OnJlY29kZSh0YXJnZXRfY2xlYW5fMywgaW5pdGlhdG9yX3RhcmdldF9tYXN0ZXJfY2xlYW4pKVxuXG5sb3dsZXZlbGFnZyA8LSBjKFxuICBcImNodXJjaFwiLCBcImtlbnlhIHBvbGljZVwiLCBcIm1lZGljaW5lXCIsIFwidHJpYmFsIHBvbGljZSByZXNlcnZlXCIsIFwiZGV0YWluZWVzXCIsIFwia2VueWEgcmVnaW1lbnRcIiwgXCJvdGhlciB3ZWFwb25zXCIsXG4gIFwicGFyYW1pbGl0YXJ5XCIsIFwiYW1tdW5pdGlvblwiLCBcImNvbW11bml0aWVzXCIsIFwiYnJpdGlzaCBtaWxpdGFyeVwiLCBcIm1pbGl0YXJ5IChnZW5lcmljKVwiLCBcInRyaWJhbCBhdXRob3JpdGllc1wiLCBcImtlbnlhIHBvbGljZSByZXNlcnZlXCIsIFwidHJpYmFsIHBvbGljZVwiLFxuICBcIktpbmdzIEFmcmljYW4gUmlmbGVzXCIsIFwiaW5mcmFzdHJ1Y3R1cmVcIiwgXCJzY2hvb2xcIiwgXCJjYXNoXCIsIFwiY29sb25pYWwgYXV0aG9yaXRpZXNcIiwgXCJwb2xpY2UgKGdlbmVyaWMpXCIsIFwic3VwcGxpZXNcIiwgXCJmaXJlYXJtc1wiLCBcImZvb2RcIiwgXCJwcml2YXRlIHByb3BlcnR5XCIsXG4gIFwiaG9tZSBndWFyZFwiLCBcImNpdmlsaWFuc1wiLCBcImxpdmVzdG9ja1wiLCBcInN1c3BlY3RlZCBpbnN1cmdlbnRzXCJcbilcblxuZXZlbnRzIDwtIGV2ZW50cyAlPiVcbiAgIG11dGF0ZSh0YXJnZXRfY2xlYW5fMV9hZ2dsb3c9aWZlbHNlKHRhcmdldF9jbGVhbl8xX2FnZ2xvdyAgJWluJSBsb3dsZXZlbGFnZyAmICFpcy5uYSh0YXJnZXRfY2xlYW5fMV9hZ2dsb3cpLHRhcmdldF9jbGVhbl8xX2FnZ2xvdywgXCJ1bmNhdGVnb3JpemVkXCIpKSAlPiVcbiAgIG11dGF0ZSh0YXJnZXRfY2xlYW5fMl9hZ2dsb3c9aWZlbHNlKHRhcmdldF9jbGVhbl8yX2FnZ2xvdyAgJWluJSBsb3dsZXZlbGFnZyAmICFpcy5uYSh0YXJnZXRfY2xlYW5fMl9hZ2dsb3cpLHRhcmdldF9jbGVhbl8yX2FnZ2xvdywgXCJ1bmNhdGVnb3JpemVkXCIpKSAlPiUgICAgbXV0YXRlKHRhcmdldF9jbGVhbl8zX2FnZ2xvdz1pZmVsc2UodGFyZ2V0X2NsZWFuXzNfYWdnbG93ICAlaW4lIGxvd2xldmVsYWdnICYgIWlzLm5hKHRhcmdldF9jbGVhbl8zX2FnZ2xvdyksdGFyZ2V0X2NsZWFuXzNfYWdnbG93LCBcInVuY2F0ZWdvcml6ZWRcIikpXG5cbmV2ZW50cyR0YXJnZXRfY2xlYW5fMV9hZ2dsb3cgJT4lXG4gIGphbml0b3I6OnRhYnlsKHNvcnQgPSBUUlVFKSAlPiVcbiAgamFuaXRvcjo6YWRvcm5fY3Jvc3N0YWIoZGlnaXRzID0gMSlcbmBgYCJ9 -->

```r
events <- events %>%
  mutate(target_clean_1_agglow = car::recode(target_clean_1, initiator_target_master_clean)) %>%
  mutate(target_clean_2_agglow = car::recode(target_clean_2, initiator_target_master_clean)) %>%
  mutate(target_clean_3_agglow = car::recode(target_clean_3, initiator_target_master_clean))

lowlevelagg <- c(
  "church", "kenya police", "medicine", "tribal police reserve", "detainees", "kenya regiment", "other weapons",
  "paramilitary", "ammunition", "communities", "british military", "military (generic)", "tribal authorities", "kenya police reserve", "tribal police",
  "Kings African Rifles", "infrastructure", "school", "cash", "colonial authorities", "police (generic)", "supplies", "firearms", "food", "private property",
  "home guard", "civilians", "livestock", "suspected insurgents"
)

events <- events %>%
   mutate(target_clean_1_agglow=ifelse(target_clean_1_agglow  %in% lowlevelagg & !is.na(target_clean_1_agglow),target_clean_1_agglow, "uncategorized")) %>%
   mutate(target_clean_2_agglow=ifelse(target_clean_2_agglow  %in% lowlevelagg & !is.na(target_clean_2_agglow),target_clean_2_agglow, "uncategorized")) %>%    mutate(target_clean_3_agglow=ifelse(target_clean_3_agglow  %in% lowlevelagg & !is.na(target_clean_3_agglow),target_clean_3_agglow, "uncategorized"))

events$target_clean_1_agglow %>%
  janitor::tabyl(sort = TRUE) %>%
  janitor::adorn_crosstab(digits = 1)
```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->




<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxuXG5ldmVudHNbLCBjKFwidGFyZ2V0X2NsZWFuXzFfYWdnbWVkXCIsIFwidGFyZ2V0X2NsZWFuXzJfYWdnbWVkXCIsIFwidGFyZ2V0X2NsZWFuXzNfYWdnbWVkXCIpXSA8LVxuICBldmVudHNbLCBjKFwidGFyZ2V0X2NsZWFuXzFfYWdnbG93XCIsIFwidGFyZ2V0X2NsZWFuXzJfYWdnbG93XCIsIFwidGFyZ2V0X2NsZWFuXzNfYWdnbG93XCIpXVxuZXZlbnRzIDwtIGV2ZW50cyAlPiVcbiAgbXV0YXRlX2F0KFxuICAgIHZhcnMoc3RhcnRzX3dpdGgoXCJpbml0aWF0b3JfY2xlYW5fMV9hZ2dtZWR8aW5pdGlhdG9yX2NsZWFuXzJfYWdnbWVkfGluaXRpYXRvcl9jbGVhbl8zX2FnZ21lZFwiKSksXG4gICAgLmZ1bnMgPSBmdW5zKGNhcjo6cmVjb2RlKHRlbXAsIFwiXG4gICAgIGMoJ2NpZCcsJ2tlbnlhIHBvbGljZSByZXNlcnZlJywna2VueWEgcG9saWNlJywncG9saWNlIChnZW5lcmljKScsJ3JhaWx3YXkgcG9saWNlJyxcbidzcGVjaWFsIGJyYW5jaCcsJ3RyaWJhbCBwb2xpY2UnLCd0cmliYWwgcG9saWNlIHJlc2VydmUnKSA9ICdwb2xpY2UnO1xuICAgICBjKCdhcmFiIGNvbWJhdCB1bml0cycsJ2FzaWFuIGNvbWJhdCB1bml0cycsJ2JyaXRpc2ggbWlsaXRhcnknLCdLaW5ncyBBZnJpY2FuIFJpZmxlcycsXG4na2VueWEgcmVnaW1lbnQnLCdtaWxpdGFyeSAoZ2VuZXJpYyknLCdwc3VlZG8gZ2FuZ3MnLCdyb3lhbCBhaXIgZm9yY2UnKSA9ICdtaWxpdGFyeSc7XG4gICAgIGMoJ2NvbG9uaWFsIGF1dGhvcml0aWVzJywgJ3RyaWJhbCBhdXRob3JpdGllcycpPSdjaXZpbCBhdXRob3JpdGllcyc7XG4gICAgIGMoJ2FtbXVuaXRpb24nLCdmaXJlYXJtcycsJ290aGVyIHdlYXBvbnMnKT0nYXJtYW1lbnRzJztcbiAgICAgYygnY2FzaCcsJ2Zvb2QnLCdsaXZlc3RvY2snLCdtZWRpY2luZScsJ3N1cHBsaWVzJyk9J3Byb3Zpc2lvbnMnO1xuICAgICBjKCdjaHVyY2gnLCdzY2hvb2wnLCdpbmZyYXN0cnVjdHVyZScpPSdwdWJsaWMgYnVpbGRpbmdzJztcbiAgICAgICAgICBcIikpXG4gIClcblxuZXZlbnRzJGluaXRpYXRvcl9jbGVhbl8xX2FnZ21lZCAlPiVcbiAgamFuaXRvcjo6dGFieWwoc29ydCA9IFRSVUUpICU+JVxuICBqYW5pdG9yOjphZG9ybl9jcm9zc3RhYihkaWdpdHMgPSAxKVxuYGBgIn0= -->

```r

events[, c("target_clean_1_aggmed", "target_clean_2_aggmed", "target_clean_3_aggmed")] <-
  events[, c("target_clean_1_agglow", "target_clean_2_agglow", "target_clean_3_agglow")]
events <- events %>%
  mutate_at(
    vars(starts_with("initiator_clean_1_aggmed|initiator_clean_2_aggmed|initiator_clean_3_aggmed")),
    .funs = funs(car::recode(temp, "
     c('cid','kenya police reserve','kenya police','police (generic)','railway police',
'special branch','tribal police','tribal police reserve') = 'police';
     c('arab combat units','asian combat units','british military','Kings African Rifles',
'kenya regiment','military (generic)','psuedo gangs','royal air force') = 'military';
     c('colonial authorities', 'tribal authorities')='civil authorities';
     c('ammunition','firearms','other weapons')='armaments';
     c('cash','food','livestock','medicine','supplies')='provisions';
     c('church','school','infrastructure')='public buildings';
          "))
  )

events$initiator_clean_1_aggmed %>%
  janitor::tabyl(sort = TRUE) %>%
  janitor::adorn_crosstab(digits = 1)
```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->



<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxuXG5ldmVudHNbLCBjKFwidGFyZ2V0X2NsZWFuXzFfYWdnaGlnaFwiLCBcInRhcmdldF9jbGVhbl8yX2FnZ2hpZ2hcIiwgXCJ0YXJnZXRfY2xlYW5fM19hZ2doaWdoXCIpXSA8LVxuICBldmVudHNbLCBjKFwidGFyZ2V0X2NsZWFuXzFfYWdnbWVkXCIsIFwidGFyZ2V0X2NsZWFuXzJfYWdnbWVkXCIsIFwidGFyZ2V0X2NsZWFuXzNfYWdnbWVkXCIpXVxuZXZlbnRzIDwtIGV2ZW50cyAlPiVcbiAgbXV0YXRlX2F0KFxuICAgIHZhcnMoc3RhcnRzX3dpdGgoXCJ0YXJnZXRfY2xlYW5fMV9hZ2doaWdofHRhcmdldF9jbGVhbl8yX2FnZ2hpZ2h8dGFyZ2V0X2NsZWFuXzNfYWdnaGlnaFwiKSksXG4gICAgLmZ1bnMgPSBmdW5zKGNhcjo6cmVjb2RlKFwiXG4gICAgICAgICAgICAgICAgICBjKCdjaXZpbCBhdXRob3JpdGllcycsICdob21lIGd1YXJkJywgJ21pbGl0YXJ5JywgJ3BvbGljZScsICdwYXJhbWlsaXRhcnknKSA9J2dvdmVybm1lbnQnO1xuICAgICAgICAgICAgICAgICAgYygnc3VzcGVjdGVkIGluc3VyZ2VudHMnLCdkZXRhaW5lZXMnKSA9J3JlYmVscyc7XG4gICAgICAgICAgICAgICAgICBjKCdhcm1hbWVudHMnLCdwcml2YXRlIHByb3BlcnR5JywncHJvdmlzaW9ucycsJ3B1YmxpYyBidWlsZGluZ3MnKSA9J3Byb3BlcnR5JztcbiAgICAgICAgICAgICAgICAgIGMoJ2NvbW11bml0aWVzJywnY29tbXVuaXRpZXMnKT0nY2l2aWxpYW5zJztcbiAgICAgICAgICBcIikpXG4gIClcblxuZXZlbnRzJHRhcmdldF9jbGVhbl8xX2FnZ2hpZ2ggJT4lXG4gIGphbml0b3I6OnRhYnlsKHNvcnQgPSBUUlVFKSAlPiVcbiAgamFuaXRvcjo6YWRvcm5fY3Jvc3N0YWIoZGlnaXRzID0gMSlcbmBgYCJ9 -->

```r

events[, c("target_clean_1_agghigh", "target_clean_2_agghigh", "target_clean_3_agghigh")] <-
  events[, c("target_clean_1_aggmed", "target_clean_2_aggmed", "target_clean_3_aggmed")]
events <- events %>%
  mutate_at(
    vars(starts_with("target_clean_1_agghigh|target_clean_2_agghigh|target_clean_3_agghigh")),
    .funs = funs(car::recode("
                  c('civil authorities', 'home guard', 'military', 'police', 'paramilitary') ='government';
                  c('suspected insurgents','detainees') ='rebels';
                  c('armaments','private property','provisions','public buildings') ='property';
                  c('communities','communities')='civilians';
          "))
  )

events$target_clean_1_agghigh %>%
  janitor::tabyl(sort = TRUE) %>%
  janitor::adorn_crosstab(digits = 1)
```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->




## Count of Initiators

Helper function for recoding


<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxuXG5yZWNvZGVyRnVuYyA8LSBmdW5jdGlvbihkYXRhLCBvbGR2YWx1ZSwgbmV3dmFsdWUpIHtcbiAgIyBjb252ZXJ0IGFueSBmYWN0b3JzIHRvIGNoYXJhY3RlcnNcbiAgaWYgKGlzLmZhY3RvcihkYXRhKSkgZGF0YSA8LSBhcy5jaGFyYWN0ZXIoZGF0YSlcbiAgaWYgKGlzLmZhY3RvcihvbGR2YWx1ZSkpIG9sZHZhbHVlIDwtIGFzLmNoYXJhY3RlcihvbGR2YWx1ZSlcbiAgaWYgKGlzLmZhY3RvcihuZXd2YWx1ZSkpIG5ld3ZhbHVlIDwtIGFzLmNoYXJhY3RlcihuZXd2YWx1ZSlcblxuICAjIGNyZWF0ZSB0aGUgcmV0dXJuIHZlY3RvclxuICBuZXd2ZWMgPC0gZGF0YVxuICAjIHB1dCByZWNvZGVkIHZhbHVlcyBpbnRvIHRoZSBjb3JyZWN0IHBvc2l0aW9uIGluIHRoZSByZXR1cm4gdmVjdG9yXG4gIGZvciAoaSBpbiB1bmlxdWUob2xkdmFsdWUpKSBuZXd2ZWNbZGF0YSAlaW4lIGldIDwtIG5ld3ZhbHVlW29sZHZhbHVlICVpbiUgaV1cbiAgbmV3dmVjXG59XG5gYGAifQ== -->

```r

recoderFunc <- function(data, oldvalue, newvalue) {
  # convert any factors to characters
  if (is.factor(data)) data <- as.character(data)
  if (is.factor(oldvalue)) oldvalue <- as.character(oldvalue)
  if (is.factor(newvalue)) newvalue <- as.character(newvalue)

  # create the return vector
  newvec <- data
  # put recoded values into the correct position in the return vector
  for (i in unique(oldvalue)) newvec[data %in% i] <- newvalue[oldvalue %in% i]
  newvec
}
```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->



<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxuXG4jIFRoZXNlIG51bWJlcnMgYXJlIGltcHJvdmlzZWQgYW5kIGNhbiBiZSBjaGFuZ2VkXG5hY291cGxlIDwtIDJcbmFmZXcgPC0gM1xuYWdhbmcgPC0gNlxuYWdhbmdfbGFyZ2UgPC0gMTJcblxucmVjb2RpbmdzIDwtIGMoXG4gIFwiMTAwK1wiLCBcIjEwMFwiLFxuICBcIj8/XCIsIFwiXCIsXG4gIFwiMSBiYWdcIiwgXCIxXCIsXG4gIFwiMSBibGFua2V0XCIsIFwiMVwiLFxuICBcIjEgYnVybnQgZG93blwiLCBcIjFcIixcbiAgXCIxIGNpdmlsaWFuXCIsIFwiMVwiLFxuICBcIjEgY293LCA2IHNoZWVwXCIsIFwiN1wiLFxuICBcIjEgY293XCIsIFwiMVwiLFxuICBcIjEgZ29hdCwgY2xvdGhpbmdcIiwgXCIxXCIsXG4gIFwiMSBnb2F0XCIsIFwiMVwiLFxuICBcIjEgbG9vdGVkXCIsIFwiMVwiLFxuICBcIjEgbG9vdGVkXCIsIFwiMVwiLFxuICBcIjEgb3hcIiwgXCIxXCIsXG4gIFwiMSBzaGVlcCBhbmQgY2hpY2tlbnNcIiwgXCIxXCIsXG4gIFwiMSBzaGVlcCwgc29tZSBjaGlja2Vuc1wiLCBcIjFcIixcbiAgXCIxIHNoZWVwXCIsIFwiMVwiLFxuICBcIjEgc2hvdGd1biAsMzAgcm91bmRzXCIsIFwiMzFcIixcbiAgXCIxIHNob3RndW4gKyAxMHJkc1wiLCBcIjExXCIsXG4gIFwiMSBzdGVlclwiLCBcIjFcIixcbiAgXCIxIHZpbGxhZ2UsIDEgbWFya2V0XCIsIFwiMVwiLFxuICBcIjEgd291bmRlZFwiLCBcIjFcIixcbiAgXCIxIHdyZWNrZWRcIiwgXCIxXCIsXG4gIFwiMStcIiwgXCIxXCIsXG4gIFwiMSszXCIsIFwiNFwiLFxuICBcIjErc29tZVwiLCBcIjFcIixcbiAgXCIxMCBhY3Jlc1wiLCBcIjEwXCIsXG4gIFwiMTAgYmFnc1wiLCBcIjEwXCIsXG4gIFwiMTAgY2F0dGxlXCIsIFwiMTBcIixcbiAgXCIxMCBzYWNrc1wiLCBcIjEwXCIsXG4gIFwiMTAgdG8gMTJcIiwgXCIxMVwiLFxuICBcIjEwIHRvIDE1XCIsIFwiMTNcIixcbiAgXCIxMC8xNC8yMDEzXCIsIFwiXCIsXG4gIFwiMTAvMTUvMjAxM1wiLCBcIlwiLFxuICBcIjEwLzIwLzIwMTNcIiwgXCJcIixcbiAgXCIxMDAgbGJcIiwgXCIxMDBcIixcbiAgXCIxMDAtMTMwXCIsIFwiMTE1XCIsXG4gIFwiMTAwLTE1MFwiLCBcIjEyNVwiLFxuICBcIjEwMCtcIiwgMTAwLFxuICBcIjEwMDAwXCIsIFwiXCIsXG4gIFwiMTA5IGNhdHRsZVwiLCBcIjEwOVwiLFxuICBcIjEwYmFncyBwb3RhdG9lc1wiLCBcIjEwXCIsXG4gIFwiMTEgY2F0dGxlXCIsIFwiMTFcIixcbiAgXCIxMSBzaGVlcFwiLCBcIjExXCIsXG4gIFwiMTEyIGJvcmUgJiAyMC4xLjQ1ICY3IHJkc1wiLCBcIjExMlwiLFxuICBcIjEyIGJhZ3NcIiwgXCIxMlwiLFxuICBcIjEyIGNhdHRsZVwiLCBcIjEyXCIsXG4gIFwiMTIgZ29hdHNcIiwgXCIxMlwiLFxuICBcIjEyIHRvIDE1XCIsIFwiMTNcIixcbiAgXCIxMiB0byAyMFwiLCBcIjE3XCIsXG4gIFwiMTIvMTQvMjAxM1wiLCBcIlwiLFxuICBcIjEyMCBjYXR0bGVcIiwgXCIxMjBcIixcbiAgXCIxMjArMVwiLCBcIjEyMVwiLFxuICBcIjEzIHNoZWVwXCIsIFwiMTNcIixcbiAgXCIxMy0xNVwiLCBcIjE0XCIsXG4gIFwiMTMwMCB3b3J0aFwiLCBcIjEzMDBcIixcbiAgXCIxNCBjYXR0bGVcIiwgXCIxNFwiLFxuICBcIjE0IGdvYXRzXCIsIFwiMTRcIixcbiAgXCIxNCBoZWFkXCIsIFwiMTRcIixcbiAgXCIxNCtcIiwgXCIxNFwiLFxuICBcIjE1IC0gMjBcIiwgXCIxOFwiLFxuICBcIjE1IGNhdHRsZVwiLCBcIjE1XCIsXG4gIFwiMTUgdG8gMjBcIiwgXCIxN1wiLFxuICBcIjE1IHRvIDIwXCIsIFwiMTdcIixcbiAgXCIxNSB0byAyNVwiLCBcIjIwXCIsXG4gIFwiMTUtMjBcIiwgXCIxN1wiLFxuICBcIjE1K1wiLCBcIjE1XCIsXG4gIFwiMTUwLTIwMFwiLCBcIjE3NVwiLFxuICBcIjE1MCtcIiwgXCIxNTBcIixcbiAgXCIxNTEgY2F0dGxlXCIsIFwiMTUxXCIsXG4gIFwiMTcgY2F0dGxlXCIsIFwiMTdcIixcbiAgXCIxNzIgYmFncyBidXJudFwiLCBcIjE3MlwiLFxuICBcIjE4IGNhdHRsZVwiLCBcIjE4XCIsXG4gIFwiMTkgYmFnc1wiLCBcIjE5XCIsXG4gIFwiMTk2IHJvdW5kc1wiLCBcIjE5NlwiLFxuICBcIjIgYmFncyBtYWl6ZVwiLCBcIjJcIixcbiAgXCIyIGJhZ3NcIiwgXCIyXCIsXG4gIFwiMiBiYWdzXCIsIFwiMlwiLFxuICBcIjIgYnVja2V0c1wiLCBcIjJcIixcbiAgXCIyIGNhdHRsZSBoYW1zdHJ1bmdcIiwgXCIyXCIsXG4gIFwiMiBjYXR0bGUsIGNvcm5cIiwgXCIzXCIsXG4gIFwiMiBjYXR0bGVcIiwgXCIyXCIsXG4gIFwiMiBjb3dzXCIsIFwiMlwiLFxuICBcIjIgZGViYmllc1wiLCBcIjJcIixcbiAgXCIyIGdvYXRzXCIsIFwiMlwiLFxuICBcIjIgZ3JvdXBzXCIsIFwiMlwiLFxuICBcIjIgaHV0cyBidXJudFwiLCBcIjJcIixcbiAgXCIyIHNoZWVwXCIsIFwiMlwiLFxuICBcIjIgd2F0Y2hlcywgY2FzaFwiLCBcIjJcIixcbiAgXCIyLzMvMjAxM1wiLCBcIlwiLFxuICBcIjIrXCIsIFwiMlwiLFxuICBcIjIwIGJhZ3MgbWFpemUsIDkgZ29hdHMsIDMyIGNoaWNrZW5zIGFuZCBkdWNrcywgY2FzaFwiLCBcIjYwXCIsXG4gIFwiMjAgYmFnc1wiLCBcIjIwXCIsXG4gIFwiMjAgY2F0dGxlXCIsIFwiMjBcIixcbiAgXCIyMCBnb2F0c1wiLCBcIjIwXCIsXG4gIFwiMjAgc2hlZXBcIiwgXCIyMFwiLFxuICBcIjIwIHRvIDI1XCIsIFwiMjNcIixcbiAgXCIyMCB0byAzMFwiLCBcIjI1XCIsXG4gIFwiMjAgdG8gNDBcIiwgXCIzMFwiLFxuICBcIjIwLTI1XCIsIFwiMjNcIixcbiAgXCIyMC0zMFwiLCBcIjI1XCIsXG4gIFwiMjAtMzVcIiwgXCIzMFwiLFxuICBcIjIwLTUwXCIsIFwiMzVcIixcbiAgXCIyMC8zMFwiLCBcIjI1XCIsXG4gIFwiMjAvMzBcIiwgXCIyNVwiLFxuICBcIjIwK1wiLCBcIjIwXCIsXG4gIFwiMjAwIHlkc1wiLCBcIjIwMFwiLFxuICBcIjIwMC0zMDBcIiwgXCIyNTBcIixcbiAgXCIyMDArXCIsIFwiMjAwXCIsXG4gIFwiMjAwMCBhY3Jlc1wiLCBcIjIwMDBcIixcbiAgXCIyMSBnb2F0c1wiLCBcIjIxXCIsXG4gIFwiMjEgaGVhZFwiLCBcIjIxXCIsXG4gIFwiMjIgY2F0dGxlXCIsIFwiMjJcIixcbiAgXCIyNSB0byAzMFwiLCBcIjI4XCIsXG4gIFwiMjUtMzBcIiwgXCIyN1wiLFxuICBcIjI1LTMwXCIsIFwiMjdcIixcbiAgXCIyOCBraWxsZWRcIiwgXCIyOFwiLFxuICBcIjI4IHNoZWVwXCIsIFwiMjhcIixcbiAgXCIzIGJhZ3NcIiwgXCIzXCIsXG4gIFwiMyBiYWdzXCIsIFwiM1wiLFxuICBcIjMgYmlrZXNcIiwgXCIzXCIsXG4gIFwiMyBjYXR0bGVcIiwgXCIzXCIsXG4gIFwiMyBjYXR0bGVcIiwgXCIzXCIsXG4gIFwiMyBnb2F0c1wiLCBcIjNcIixcbiAgXCIzIG9yIDRcIiwgXCIzXCIsXG4gIFwiMyBvciA0XCIsIFwiM1wiLFxuICBcIjMgcGFuZ2FzXCIsIFwiM1wiLFxuICBcIjMgc2hlZXAsIDIgY2FsdmVzXCIsIFwiNVwiLFxuICBcIjMgc2hlZXBcIiwgXCIzXCIsXG4gIFwiMyB0byA0XCIsIFwiM1wiLFxuICBcIjMgdG8gNFwiLCBcIjNcIixcbiAgXCIzLzEwLzIwMTNcIiwgXCJcIixcbiAgXCIzLzQvMjAxM1wiLCBcIlwiLFxuICBcIjMvNS8yMDEzXCIsIFwiXCIsXG4gIFwiMy82LzIwMTNcIiwgXCJcIixcbiAgXCIzK1wiLCBcIjNcIixcbiAgXCIzKzMrMSsyXCIsIFwiOVwiLFxuICBcIjMrc29tZVwiLCBcIjNcIixcbiAgXCIzMCBhY3Jlc1wiLCBcIjMwXCIsXG4gIFwiMzAgY2F0dGxlXCIsIFwiMzBcIixcbiAgXCIzMCB0byA0MFwiLCBcIjM1XCIsXG4gIFwiMzAtMzVcIiwgXCIzM1wiLFxuICBcIjMwLTQwXCIsIFwiMzVcIixcbiAgXCIzMC01MFwiLCBcIjQwXCIsXG4gIFwiMzArXCIsIFwiMzBcIixcbiAgXCIzMDAtNDAwXCIsIFwiMzUwXCIsXG4gIFwiMzAwK1wiLCBcIjMwMFwiLFxuICBcIjM1IGJhZ3NcIiwgXCIzNVwiLFxuICBcIjM1IHRvIDQwXCIsIFwiMzdcIixcbiAgXCIzOCBjYXR0bGVcIiwgXCIzOFwiLFxuICBcIjNvciA0XCIsIFwiM1wiLFxuICBcIjQgYmFncyBwb3RhdG9lc1wiLCBcIjRcIixcbiAgXCI0IGJhZ3NcIiwgXCI0XCIsXG4gIFwiNCBnb2F0c1wiLCBcIjRcIixcbiAgXCI0IGdyb3Vwc1wiLCBcIlwiLFxuICBcIjQgb3IgNVwiLCBcIjRcIixcbiAgXCI0IG94ZW5cIiwgXCI0XCIsXG4gIFwiNCBzaGVlcFwiLCBcIjRcIixcbiAgXCI0IHRvIDhcIiwgXCI2XCIsXG4gIFwiNC82LzIwMTNcIiwgXCJcIixcbiAgXCI0MCBiYWdcIiwgXCI0MFwiLFxuICBcIjQwIGNhdHRsZVwiLCBcIjQwXCIsXG4gIFwiNDAgc2Fja3NcIiwgXCI0MFwiLFxuICBcIjQwIHNoZWVwXCIsIFwiNDBcIixcbiAgXCI0MCB0byA1MFwiLCBcIjQ1XCIsXG4gIFwiNDAvNTBcIiwgXCI0NVwiLFxuICBcIjQwMCBjYXR0bGVcIiwgXCI0MDBcIixcbiAgXCI0MDAwXCIsIFwiXCIsXG4gIFwiNDQgY2F0dGxlXCIsIFwiNDRcIixcbiAgXCI1IGJhZ3NcIiwgXCI1XCIsXG4gIFwiNSBjYWx2ZXNcIiwgXCI1XCIsXG4gIFwiNSBjYXR0bGVcIiwgXCI1XCIsXG4gIFwiNSBkZXN0cm95ZWRcIiwgXCI1XCIsXG4gIFwiNSBnb2F0c1wiLCBcIjVcIixcbiAgXCI1IGtpbGxlZFwiLCBcIjVcIixcbiAgXCI1IG9yIDZcIiwgXCI1XCIsXG4gIFwiNSBzaGVlcCwgMSBveFwiLCBcIjZcIixcbiAgXCI1IHNoZWVwXCIsIFwiNVwiLFxuICBcIjUgdG8gNlwiLCBcIjVcIixcbiAgXCI1LzEwLzIwMTNcIiwgXCJcIixcbiAgXCI1LzYvMjAxM1wiLCBcIlwiLFxuICBcIjUwIGNhdHRsZVwiLCBcIjUwXCIsXG4gIFwiNTAgdG8gNjBcIiwgXCI1NVwiLFxuICBcIjUwLTEwMFwiLCBcIjc1XCIsXG4gIFwiNTAtNjBcIiwgXCI1NVwiLFxuICBcIjUwLTc1XCIsIFwiNjJcIixcbiAgXCI1MCtcIiwgXCI1MFwiLFxuICBcIjUwK1wiLCBcIjUwXCIsXG4gIFwiNTAwMCBhY3Jlc1wiLCBcIjUwMDBcIixcbiAgXCI1MTkgK1wiLCBcIjUxOVwiLFxuICBcIjUzIGRldGFpbmVkXCIsIFwiNTNcIixcbiAgXCI1NCBzaGVlcCBhbmQgZ29hdHNcIiwgXCI1NFwiLFxuICBcIjU2IGNvbW1pdHRlZSBtZW1iZXJzXCIsIFwiNTZcIixcbiAgXCI2IGJhZ1wiLCBcIjZcIixcbiAgXCI2IGJhZ3NcIiwgXCI2XCIsXG4gIFwiNiBjYXR0bGVcIiwgXCI2XCIsXG4gIFwiNiBjYXR0bGVcIiwgXCI2XCIsXG4gIFwiNiBnb2F0c1wiLCBcIjZcIixcbiAgXCI2IG9yIDdcIiwgXCI2XCIsXG4gIFwiNiBzaGVlcCBhbmQgZ29hdHNcIiwgXCI2XCIsXG4gIFwiNiBzaGVlcFwiLCBcIjZcIixcbiAgXCI2IHRvIDdcIiwgXCI2XCIsXG4gIFwiNiB0byA4XCIsIFwiN1wiLFxuICBcIjYgdG8gOVwiLCBcIjhcIixcbiAgXCI2LTggbWFuXCIsIFwiN1wiLFxuICBcIjYvMTAvMjAxM1wiLCBcIlwiLFxuICBcIjYvOC8yMDEzXCIsIFwiXCIsXG4gIFwiNjAtMTAwXCIsIFwiODBcIixcbiAgXCI2MC03MFwiLCBcIjY1XCIsXG4gIFwiNjQgY2F0dGxlXCIsIFwiNjRcIixcbiAgXCI3IGJhZ3NcIiwgXCI3XCIsXG4gIFwiNyBjYXR0bGVcIiwgXCI3XCIsXG4gIFwiNyBzaGVlcFwiLCBcIjdcIixcbiAgXCI3LzEwLzIwMTNcIiwgXCJcIixcbiAgXCI3MCBiYWdzXCIsIFwiNzBcIixcbiAgXCI3MCBjYXR0bGUsIHNoZWVwXCIsIFwiNzBcIixcbiAgXCI3MC0xMDBcIiwgXCI4NVwiLFxuICBcIjcwMDAwXCIsIFwiXCIsXG4gIFwiNzUgcm91bmRzXCIsIFwiNzVcIixcbiAgXCI4IGJhZ3MgcG90YXRvZXNcIiwgXCI4XCIsXG4gIFwiOCBjYXR0bGVcIiwgXCI4XCIsXG4gIFwiOCBjb3dzIHNsYXNoZWRcIiwgXCI4XCIsXG4gIFwiOCBjb3dzXCIsIFwiOFwiLFxuICBcIjggc2hlZXBcIiwgXCI4XCIsXG4gIFwiOCB0byAxMFwiLCBcIjlcIixcbiAgXCI4LzEwLzIwMTNcIiwgXCJcIixcbiAgXCI4MCBjYXR0bGVcIiwgXCI4MFwiLFxuICBcIjgwLTEwMFwiLCBcIjkwXCIsXG4gIFwiODQgc2hlZXAsIDEgY293LCA1IGNoaWNrZW5zXCIsIFwiOTBcIixcbiAgXCI5IGNhdHRsZVwiLCBcIjlcIixcbiAgXCI5IHNoZWVwXCIsIFwiOVwiLFxuICBcIjkgdG8gMTBcIiwgXCI5XCIsXG4gIFwiOSs5XCIsIFwiMThcIixcbiAgXCI5MDAobm90IGNsZWFyKVwiLCBcIjkwMFwiLFxuICBcImFsbCBsb2NhbHNcIiwgXCJcIixcbiAgXCJhbGxcIiwgXCJcIixcbiAgXCJhcHAgNVwiLCBcIjVcIixcbiAgXCJhcHAuIDEwMFwiLCBcIjEwMFwiLFxuICBcImFwcC4gMTIwXCIsIFwiMTIwXCIsXG4gIFwiYXJtZWQgZ2FuZ1wiLCBhZ2FuZyxcbiAgXCJiYW5kXCIsIGFnYW5nLFxuICBcImJhbmRzXCIsIFwiXCIsXG4gIFwiY2F0dGxlIHNsYXNoaW5nXCIsIFwiXCIsXG4gIFwiY2xvdGhpbmdcIiwgXCJcIixcbiAgXCJjb25zaWRlcmFibGUgcXVhbnRpdHlcIiwgXCJcIixcbiAgXCJmYWlybHkgbGFyZ2UgZ2FuZ1wiLCBhZ2FuZ19sYXJnZSxcbiAgXCJmZXcgYmFnc1wiLCBcIlwiLFxuICBcImZld1wiLCBcIlwiLFxuICBcImZvb2RcIiwgXCJcIixcbiAgXCJnYW5nXCIsIGFnYW5nLFxuICBcImdhbmdzXCIsIGFnYW5nX2xhcmdlLFxuICBcImd1YXJkc1wiLCBhZmV3LFxuICBcImhhbGYgdmlsbGFnZVwiLCBcIlwiLFxuICBcImxhYm91clwiLCBcIlwiLFxuICBcImxhcmdlIGNyb3dkXCIsIFwiXCIsXG4gIFwibGFyZ2UgZm9yY2VcIiwgYWdhbmdfbGFyZ2UsXG4gIFwibGFyZ2UgZ2FuZ1wiLCBhZ2FuZ19sYXJnZSxcbiAgXCJsYXJnZSBtZWV0aW5nXCIsIFwiXCIsXG4gIFwibGFyZ2UgbnVtYmVyXCIsIFwiXCIsXG4gIFwibGFyZ2UgbnVtYmVyc1wiLCBcIlwiLFxuICBcImxhcmdlIHF1YW50aXRpZXNcIiwgXCJcIixcbiAgXCJsYXJnZSBxdWFudGl0eVwiLCBcIlwiLFxuICBcImxhcmdlIHJlLW9hdGhpbmcgY2VyZW1vbnlcIiwgXCJcIixcbiAgXCJsYXJnZSBzY2FsZVwiLCBcIlwiLFxuICBcImxhcmdlXCIsIGFnYW5nX2xhcmdlLFxuICBcImxhcmdpc2ggZ2FuZ1wiLCBhZ2FuZ19sYXJnZSxcbiAgXCJsb2NhbCBwb3B1bGFjZVwiLCBcIlwiLFxuICBcIm1hbnkgdGhvdXNhbmRcIiwgXCIyMDAwXCIsXG4gIFwibW9iXCIsIFwiXCIsXG4gIFwibm90IGdpdmVuXCIsIFwiXCIsXG4gIFwibnVtYmVyXCIsIFwiXCIsXG4gIFwib2NjdXBhbnRzXCIsIFwiXCIsXG4gIFwib3ZlciAyMDBcIiwgXCIyMDBcIixcbiAgXCJwYXJ0eVwiLCBcIlwiLFxuICBcInBhcnR5XCIsIGFnYW5nLFxuICBcInBhdHJvbFwiLCBhZ2FuZyxcbiAgXCJwb3Nob1wiLCBcIlwiLFxuICBcInBvdGF0b2VzXCIsIFwiXCIsXG4gIFwicXVhbnRpdHkgb2YgY2xvdGhpbmdcIiwgXCJcIixcbiAgXCJzZWN0aW9uXCIsIFwiXCIsXG4gIFwic2V2ZXJhbCBnYW5nc1wiLCBcImFnYW5nX2xhcmdlXCIsXG4gIFwic2V2ZXJhbFwiLCBcIjNcIixcbiAgXCJzaGVlcCBhbmQgZ29hdHNcIiwgXCJcIixcbiAgXCJzaHMgMiwzMDAvLVwiLCBcIjIzMDBcIixcbiAgXCJzaHMgNjAvLVwiLCBcIjYwXCIsXG4gIFwic2hzLiAxLDAwMFwiLCBcIjEwMDBcIixcbiAgXCJzaHMuIDE4XCIsIFwiMThcIixcbiAgXCJzaHMuIDMwXCIsIFwiMzBcIixcbiAgXCJzbWFsbCBnYW5nXCIsIGFnYW5nLFxuICBcInNtYWxsIGdhbmdzXCIsIFwiYWdhbmdcIixcbiAgXCJzbWFsbCBncm91cFwiLCBhZ2FuZyxcbiAgXCJzbWFsbCBwYXJ0eVwiLCBhZmV3LFxuICBcInNtYWxsXCIsIGFnYW5nLFxuICBcInNvbWVcIiwgYWZldyxcbiAgXCJzdWZmaWNpZW50IGZvb2RcIiwgXCJcIixcbiAgXCJ1bmtub3duXCIsIFwiXCIsXG4gIFwidmVyeSBsYXJnZSBnYW5nXCIsIFwiYWdhbmdfbGFyZ2VcIixcbiAgXCJ2aWxsYWdlcyBpbiBuZGlhLCBnaWNodWd1LCBlbWJ1IGRpdmlzaW9uc1wiLCBcIlwiLFxuICBcIndpdmVzXCIsIFwiXCJcbilcbnJlY29kaW5ncyA8LSBtYXRyaXgocmVjb2RpbmdzLCBuY29sID0gMiwgYnlyb3cgPSBUKVxuXG5ldmVudHMkaW5pdGlhdG9yX251bWJlcnNfbnVtZXJpYyA8LSBldmVudHMkaW5pdGlhdG9yX251bWJlcnMgJT4lIHJlY29kZXJGdW5jKC4sIHJlY29kaW5nc1ssIDFdLCByZWNvZGluZ3NbLCAyXSkgJT4lIGFzLm51bWVyaWMoKVxuZXZlbnRzJHRhcmdldF9udW1iZXJzX251bWVyaWMgPC0gZXZlbnRzJHRhcmdldF9udW1iZXJzICU+JSByZWNvZGVyRnVuYyguLCByZWNvZGluZ3NbLCAxXSwgcmVjb2RpbmdzWywgMl0pICU+JSBhcy5udW1lcmljKClcbmV2ZW50cyRhZmZlY3RlZF9jb3VudF9udW1lcmljIDwtIGV2ZW50cyRhZmZlY3RlZF9jb3VudCAlPiUgcmVjb2RlckZ1bmMoLiwgcmVjb2RpbmdzWywgMV0sIHJlY29kaW5nc1ssIDJdKSAlPiUgYXMubnVtZXJpYygpXG5cbmBgYCJ9 -->

```r

# These numbers are improvised and can be changed
acouple <- 2
afew <- 3
agang <- 6
agang_large <- 12

recodings <- c(
  "100+", "100",
  "??", "",
  "1 bag", "1",
  "1 blanket", "1",
  "1 burnt down", "1",
  "1 civilian", "1",
  "1 cow, 6 sheep", "7",
  "1 cow", "1",
  "1 goat, clothing", "1",
  "1 goat", "1",
  "1 looted", "1",
  "1 looted", "1",
  "1 ox", "1",
  "1 sheep and chickens", "1",
  "1 sheep, some chickens", "1",
  "1 sheep", "1",
  "1 shotgun ,30 rounds", "31",
  "1 shotgun + 10rds", "11",
  "1 steer", "1",
  "1 village, 1 market", "1",
  "1 wounded", "1",
  "1 wrecked", "1",
  "1+", "1",
  "1+3", "4",
  "1+some", "1",
  "10 acres", "10",
  "10 bags", "10",
  "10 cattle", "10",
  "10 sacks", "10",
  "10 to 12", "11",
  "10 to 15", "13",
  "10/14/2013", "",
  "10/15/2013", "",
  "10/20/2013", "",
  "100 lb", "100",
  "100-130", "115",
  "100-150", "125",
  "100+", 100,
  "10000", "",
  "109 cattle", "109",
  "10bags potatoes", "10",
  "11 cattle", "11",
  "11 sheep", "11",
  "112 bore & 20.1.45 &7 rds", "112",
  "12 bags", "12",
  "12 cattle", "12",
  "12 goats", "12",
  "12 to 15", "13",
  "12 to 20", "17",
  "12/14/2013", "",
  "120 cattle", "120",
  "120+1", "121",
  "13 sheep", "13",
  "13-15", "14",
  "1300 worth", "1300",
  "14 cattle", "14",
  "14 goats", "14",
  "14 head", "14",
  "14+", "14",
  "15 - 20", "18",
  "15 cattle", "15",
  "15 to 20", "17",
  "15 to 20", "17",
  "15 to 25", "20",
  "15-20", "17",
  "15+", "15",
  "150-200", "175",
  "150+", "150",
  "151 cattle", "151",
  "17 cattle", "17",
  "172 bags burnt", "172",
  "18 cattle", "18",
  "19 bags", "19",
  "196 rounds", "196",
  "2 bags maize", "2",
  "2 bags", "2",
  "2 bags", "2",
  "2 buckets", "2",
  "2 cattle hamstrung", "2",
  "2 cattle, corn", "3",
  "2 cattle", "2",
  "2 cows", "2",
  "2 debbies", "2",
  "2 goats", "2",
  "2 groups", "2",
  "2 huts burnt", "2",
  "2 sheep", "2",
  "2 watches, cash", "2",
  "2/3/2013", "",
  "2+", "2",
  "20 bags maize, 9 goats, 32 chickens and ducks, cash", "60",
  "20 bags", "20",
  "20 cattle", "20",
  "20 goats", "20",
  "20 sheep", "20",
  "20 to 25", "23",
  "20 to 30", "25",
  "20 to 40", "30",
  "20-25", "23",
  "20-30", "25",
  "20-35", "30",
  "20-50", "35",
  "20/30", "25",
  "20/30", "25",
  "20+", "20",
  "200 yds", "200",
  "200-300", "250",
  "200+", "200",
  "2000 acres", "2000",
  "21 goats", "21",
  "21 head", "21",
  "22 cattle", "22",
  "25 to 30", "28",
  "25-30", "27",
  "25-30", "27",
  "28 killed", "28",
  "28 sheep", "28",
  "3 bags", "3",
  "3 bags", "3",
  "3 bikes", "3",
  "3 cattle", "3",
  "3 cattle", "3",
  "3 goats", "3",
  "3 or 4", "3",
  "3 or 4", "3",
  "3 pangas", "3",
  "3 sheep, 2 calves", "5",
  "3 sheep", "3",
  "3 to 4", "3",
  "3 to 4", "3",
  "3/10/2013", "",
  "3/4/2013", "",
  "3/5/2013", "",
  "3/6/2013", "",
  "3+", "3",
  "3+3+1+2", "9",
  "3+some", "3",
  "30 acres", "30",
  "30 cattle", "30",
  "30 to 40", "35",
  "30-35", "33",
  "30-40", "35",
  "30-50", "40",
  "30+", "30",
  "300-400", "350",
  "300+", "300",
  "35 bags", "35",
  "35 to 40", "37",
  "38 cattle", "38",
  "3or 4", "3",
  "4 bags potatoes", "4",
  "4 bags", "4",
  "4 goats", "4",
  "4 groups", "",
  "4 or 5", "4",
  "4 oxen", "4",
  "4 sheep", "4",
  "4 to 8", "6",
  "4/6/2013", "",
  "40 bag", "40",
  "40 cattle", "40",
  "40 sacks", "40",
  "40 sheep", "40",
  "40 to 50", "45",
  "40/50", "45",
  "400 cattle", "400",
  "4000", "",
  "44 cattle", "44",
  "5 bags", "5",
  "5 calves", "5",
  "5 cattle", "5",
  "5 destroyed", "5",
  "5 goats", "5",
  "5 killed", "5",
  "5 or 6", "5",
  "5 sheep, 1 ox", "6",
  "5 sheep", "5",
  "5 to 6", "5",
  "5/10/2013", "",
  "5/6/2013", "",
  "50 cattle", "50",
  "50 to 60", "55",
  "50-100", "75",
  "50-60", "55",
  "50-75", "62",
  "50+", "50",
  "50+", "50",
  "5000 acres", "5000",
  "519 +", "519",
  "53 detained", "53",
  "54 sheep and goats", "54",
  "56 committee members", "56",
  "6 bag", "6",
  "6 bags", "6",
  "6 cattle", "6",
  "6 cattle", "6",
  "6 goats", "6",
  "6 or 7", "6",
  "6 sheep and goats", "6",
  "6 sheep", "6",
  "6 to 7", "6",
  "6 to 8", "7",
  "6 to 9", "8",
  "6-8 man", "7",
  "6/10/2013", "",
  "6/8/2013", "",
  "60-100", "80",
  "60-70", "65",
  "64 cattle", "64",
  "7 bags", "7",
  "7 cattle", "7",
  "7 sheep", "7",
  "7/10/2013", "",
  "70 bags", "70",
  "70 cattle, sheep", "70",
  "70-100", "85",
  "70000", "",
  "75 rounds", "75",
  "8 bags potatoes", "8",
  "8 cattle", "8",
  "8 cows slashed", "8",
  "8 cows", "8",
  "8 sheep", "8",
  "8 to 10", "9",
  "8/10/2013", "",
  "80 cattle", "80",
  "80-100", "90",
  "84 sheep, 1 cow, 5 chickens", "90",
  "9 cattle", "9",
  "9 sheep", "9",
  "9 to 10", "9",
  "9+9", "18",
  "900(not clear)", "900",
  "all locals", "",
  "all", "",
  "app 5", "5",
  "app. 100", "100",
  "app. 120", "120",
  "armed gang", agang,
  "band", agang,
  "bands", "",
  "cattle slashing", "",
  "clothing", "",
  "considerable quantity", "",
  "fairly large gang", agang_large,
  "few bags", "",
  "few", "",
  "food", "",
  "gang", agang,
  "gangs", agang_large,
  "guards", afew,
  "half village", "",
  "labour", "",
  "large crowd", "",
  "large force", agang_large,
  "large gang", agang_large,
  "large meeting", "",
  "large number", "",
  "large numbers", "",
  "large quantities", "",
  "large quantity", "",
  "large re-oathing ceremony", "",
  "large scale", "",
  "large", agang_large,
  "largish gang", agang_large,
  "local populace", "",
  "many thousand", "2000",
  "mob", "",
  "not given", "",
  "number", "",
  "occupants", "",
  "over 200", "200",
  "party", "",
  "party", agang,
  "patrol", agang,
  "posho", "",
  "potatoes", "",
  "quantity of clothing", "",
  "section", "",
  "several gangs", "agang_large",
  "several", "3",
  "sheep and goats", "",
  "shs 2,300/-", "2300",
  "shs 60/-", "60",
  "shs. 1,000", "1000",
  "shs. 18", "18",
  "shs. 30", "30",
  "small gang", agang,
  "small gangs", "agang",
  "small group", agang,
  "small party", afew,
  "small", agang,
  "some", afew,
  "sufficient food", "",
  "unknown", "",
  "very large gang", "agang_large",
  "villages in ndia, gichugu, embu divisions", "",
  "wives", ""
)
recodings <- matrix(recodings, ncol = 2, byrow = T)

events$initiator_numbers_numeric <- events$initiator_numbers %>% recoderFunc(., recodings[, 1], recodings[, 2]) %>% as.numeric()
events$target_numbers_numeric <- events$target_numbers %>% recoderFunc(., recodings[, 1], recodings[, 2]) %>% as.numeric()
events$affected_count_numeric <- events$affected_count %>% recoderFunc(., recodings[, 1], recodings[, 2]) %>% as.numeric()

```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->


# Casualties


<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxuXG5ldmVudHNbLCBjKFxuICBcImdvdmVybm1lbnRfa2lsbGVkX2NsZWFuXCIsIFwiZ292ZXJubWVudF93b3VuZGVkX2NsZWFuXCIsIFwiZ292ZXJubWVudF9jYXB0dXJlZF9jbGVhblwiLFxuICBcInJlYmVsc19raWxsZWRfY2xlYW5cIiwgXCJyZWJlbHNfd291bmRlZF9jbGVhblwiLCBcInJlYmVsc19jYXB0dXJlZF9jbGVhblwiLFxuICBcImNpdmlsaWFuc19raWxsZWRfY2xlYW5cIiwgXCJjaXZpbGlhbnNfd291bmRlZF9jbGVhblwiLCBcImNpdmlsaWFuc19jYXB0dXJlZF9jbGVhblwiXG4pXSA8LVxuICBldmVudHNbLCBjKFxuICAgIFwiZ292ZXJubWVudF9raWxsZWRcIiwgXCJnb3Zlcm5tZW50X3dvdW5kZWRcIiwgXCJnb3Zlcm5tZW50X2NhcHR1cmVkXCIsXG4gICAgXCJyZWJlbHNfa2lsbGVkXCIsIFwicmViZWxzX3dvdW5kZWRcIiwgXCJyZWJlbHNfY2FwdHVyZWRcIixcbiAgICBcImNpdmlsaWFuc19raWxsZWRcIiwgXCJjaXZpbGlhbnNfd291bmRlZFwiLCBcImNpdmlsaWFuc19jYXB0dXJlZFwiXG4gICldXG5cbmV2ZW50cyA8LSBldmVudHMgJT4lIG11dGF0ZV9hdChcbiAgLnZhcnMgPSBjKFxuICAgIFwiZ292ZXJubWVudF9raWxsZWRfY2xlYW5cIiwgXCJnb3Zlcm5tZW50X3dvdW5kZWRfY2xlYW5cIiwgXCJnb3Zlcm5tZW50X2NhcHR1cmVkX2NsZWFuXCIsXG4gICAgXCJyZWJlbHNfa2lsbGVkX2NsZWFuXCIsIFwicmViZWxzX3dvdW5kZWRfY2xlYW5cIiwgXCJyZWJlbHNfY2FwdHVyZWRfY2xlYW5cIixcbiAgICBcImNpdmlsaWFuc19raWxsZWRfY2xlYW5cIiwgXCJjaXZpbGlhbnNfd291bmRlZF9jbGVhblwiLCBcImNpdmlsaWFuc19jYXB0dXJlZF9jbGVhblwiXG4gICksXG4gIGZ1bnMoYXMubnVtZXJpYyhjYXI6OnJlY29kZSguLCBcIiAnRmV3Jz0nMic7J01hbnknPSczJzsnb3RoZXJzJz0nMic7J1NldmFyYWwnPSczJztcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAnc2V2ZXJhbCc9JzMnOyAnU2V2ZXJhbCBNb3JlJz0nMyc7ICdTZXZlcmFsIG90aGVycyc9JzMnO1xuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAnU29tZSc9JzMnO1xuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAnMTAwKyc9JzEwMCc7ICcyMyBGYW1pbGllcyc9JzIzJzsgJzI4IGZhbWlsaWVzJz0nMjgnOyAnMzAtNDAnPSczNSc7XG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICc1MCsnPSc1MCc7ICdDb3VuY2lsIG9mIGVsZGVycyc9JzMnO1xuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICdDb3VuY2lsIG9mIHdhcic9JzMnOyAnRmV3Jz0nMic7ICdzb21lJz0nMic7IFxuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAnU2V2ZXJhbCc9JzMnOyAgJzQ1MDAnPSc0NSc7ICc4MDAnPSc4MCc7ICdHYW5nJz0nMyc7ICdNYWpvcml0eSc9JzMnOyBcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIDsgJ21hbnknPSczJyAgOyAnU2V2ZXJhbCc9JzMnIDsgJ1NtYWxsIGdhbmcnPSczJyA7XG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgJzYrJz0nNicgOyAnMTArJz0nMTAnIDsgJzMrJz0nMyc7XG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAndW5Lbm93bic9JzEnOyAndW5rbm93bic9JzEnOyAnVW5Lbm93bic9JzEnOyAgJ1VOS05PV04nPScxJzsgJ1Vua293bic9JzEnO1xuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgJ1Vua25vd24nPScxJyA7ICdOdW1iZXInPScxJzsnTW9yZSc9JzEnOyAnMTAxOTcnPScnIDsgJzEwMSc9JzEnIDtcbic0OCc9JzcnIDsgJzE0Nic9JzEnIDsgJzEyMic9JzEnOyAgJzIwOCc9JzEnOyAnOTQnPScxJyA7XG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICBOQT0wXCIpKSlcbilcblxuZXZlbnRzIDwtIGV2ZW50cyAlPiUgbXV0YXRlX2F0KC52YXJzID0gYyhcbiAgXCJnb3Zlcm5tZW50X2tpbGxlZF9jbGVhblwiLCBcImdvdmVybm1lbnRfd291bmRlZF9jbGVhblwiLCBcImdvdmVybm1lbnRfY2FwdHVyZWRfY2xlYW5cIixcbiAgXCJyZWJlbHNfa2lsbGVkX2NsZWFuXCIsIFwicmViZWxzX3dvdW5kZWRfY2xlYW5cIiwgXCJyZWJlbHNfY2FwdHVyZWRfY2xlYW5cIixcbiAgXCJjaXZpbGlhbnNfa2lsbGVkX2NsZWFuXCIsIFwiY2l2aWxpYW5zX3dvdW5kZWRfY2xlYW5cIiwgXCJjaXZpbGlhbnNfY2FwdHVyZWRfY2xlYW5cIlxuKSwgZnVucyhhcy5udW1lcmljKSlcblxuZXZlbnRzIDwtIGV2ZW50cyAlPiVcbiAgbXV0YXRlKHJlYmVsc19raWxsZWR3b3VuZGVkX2NsZWFuID0gcmViZWxzX2tpbGxlZF9jbGVhbiArIHJlYmVsc193b3VuZGVkX2NsZWFuKSAlPiVcbiAgbXV0YXRlKGdvdmVybm1lbnRfa2lsbGVkX3dvdW5kZWRfY2xlYW4gPSBnb3Zlcm5tZW50X2tpbGxlZF9jbGVhbiArIGdvdmVybm1lbnRfd291bmRlZF9jbGVhbikgJT4lXG4gIG11dGF0ZShyZWJlbHNfZ292ZXJubWVudF9raWxsZWR3b3VuZGVkX2NsZWFuID0gcmViZWxzX2tpbGxlZF9jbGVhbiArIHJlYmVsc193b3VuZGVkX2NsZWFuKSAlPiVcbiAgbXV0YXRlKHJlYmVsc19nb3Zlcm5tZW50X2tpbGxlZF9jbGVhbiA9IHJlYmVsc19raWxsZWRfY2xlYW4gKyBnb3Zlcm5tZW50X2tpbGxlZF9jbGVhbikgJT4lXG4gIG11dGF0ZShyZWJlbHNfZ292ZXJubWVudF9jaXZpbGlhbnNfa2lsbGVkX2NsZWFuID0gcmViZWxzX2tpbGxlZF9jbGVhbiArIGdvdmVybm1lbnRfa2lsbGVkX2NsZWFuICsgY2l2aWxpYW5zX2tpbGxlZF9jbGVhbilcbmBgYCJ9 -->

```r

events[, c(
  "government_killed_clean", "government_wounded_clean", "government_captured_clean",
  "rebels_killed_clean", "rebels_wounded_clean", "rebels_captured_clean",
  "civilians_killed_clean", "civilians_wounded_clean", "civilians_captured_clean"
)] <-
  events[, c(
    "government_killed", "government_wounded", "government_captured",
    "rebels_killed", "rebels_wounded", "rebels_captured",
    "civilians_killed", "civilians_wounded", "civilians_captured"
  )]

events <- events %>% mutate_at(
  .vars = c(
    "government_killed_clean", "government_wounded_clean", "government_captured_clean",
    "rebels_killed_clean", "rebels_wounded_clean", "rebels_captured_clean",
    "civilians_killed_clean", "civilians_wounded_clean", "civilians_captured_clean"
  ),
  funs(as.numeric(car::recode(., " 'Few'='2';'Many'='3';'others'='2';'Sevaral'='3';
                                  'several'='3'; 'Several More'='3'; 'Several others'='3';
                                   'Some'='3';
                                   '100+'='100'; '23 Families'='23'; '28 families'='28'; '30-40'='35';
                                   '50+'='50'; 'Council of elders'='3';
                                  'Council of war'='3'; 'Few'='2'; 'some'='2'; 
                                   'Several'='3';  '4500'='45'; '800'='80'; 'Gang'='3'; 'Majority'='3'; 
                                 ; 'many'='3'  ; 'Several'='3' ; 'Small gang'='3' ;
                                  '6+'='6' ; '10+'='10' ; '3+'='3';
                                 'unKnown'='1'; 'unknown'='1'; 'UnKnown'='1';  'UNKNOWN'='1'; 'Unkown'='1';
                                 'Unknown'='1' ; 'Number'='1';'More'='1'; '10197'='' ; '101'='1' ;
'48'='7' ; '146'='1' ; '122'='1';  '208'='1'; '94'='1' ;
                                 NA=0")))
)

events <- events %>% mutate_at(.vars = c(
  "government_killed_clean", "government_wounded_clean", "government_captured_clean",
  "rebels_killed_clean", "rebels_wounded_clean", "rebels_captured_clean",
  "civilians_killed_clean", "civilians_wounded_clean", "civilians_captured_clean"
), funs(as.numeric))

events <- events %>%
  mutate(rebels_killedwounded_clean = rebels_killed_clean + rebels_wounded_clean) %>%
  mutate(government_killed_wounded_clean = government_killed_clean + government_wounded_clean) %>%
  mutate(rebels_government_killedwounded_clean = rebels_killed_clean + rebels_wounded_clean) %>%
  mutate(rebels_government_killed_clean = rebels_killed_clean + government_killed_clean) %>%
  mutate(rebels_government_civilians_killed_clean = rebels_killed_clean + government_killed_clean + civilians_killed_clean)
```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->



<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxuXG5ldmVudHMgJT4lIGphbml0b3I6OmNyb3NzdGFiKGluaXRpYXRvcl9jbGVhbl8xX2FnZ2hpZ2gsIHR5cGVfY2xlYW5fYWdnaGlnaCkgJT4lIGphbml0b3I6OmFkb3JuX2Nyb3NzdGFiKGRpZ2l0cyA9IDEpXG5ldmVudHMgJT4lIGphbml0b3I6OmNyb3NzdGFiKHRhcmdldF9jbGVhbl8xX2FnZ2hpZ2gsIHR5cGVfY2xlYW5fYWdnaGlnaCkgJT4lIGphbml0b3I6OmFkb3JuX2Nyb3NzdGFiKGRpZ2l0cyA9IDEpXG5ldmVudHMgJT4lIGphbml0b3I6OmNyb3NzdGFiKHRhcmdldF9jbGVhbl8xX2FnZ2hpZ2gsIGluaXRpYXRvcl9jbGVhbl8xX2FnZ2hpZ2gpICU+JSBqYW5pdG9yOjphZG9ybl9jcm9zc3RhYihkaWdpdHMgPSAxKVxuYGBgIn0= -->

```r

events %>% janitor::crosstab(initiator_clean_1_agghigh, type_clean_agghigh) %>% janitor::adorn_crosstab(digits = 1)
events %>% janitor::crosstab(target_clean_1_agghigh, type_clean_agghigh) %>% janitor::adorn_crosstab(digits = 1)
events %>% janitor::crosstab(target_clean_1_agghigh, initiator_clean_1_agghigh) %>% janitor::adorn_crosstab(digits = 1)
```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->




# Clean Map Coordinates (East Africa Grid System)


<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxuXG5jYXQoXCJcXDAxNFwiKVxuXG4jQ2FzZXMgdG8gaGFuZGxlXG4jXCI5MjgxNDFcIlxuI1wiMzExNDQ5ICAzMjg0NDUgICAgMzM4NDQzXCJcbiNcIkVBU1RJTkcgMzAgYW5kIDI3XCJcbiNcIkVhc3RMZWlnaCBTZWN0LjdcIlxuI1wiRkFSTSA1MzUvNFwiXG4jXCJIQUMgIDAyMDJcIlxuI1wiSEFDLjU3NzIzNlwiXG4jXCJIWk4gOTc0NjQxICYgSFpOIDk3NDY1MVwiXG4jXCJIWkouIDg1OTVcIlxuI1wiSFpKLiA0NjU3NjUsIEhaSi4gNDU5NzcxLCBIWkouIDQ1MTc1NlwiXG4jXCJIWkogNDI3NjUsIEhaSiA0MjM3NWFuZCBIWkogNDI5NzYxXCJcbiNcIkhaSCA5NjA2MTAsIEhaSCA5NjA2MzAsIEhaSCA5Nzc1MzhcIlxuI1wiSC5aLlIuIDQ3ODZcIlxuI1wiSEFEIDE3MDgsIEhBRCAxNzA5XCJcbiNcIkhBRCAzMzI2LzMzMjdcIlxuI1wiSFpKIDQyNzY1LCBIWkogNDIzNzVhbmQgSFpKIDQyOTc2MVwiXG4jXCJIWkogOTUxOCAgOTYxN1wiXG4jXCJIWlAgNzQzMCwgSFpQIDkwMjksIEhaUCA2NDQ4LCBIWlAgNzI1MiwgSFpQIDk0NDhcIlxuXG5ldmVudHMkbWFwX2Nvb3JkaW5hdGUgJT4lIGphbml0b3I6OnRhYnlsKCkgXG5cbmV2ZW50cyRtYXBfY29vcmRpbmF0ZV9jbGVhbiA8LSBldmVudHMkbWFwX2Nvb3JkaW5hdGUgJT4lIHN0cmluZ3I6OnN0cl9yZXBsYWNlX2FsbChcIltbOnB1bmN0Ol1dfCBcIiwgXCJcIikgXG4oZXZlbnRzJG1hcF9jb29yZGluYXRlX2NsZWFuX2xlbmd0aCA8LSBldmVudHMkbWFwX2Nvb3JkaW5hdGVfY2xlYW4gJT4lIG5jaGFyKCkgKSAlPiUgamFuaXRvcjo6dGFieWwoKSAlPiUgcm91bmQoMylcblxuKGV2ZW50cyRtYXBfY29vcmRpbmF0ZV9jbGVhbl90ZXh0IDwtIGV2ZW50cyRtYXBfY29vcmRpbmF0ZV9jbGVhbiAlPiUgZ3N1YihcIlswLTldXCIsIFwiXFxcXDFcIiwuKSkgJT4lIGphbml0b3I6OnRhYnlsKCkgICU+JSBtdXRhdGVfaWYoaXMubnVtZXJpYywgcm91bmQsMikgI1NwbGl0IGludG8gYSB0ZXh0IGNvbXBvbmVudCBhbmQgbnVtZXJpYyBjb21wb25lbnRcbihldmVudHMkbWFwX2Nvb3JkaW5hdGVfY2xlYW5fbnVtYmVyIDwtIGV2ZW50cyRtYXBfY29vcmRpbmF0ZV9jbGVhbiAlPiUgZ3N1YihcIltBLVphLXpdXCIsIFwiXFxcXDFcIiwgLikgKSAlPiUgamFuaXRvcjo6dGFieWwoKSAgJT4lIG11dGF0ZV9pZihpcy5udW1lcmljLCByb3VuZCwyKVxuXG4oZXZlbnRzJG1hcF9jb29yZGluYXRlX2NsZWFuX3RleHRfYmFuZCA8LSBldmVudHMkbWFwX2Nvb3JkaW5hdGVfY2xlYW5fdGV4dCAlPiUgc3Vic3RyaW5nKDEsMSkgKSAlPiUgamFuaXRvcjo6dGFieWwoKSAgJT4lIG11dGF0ZV9pZihpcy5udW1lcmljLCByb3VuZCwyKVxuKGV2ZW50cyRtYXBfY29vcmRpbmF0ZV9jbGVhbl90ZXh0X2Jsb2NrIDwtIGV2ZW50cyRtYXBfY29vcmRpbmF0ZV9jbGVhbl90ZXh0ICU+JSBzdWJzdHJpbmcoMiwyKSApICU+JSBqYW5pdG9yOjp0YWJ5bCgpICAlPiUgbXV0YXRlX2lmKGlzLm51bWVyaWMsIHJvdW5kLDIpXG4oZXZlbnRzJG1hcF9jb29yZGluYXRlX2NsZWFuX3RleHRfc3ViYmxvY2sgPC0gIGV2ZW50cyRtYXBfY29vcmRpbmF0ZV9jbGVhbl90ZXh0ICU+JSAgc3Vic3RyaW5nKDMsMykgKSAlPiUgamFuaXRvcjo6dGFieWwoKSAgJT4lIG11dGF0ZV9pZihpcy5udW1lcmljLCByb3VuZCwyKVxuXG4oZXZlbnRzJG1hcF9jb29yZGluYXRlX2NsZWFuX251bWJlcl9sZW5ndGggPC0gZXZlbnRzJG1hcF9jb29yZGluYXRlX2NsZWFuX251bWJlciAlPiUgbmNoYXIoKSApICU+JSBqYW5pdG9yOjp0YWJ5bCgpICAlPiUgbXV0YXRlX2lmKGlzLm51bWVyaWMsIHJvdW5kLDIpXG4oZXZlbnRzJG1hcF9jb29yZGluYXRlX2NsZWFuX251bWJlcl9lYXN0aW5nIDwtIGV2ZW50cyRtYXBfY29vcmRpbmF0ZV9jbGVhbl9udW1iZXIgJT4lXG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgc3Vic3RyaW5nKDEsIGV2ZW50cyRtYXBfY29vcmRpbmF0ZV9jbGVhbl9udW1iZXJfbGVuZ3RoLzIpICU+JSBhcy5udW1lcmljKCkgKSAlPiVcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICBqYW5pdG9yOjp0YWJ5bCgpICAlPiUgbXV0YXRlX2lmKGlzLm51bWVyaWMsIHJvdW5kLDIpXG5cbihldmVudHMkbWFwX2Nvb3JkaW5hdGVfY2xlYW5fbnVtYmVyX25vcnRoaW5nIDwtIGV2ZW50cyRtYXBfY29vcmRpbmF0ZV9jbGVhbl9udW1iZXIgJT4lXG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgc3Vic3RyaW5nKGV2ZW50cyRtYXBfY29vcmRpbmF0ZV9jbGVhbl9udW1iZXJfbGVuZ3RoLzIrMSwgZXZlbnRzJG1hcF9jb29yZGluYXRlX2NsZWFuX251bWJlcl9sZW5ndGgpICU+JVxuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIGFzLm51bWVyaWMoKSApICAlPiVcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICBqYW5pdG9yOjp0YWJ5bCgpICAlPiUgbXV0YXRlX2lmKGlzLm51bWVyaWMsIHJvdW5kLDIpXG5cbmBgYCJ9 -->

```r

cat("\014")

#Cases to handle
#"928141"
#"311449  328445    338443"
#"EASTING 30 and 27"
#"EastLeigh Sect.7"
#"FARM 535/4"
#"HAC  0202"
#"HAC.577236"
#"HZN 974641 & HZN 974651"
#"HZJ. 8595"
#"HZJ. 465765, HZJ. 459771, HZJ. 451756"
#"HZJ 42765, HZJ 42375and HZJ 429761"
#"HZH 960610, HZH 960630, HZH 977538"
#"H.Z.R. 4786"
#"HAD 1708, HAD 1709"
#"HAD 3326/3327"
#"HZJ 42765, HZJ 42375and HZJ 429761"
#"HZJ 9518  9617"
#"HZP 7430, HZP 9029, HZP 6448, HZP 7252, HZP 9448"

events$map_coordinate %>% janitor::tabyl() 

events$map_coordinate_clean <- events$map_coordinate %>% stringr::str_replace_all("[[:punct:]]| ", "") 
(events$map_coordinate_clean_length <- events$map_coordinate_clean %>% nchar() ) %>% janitor::tabyl() %>% round(3)

(events$map_coordinate_clean_text <- events$map_coordinate_clean %>% gsub("[0-9]", "\\1",.)) %>% janitor::tabyl()  %>% mutate_if(is.numeric, round,2) #Split into a text component and numeric component
(events$map_coordinate_clean_number <- events$map_coordinate_clean %>% gsub("[A-Za-z]", "\\1", .) ) %>% janitor::tabyl()  %>% mutate_if(is.numeric, round,2)

(events$map_coordinate_clean_text_band <- events$map_coordinate_clean_text %>% substring(1,1) ) %>% janitor::tabyl()  %>% mutate_if(is.numeric, round,2)
(events$map_coordinate_clean_text_block <- events$map_coordinate_clean_text %>% substring(2,2) ) %>% janitor::tabyl()  %>% mutate_if(is.numeric, round,2)
(events$map_coordinate_clean_text_subblock <-  events$map_coordinate_clean_text %>%  substring(3,3) ) %>% janitor::tabyl()  %>% mutate_if(is.numeric, round,2)

(events$map_coordinate_clean_number_length <- events$map_coordinate_clean_number %>% nchar() ) %>% janitor::tabyl()  %>% mutate_if(is.numeric, round,2)
(events$map_coordinate_clean_number_easting <- events$map_coordinate_clean_number %>%
                                              substring(1, events$map_coordinate_clean_number_length/2) %>% as.numeric() ) %>%
                                              janitor::tabyl()  %>% mutate_if(is.numeric, round,2)

(events$map_coordinate_clean_number_northing <- events$map_coordinate_clean_number %>%
                                              substring(events$map_coordinate_clean_number_length/2+1, events$map_coordinate_clean_number_length) %>%
                                              as.numeric() )  %>%
                                              janitor::tabyl()  %>% mutate_if(is.numeric, round,2)

```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->


## Convert Coordinates to lat long


<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->



<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->



<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxuXG4jKHRlbXAgPC0gZXZlbnRzICU+JSBtdXRhdGUobWFwX2Nvb3JkaW5hdGVfY2xlYW5fcm93PTE6bigpKSAlPiUgZmlsdGVyKGlzLm5hKG1hcF9jb29yZGluYXRlX2NsZWFuX2xhdGl0dWRlKSAmICFpcy5uYShtYXBfY29vcmRpbmF0ZV9jbGVhbikpICU+JSBzZWxlY3Qoc3RhcnRzX3dpdGgoXCJtYXBfY29vcmRpbmF0ZV9jbGVhblwiKSkgKSAlPiUgZGlzdGluY3QoKSAlPiUgcHJpbnQobj00MCkgI3Zpc3VhbGl6ZSBlcnJvcnNcbiNkaW0odGVtcCkgIzE5NSBjb29yZGluYXRlcyBkb24ndCBjb252ZXJ0LlxuXG50ZXN0aW5nPUZcbmlmKHRlc3Rpbmcpe1xuICBpPTM2ODRcbiAgZXZlbnRzW2ksXSAlPiUgIHNlbGVjdChzdGFydHNfd2l0aChcIm1hcF9jb29yZGluYXRlX2NsZWFuXCIpKSAlJCUgRUFHUzJMYXRMb25nKGJhbmQ9bWFwX2Nvb3JkaW5hdGVfY2xlYW5fdGV4dF9iYW5kLFxuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgYmxvY2s9bWFwX2Nvb3JkaW5hdGVfY2xlYW5fdGV4dF9ibG9jayxcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIHN1YmJsb2NrPW1hcF9jb29yZGluYXRlX2NsZWFuX3RleHRfc3ViYmxvY2ssXG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICBlYXN0aW5nPW1hcF9jb29yZGluYXRlX2NsZWFuX251bWJlcl9lYXN0aW5nICwgXG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICBub3J0aGluZz1tYXBfY29vcmRpbmF0ZV9jbGVhbl9udW1iZXJfbm9ydGhpbmcpXG4gIFxuICB3aXRoKGV2ZW50c1tpLF0sIG1hcF9jb29yZGluYXRlX2NsZWFuKVxuICB3aXRoKGV2ZW50c1tpLF0sIG1hcF9jb29yZGluYXRlKVxuICBiYW5kIDwtIHdpdGgoZXZlbnRzW2ksXSwgbWFwX2Nvb3JkaW5hdGVfY2xlYW5fdGV4dF9iYW5kKVxuICBibG9jayA8LSB3aXRoKGV2ZW50c1tpLF0sbWFwX2Nvb3JkaW5hdGVfY2xlYW5fdGV4dF9ibG9jaylcbiAgc3ViYmxvY2sgPC0gd2l0aChldmVudHNbaSxdLG1hcF9jb29yZGluYXRlX2NsZWFuX3RleHRfc3ViYmxvY2spICNcbiAgZWFzdGluZyA8LSB3aXRoKGV2ZW50c1tpLF0sbWFwX2Nvb3JkaW5hdGVfY2xlYW5fbnVtYmVyX2Vhc3RpbmcpXG4gIG5vcnRoaW5nIDwtIHdpdGgoZXZlbnRzW2ksXSxtYXBfY29vcmRpbmF0ZV9jbGVhbl9udW1iZXJfbm9ydGhpbmcpXG59XG5cbnN0YXRzOjpxdWFudGlsZShldmVudHMkbWFwX2Nvb3JkaW5hdGVfY2xlYW5fbGF0aXR1ZGUsIHByb2JzID1jKC4wMDUsLjAxLC4xLC41LC45LC45OSwuOTk1KSwgbmEucm09VCwgdHlwZT05KSBcbnN0YXRzOjpxdWFudGlsZShldmVudHMkbWFwX2Nvb3JkaW5hdGVfY2xlYW5fbG9uZ2l0dWRlLCBwcm9icyA9YyguMDA1LC4wMSwuMSwuNSwuOSwuOTksLjk5NSksIG5hLnJtPVQsIHR5cGU9OSlcbnBsb3QoZXZlbnRzJG1hcF9jb29yZGluYXRlX2NsZWFuX2xvbmdpdHVkZSxldmVudHMkbWFwX2Nvb3JkaW5hdGVfY2xlYW5fbGF0aXR1ZGUpXG5cbiNUaGlzIGlzIGp1c3QgdG8gcmVtb3ZlIGFic29sdXRlbHkgY2xlYXIgb3V0bGllcnMuIE5vdCB0byBzZXQgdGhlIHJlZ2lvbiBvZiBpbnRlcmVzdC5cbiNPdXRsaWVyIEJvdW5kaW5nIEJveDpcbiNORSA0LjYyOTMzLCA0MS44OTkwNTlcbiNTVyAtNC43MTcxMiwgMzMuOTA4ODRcbmV2ZW50cyRtYXBfY29vcmRpbmF0ZV9jbGVhbl9sYXRpdHVkZVtldmVudHMkbWFwX2Nvb3JkaW5hdGVfY2xlYW5fbGF0aXR1ZGUgPCAtNC43MTcxMiB8XG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIGV2ZW50cyRtYXBfY29vcmRpbmF0ZV9jbGVhbl9sYXRpdHVkZT40LjYyOTMzXSA8LSBOQVxuZXZlbnRzJG1hcF9jb29yZGluYXRlX2NsZWFuX2xvbmdpdHVkZVtldmVudHMkbWFwX2Nvb3JkaW5hdGVfY2xlYW5fbG9uZ2l0dWRlIDwgMzMuOTA4ODQgfFxuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICBldmVudHMkbWFwX2Nvb3JkaW5hdGVfY2xlYW5fbG9uZ2l0dWRlPjQxLjg5OTA1OV0gPC0gTkFcbnBsb3QoZXZlbnRzJG1hcF9jb29yZGluYXRlX2NsZWFuX2xvbmdpdHVkZSxldmVudHMkbWFwX2Nvb3JkaW5hdGVfY2xlYW5fbGF0aXR1ZGUpXG5cbmBgYCJ9 -->

```r

#(temp <- events %>% mutate(map_coordinate_clean_row=1:n()) %>% filter(is.na(map_coordinate_clean_latitude) & !is.na(map_coordinate_clean)) %>% select(starts_with("map_coordinate_clean")) ) %>% distinct() %>% print(n=40) #visualize errors
#dim(temp) #195 coordinates don't convert.

testing=F
if(testing){
  i=3684
  events[i,] %>%  select(starts_with("map_coordinate_clean")) %$% EAGS2LatLong(band=map_coordinate_clean_text_band,
                                                                              block=map_coordinate_clean_text_block,
                                                                              subblock=map_coordinate_clean_text_subblock,
                                                                              easting=map_coordinate_clean_number_easting , 
                                                                              northing=map_coordinate_clean_number_northing)
  
  with(events[i,], map_coordinate_clean)
  with(events[i,], map_coordinate)
  band <- with(events[i,], map_coordinate_clean_text_band)
  block <- with(events[i,],map_coordinate_clean_text_block)
  subblock <- with(events[i,],map_coordinate_clean_text_subblock) #
  easting <- with(events[i,],map_coordinate_clean_number_easting)
  northing <- with(events[i,],map_coordinate_clean_number_northing)
}

stats::quantile(events$map_coordinate_clean_latitude, probs =c(.005,.01,.1,.5,.9,.99,.995), na.rm=T, type=9) 
stats::quantile(events$map_coordinate_clean_longitude, probs =c(.005,.01,.1,.5,.9,.99,.995), na.rm=T, type=9)
plot(events$map_coordinate_clean_longitude,events$map_coordinate_clean_latitude)

#This is just to remove absolutely clear outliers. Not to set the region of interest.
#Outlier Bounding Box:
#NE 4.62933, 41.899059
#SW -4.71712, 33.90884
events$map_coordinate_clean_latitude[events$map_coordinate_clean_latitude < -4.71712 |
                                      events$map_coordinate_clean_latitude>4.62933] <- NA
events$map_coordinate_clean_longitude[events$map_coordinate_clean_longitude < 33.90884 |
                                      events$map_coordinate_clean_longitude>41.899059] <- NA
plot(events$map_coordinate_clean_longitude,events$map_coordinate_clean_latitude)

```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->


    

# District of Document

-Likewise, the plots suggest some district cleaning still necessary. Jock Scott was an operation in Nairobi and all of those can be changed to Nairobi. Central Province and Rift Valley Province are not districts. Not sure if that matters at this stage... but we probably want to reassign the points we can geolocate from them to an actual district.

Districts: 
-Jock Scott, rift valley, and central province aren't districts. I think Jock Scott is a small town within a district, not sure which one. Central province and rift valley are provinces, one administrative unit higher than the district.
-the districts should be: Baringo, Elgeyo/Marakwet, Embu, Fort Hall, Kajiado, Kiambu, Kitui, Laikipia, Machakos, Meru, Naivasha, Nakuru, Nanyuki, Narok, Nyeri, Thika, and then Nairobi City
-we retrieved events from both district level and provincial level files and we should make sure that that didn't create duplicate events
-Here is my list of our coverage from the files we looked at (the star means we have complete files for the emergency): 

*Baringo
    Mar 51-Aug 61
*Elgeyo/Marakwet
    Mar 53-Aug 61
*Embu
    Nov 51-Aug 61    
Fort Hall
    Nov 51-Aug 54 (possibly missing end)
*Kajiado
    Aug 53-Aug 61
*Kiambu
    Nov 51-Aug 61
Kitui
    Dec 51-Feb 53 (possibly missing end)
*Laikipia
    Nov 51-Dec 57 (Jan 58-Aug 60 in provincial files)
Machakos
    Mar 53-Dec 55
*Meru
    Dec 51-Aug 61
*Naivasha
    May 53-Dec 57; Jan 58-Aug 60 in provincial files
*Nakuru
    Mar 53-Dec 53; Oct 54-Dec 57; Jan 58-Aug 60 in provincial files
*Nanyuki
    Dec 51-Sep 53; Jan 54-Dec 54; Dec 55-Aug 61
*Narok
    May 51-Feb 53; Aug 53-Aug 61
*Nyeri
    Nov 51-Aug 61
*Thika
    Dec 51-Aug 61

Nairobi City
    Mar 53-Dec 56

Central Province
    Mar 53-Jan 57

Rift Valley Province
    Feb 51- Dec 53; Jan 58-Aug 60


<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxuXG5jYXQoXCJcXDAxNFwiKVxuXG4jY2xlYW4gZG9jdW1lbnQgZGlzdHJpY3RcbiNwX2xvYWQoY2FyLHN0cmluZ2kseHRhYmxlKVxuXG5ldmVudHMkZG9jdW1lbnRfZGlzdHJpY3RfY2xlYW4gPC0gc3RyaW5ncjo6c3RyX3RyaW0oc3RyaW5naTo6c3RyaV90cmFuc190b3RpdGxlKGV2ZW50cyRkb2N1bWVudF9kaXN0cmljdCkpXG5cbmV2ZW50cyRkb2N1bWVudF9kaXN0cmljdF9jbGVhbiA8LSBldmVudHMkZG9jdW1lbnRfZGlzdHJpY3RfY2xlYW4gJT4lIGNhcjo6cmVjb2RlKFwiIFxuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAnRW1idS1Gb3J0IEhhbGwgQm9yZGVyJz0nRW1idSc7XG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgJ0JBUklOR08nPSdCYXJpbmdvJztcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgJ0ZPUlQgSEFMTCc9J0ZvcnQgSGFsbCc7IFxuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAnTmF2aWFzaGEnPSdOYWl2YXNoYSc7XG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICdOeWVyaSBTZXR0bGVkIEFyZWEnPSdOeWVyaSc7XG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgJ1NvdXRoIE55ZXJpIFJlc2VydmUnPSdOeWVyaScgO1xuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAnUmVmZXJlbmNlIFNlcmlhbCc9TkEgO1xuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICdIL00nPU5BOyAnTWF0YXRoaWEnPU5BOyAgXG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICdLaXR1aSc9TkE7IFxuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICdKb2NrIFNjb3R0Jz0nTmFpcm9iaSc7XG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICcnPU5BOyBcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgJyAnPU5BIDtcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgJ0RvY3VtZW50IERpc3RyaWN0JyA9IE5BXCIpXG5cblxuZXZlbnRzJGRvY3VtZW50X3VuaXRfdHlwZSA8LSBOQVxuY29uZGl0aW9uIDwtIGV2ZW50cyRkb2N1bWVudF9kaXN0cmljdF9jbGVhbiAlaW4lIGMoXCJSaWZ0IFZhbGxleVwiLFwiQ2VudHJhbCBQcm92aW5jZVwiKTsgdGFibGUoY29uZGl0aW9uKVxuZXZlbnRzJGRvY3VtZW50X3VuaXRfdHlwZVtjb25kaXRpb25dIDwtIFwiUHJvdmluY2VcIlxuXG4jSm9jayBTY290dCBOYWlyb2JpIENpdHlcbmNvbmRpdGlvbiA8LSBldmVudHMkZG9jdW1lbnRfZGlzdHJpY3RfY2xlYW4gJWluJSBjKFwiTmFpcm9iaVwiKTsgdGFibGUoY29uZGl0aW9uKVxuZXZlbnRzJGRvY3VtZW50X3VuaXRfdHlwZVtjb25kaXRpb25dIDwtIFwiQ2l0eVwiXG5cbmNvbmRpdGlvbiA8LSBldmVudHMkZG9jdW1lbnRfZGlzdHJpY3QgJWluJSBjKFwiSk9DSyBTQ09UVFwiKTsgdGFibGUoY29uZGl0aW9uKVxuZXZlbnRzJGRvY3VtZW50X3VuaXRfdHlwZVtjb25kaXRpb25dIDwtIFwiT3BlcmF0aW9uIEpvY2sgU2NvdHRcIlxuXG5cbiNNaXNzaW5nPyBFbGdleW8vTWFyYWt3ZXRcbiNCYXJpbmdvLCAsIEVtYnUsIEZvcnQgSGFsbCwgS2FqaWFkbywgS2lhbWJ1LCBLaXR1aSwgTGFpa2lwaWEsIE1hY2hha29zLCBNZXJ1LCBOYWl2YXNoYSwgTmFrdXJ1LCBOYW55dWtpLCBOYXJvaywgTnllcmksIFRoaWthXG5jb25kaXRpb24gPC0gZXZlbnRzJGRvY3VtZW50X2Rpc3RyaWN0X2NsZWFuICVpbiUgYyhcIkJhcmluZ29cIixcIkVtYnVcIixcIkZvcnQgSGFsbFwiLFwiS2FqaWFkb1wiLFwiS2lhbWJ1XCIsXG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgXCJMYWlraXBpYVwiLFwiTWFjaGFrb3NcIixcIk1lcnVcIixcIk5haXZhc2hhXCIsXCJOYWt1cnVcIixcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgXCJOYW55dWtpXCIsXCJOYXJva1wiLFwiTnllcmlcIixcIlRoaWthXCIpOyB0YWJsZShjb25kaXRpb24pXG5ldmVudHMkZG9jdW1lbnRfdW5pdF90eXBlW2NvbmRpdGlvbl0gPC0gXCJEaXN0cmljdFwiXG5cbmV2ZW50cyRkb2N1bWVudF91bml0X3R5cGUgJT4lXG4gIGphbml0b3I6OnRhYnlsKHNvcnQgPSBUUlVFKSAlPiVcbiAgamFuaXRvcjo6YWRvcm5fY3Jvc3N0YWIoZGlnaXRzID0gMSlcblxuZXZlbnRzJGRvY3VtZW50X2Rpc3RyaWN0X2NsZWFuICAlPiVcbiAgamFuaXRvcjo6dGFieWwoc29ydCA9IFRSVUUpICU+JVxuICBqYW5pdG9yOjphZG9ybl9jcm9zc3RhYihkaWdpdHMgPSAxKVxuXG5gYGAifQ== -->

```r

cat("\014")

#clean document district
#p_load(car,stringi,xtable)

events$document_district_clean <- stringr::str_trim(stringi::stri_trans_totitle(events$document_district))

events$document_district_clean <- events$document_district_clean %>% car::recode(" 
                                   'Embu-Fort Hall Border'='Embu';
                                  'BARINGO'='Baringo';
                                   'FORT HALL'='Fort Hall'; 
                                   'Naviasha'='Naivasha';
                                   'Nyeri Settled Area'='Nyeri';
                                  'South Nyeri Reserve'='Nyeri' ;
                                   'Reference Serial'=NA ;
                                  'H/M'=NA; 'Matathia'=NA;  
                                   'Kitui'=NA; 
                                  'Jock Scott'='Nairobi';
                                   ''=NA; 
                                   ' '=NA ;
                                   'Document District' = NA")


events$document_unit_type <- NA
condition <- events$document_district_clean %in% c("Rift Valley","Central Province"); table(condition)
events$document_unit_type[condition] <- "Province"

#Jock Scott Nairobi City
condition <- events$document_district_clean %in% c("Nairobi"); table(condition)
events$document_unit_type[condition] <- "City"

condition <- events$document_district %in% c("JOCK SCOTT"); table(condition)
events$document_unit_type[condition] <- "Operation Jock Scott"


#Missing? Elgeyo/Marakwet
#Baringo, , Embu, Fort Hall, Kajiado, Kiambu, Kitui, Laikipia, Machakos, Meru, Naivasha, Nakuru, Nanyuki, Narok, Nyeri, Thika
condition <- events$document_district_clean %in% c("Baringo","Embu","Fort Hall","Kajiado","Kiambu",
                                                 "Laikipia","Machakos","Meru","Naivasha","Nakuru",
                                                  "Nanyuki","Narok","Nyeri","Thika"); table(condition)
events$document_unit_type[condition] <- "District"

events$document_unit_type %>%
  janitor::tabyl(sort = TRUE) %>%
  janitor::adorn_crosstab(digits = 1)

events$document_district_clean  %>%
  janitor::tabyl(sort = TRUE) %>%
  janitor::adorn_crosstab(digits = 1)

```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->


# Handle suffixes and directions


<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxuXG4jTm93IHdlIG5lZWQgdG8gaGFuZGxlIHN1ZmZpeGVzIGFuZCBjb21iaW5lZCBsb2NhdGlvbnNcbiNcImZhcm1cIiBub3cgaXMgZm9sbG93ZWQgYnkgdGhpbmdzIGJlY2F1c2UgdGhleSBjcnVuY2hlZCBpbiBhZGRpdGlvbmFsIGxvY2F0aW9uIGluZm8gYXQgdGhlIGVuZFxuI1wiY29sZXMgZXN0YXRlIGZhcm1cbiMgYWdyaWN1bHR1cmUgZXhwZXJpbWVudGFsIGZhcm1cbiNkZW1vbnN0cmF0aW9uIGZhcm1cbiNcImZhcm0gbmVhciBjaHVyb1wiXG4jcmV1YmVucyBmYXJtIG5lYXIgY2h1cm9cbiMjIyMjIyMjIyMjIyMjIyMjIyMjIyMjIyMjIyMjIyMjIyMjIyMjIyMjIyMjIyMjI1xuI1xuZXZlbnRzJGxvY2F0aW9uX3RleHRfcnVsZWNsZWFuIDwtIGV2ZW50cyRsb2NhdGlvbl90ZXh0ICU+JSBzdHJpbmdyOjpzdHJfdHJpbSgpICU+JSB0b2xvd2VyKClcbmV2ZW50cyA8LSBldmVudHMgJT4lIFxuICAgICAgICAgZHBseXI6OnNlbGVjdCgtb25lX29mKFwibG9jYXRpb25fdGV4dF9ydWxlY2xlYW5fY29ubmVjdG9yX3ByZWZpeFwiLFwibG9jYXRpb25fdGV4dF9ydWxlY2xlYW5fY29ubmVjdG9yX3N1ZmZpeFwiKSkgJT4lICBcbiAgICAgICAgICB0aWR5ciA6OnNlcGFyYXRlKGNvbD1sb2NhdGlvbl90ZXh0X3J1bGVjbGVhbixcbiAgICAgICAgICAgICAgICAgICAgaW50bz1jKFwibG9jYXRpb25fdGV4dF9ydWxlY2xlYW5fY29ubmVjdG9yX3ByZWZpeFwiLFwibG9jYXRpb25fdGV4dF9ydWxlY2xlYW5fY29ubmVjdG9yX3N1ZmZpeFwiKSxcbiAgICAgICAgICAgICAgICAgICAgc2VwID0gXCIgb2YgfCBuZWFyIFwiLCByZW1vdmU9RiwgZXh0cmE9XCJkcm9wXCIsIGZpbGw9XCJyaWdodFwiKVxuXG5gYGAifQ== -->

```r

#Now we need to handle suffixes and combined locations
#"farm" now is followed by things because they crunched in additional location info at the end
#"coles estate farm
# agriculture experimental farm
#demonstration farm
#"farm near churo"
#reubens farm near churo
################################################
#
events$location_text_ruleclean <- events$location_text %>% stringr::str_trim() %>% tolower()
events <- events %>% 
         dplyr::select(-one_of("location_text_ruleclean_connector_prefix","location_text_ruleclean_connector_suffix")) %>%  
          tidyr ::separate(col=location_text_ruleclean,
                    into=c("location_text_ruleclean_connector_prefix","location_text_ruleclean_connector_suffix"),
                    sep = " of | near ", remove=F, extra="drop", fill="right")

```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->





<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxuXG5ldmVudHMgPC0gZXZlbnRzICU+JSBtdXRhdGUobmFtZV9jbGVhbj1zdHJpbmdyOjpzdHJfdHJpbSh0b2xvd2VyKGxvY2F0aW9uX3RleHQpKSkgJT4lXG4gICAgICAgICAgIG11dGF0ZShuYW1lX2NsZWFuX3Bvc2Vzc2l2ZT1ncmVwbChcIidzfGBzXCIsbmFtZV9jbGVhbikpICU+JVxuICAgICAgICAgICBtdXRhdGUobmFtZV9jbGVhbmVyPXRyaW13cyhuYW1lX2NsZWFuKSAgKSAlPiVcbiAgICAgICAgICAgbXV0YXRlKG5hbWVfY2xlYW5lcj1nc3ViKFwiJ3N8YHNcIixcIlwiLG5hbWVfY2xlYW5lciwgZml4ZWQ9VCkgICkgJT4lXG4gICAgICAgICAgIG11dGF0ZShuYW1lX2NsZWFuZXI9IHN0cmluZ3I6OnN0cl9yZXBsYWNlX2FsbChuYW1lX2NsZWFuZXIsIFwiW1s6cHVuY3Q6XV18YFwiLCBcIlwiKSAgKSAlPiUgXG4gICAgICAgICAgIG11dGF0ZShuYW1lX2NsZWFuZXI9IHN0cmluZ3I6OnN0cl9yZXBsYWNlX2FsbChuYW1lX2NsZWFuZXIsIFwiW15bOmFsbnVtOl0gXVwiLCBcIlwiKSAgKSAlPiUgICNyZW1vdmVzIGFsbCB0aGUgd2VpcmQgdW5pY29kZSBhbmQgYXNjaWlcbiAgICAgICAgICAgbXV0YXRlKG5hbWVfY2xlYW5lcj10cmltd3MobmFtZV9jbGVhbmVyKSAgKSAlPiVcbiAgICAgICAgICAgbXV0YXRlKG5hbWVfY2xlYW5lcl9ub3NwYWNlPSBzdHJpbmdyOjpzdHJfcmVwbGFjZV9hbGwobmFtZV9jbGVhbmVyLCBcIiBcIiwgXCJcIikgKVxuXG50YWJsZSh1bmxpc3Qoc3Ryc3BsaXQoZXZlbnRzJG5hbWVfY2xlYW5lcixcIlwiKSkpXG5cbmBgYCJ9 -->

```r

events <- events %>% mutate(name_clean=stringr::str_trim(tolower(location_text))) %>%
           mutate(name_clean_posessive=grepl("'s|`s",name_clean)) %>%
           mutate(name_cleaner=trimws(name_clean)  ) %>%
           mutate(name_cleaner=gsub("'s|`s","",name_cleaner, fixed=T)  ) %>%
           mutate(name_cleaner= stringr::str_replace_all(name_cleaner, "[[:punct:]]|`", "")  ) %>% 
           mutate(name_cleaner= stringr::str_replace_all(name_cleaner, "[^[:alnum:] ]", "")  ) %>%  #removes all the weird unicode and ascii
           mutate(name_cleaner=trimws(name_cleaner)  ) %>%
           mutate(name_cleaner_nospace= stringr::str_replace_all(name_cleaner, " ", "") )

table(unlist(strsplit(events$name_cleaner,"")))

```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->


# Create a Simple Features Version GIS Version

It is based on an events dataset built and cleaned in another file.


<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxuXG5ldmVudHNfc2YgPC0gZXZlbnRzICU+JSAjIGZpbHRlcighaXMubmEobG9uZ2l0dWRlKSAmICFpcy5uYShsYXRpdHVkZSkpICAlPiVcbiAgZGlzdGluY3QoKSAlPiVcbiAgIyBmaWx0ZXIoIGJldHdlZW4obG9uZ2l0dWRlLCAzMC4wLDQ1LjApICApICAlPiUgICNGbGFnIFJPSSBidXQgZG9uJ3Qgc3Vic2V0IG9uIGl0IHlldFxuICAjIGZpbHRlciggYmV0d2VlbihsYXRpdHVkZSwgLTUuMCw1LjApICkgJT4lXG4gIG11dGF0ZShuYW1lX2NsZWFuID0gc3RyaW5ncjo6c3RyX3RyaW0odG9sb3dlcihsb2NhdGlvbl90ZXh0KSkpICU+JVxuICBtdXRhdGUobmFtZV9jbGVhbl9wb3Nlc3NpdmUgPSBncmVwbChcIidzfGBzXCIsIG5hbWVfY2xlYW4pKSAlPiVcbiAgbXV0YXRlKG5hbWVfY2xlYW5lciA9IHRyaW13cyhuYW1lX2NsZWFuKSkgJT4lXG4gIG11dGF0ZShuYW1lX2NsZWFuZXIgPSBnc3ViKFwiJ3N8YHNcIiwgXCJcIiwgbmFtZV9jbGVhbmVyLCBmaXhlZCA9IFQpKSAlPiVcbiAgbXV0YXRlKG5hbWVfY2xlYW5lciA9IHN0cmluZ3I6OnN0cl9yZXBsYWNlX2FsbChuYW1lX2NsZWFuZXIsIFwiW1s6cHVuY3Q6XV18YFwiLCBcIlwiKSkgJT4lXG4gIG11dGF0ZShuYW1lX2NsZWFuZXIgPSB0cmltd3MobmFtZV9jbGVhbmVyKSkgJT4lXG4gIG11dGF0ZShuYW1lX2NsZWFuZXJfbm9zcGFjZSA9IHN0cmluZ3I6OnN0cl9yZXBsYWNlX2FsbChuYW1lX2NsZWFuZXIsIFwiIFwiLCBcIlwiKSlcblxuIyBBdm9pZCBjcmVhdGluZyBnZW9tZXRyaWVzIHdoZXJlIG9uZSBvZiB0aGUgdHdvIGlzIE5BXG5ldmVudHNfc2YkbWFwX2Nvb3JkaW5hdGVfY2xlYW5fbG9uZ2l0dWRlW2lzLm5hKGV2ZW50c19zZiRtYXBfY29vcmRpbmF0ZV9jbGVhbl9sYXRpdHVkZSldIDwtIE5BXG5ldmVudHNfc2YkbWFwX2Nvb3JkaW5hdGVfY2xlYW5fbGF0aXR1ZGVbaXMubmEoZXZlbnRzX3NmJG1hcF9jb29yZGluYXRlX2NsZWFuX2xvbmdpdHVkZSldIDwtIE5BXG4jZXZlbnRzX3NmJGV2ZW50X2hhc2ggPC0gTlVMTCAjTWFrZSBzdXJlIHdlJ3JlIG5vdCBoYXNoaW5nIG9uIHRoZSBwcmV2aW91cyBoYXNoIHdoaWNoIG1pZ2h0IGJlIGEgcmFuZG9tIHdhbGtcblxuZXZlbnRzX3NmIDwtIGV2ZW50c19zZiAlPiVcbiAgICAgICAgICAgIHNmOjpzdF9hc19zZihjb29yZHMgPSBjKFwibWFwX2Nvb3JkaW5hdGVfY2xlYW5fbG9uZ2l0dWRlXCIsIFwibWFwX2Nvb3JkaW5hdGVfY2xlYW5fbGF0aXR1ZGVcIiksXG4gICAgICAgICAgICAgICAgICAgICBjcnMgPSA0MzI2LCBhZ3IgPSBcImNvbnN0YW50XCIsIHJlbW92ZSA9IEYsIG5hLmZhaWwgPSBGKSAjICU+JSBcbiAgICAgICAgICAgICAjbXV0YXRlKGV2ZW50X2hhc2ggPSBhcHBseSguLCAxLCBkaWdlc3QsIGFsZ289XCJ4eGhhc2g2NFwiKSApICNEbyB0aGlzIG9uY2UgYW5kIG9ubHkgb25jZVxudmFsaWQgPC0gc2Y6OnN0X2lzX3ZhbGlkKGV2ZW50c19zZiRnZW9tZXRyeSk7IHRhYmxlKHZhbGlkKVxuXG5ldmVudHNuYW1lc19zZiA8LSBldmVudHNfc2YgJT4lIFxuICBzZWxlY3QoXCJuYW1lX2NsZWFuZXJcIiwgXCJnZW9tZXRyeVwiKSAlPiUgXG4gIHNldE5hbWVzKGMoXCJuYW1lXCIsIFwiZ2VvbWV0cnlcIikpICU+JSBcbiAgbXV0YXRlKHNvdXJjZV9kYXRhc2V0ID0gXCJldmVudHNcIilcblxuYGBgIn0= -->

```r

events_sf <- events %>% # filter(!is.na(longitude) & !is.na(latitude))  %>%
  distinct() %>%
  # filter( between(longitude, 30.0,45.0)  )  %>%  #Flag ROI but don't subset on it yet
  # filter( between(latitude, -5.0,5.0) ) %>%
  mutate(name_clean = stringr::str_trim(tolower(location_text))) %>%
  mutate(name_clean_posessive = grepl("'s|`s", name_clean)) %>%
  mutate(name_cleaner = trimws(name_clean)) %>%
  mutate(name_cleaner = gsub("'s|`s", "", name_cleaner, fixed = T)) %>%
  mutate(name_cleaner = stringr::str_replace_all(name_cleaner, "[[:punct:]]|`", "")) %>%
  mutate(name_cleaner = trimws(name_cleaner)) %>%
  mutate(name_cleaner_nospace = stringr::str_replace_all(name_cleaner, " ", ""))

# Avoid creating geometries where one of the two is NA
events_sf$map_coordinate_clean_longitude[is.na(events_sf$map_coordinate_clean_latitude)] <- NA
events_sf$map_coordinate_clean_latitude[is.na(events_sf$map_coordinate_clean_longitude)] <- NA
#events_sf$event_hash <- NULL #Make sure we're not hashing on the previous hash which might be a random walk

events_sf <- events_sf %>%
            sf::st_as_sf(coords = c("map_coordinate_clean_longitude", "map_coordinate_clean_latitude"),
                     crs = 4326, agr = "constant", remove = F, na.fail = F) # %>% 
             #mutate(event_hash = apply(., 1, digest, algo="xxhash64") ) #Do this once and only once
valid <- sf::st_is_valid(events_sf$geometry); table(valid)

eventsnames_sf <- events_sf %>% 
  select("name_cleaner", "geometry") %>% 
  setNames(c("name", "geometry")) %>% 
  mutate(source_dataset = "events")

```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->


# Output Cleaned Files


<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxuXG5zYXZlUkRTKGV2ZW50c19zZiwgZ2x1ZTo6Z2x1ZShnZXR3ZCgpLCBcIi8uLi9pbnN0L2V4dGRhdGEvZXZlbnRzX3NmLlJkYXRhXCIpKVxuXG5gYGAifQ== -->

```r

saveRDS(events_sf, glue::glue(getwd(), "/../inst/extdata/events_sf.Rdata"))

```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->



<!-- rnb-text-end -->

