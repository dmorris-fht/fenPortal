# Process UKSI for selectInput
con <- fenDb("dmorris","Taraxacum1!")

uksi <- dbGetQuery(con,"SELECT * FROM lookups.uksi WHERE language = 'la'")

tvk <- uksi[,c("nbn_taxon_version_key")]

full_name <- apply(uksi[c("taxon_name","taxon_qualifier","taxon_authority")],1,concatenate_na.rm)
x <- data.frame("tvk" = tvk, "full_name" = full_name)
write.csv(x,"./www/uksi_full_names.csv")

name <- apply(uksi[c("taxon_name","taxon_qualifier")],1,concatenate_na.rm)
x2 <- data.frame("tvk" = tvk, name = name)
write.csv(x2,"./www/uksi_names.csv")

taxon_groups <- unique(uksi$informal_group)
write.csv(taxon_groups,"./www/taxon_groups.csv")

cols <- c("site_record",
"site",
"subsite",
"taxon_nbn",
"gridref",
"quantity",
"status",
"sex",
"stage",
"note",
"record_date",
"record_date_end",
"recorder",
"determiner",
"method",
"survey",
"start_year",
"end_year",
"start_month",
"end_month")

type <- c("TextInput","SelectInput","SelectInput","SelectizeInput",
          "TextInput","TextInput","TextInput","TextInput","TextInput","TextInput",
          "DateInput","DateInput","TextInput","TextInput","TextInput","SelectInput",
          "NumericInput","NumericInput","NumericInput","NumericInput")

check <- function(x){
  c <- paste0("observeEvent(input$",x,"_check,{
    if(input$",x,"_check == 0){
      shinyjs::enable('",x,"')
    }
    if(input$",x,"_check == 1){
      shinyjs::disable('",x,"')
      updateSelectInput(session,'",x,"',selected = input$",x,")}
  })")
  return(c)
}

code <- ""
for(i in cols){
  code <- paste(code,check(i),sep="\n")
}


clear <- function(x){
  if(grepl("Select",type[x])){
    c <- paste0("if(input$",cols[x],"_check == 0){
    update",type[x],"(session,'",cols[x],"',selected ='')
  }")
  }
  else{
  c <-paste0("if(input$",cols[x],"_check == 0){
    update",type[x],"(session,'",cols[x],"',value =NA)
  }")
  }
  return(c)
}

code2 <-""
for(i in 1:length(cols)){
  code2 <- paste(code2,clear(i),sep="\n")
}



update_form <- function(x){
  if(grepl("Select",type[x])){
    c <- paste0(
    "update",type[x],"(session,'",cols[x],"',selected = d$data[a,c('",cols[x],"')])"
    )
  }
  else{
    c <- paste0(
      "update",type[x],"(session,'",cols[x],"', value = d$data[a,c('",cols[x],"')])"
    )
  }
  return(c)
}

code3 <- ""
for(i in 1:length(cols)){
  code3 <- paste(code3, update_form(i),sep="\n")
}

updatecheck <- function(x){
  paste0(
    "updateCheckboxInput(session,'",cols[x],"_check',value = 0)"
  )
}

code4 <- ""
for(i in 1:length(cols)){
  code4 <- paste(code4,updatecheck(i),sep="\n")
}