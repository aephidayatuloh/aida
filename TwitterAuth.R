
twitter.connect <- function(path = "data/key.txt")
{
  # D:/Project/Shiny/shinyTwitter/
  if(file.exists(path))
  {
    key <- read_delim(path, 
                      delim = ";", escape_double = FALSE, trim_ws = TRUE)
    
    setup_twitter_oauth(key$consumer_key, key$consumer_secret, key$access_token,  key$access_secret)
    1
    fail <- FALSE
  } else {
    fail <- TRUE
  }
  if(fail)
  {
    msg = "Koneksi gagal"
    type = "error"
  } else {
    msg = "Koneksi berhasil"
    type = "message"
  }
  list(fail = fail, msg = msg, type = type)
}
