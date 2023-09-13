download_kobo_attachment<-function(kobo_url, local_file_path, username, password){

## Arguments:
# url = url to the file to download. For example for pop data downloaded using the template, the url is stored in the column pop_tabular_file_URL of kobo_clean
# local_file_path  = local path (and file name, including extension) where to save file
# username = your kobo username, should have permissions to download this data
# password = your kobo password
  
### Function
  
# Construct the authentication string in the format "username:password"
auth_string <- paste(username, password, sep = ":")

# Encode the authentication string in base64
encoded_auth_string <- base64enc::base64encode(charToRaw(auth_string))

# Create the authorization header
auth_header <- paste("Basic", encoded_auth_string, sep = " ")

# Create additional headers list
headers <- c("Authorization" = auth_header)

# Download the file with headers (headers are used to add the username and password)
download.file(kobo_url, local_file_path, headers = headers, mode = "wb")
}
