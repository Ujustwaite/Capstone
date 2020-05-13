#This is the general link structure we are trying to mimic
#https://offshoreweb.pnnl.gov/data//current_data/2015/01/01/primary_10.dat?proj=0&index=1

urlroot = 'https://offshoreweb.pnnl.gov/data/current_data/'
#Update the years to match the deployment you want
years = c('2015','2016','2017')
months = rep(1:12)
days = rep(1:31)

months = formatC(months, width=2, flag="0")
days = formatC(days, width = 2, flag ="0")





#This creates URLs for all of the dates of the months / years listed. You can remove the undesired dates easily in the text file
#Update the number in the filename to get the "primary" record that you are after in accordance with the PNNL documents
for(year in years) { 
  for(month in months) { 
    for(day in days) { 
        url = paste("'",urlroot,year, "/", month, "/", day, "/", "primary_1.dat?proj=1&index=1","'", sep = "" )
        write(url, file = "primary_1_urls.txt", append = TRUE)
        print(url)
      }
    }
}


###Argument to CURL below
###Run on linux as xargs is not included in windows
###Requires the line endings be set to Unix mode. Can be done in Atom or Notepad++ after R saves the URL file
###If line feeds are not changed, then the curl call will return an error of invalid characters in the URL
###You can get this curl command (generally, but still requires mods) from Firefox by navigating to the page in developer mode and 
###Use the guide here: https://askubuntu.com/questions/161778/how-do-i-use-wget-curl-to-download-from-a-site-i-am-logged-into
###Saving the parameters to curl format

xargs -n 1 curl < primary_1_urls.txt -H "User-Agent: Mozilla/5.0 (Windows NT 10.0; Win64; x64; rv:74.0) Gecko/20100101 Firefox/74.0" -H "Accept: text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,*/*;q=0.8" -H "Accept-Language: en-US,en;q=0.5" --compressed -H "Connection: keep-alive" -H "Cookie: _ga=GA1.2.2099874300.1585490146; connect.sid=s"%"3AI6lsQzba0gQT2oxosHrL7YhplHaHOx_J.W9s37aueRItA"%"2Bk9uzBsPCjzmWw5W31vgJ1OH1umt9P8; TS011b46f7=0194455fd25fb490743ded9a974761d0d487812a908f5323446398aa931c4b50e1f5626d903efb8dd88bddd0d5808a0fd2d5ea5236eac5f5db1730f1c0d459dcf3d29c212a3aaad6b4f2bf4523be3b9808e539f6c5; ASP.NET_SessionId=abw5fuzbpuiq0zjvmjaard44" -H "Upgrade-Insecure-Requests: 1" >> primary_1_NJ.csv