import urllib.request
delete = urllib.request.urlopen("http://localhost/JPS_POSTGRESQL/emptyDB")
populate = urllib.request.urlopen("http://localhost/JPS_POSTGRESQL/populateDB")
print(delete.read())
print(populate.read())
