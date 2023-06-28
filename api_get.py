import urllib.request
url = 'https://birmingham-city-observatory.datopian.com/api/3/action/datastore_search?resource_id=c0e97947-e0f2-4099-98c0-3aeb016b88a7&limit=5&q=title:jones' 

with urllib.request.urlopen(url) as response:
  htmlSource = response.read()

print fileobj.read()


# Documentation for API on website it incomplete.
# urllib like this has been superceded, as it was python 2.7
