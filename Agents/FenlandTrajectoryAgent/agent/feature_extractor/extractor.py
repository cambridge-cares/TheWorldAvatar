import requests

# Define the SPARQL endpoint and the query
endpoint_url = "http://174.138.27.240:5007/ontop/ui/sparql"
sparql_query = """
PREFIX fh: <http://www.theworldavatar.com/ontology/OntoFHRS/>

SELECT ?id ?businessName ?businessType ?ratingTime ?ratingValue ?longitude ?latitude
WHERE {
  # Match entities that are part of the Food Hygiene Rating domain
  ?id fh:isPartOfDomain <http://www.theworldavatar.com/ontology/OntoFHRS/FoodHygieneRating> ;
      fh:hasBusinessName ?businessName ;
      fh:hasBusinessType ?businessType ;
      fh:hasRatingDate ?ratingTime ;
      fh:hasRatingValue ?ratingValue .

  # Match geolocation details linked to the business
  ?geo fh:isPartOf ?id ;
       fh:hasLongitude ?longitude ;
       fh:hasLatitude ?latitude .
}
"""

# Headers for the request
headers = {
    "Content-Type": "application/sparql-query",
    "Accept": "application/json",
}

# Send the SPARQL query to the endpoint
response = requests.post(endpoint_url, data=sparql_query, headers=headers)

# Check if the request was successful
if response.status_code == 200:
    # Parse and print the results
    results = response.json()
    count = 0  # Counter for results
    for binding in results["results"]["bindings"]:
        count += 1
        id_value = binding.get("id", {}).get("value", "N/A")
        business_name = binding.get("businessName", {}).get("value", "N/A")
        business_type = binding.get("businessType", {}).get("value", "N/A")
        rating_time = binding.get("ratingTime", {}).get("value", "N/A")
        rating_value = binding.get("ratingValue", {}).get("value", "N/A")
        longitude = binding.get("longitude", {}).get("value", "N/A")
        latitude = binding.get("latitude", {}).get("value", "N/A")

        print(f"ID: {id_value}")
        print(f"Business Name: {business_name}")
        print(f"Business Type: {business_type}")
        print(f"Rating Time: {rating_time}")
        print(f"Rating Value: {rating_value}")
        print(f"Longitude: {longitude}")
        print(f"Latitude: {latitude}")
        print("-" * 50)
    
    print(f"Total number of results: {count}")
else:
    # Print error details if the request failed
    print(f"Error: {response.status_code}")
    print(response.text)
