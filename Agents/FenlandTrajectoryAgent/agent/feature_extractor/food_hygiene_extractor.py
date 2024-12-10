import requests
import pandas as pd

def fetch_food_hygiene_data(domain_iri, endpoint_url="http://174.138.27.240:5007/ontop/ui/sparql"):
    """
    Fetch food hygiene data based on a given domain IRI.

    Parameters:
        domain_iri (str): The IRI of the domain to query.
        endpoint_url (str): The SPARQL endpoint URL.

    Returns:
        pd.DataFrame: A DataFrame containing the query results.
    """
    # Define the SPARQL query
    sparql_query = f"""
    PREFIX fh: <http://www.theworldavatar.com/ontology/OntoFHRS/>
    
    SELECT ?id ?businessName ?businessType ?ratingTime ?ratingValue ?longitude ?latitude
    WHERE {{
        # Match entities that are part of the specified domain
        ?id fh:isPartOfDomain <{domain_iri}> ;
            fh:hasBusinessName ?businessName ;
            fh:hasBusinessType ?businessType ;
            fh:hasRatingDate ?ratingTime ;
            fh:hasRatingValue ?ratingValue .

        # Match geolocation details linked to the business
        ?geo fh:isPartOf ?id ;
             fh:hasLongitude ?longitude ;
             fh:hasLatitude ?latitude .
    }}
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
        # Parse the JSON response
        results = response.json()
        data = []
        for binding in results["results"]["bindings"]:
            row = {
                "ID": binding.get("id", {}).get("value", "N/A"),
                "Business Name": binding.get("businessName", {}).get("value", "N/A"),
                "Business Type": binding.get("businessType", {}).get("value", "N/A"),
                "Rating Time": binding.get("ratingTime", {}).get("value", "N/A"),
                "Rating Value": binding.get("ratingValue", {}).get("value", "N/A"),
                "Longitude": binding.get("longitude", {}).get("value", "N/A"),
                "Latitude": binding.get("latitude", {}).get("value", "N/A"),
            }
            data.append(row)

        # Convert the results to a DataFrame
        df = pd.DataFrame(data)
        return df
    else:
        # Raise an exception if the query fails
        raise Exception(f"SPARQL query failed with status code {response.status_code}: {response.text}")

# Example usage
if __name__ == "__main__":
    domain_iri = "http://www.theworldavatar.com/ontology/OntoFHRS/FoodHygieneRating"
    try:
        df = fetch_food_hygiene_data(domain_iri)
        print(df)
    except Exception as e:
        print(f"Error: {e}")
