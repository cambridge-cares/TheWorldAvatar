import requests
import pandas as pd

def fetch_greenspace_data(domain_iri: str, endpoint_url: str = "http://174.138.27.240:5007/ontop/ui/sparql") -> pd.DataFrame:
    """
    Fetch GreenSpace data from a SPARQL endpoint given a domain IRI.

    Parameters:
    - domain_iri (str): The IRI of the GreenSpace domain.
    - endpoint_url (str): The SPARQL endpoint URL. Default is set for the example.

    Returns:
    - pd.DataFrame: A DataFrame containing GreenSpace data.
    """
    # Define the SPARQL query
    sparql_query = f"""
    PREFIX gs: <https://www.theworldavatar.com/kg/ontogreenspace/>

    SELECT ?feature ?function ?geometry
    WHERE {{
      ?feature gs:isPartOfDomain <{domain_iri}> ;
               gs:hasFunction ?function .
      ?geoPoint gs:isPartOf ?feature ;
                gs:hasGeometry ?geometry .
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
        # Parse the results
        results = response.json()
        data = []
        for binding in results["results"]["bindings"]:
            feature = binding.get("feature", {}).get("value", "N/A")
            function = binding.get("function", {}).get("value", "N/A")
            geometry = binding.get("geometry", {}).get("value", "N/A")

            data.append({
                "Feature": feature,
                "Function": function,
                "Geometry": geometry,
            })
        
        # Convert the data to a DataFrame
        df = pd.DataFrame(data)
        return df
    else:
        # Raise an exception if the request failed
        raise Exception(f"Error: {response.status_code} - {response.text}")

if __name__ == "__main__":
    # Define the domain IRI for GreenSpace
    greenspace_domain_iri = "https://www.theworldavatar.com/kg/ontogreenspace/Greenspace"
    
    # Fetch the data
    try:
        greenspace_df = fetch_greenspace_data(greenspace_domain_iri)
        print(f"Total number of rows retrieved: {len(greenspace_df)}")  # Print the total row count
        print(greenspace_df.head())  # Display the first few rows of the DataFrame
    except Exception as e:
        print(e)
