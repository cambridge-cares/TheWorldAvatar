import psycopg2
import requests
from tqdm import tqdm

# Function to get UPRN from API by Given OSGB
def get_uprn(osgb):
    api_key = 'aXLg2jRUgjmtRobP73q8gWSp3dtku05f'
    url = f'https://api.os.uk/search/links/v1/identifiers/{osgb}?key={api_key}'
    response = requests.get(url)
    data = response.json()
    identifier = data['linkedIdentifiers'][0]['correlations'][0]['correlatedIdentifiers'][0]['identifier']
    return identifier

def update_table_with_uprn(table_name, host, dbname, user, password):
    # Connect to your postgres server
    conn = psycopg2.connect(
        host=host,
        dbname=dbname,
        user=user,
        password=password
    )

    cur = conn.cursor()

    # Add a new column 'uprn'
    cur.execute(f"ALTER TABLE public.{table_name} ADD COLUMN uprn text;")

    # Fetch the records
    cur.execute(f"SELECT os_topo_toid FROM public.{table_name};")
    records = cur.fetchall()

    # Initialize progress bar
    progress_bar = tqdm(total=len(records), desc="Updating Records")

    # Loop through records and update with UPRN
    for record in records:
        osgb = record[0]
        uprn = get_uprn(osgb)
        cur.execute(f"UPDATE public.{table_name} SET uprn = %s WHERE os_topo_toid = %s;", (uprn, osgb))
        progress_bar.update(1)  # Increment progress bar

    progress_bar.close()  # Close progress bar

    # Commit the transaction
    conn.commit()

    # Close cursor and connection
    cur.close()
    conn.close()

    print("Update completed.")


# Set the target table name
table_name = "uprn_testcase2"

# Set the PostgreSQL connection parameters
host = "localhost"
dbname = "postgres"
user = "postgres"
password = "postgres"

# Call the function to update the table
update_table_with_uprn(table_name, host, dbname, user, password)
