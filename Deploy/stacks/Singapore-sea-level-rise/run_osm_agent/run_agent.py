import pandas as pd
import requests
import argparse
import time
import datetime
import pytz


def main(r):
    df = pd.read_csv(r)

    wkt = df['wk'].tolist()

    run_endpoint = "http://localhost:3838/osmagent/update"

    for i in range(0, len(wkt)):
    run_request = {"bound_srid": "4326",
        "bound_wkt": wkt[i]}

    n = datetime.datetime.now()

    now = n.astimezone(pytz.timezone('Asia/Shanghai'))

    print(now)
    print("send request")

    response = requests.post(run_endpoint, json = run_request)

    n = datetime.datetime.now()

    now = n.astimezone(pytz.timezone('Asia/Shanghai'))

    print(now)
    print("request sent")

    if response.status_code == 200:
    time.sleep(3600)
    elif response.status_code == 504:
    time.sleep(3900)

    print(now)
    print(response)
    print(i)
    print("\n")

if __name__ == '__main__':
    parser = argparse.ArgumentParser()

    # add arguments to the parser
    parser.add_argument("--csv_file_location")

    # parse the arguments
    args = parser.parse_args()
    main(args.csv_file_location)



