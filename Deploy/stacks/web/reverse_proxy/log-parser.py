import os
import re
import json
import urllib.request

# Location of NGINX daily log files
nginxLogDir = "/var/log/nginx/daily"

# Where to store monthly, parsed, CSV files
persistentDir = "/var/log/nginx/persistent"

# Months
months = ["unknown", "jan", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec"]

# Cache of IP to country
ipCache = {}

# Cycle through daily log files
def main():
    for filename in os.listdir(nginxLogDir):
        if not filename.startswith("parsed-") and filename.endswith(".log"):
            # Valid, unparsed daily log
            print("Now parsing log file: " + filename)
            parseLog(filename)

    print("Parsed all pending logs, script complete.")



# Parse the input log file and add to CSV
def parseLog(filename):
    # Get the month from the file name
    month = int(filename.split("-")[2])

    # Get the year
    year = int(filename.split("-")[1].split(".")[1])

    # Get the CSV name that parsed data will be added to
    csvFile = os.path.join(persistentDir, months[month] + "-" + str(year) + ".csv")

    # Create the CSV if it doesn't already exist
    if not os.path.isfile(csvFile):
        csvFileObj = open(csvFile, "w")
        csvFileObj.write("Datetime,IP,Origin,Target")
        csvFileObj.write("\n")
    else:
        csvFileObj = open(csvFile, "a")

    # Parse the nginx log line by line
        f = open(os.path.join(nginxLogDir, filename))
        lines = f.readlines()

        for line in lines:
            if not " - - " in line:
                continue

            # Access datetime
            date = re.search("\[(.*?)\]", line).group(0).split(" ")[0]
            date = date[1:]

            # Source IP
            ip = line.split(" - - ")[0]

            # Find out the originating country via the IP
            country = getCountry(ip)

            # Get the target page (only if page, not css/js file)
            target = line.split(" ")[6]
            if "." in target:
                continue

            # Add line to the csv file
            newline = date + "," + ip + "," + country + "," + target
            csvFileObj.write(newline)
            csvFileObj.write("\n")

        # Close log file
        f.close()

        # Rename the (now parsed) daily log file
        os.rename(os.path.join(nginxLogDir, filename), os.path.join(nginxLogDir, "parsed-" + filename))
        print("Finished parsing log file: " + filename)

    # Close the CSV file
    csvFileObj.close()

    

# Returns the country of origin from the input IP
def getCountry(ip):
    if ip in ipCache:
        return ipCache[ip]

    try:
        request = urllib.request.Request("http://ip-api.com/json/" + ip)
        response = urllib.request.urlopen(request).read()
        json_response = json.loads(response.decode('utf-8'))
    except:
        ipCache[ip] = "Unknown"
        return "Unknown"
    
    country = json_response["country"].replace(",", "-")
    ipCache[ip] = country
    return country



if __name__ == '__main__':
    main()