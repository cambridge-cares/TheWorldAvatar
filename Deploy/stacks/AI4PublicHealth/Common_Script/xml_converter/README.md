# XML to CSV Converter

## Introduction

The `xml_converter.py` script is designed to process XML files, specifically for converting Food Hygiene Rating Scheme (FHRS) data into CSV format. This documentation explains how to create a virtual environment, install the required libraries, and run the script. It is strongly recommended to create and activate a virtual environment before running the Python script.

---

## Creating and Activating a Virtual Environment

To create a virtual environment named `xmlconvert`, run the following command:
```bash
python -m venv xmlconvert
```

Activate the virtual environment by executing the following command:
```bash
.\xmlconvert\Scripts\activate
```

---

## Installing Libraries

To install the required libraries, run the following command:
```bash
pip install -r .\requirements.txt
```

---

## Script Usage

### Configuration

Create a `xml_to_csv.json` file in the same directory as the `xml_converter.py` script. This JSON file should define the XML URL and the desired output CSV file path. Below is an example configuration:

```json
{
    "xml_url": "https://ratings.food.gov.uk/api/open-data-files/FHRS297en-GB.xml",
    "csv_file_path": "Your_Output_Path/output.csv"
}
```

Replace the example XML URL with the desired URL and output path with your preferred location.

---

### Running the Script

To execute the script, use the following command:
```bash
python .\xml_converter.py
```

The script will:
1. Fetch the XML file from the specified URL.
2. Dynamically parse the XML structure.
3. Save the data as a CSV file to the specified path.

Progress bars are displayed during processing to indicate real-time progress.

---

## Authors

Jiying Chen (jc2341), September 2024
