import csv
from IRI_wrapper.species_IRI import run_iri_query

"""This function:
    i) reads inchi entries from the input csv file;
    ii) calls IRI query function that uses the inchi input.
    iii) writes the content of the input fule and the retured IRI in a new updated csv file."""

def updateIRIcsv(cbuCSVFilePath, cbuIRICSVFilePath):
    with open(cbuCSVFilePath, "r", encoding='utf-8-sig') as csvRead: 
        data = []
        myCSV = csv.DictReader(csvRead)
        datarow = {}
        for row in myCSV:
            row['IRI'] = None
            inchi = row['Inchi']
            a = run_iri_query(inchi)
            row['IRI'] = a
            datarow=row.copy()
            data.append(datarow)
    keys = data[0].keys()
    with open(cbuIRICSVFilePath, 'w', encoding='utf8', newline='') as output_file:
        fc = csv.DictWriter(output_file, fieldnames=keys)
        fc.writeheader()
        for entry in data:
            fc.writerow(entry)
