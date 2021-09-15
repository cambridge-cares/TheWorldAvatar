import json
import csv
from io import StringIO
import chemaboxwriters.common.commonvars as commonv
from  chemaboxwriters.ontopesscan.prefixes import spec_pref, \
                                                  pes_pref, \
                                                  gain_pref, \
                                                  unit_pref, \
                                                  onto_spec, \
                                                  onto_comp, \
                                                  onto_pes
from chemaboxwriters.ontopesscan.jsonwriter import SCAN_COORDINATE_ATOMS_IRIS, \
                                                   SCAN_COORDINATE_TYPE, \
                                                   SCAN_COORDINATE_UNIT, \
                                                   SCAN_COORDINATE_VALUE, \
                                                   SCAN_POINTS_JOBS

def ops_csvwriter(data):
    data = json.loads(data)
    spec_IRI=data[commonv.SPECIES_IRI]
    calc_id = data[commonv.ENTRY_UUID]
    entryIRI = data[commonv.ENTRY_IRI]

    csvfile = StringIO(newline='')

    spamwriter = csv.writer(csvfile, delimiter=',',
                            quotechar='"', quoting=csv.QUOTE_MINIMAL)

    spamwriter.writerow(['Source', 'Type', 'Target', 'Relation','Value','Data Type'])
    write_initial(spamwriter,entryIRI,spec_IRI)
    write_scancoordinate(spamwriter,calc_id,data)
    write_scanpoints(spamwriter,entryIRI,calc_id,data)

    csvcontent = csvfile.getvalue()
    csvfile.close()
    return [csvcontent]


def write_initial(spamwriter,entryIRI,spec_IRI):
    spamwriter.writerow(['ABoxOntoPESSscan','Ontology',onto_pes,'http://www.w3.org/2002/07/owl#imports','',''])
    spamwriter.writerow(['ABoxOntoPESSscan','Ontology',pes_pref[:-1],'base','',''])
    spamwriter.writerow([pes_pref + entryIRI,'Instance',onto_pes + '#PotentialEnergySurfaceScan',
                            '','',''])
    spamwriter.writerow([spec_pref + spec_IRI[0],'Instance',onto_spec + '#Species',
                            '','',''])
    spamwriter.writerow([pes_pref + entryIRI,'Instance',
                            spec_pref + spec_IRI[0],onto_pes + '#onSpecies','',''])

def write_scancoordinate(spamwriter,calc_id,data):
    scan_type = data[SCAN_COORDINATE_TYPE]
    spamwriter.writerow([pes_pref + scan_type + '_' + calc_id,'Instance',onto_pes + '#' + scan_type,
                             '','',''])
    spamwriter.writerow([pes_pref + data['EntryIRI'],'Instance',pes_pref + scan_type + '_' + calc_id,
                            onto_pes + '#hasScanCoordinate','',''],)
    for atomiri in data[SCAN_COORDINATE_ATOMS_IRIS]:
        spamwriter.writerow([spec_pref + atomiri,'Instance',gain_pref + 'Atom','','',''])
        spamwriter.writerow([pes_pref + scan_type + '_' + calc_id,'Instance',spec_pref + atomiri,
                                onto_pes + '#hasScanAtom','',''])

def write_scanpoints(spamwriter,entryIRI,calc_id,data):
    for k in range(len(data[SCAN_COORDINATE_VALUE])):
        gauss_type = data[SCAN_POINTS_JOBS][k].split('_')[0][-3:]
        spamwriter.writerow([pes_pref + 'ScanPoint_' + calc_id + '_' + str(k+1),'Instance',onto_pes + '#ScanPoint',
                                '','',''])
        spamwriter.writerow([pes_pref + entryIRI,'Instance',pes_pref + 'ScanPoint_' + calc_id + '_' + str(k+1),
                                onto_pes + '#hasScanPoint','',''])
        spamwriter.writerow([data[SCAN_POINTS_JOBS][k],'Instance',onto_comp + '#' + gauss_type,'','',''])
        spamwriter.writerow([pes_pref + 'ScanPoint_' + calc_id + '_' + str(k+1),'Instance',
                                data[SCAN_POINTS_JOBS][k],onto_pes + '#hasCalculation','',''])

        spamwriter.writerow([pes_pref + 'ScanCoordinateValue_' + calc_id + '_' + str(k+1),'Instance',onto_pes + '#ScanCoordinateValue',
                                '','',''])
        spamwriter.writerow([pes_pref + 'ScanPoint_' + calc_id + '_' + str(k+1),'Instance',
                                pes_pref + 'ScanCoordinateValue_' + calc_id + '_' + str(k+1),
                                onto_pes + '#hasScanCoordinateValue','',''])
        spamwriter.writerow([gain_pref + 'hasValue','Data Property',pes_pref + 'ScanCoordinateValue_' + calc_id + '_' + str(k+1),
                                '',data[SCAN_COORDINATE_VALUE][k],'String'])
        if data[SCAN_COORDINATE_UNIT] == 'Angstrom':
            spamwriter.writerow([pes_pref + 'ScanCoordinateValue_' + calc_id + '_' + str(k+1),'Instance',
                                    unit_pref + 'unit#Angstrom',gain_pref + 'hasUnit','',''])

        elif data[SCAN_COORDINATE_UNIT] == 'Degrees':
            spamwriter.writerow([pes_pref + 'ScanCoordinateValue_' + calc_id + '_' + str(k+1),'Instance',
                                    unit_pref + 'unit#DegreeAngle',gain_pref + 'hasUnit','',''])
