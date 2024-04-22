"""
    Part of ontozeolite package.
    Author: Rutkevych Pavlo, rutkevych.cares@gmail.com
    Date: 2024/04/01
"""

import os
import sys
import json

import tools

if len(sys.argv) < 2:
    print("Not enough command line arguments")
    PARENT_DIR = "ontozeolite"
else:
    PARENT_DIR = sys.argv[1]
# print("parent dir:", PARENT_DIR)

# Bibliography files (input):
BIBFOLDER = os.path.join(PARENT_DIR, "biblio", "bibfiles")
BIBFILE1 = os.path.join(PARENT_DIR, "biblio", "bibdata_original_pdf.tex")
BIBFILE2 = os.path.join(PARENT_DIR, "biblio", "bibdata_crossref_doi.tex")

# A single file made of all input files (except the duplicated citations) (output):
BIBFILE_OUT = os.path.join(PARENT_DIR, "biblio", "bibcombine.bib")

# A correspondance between a bibliography entry and IRI (output):
FILE_IRI = os.path.join(PARENT_DIR, "biblio", "bib_iri_list.csv")

# Information about the duplicates to be checked (mostly for debugging):
FILE_DUPLICATES = os.path.join(PARENT_DIR, "biblio", "log_duplicate_bibs.csv")

# Input json file with materials to check the bibliography entries and to correct them:
# Format of the json file is described in zeolite_db.py module.
INPUT_MATERIALS = os.path.join(PARENT_DIR, "zeolite", "a_final_species_updated.json")

# The output json file with corrected materials.
# Format of the json file is described in zeolite_db.py module.
OUTPUT_MATERIALS = os.path.join(PARENT_DIR, "zeolite", "a_final_species_nodup.json")


def load_files(folder, ext):
    files = []

    if not os.path.isdir(folder):
        print(f"Folder does not exist '{folder}'.")
        return files
 
    for file in os.listdir(folder):
        if file.lower().endswith(ext):
            files.append(file)

    return files


def _make_safe_string(fname):
    fname = fname.replace("https://dx.doi.org/", "")
    fname = fname.replace("http://dx.doi.org/", "")
    fname = fname.replace("https://doi.org/", "")
    fname = fname.replace("http://doi.org/", "")
    fname = fname.replace("doi:", "")
    fname = fname.replace(",", "_").replace("/", "_")
    fname = fname.replace("<", "_").replace(">", "_")
    fname = fname.replace("(", "_").replace(")", "_")
    fname = fname.replace("{", "_").replace("}", "_")
    fname = fname.replace(":", "_").replace(";", "_")

    return fname


############################### 
BIB_LINES = []

ITEMNAMES = ["@incollection", "@article", "@inbook", "@misc",
             "@book", "@inproceedings"]
class BibData:
    #def __init__(self, folder, filename):
    #    self.data = self.load_bib_file(folder, filename)
    #    pass
    def __init__(self):
        #self.data = self.load_bib_file(folder, filename)
        self.data = []
        pass

    def load_bib_file(self, folder, filename):
        global BIB_LINES
        self.file = filename

        filepath = os.path.join(folder, filename)

        if not os.path.isfile(filepath):
            print(f"File '{filepath}' does not exist.")
            return None

        with open(filepath, encoding="utf-8") as fp:
            output = []
            for line in fp:
                BIB_LINES.append(_make_safe_string(line.strip()))
                if self._is_header_line(line):
                    # print("Header line", line.strip())
                    fname, fext = os.path.splitext(filename)
                    # print(fname, fext)
                    fname = _make_safe_string(fname)
                    line = self._set_citation_id(line, fname)
                if len(line.strip()) > 0:
                    output.append(line)

        self.data = output
        for line in self.data:
            #print(line)
            pass

        return output

    def new_from_lines(self, chunk):
        self.file = "common bib"
        output = []
        for line in chunk:
            #if self._is_header_line(line):
                # print("Header line", line.strip())
                #fname, fext = os.path.splitext(filename)
                # print(fname, fext)
                #fname = _make_safe_string(fname)
                #line = self._set_citation_id(line, fname)
            if len(line.strip()) > 0 and len(line.strip()) < 3 and \
               line.strip() != "," and line.strip() != "}":
                print(chunk)
                print(f"No , or ] in line '{line}'. '{ord(line[0])}'" )
                #break

            if len(line.strip()) > 0:
                output.append(line)

        #print(output)
        self.data = output
        return output

    def _get_citation_id(self):
        line = self.data[0]
        if len(line.strip()) < 3:
            line = self.data[1]
            # I don't know why first line is emply in self.data

        if "@" not in line:
            print("error, no @ in first line")
            for line in self.data[:3]:
                print(f"    > '{line.strip()}' ")
            return ""

        pos = line.find("{")
        if pos < 0:
            print("Error in bib file, no { on first line")

        line_out = line[:pos+1]
        comma = line[pos:].find(",") + pos
        return line[pos+1:comma]

        #print(line_out)

    def _set_citation_id(self, line, new_id):
        if "@" not in line:
            return line
        pos = line.find("{")
        if pos < 0:
            print("Error in bib file, no { on first line")

        line_out = line[:pos+1]
        if line[pos:].find(",") < 0:
            print("Smth wrong:", line, pos, new_id)
            1/0
        comma = line[pos:].find(",") + pos
        line_out += new_id
        line_out += line[comma:]  # .strip()

        #print("First line:", line_out)
        return line_out

    def _is_header_line(self, line):
        known_item = False

        if "@" in line:
            for item in ITEMNAMES:
                if item in line.lower():
                    #print("Found", item)
                    known_item = True
                    break
            if not known_item:
                print("Error! Unknown line with @:", line.strip())

        return known_item

    def _get_element(self, name):
        name = name + "="
        output = ""
        for line in self.data:
            short = line.replace(" ", "").replace("\t", "").strip()
            if short.lower().startswith(name):
                pos1 = len(name)
                if short[pos1] == "{":
                    #print("{")
                    pos1 = line.find("{")
                    pos2 = pos1 + line[pos1:].find("}")
                elif short[pos1] == '"':
                    #print("iii")
                    pos1 = line.find('"')
                    pos2 = pos1 + 1 + line[pos1 + 1:].find('"')
                else:
                    print("Error, not valid journal string:", line, "in", self.file)
                output = line[pos1+1:pos2]
                #print(output)

        if "pages" in name:
            words = output.split("-")
            if len(words) > 1:
                output = words[0]

        if "doi" in name:
            output = output.replace("https://dx.doi.org/", "")
            output = output.replace("http://dx.doi.org/", "")
            output = output.replace("https://doi.org/", "")
            output = output.replace("http://doi.org/", "")
            output = output.replace("doi:", "")
            
        return output

    def append_to_bib(self, fp):
        for line in self.data:
            fp.write(line)
            #fp.write("\n")
        fp.write("\n"*2)

def is_unique_bib(bibs, data):
    output = True
    log_data = []

    jour = data._get_element("journal")
    volu = data._get_element("volume")
    page = data._get_element("pages")
    year = data._get_element("year")
    doi  = data._get_element("doi")

    for bib in bibs:
        # 1. equal DOI
        doi2 = bib._get_element("doi")
        count_equal = 0
        if doi != "" and doi2 != "" and doi == doi2:
            output = False
            #print(f"'{doi}'  vs '{doi2}'")
            #print(bib.file, "\n", data.file, sep="")
            #print("id", bib._get_citation_id(), data._get_citation_id())
            count_equal += 1

        #   jour == bib._get_element("journal") and \
        # 2. equal journal + volume/year + page_start
        if volu == bib._get_element("volume") and \
           page == bib._get_element("pages")  and year == bib._get_element("year"):
            output = False
            count_equal += 1
            #print(bib._get_element("journal"), jour)
            #print(bib._get_element("volume"), volu)
            #print(bib._get_element("pages"), page)
            #print(bib._get_element("year"), year)
            #print(bib.file, "\n", data.file, sep="")
            #print("id", bib._get_citation_id(), data._get_citation_id())
            log_data = [bib._get_citation_id(), data._get_citation_id(),
                        bib._get_element("doi"),     data._get_element("doi"), 
                        bib._get_element("volume"),  data._get_element("volume"), 
                        bib._get_element("pages"),   data._get_element("pages"), 
                        bib._get_element("year"),    data._get_element("year"), 
                        bib._get_element("journal"), data._get_element("journal")] 


        if count_equal > 0:
            if bib._get_element("doi").lower() != data._get_element("doi").lower() or \
               bib._get_element("volume")      != data._get_element("volume")      or \
               bib._get_element("pages")       != data._get_element("pages")       or \
               bib._get_element("year")        != data._get_element("year"):
               #bib._get_element("journal") != data._get_element("journal"):
                log_data = [bib._get_citation_id(), data._get_citation_id(),
                            bib._get_element("doi"),     data._get_element("doi"), 
                            bib._get_element("volume"),  data._get_element("volume"), 
                            bib._get_element("pages"),   data._get_element("pages"), 
                            bib._get_element("year"),    data._get_element("year"), 
                            bib._get_element("journal"), data._get_element("journal")] 

        pass

    return output, log_data

def append_bib_file(folder, bibfiles, file_out, file_duplicates):

    files = load_files(folder, ext=".bib")
    #print("There are", len(files), "bib files in folder", folder)
    #files = files[:3]  # FIXME

    bibs = []

    # Data structure: cite1, cite2, vol1, vol2, page1, page2, year1, year2, jour1, jour2
    log_list = []

    if True:
        for i_file, file in enumerate(files):
            #print("loading file:", i_file, file)
            #data = BibData(folder, file)
            bib = BibData()
            bib.load_bib_file(folder, file)
            if bib:
                is_uniq, log = is_unique_bib(bibs, bib)
                if is_uniq and len(bib.data) > 0:
                    bibs.append(bib)
                else:
                    print("duplicated citation in", file)
                    if len(log) > 0:
                        log_list.append(log)
    else:
        print("Skipping bib files (debugging")

    if len(bibfiles) > 0:
        file1 = bibfiles[0]

    if os.path.isfile(file1):
        with open(file1, encoding="utf-8") as fp:
            chunk = []
            for i_line, line in enumerate(fp):
                #print("Starting line", i_line, "in", file1)
                line = line.replace("}}", "}")
                BIB_LINES.append(_make_safe_string(line.strip()))
                if len(line.strip()) == 0 and len(chunk) > 0:
                    bib = BibData()
                    bib.new_from_lines(chunk)
                    chunk = []
                    if bib and len(bib.data) > 0:
                        is_uniq, log = is_unique_bib(bibs, bib)
                        if is_uniq:
                            bibs.append(bib)
                        else:
                            #print(bib.data)
                            print("duplicated citation on", i_line, file1, bib.data[0].strip())
                            if len(log) > 0:
                                log_list.append(log)
                else:
                    chunk.append(line)
    else:
        print("Skipping bib file (debugging")

    if len(bibfiles) > 1:
        file1 = bibfiles[1]

    if os.path.isfile(file1):
        with open(file1, encoding="utf-8") as fp:
            chunk = []
            for i_line, line in enumerate(fp):
                #print("Starting line", i_line, "in", file1)
                line = line.replace("}}", "}")
                BIB_LINES.append(_make_safe_string(line.strip()))
                if len(line.strip()) == 0 and len(chunk) > 0:
                    bib = BibData()
                    bib.new_from_lines(chunk)
                    chunk = []
                    if bib and len(bib.data) > 0:
                        is_uniq, log = is_unique_bib(bibs, bib)
                        if is_uniq:
                            bibs.append(bib)
                        else:
                            print("duplicated citation on", i_line, file1, bib.data[0].strip())
                            if len(log) > 0:
                                log_list.append(log)
                else:
                    chunk.append(line)
    else:
        print("Skipping bib file (debugging")

    """
    """
    # Check for duplicates:
    for i in range(1, len(bibs)):
        #if i > 1220:
        #    print("starting i =", i, "of", len(bibs))
        #    print( "       ", bibs[i].data)
        cite1 = bibs[i]._get_citation_id()
        #if i > 4:
        #    break
        for j in range(i):
            #print("in j loop citation:", bibs[j].data)
            cite2 = bibs[j]._get_citation_id()
            if cite1.lower().strip() == cite2.lower().strip():
                print("Duplicating citation ID:", cite1)

    #bib_iri_list = []
    #for bib in bibs:
    #    doi = bib._get_element("doi")
    #    #import bib2csv
    #    #bib2csv.doi_to_bib_id(doi)
    #    bib_id = bib._get_citation_id()
    #    bib_iri_list.append([

    for log_line in log_list:
        #print(log_line)
        pass

    tools.writeCsv(file_duplicates, log_list)

    #tools.writeCsv(FILE_IRI, log_list)

    #print("Going to write to ", file_out)
    f_out = open(file_out, "w", encoding="utf-8")
    for data in bibs:
        data.append_to_bib(f_out)
    f_out.close()
    print("Finished append_bib_file(): writing to ", file_out)

def check_zeolite_cites(file_materials, bib_file):
    """ 
    1) For all "bib_path" in file_materials check whether 
       the "bib_path" exists in bibliography file bibcombine.bib
    """
    if not os.path.isfile(file_materials):
        print("Not found json file with materials:", file_materials)
        return
    if not os.path.isfile(bib_file):
        print("Not found bib file with bib items:", bib_file)
        return
    #if not os.path.isfile(file_duplicates):
    #    print("Not found csv file with duplicates:", file_duplicates)
    #    return

    # Load all materials:
    with open(file_materials, encoding="utf-8") as fp:
        materials = json.load(fp)

    # Load all citation IDs from bib file
    bib_ids = []
    with open(bib_file, encoding="utf-8") as fp:
        lines = fp.readlines()
        for i_line, line in enumerate(lines):
            #print(line[:-1])
            #if "wwwwwwwwww" in line:
            #    1/0
            if line.strip().startswith("@"):
                first_line = line.strip()
                i = i_line
                while first_line.find(",") < 0:
                    i += 1
                    first_line += lines[i].strip()

                first_line = first_line.replace(" ", "")
                fr = first_line.find("{")
                to = first_line.find(",")
                if fr < to:
                    tmp = first_line[fr+1:to]
                    bib_ids.append(tmp)
                    #print(first_line, "=>", tmp)
                else:
                    print("Something is wrong in line", first_line)
    print("Loaded bibliography file:", bib_file)

    # Check whether citation from material is present in bib
    miss_count = 0
    for key, material in materials.items():
        #print(key)
        if key == "a5bb2a43-26cf-4648-9ddd-fc944235b15f":
            #print("Force break, debugging")
            #break
            pass

        if "bib_path" in material:
            bib_path = material["bib_path"]
            if bib_path in bib_ids:
                pass
            else:
                print(f"In {bib_file} missing bib: '{bib_path}'") 
                miss_count += 1
 
    print("Materials:", len(materials), ", bib-ids:", len(bib_ids), ", missed:", miss_count)

    return

    # Old version
    raw = tools.readCsv(file_cite_iri)
    cite_iri = {}
    for line in raw:
        if line[0] in cite_iri:
            print("Repeated citation:", line[0], cite_iri[line[0]], line[2])
        cite_iri[line[0]] = line[2]

    err_count = 0
    for key, material in materials.items():
        #print(key)
        if key == "a5bb2a43-26cf-4648-9ddd-fc944235b15f":
            #print("Force break, debugging")
            #break
            pass

        if "bib_path" in material:
            bib = material["bib_path"]
            if bib not in cite_iri:
                err_count += 1
                print("Missing citation: '", bib, "'", sep="")
                if bib in BIB_LINES:
                    print("    But this citation exists in input bib files")

                if bib.startswith("bibfiles\\") and bib.endswith(".bib"):
                    new_path = bib[9:-4].replace("(", "_").replace(")", "_")
                    new_path = new_path.replace(":", "_").replace(";", "_")
                    print(f"new_path: '{new_path}'")
                    for line in list(cite_iri.keys()):
                        if "S0167" in line.upper():
                            print(f"cite_iri: '{line}'")

                    if new_path in cite_iri:
                        print("    Found citation: '", new_path, "'  ", bib, sep="")
                        materials[key]["bib_path"] = new_path
                    if new_path in BIB_LINES:
                        print("    But this line exists in input bib files", new_path)
                else:
                    new_path = bib

                if new_path in duplicates:
                    new_2 = duplicates[new_path]
                    print("    Found duplicate: '", new_2, "'  ", new_path, sep="")
                    materials[key]["bib_path"] = new_2
                elif new_path in duplicates2:
                    new_2 = duplicates2[new_path]
                    print("    Found duplicate2: '", new_2, "'  ", new_path, sep="")
                    materials[key]["bib_path"] = new_2

    print("Checked material citations. Total number of errors:", err_count)

def correct_zeolite_cites(input_materials, output_materials, file_duplicates):
    """
    1) Check if the "bib_path" is in duplicates of bib_path.
       If so: replace the bib_path by a corrected value
       (i.e. existing in file_cite_iri). 
    """
    if not os.path.isfile(input_materials):
        print("Not found json file with materials:", input_materials)
        return
    if not os.path.isfile(file_duplicates):
        print("Not found csv file with duplicates:", file_duplicates)
        return

    # Load duplicates:
    raw = tools.readCsv(file_duplicates)
    duplicates = {}
    #duplicates2 = {}
    for line in raw:
        if line[1] in duplicates:
            print("Duplicated citation:", line[1], duplicates[line[1]], line[0])
        duplicates[line[1]] = line[0]
        #if line[0] in duplicates2:
        #    print("Duplicate2 citation:", line[0], duplicates2[line[0]], line[1])
        #duplicates2[line[0]] = line[1]

    # Display the content of duplicates for testing/debugging:
    for key, val in duplicates.items():
        #print(key, "=>", val)
        pass

    # Load the input file with materials:
    with open(input_materials, encoding="utf-8") as fp:
        materials = json.load(fp)

    # Fix the materials data:
    for key, material in materials.items():
        #print(key)
        if key == "a5bb2a43-26cf-4648-9ddd-fc944235b15f":
            #print("Force break, debugging")
            #break
            pass

        if "bib_path" in material:
            bib_path = material["bib_path"]
            if bib_path in duplicates:
                print("Need to replace bib:", bib_path, "to", duplicates[bib_path])
                materials[key]["bib_path"] = duplicates[bib_path]
 
    # Save updated materials:
    with open(OUTPUT_MATERIALS, "w", encoding="utf-8") as fp:
        materials = json.dump(materials, fp, indent=4)

    pass

if __name__ == "__main__":

    #print("------------------------------------------------")
    append_bib_file(BIBFOLDER, [BIBFILE1, BIBFILE2], BIBFILE_OUT, FILE_DUPLICATES)
    #print("------------------------------------------------")
    for line in BIB_LINES:
        #print(line)
        pass

    correct_zeolite_cites(INPUT_MATERIALS, OUTPUT_MATERIALS, FILE_DUPLICATES) 
    #print("------------------------------------------------")
    check_zeolite_cites(OUTPUT_MATERIALS, BIBFILE_OUT) 

    #check_zeolite_cites(INPUT_MATERIALS, FILE_IRI, FILE_DUPLICATES) 
    #check_zeolite_cites(OUTPUT_MATERIALS, FILE_IRI, FILE_DUPLICATES) 
