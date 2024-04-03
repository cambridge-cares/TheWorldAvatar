import os
import sys
import logging
import json
import uuid

# BibLib library:
# https://github.com/aclements/biblib/tree/master/examples
# Important: there are 2 different BibLib libraries. Don't use 'pip install biblib':
# https://gerardmjuan.github.io/2019/08/06/working-with-bibtex-python/
# There is a problem: the library is old, and not fully compatible with python,
# for example collections.Iteratable. But I fixed it.

# sys.path.append('biblib')
# import biblib.bib
# import biblib.biblib as biblib

# Warning! BibtexParser library requires version 2+. It is loaded from github,
# https://github.com/sciunto-org/python-bibtexparser
# and NOT from 'pip install'. Pip install has version 1.3 or 1.4.
# Command line to install:
# install --no-cache-dir --force-reinstall git+https://github.com/sciunto-org/python-bibtexparser@main
import bibtexparser
import bibtexparser.middlewares as btpmiddle

import tools
logging.basicConfig(level=logging.WARNING)

# Warnings:
W_SKIP_NON_BIB_EXT = True

"""
TODO
-
-
.

"""

"""
For journal abbreviation I use 'journals.json' from
this database of Journals and their abbreviations (updated around 2020-2021):
https://github.com/jxhe/bib-journal-abbreviation
"""

prefixFOAF = "foaf"
prefixBIBO = "bibo"

# biboPrefix = "http://bibo/"
biboPrefix = "http://purl.org/ontology/bibo/"

# These two are required for the extension of Bibo (for list of authors):
foafPrefix = "http://xmlns.com/foaf/0.1/"
crystPrefix = "http://www.theworldavatar.com/kg/ontocrystal/"


def is_http(value):
    """
    Function to check whether the input parameter is a http:// web address

    Parameters:
    - value (str): the input string to be checked

    Return:
    Boolean
    """
    if value.strip().lower().startswith("http://") or \
       value.strip().lower().startswith("https://"):
        return True
    return False


def str2owlsafe(string):
    output = string
    output = output.replace("\u03b1", "alpha").replace("\u03b2", "beta")
    output = output.replace("\u03b3", "gamma")
    output = output.replace(":", "_")

    return output

def doi_to_bib_id(doi):
    tmp = doi.replace("", "")
    tmp = tmp.replace("/", "_").replace(",", "_")
    tmp = tmp.replace("{", "_").replace("}", "_")
    tmp = tmp.replace("(", "_").replace(")", "_")
    tmp = tmp.replace("<", "_").replace(">", "_")
    tmp = tmp.replace(":", "_").replace(";", "_")

    #tmp = tmp.replace("(", "_").replace(")", "_")
    return tmp

BIB_IRI_FILE = "bib_iri_list.csv"  # Format: doi, bib_id, iri, uuid
BIB_IRI_LIST = None
def get_bib_iri(doi):
    if doi.startswith("bibfiles\\"):
        doi = doi.replace("bibfiles\\", "")
    if doi.endswith(".bib"):
        doi = doi.replace(".bib", "")

    global BIB_IRI_LIST
    if BIB_IRI_LIST is None:
        BIB_IRI_LIST = tools.readCsv(BIB_IRI_FILE)

    for bib_iri in BIB_IRI_LIST:
        key = bib_iri[0].lower().strip()
        if doi == key:
            return bib_iri[2].strip()

    bib_id = doi_to_bib_id(doi)
    bib_id = bib_id.lower().strip()
    for bib_iri in BIB_IRI_LIST:
        key = bib_iri[1].lower().strip()
        if bib_id == key:
            return bib_iri[2].strip()

    logging.error(" bib2csv.py: Not found bib_iri '%s' in '%s'.",
                  bib_id, BIB_IRI_FILE)
    return None

class OntoBibo:
    """
    I use biblib as the parser of the bib files.
    Input for biblib.parse() is a stream (need to open the file in advance).
    biblib has classes:
       Parser which is a dict() of Entries.
       Entry  which is a dict() containing a single citation/article/book/etc.

    """
    __slots__ = ["item_name", "class_name", "uuidDB", "err_count",
                 "tbox_prefix", "abox_prefix", "entries", "entryInBibFile", "btpLibrary",
                 "journalAbbreviations", "journalFullLow", "journalAltSpell",
                 "journalShortLow", "foreignWords",
                 "field_count_note", "field_count_lastchecked",
                 "uuid", "doi_iri_list"]
    # __slots__ = ["year", "month", "title", "journal", "volume", "number",
    #             "pages", "abstract", "issn", "doi", "eprint"
    # ]

    def __init__(self, file_path=[], uuidDB=None,
                 tbox_prefix="", abox_prefix=""):
        '''
        self.year    = None
        self.month   = None
        self.title   = None
        self.journal = None
        self.volume  = None
        self.number  = None
        self.pages   = None
        self.abstract = None
        self.issn    = None
        self.doi     = None
        self.eprint  = None
        #self.pages   = None
        '''
        self.item_name  = "TODO_ITEM_NAME"
        self.class_name = "TODO_CLASS_NAME"
        self.err_count = 0

        if "" == tbox_prefix:
            self.tbox_prefix = "http://tPrefix/"
        else:
            self.tbox_prefix = tbox_prefix

        if "" == abox_prefix:
            self.abox_prefix = "http://aPrefix/"
        else:
            self.abox_prefix = abox_prefix

        #self.uuidDB = None
        if uuidDB is None:
            logging.warning(" Missing argument uuidDB in '%s' in class" +
                            " OntoBibo. Using default.", self.item_name)
            self.uuidDB = tools.UuidDB("biblio")
        elif isinstance(uuidDB, tools.UuidDB):
            self.uuidDB = uuidDB
        else:
            logging.error(" Wrong type of uuidDB, expected 'UuidDB', got" +
                          " '%s'. Using default instead.", str(type(uuidDB)))
            self.uuidDB = tools.UuidDB()
            pass

        self.entries = {}
        self.entryInBibFile = {}
        self.btpLibrary = bibtexparser.Library()  # btp = Bib Tex Parser

        if [] == file_path:
            # Do nothing
            pass
        elif isinstance(file_path, list):
            for f_path in file_path:
                self.readBib(f_path)
        elif isinstance(file_path, str):
            self.readBib(file_path)
        else:
            logging.error(" Unknown file path in OntoBibo: '%s'.",
                          str(file_path))

        self.journalAbbreviations = {}
        # journals = ["journals.json", "more-journals.json"]
        journals = [os.path.join("bib2csvp", "journals.json"),
                    os.path.join("bib2csvp", "more-journals.json")]
        for path in journals:
            self.loadJournal(path)

        self.updateJournalDB()

        self.uuid = None

        self.field_count_note = 0
        self.field_count_lastchecked = 0

        self.doi_iri_list = []
        # logging.error(">>>>>>>>>>>>>>>>>>>>>>>")
        # for k in self.entryInBibFile.keys():
        #    logging.error("  > " + k)

        # === end of OntoBibo.__init__()

    def loadJournal(self, path):
        if os.path.isfile(path):
            with open(path, encoding="utf-8") as fp:
                tmp = json.load(fp)
                self.journalAbbreviations |= tmp
        else:
            logging.error("Unknown journal database: '%s' in OntoBibo.", path)
            self.err_count += 1

        # === end of OntoBibo.loadJournal()

    def updateJournalDB(self):
        self.journalFullLow  = {}
        self.journalAltSpell = {}
        self.journalShortLow = {}
        self.foreignWords = {}

        for k in self.journalAbbreviations.keys():
            self.journalFullLow[k.lower().strip()] = k.strip()
            key = self.journalAbbreviations[k].lower().strip()
            self.journalShortLow[key] = k.strip()

        path = os.path.join("bib2csvp", "journal-data.json")
        if os.path.isfile(path):
            with open(path, encoding="utf-8") as fp:
                tmp = json.load(fp)
                self.foreignWords = tmp["foreign"]
                for k in tmp["alternative"]:
                    self.journalAltSpell[k.lower()] = tmp["alternative"][k]

                self.foreignWords = tmp["foreign"]

        """
        print("full low:")
        for k in self.journalFullLow:
            #if k.find("araday") >= 0:
            if k.find("olites") >= 0:
                print("   ", k, "=>", self.journalFullLow[k])
            pass

        1/0
        print("short low:")
        for k in self.journalShortLow:
            if k.find("araday") >= 0:
                print("   ", k, "=>", self.journalShortLow[k])
            pass

        print("alt spell: ")
        for k in self.journalAltSpell:
            print("   ", k, "=>", self.journalAltSpell[k])
            pass
        1/0
        """
        # logging.error(" No foreignWords, no AltSpell ")

        # === end of OntoBibo.updateJournalDB()

    # def addBib(self, file_path):
    #    #self.btpLibrary.add(file_path)I
    #    pass # .addBib()

    def readBib(self, file_path):
        """
        Reading a .bibtex file.
        If file_path is a filename with one or more bib entries,
                    the function return a list of all bib entries.
        If file_path is a directory, then all ".bib" files in this directory
                    and its subdirectories are loaded,
                    the function returns a list of bib OntoBibo.
        Empty list is returned on error.

        """
        # Bib file may contain several entries, so this function returns
        # a list containing a number of entities of

        if not isinstance(file_path, str):
            logging.error(" File path must be a string, but got '%s'" +
                          " in bib2csv.", str(file_path))
            self.err_count += 1
            return

        if os.path.isdir(file_path):
            print("file_path is a directory:", file_path)
            paths = os.listdir(file_path)
            for path in paths:
                pathFull = os.path.join(file_path, path)
                self.readBib(pathFull)

        elif os.path.isfile(file_path):
            _, ext = os.path.splitext(file_path)
            if ".bib" != ext:

                if False == W_SKIP_NON_BIB_EXT:
                    logging.error(" In readBib() input file '%s', but" +
                                  " expect extension .bib. Skipped.",
                                  file_path)
                    self.err_count += 1
                return

            # self._readAsBibLib(file_path)

            # print("Starting file_path", file_path)
            self._checkBibFile(file_path)
            self._readAsBibtexParser(file_path)

        else:
            logging.error(" In bib2csv file '%s' does not exist.", file_path)
            self.err_count += 1
            return

        # === end of OntoBibo.readBib()

    def _readAsBibLib(self, file_path):
        with open(file_path, encoding="utf-8") as fp:
            # p = biblib.bib.Parser().parse(fp, log_fp=sys.stderr).get_entries()
            # p = biblib.bib.resolve_crossrefs(p)
            self.entries |= p

        # === end of OntoBibo._readAsBibLib()

    def _checkBibFile(self, file_path):
        """
        There is a problem in bibtex.parser if author and others contain capital letters.

        Need to read the file, find all entries names and list them.
        """

        # TODO
        # logging.error("Not implemented _checkBibFile() in ontobibo ")

        # === end of OntoBibo._checkBibFile()

    def _readAsBibtexParser(self, file_path):

        layers = [
                  # Months should be represented as int (0-12):
                  btpmiddle.MonthIntMiddleware(True),
                  # Co-authors should be separated:
                  btpmiddle.SeparateCoAuthors(True),
                  # Names should be split into first, von, last, jr parts:
                  btpmiddle.SplitNameParts(True)
                 ]
        library = bibtexparser.parse_file(file_path, append_middleware=layers)

        for entry in library.entries:
            # print("====", entry.key)
            if entry.key in self.entryInBibFile:
                logging.error(" Bibliography entry repeated in '%s'," +
                              " previous definition in '%s'.",
                              file_path, self.entryInBibFile[entry.key])
                self.err_count += 1
            else:
                self.entryInBibFile[entry.key] = file_path
        # for k in self.entryInBibFile:
        #    logging.error("  - " + k)

        self.btpLibrary.add(library.entries)

        # === end of OntoBibo._readAsBibtexParser()

    def _getCsvArrBibtexParser(self, subject, predicate, options):
        err_count = 0
        output = []

        self.field_count_note = 0
        self.field_count_lastchecked = 0

        for i, ent in enumerate(self.btpLibrary.entries):
            # print ("entry", i)
            if "article" == ent.entry_type:
                logging.info(" Loading an article '%s'", str(i+1))
                out, err = self._csvBtpArticle(ent, subject,
                                               predicate, options)
                output += out
                err_count += err
                pass

            elif "inproceedings" == ent.entry_type:
                logging.info(" Loading an inproceedings %d", (i+1))
                out, err = self._csvBtpInProceedings(ent, subject,
                                                     predicate, options)
                output += out
                err_count += err
                pass

            # TODO other types of entries
            else:
                logging.error(" Unknown citation type '%s'.", ent.entry_type)


        return output, err_count
        # === end of OntoBibo._getCsvArrBibtexParser()

    def pagesToStartEnd(self, pages):
        start = None
        end = None

        if not isinstance(pages, str):
            return start, end

        pos = pages.find("-")
        if pos < 0:
            # No '-', i.e. pages is pagesStart
            start = pages.strip()
        elif pos >= 0:
            start = pages[:pos].strip()
            end = pages[pos+1:]
            end = end.replace("-", "").strip()

        if "" == start:
            end = None
        if "" == end:
            end = None

        return start, end
        # === end of OntoBibo.pagesToStartEnd()

    def toStandardJournalName(self, name):
        """ The journal name can be corrupted by the following
        1) Capital to low and vice versa
        2) Misprint (extra comma, quote, etc)
        3) Wrond spelling of the journal
        4) Using short journal name instead of full name
        5) Using foreign letters instead of English, like German fur
        6)
        """
        # The
        output = name.strip()
        # print(">>> ", name)

        for word in self.foreignWords:
            # Case (5) foreign letters instead of English
            # print("  Before:", output, word)
            output = output.replace(word, self.foreignWords[word])
            # print("   After:", output)

        # print("  aaa:", output)
        if output.lower() in self.journalFullLow:
            # Case (1) Correction of
            output = self.journalFullLow[output.lower()]

        # print("  aaa:", output)
        if output.lower() in self.journalShortLow:
            # Case (4) Already in short form of the journal
            output = self.journalShortLow[output.lower()]

        # print("  aaa:", output)
        if output.lower() in self.journalAltSpell:
            # Case (2) Misprint (comma, quote, etc)
            # Case (3) Misspelling
            # print("Found alt spelling:", output)
            output = self.journalAltSpell[output.lower()]
            # print("     New spelling = ", output)

        # print("     New spelling = ", output)
        return output
        # === end of OntoBibo.toStandardJournalName()

    def getIssueInfo(self, entity):

        jFull = None
        jAbbr = None
        vol = None
        journal = "None"
        issue = None

        # print(" Missing journal for ", entity.key)
        # print("datatype of fields:", type(entity.fields))
        for f in entity.fields:
            # print("key:", f.key)
            if "journal" == f.key.lower():
                # print("Assigned journal", f.value)
                journal = f.value
                if journal in self.journalAbbreviations:
                    jFull = journal
                    jAbbr = self.journalAbbreviations[journal]
                    # print("a know entry", jAbbr)
                else:
                    jNew = self.toStandardJournalName(journal)
                    # print("jNew =", jNew)
                    if jNew in self.journalAbbreviations:
                        jFull = jNew
                        jAbbr = self.journalAbbreviations[jNew]
                    else:
                        file = self.entryInBibFile[entity.key]
                        logging.error(" Unknown journal name in bib file:" +
                                      " '%s' in bib entry '%s'.",
                                      journal, file)
                        # print (    "jNew = ", jNew)
                        self.err_count += 1

                        jFull = journal
                        jAbbr = journal

            elif "volume" == f.key.lower():
                # print("    ", f.value)
                vol = str(f.value)
                # break

            elif "issue" == f.key.lower():
                issue = str(f.value)

        if jFull is None:
            logging.error(" Unknown journal full name '%s'", journal)
            self.err_count += 1

        if jAbbr is None:
            logging.error(" Unknown journal abbreviation '%s'", journal)
            self.err_count += 1

        if vol is None: # and None == issue:
            file = self.entryInBibFile[entity.key]
            logging.error(" In bib entry '%s' volume is not specified in %s.",
                          entity.key, file)
            vol = "None"
            self.err_count += 1

        # if None == issue:
        #    file = self.entryInBibFile[entity.key]
        #    logging.error(" In bib entry '%s' issue is not specified in %s.",
        #                   entity.key, file)
        #    issue = "None"
        #    self.err_count += 1

        return jFull, jAbbr, vol, issue
        # === end of OntoBibo.getIssueInfo()

    def _csvBtpArticle(self, entity, subject, predicate, options):
        output = []
        err_count = 0

        # file = self.entryInBibFile[entity.key]
        # print("Starting key: ",  entity.key, file)

        """
    print("type: ", library.entries[0].entry_type)
    k = library.entries[0].key
        print("Starting key: ",  library.entries[0].key)
    print(type(library.entries[0].fields))
    #print(library.entries[0].fields)
    for f in library.entries[0].fields:
        if "author" == f.key:
            for a in f.value:
              print(f.key, ":", a.first, a.last)
        else:
            print(f.key, ":", f.value)

        """
        dctPrefix = "http://purl.org/dc/terms/"
        item_name = entity.key
        class_name = "AcademicArticle"

        #safe_name = item_name.replace("/", "_")
        #safe_name = safe_name.replace("<", "_").replace(">", "_")
        safe_name = doi_to_bib_id(item_name)

        self.uuid, uuidStr = self.uuidDB.addUUID(biboPrefix + class_name,
                                                 self.abox_prefix + "Citation_" + safe_name)
        output.append([self.uuid, "Instance", biboPrefix + class_name,
                       "", "", ""])

        if subject != "" and predicate != "":
            output.append([subject, "Instance", self.uuid, predicate, "", ""])
        else:
            logging.warning(" No subject in csvBtpArr(), skipping" +
                            " subject-predicate triple. %s", item_name)

        #if subject != "" and predicate != "":
        #    output.append([subject, "Instance", self.uuid, predicate, "", ""])
        #    return output, err_count 
        self.doi_iri_list.append([item_name, safe_name, self.uuid, uuidStr])

        # Issue of the Journal:
        # The name consists of the abbreviated journal name, the volume, and uuid:
        # "J.Name"_"V"str(volume)_UUID
        jFull, jAbbr, vol, issue = self.getIssueInfo(entity)
        # print(f"'{jFull}', '{jAbbr}', '{vol}'")
        jShort = jAbbr.replace(".", "").replace(" ", "")

        class_name = "Issue"

        iUuid, _ = self.uuidDB.addUUID(biboPrefix + class_name,
                                       self.abox_prefix + jShort + "_V" + vol)

        if self.uuidDB.getUUID(biboPrefix + class_name,
                               self.abox_prefix + jShort + "_V" + vol) is None:

            #iUuid, _ = self.uuidDB.addUUID(biboPrefix + class_name,
            #                               self.abox_prefix + jShort + "_V" + vol)

            #output.append([iUuid, "Instance", biboPrefix + class_name, "", "", ""])
            pass

        output.append([iUuid, "Instance", biboPrefix + class_name, "", "", ""])

        output.append([self.uuid, "Instance", iUuid,
                       dctPrefix + "isPartOf", "", ""])

        # Journal information:
        class_name = "Journal"

        jUuid, _ = self.uuidDB.addUUID(biboPrefix + class_name,
                                       self.abox_prefix + "Journal_" + jShort)

        if self.uuidDB.getUUID(biboPrefix + class_name,
                               self.abox_prefix + "Journal_" + jShort) is None:

                        #output.append([auth, "Instance", foafPrefix + class_name,
                        #               "", "", ""])
            pass
        output.append([jUuid, "Instance", biboPrefix + class_name, "", "", ""])

        output.append([iUuid, "Instance", jUuid,
                       dctPrefix + "isPartOf", "", ""])

        # For debugging only:
        if False:
            print("-------------------------------------------")
            print("----------     In bib2csv.py     ----------")
            print("Citation:", self.uuid)
            print("Journal: ", jUuid)
            print("Issue:   ", iUuid)
            print("-------------------------------------------")

        # self.uuid,_ = self.uuidDB.addUUID(self.tPrefix + class_name,
        #                                   self.abox_prefix + self.item_name)
        # ###                                self.abox_prefix + self.item_name)
        # output.append([self.uuid, "Instance",
        #                self.tbox_prefix + self.class_name, "", "", ""])
        # output.append([subject, "Instance", self.uuid, predicate, "", ""])

        # if self.value != None:
        #     output.append([omOntoPrefix + "hasNumericalValue",
        #                    "Data Property",
        #                    self.uuid, "", self.value, "decimal"])
        #     pass
        # output += [ ]
        for field in entity.fields:
            if "author" == field.key.lower():
                # print(field.key, "not implemented")
                class_name = "rdf:List"
                """
                alUuid, _ = self.uuidDB.addUUID(class_name, # al = Author List
                                                self.abox_prefix + "Authors_" + self.item_name,
                                                newUuid = uuidStr)
                output.append([alUuid, "Instance", class_name, "", "", ""])
                output.append([self.uuid, "Instance", alUuid,
                               biboPrefix + "authorList", "", ""])
                """

                class_name = "Person"
                # prev, _ = self.uuidDB.addUUID(class_name, # al = Author List
                #                              self.abox_prefix + "Person_" + "first")
                # output.append([prev, "Instance", class_name, "", "", ""])
                # output.append([alUuid, "Instance", prev,
                #                biboPrefix + "authorList", "", ""])

                for ia, author in enumerate(field.value):
                    # print("   ", author, "not implemented")
                    # print("      ", " ".join(author.first), " ".join(author.last))
                    # auth, _ = self.uuidDB.addUUID(class_name, # al = Author List
                    #                              self.abox_prefix + "Person_" + "first")
                    # output.append([prev, "Instance", class_name, "", "", ""])
                    # output.append([alUuid, "Instance", auth, "rdf:first", "", ""])

                    nameID = "".join(author.first + author.last)

                    # print("aaa")

                    auth_iri, _ = self.uuidDB.addUUID(foafPrefix + class_name,
                                                  self.abox_prefix + "Person_" + nameID)
                                                      #foafPrefix + "Person_" + nameID + str(ia+1))
                    if self.uuidDB.getUUID(foafPrefix + class_name,
                                           self.abox_prefix + "Person_" + nameID) is None:

                        pass
                    output.append([auth_iri, "Instance", foafPrefix + class_name,
                                       "", "", ""])

                    output.append([self.uuid, "Instance", auth_iri,
                                   crystPrefix + "hasAuthor", "", ""])

                    #Creating the author's position in the list
                    uuid_tmp = str(uuid.uuid4())  # New unique uuid
                    #auth_ind, _ = self.uuidDB.addUUID(foafPrefix + "AuthorIndex",
                    #                                  foafPrefix + "AuthorIndex_" + nameID + str(ia+1),
                    #                                  newUuid=uuid_tmp)
                    auth_ind = crystPrefix + "AuthorIndex_" + uuid_tmp

                    output.append([auth_ind, "Instance", crystPrefix + "AuthorIndex",
                                   "", "", ""])

                    output.append([self.uuid, "Instance", auth_ind,
                                   crystPrefix + "hasAuthorIndex", "", ""])

                    output.append([crystPrefix + "hasIndexValue", "Data Property",
                                   auth_ind, "", ia + 1, "xsd:integer"])

                    output.append([auth_ind, "Instance", auth_iri,
                                   crystPrefix + "isAuthorIndexOf", "", ""])

                    output.append([foafPrefix + "firstName", "Data Property",
                                   auth_iri, "",
                                   " ".join(author.first), "xsd:string"])
                    output.append([foafPrefix + "family_name", "Data Property",
                                   auth_iri, "",
                                   " ".join(author.last), "xsd:string"])
                    #output.append([crystPrefix + "hasOrderId", "Data Property",
                    #               auth_iri, "", ia+1, "xsd:integer"])

            elif "title" == field.key.lower():
                # print(field.key, "not implemented")
                output.append([dctPrefix + "title", "Data Property",
                               self.uuid, "", str2owlsafe(field.value),
                               "xsd:string"])

            elif "abstract" == field.key.lower():
                # print(field.key, "not implemented")
                if field.value.strip() != "":
                    output.append([crystPrefix + "hasAbstract",
                                   "Data Property", self.uuid, "",
                                   str2owlsafe(field.value), "xsd:string"])

            elif "journal" == field.key.lower():
                # print(field.key, "not implemented")
                output.append([dctPrefix + "title", "Data Property",
                               jUuid, "", str2owlsafe(field.value),
                               "xsd:string"])

            elif "issn" == field.key.lower():
                # print(field.key, "not implemented")
                output.append([biboPrefix + "issn", "Data Property",
                               jUuid, "", field.value, "xsd:string"])

            elif "publisher" == field.key.lower():
                # print(field.key, "not implemented")
                output.append([dctPrefix + "publisher", "Data Property",
                               jUuid, "", field.value, "xsd:string"])

            elif "volume" == field.key.lower():
                # print(field.key, "not implemented")
                output.append([biboPrefix + "volume", "Data Property",
                               iUuid, "", field.value, "xsd:integer"])

            elif "year" == field.key.lower():
                # print(field.key, "not implemented")
                output.append([dctPrefix + "issued", "Data Property",
                               iUuid, "", field.value, "xsd:gYear"])
                #if int(field.value) < 1900:
                #    print("Error in bibfile", item_name)
                #    if field.value < 4:
                #        print("sss")

            elif "month" == field.key.lower():
                # print(field.key, "not implemented, is it supported by bibo?")
                output.append([dctPrefix + "issued", "Data Property",
                               iUuid, "", field.value, "xsd:gMonth"])

            elif "day" == field.key.lower():
                # print(field.key, "not implemented, is it supported by bibo?")
                output.append([dctPrefix + "issued", "Data Property",
                               iUuid, "", field.value, "xsd:gDay"])

            elif "number" == field.key.lower():
                # This is tricky.
                # Journal has volumes and an article needs at least:
                # journal name, volume, page, (year). 
                # Year is redundant, but historically is kept for convenience.
                # But:
                # Usually, a volume consists of numbers (a.k.a. issues), and 
                # an article is associated with the number, not the volume.
                # Logically, each number/issue should have its own IRI.
                # However, often the number is missing/skipped/ignored 
                # in citation as non-important, this makes possible split
                # of Journal_volume_issue or simply Journal_volume.
                # I.e. the knowledge graph structure becomes non-unique.
                # I have chosen the later option: article->Journal_volume,
                # and the issue information is ignored.
                #
                # This is different from original Bibo ontology, where
                # Article -> Issue (with volume and number) -> Journal.
                #
                # If you add the issue number here, you also need to change
                # the Journal-volume logic. Otherwise the volume has multiple
                # numbers associated with it.
                # It may also require a change of TBox and SPARQL queries.

                # print(field.key, "not implemented")
                #output.append([biboPrefix + "issue", "Data Property",
                #               iUuid, "", field.value, "xsd:integer"])
                pass

            elif "issue" == field.key.lower():
                # print(field.key, "not implemented")
                output.append([biboPrefix + "issue", "Data Property",
                               iUuid, "", field.value, "xsd:integer"])

            elif "pages" == field.key:
                output.append([biboPrefix + "pages", "Data Property",
                               self.uuid, "", field.value, "xsd:string"])

                # logging.warning(field.key + " need to add pageStart and pageEnd properly")
                start, end = self.pagesToStartEnd(field.value)
                if start:
                    output.append([biboPrefix + "pageStart", "Data Property",
                                   self.uuid, "", start, "xsd:integer"])
                if end:
                    output.append([biboPrefix + "pageEnd", "Data Property",
                                   self.uuid, "", end, "xsd:integer"])

            elif "doi" == field.key.lower():
                if field.value.strip() != "":
                    # print(field.key, "not implemented")
                    output.append([biboPrefix + "doi", "Data Property",
                                   self.uuid, "", field.value, "xsd:string"])

            elif "url" == field.key.lower():
                if field.value.strip() != "":
                    if len(field.value.strip()) < 3:
                        print("field value: '", field.value, "'", sep="")
                        1/0
                    # print(field.key, "not implemented")
                    output.append([crystPrefix + "hasUrl", "Data Property",
                                   self.uuid, "", field.value.strip(), "xsd:string"])

            elif "eprint" == field.key.lower():
                if field.value.strip() != "":
                    # print(field.key, "not implemented")
                    output.append([crystPrefix + "hasEPrint", "Data Property",
                                   self.uuid, "", field.value.strip(), "xsd:string"])

            elif "keywords" == field.key:
                if field.value.strip() != "":
                    # print(field.key, "not implemented")
                    for kw in field.value.split(","):
                        output.append([crystPrefix + "hasKeyword", "Data Property",
                                       self.uuid, "", kw.strip(), "xsd:string"])

            elif "note" == field.key:
                if field.value.strip() != "":
                    pass
                if 0 == self.field_count_note:
                    logging.warning(" Bib entry '%s' is not implemented" +
                                    " (warning only once)", field.key)
                    self.err_count += 1
                self.field_count_note += 1

            elif "lastchecked" == field.key:
                if 0 == self.field_count_lastchecked:
                    logging.warning(" Bib entry '%s' is not implemented" +
                                    " (warning only once)", field.key)
                    self.err_count += 1
                self.field_count_lastchecked += 1

                """
            elif "url" == field.key:
                print(field.key, "not implemented")
                #output.append([dctPrefix + "title", "Data Property",
                #               self.uuid, "", field.value, "xsd:string"])
                """

            else:
                logging.error(" Unknown key value '%s' in bibliography" +
                              " '%s'.", field.key, entity.key)
                self.err_count += 1

        return output, err_count
        # === end of OntoBibo._csvBtpArticle()

    def _csvBtpInProceedings(self, entity, subject, predicate, options):
        output = []
        err_count = 0

        # file = self.entryInBibFile[entity.key]
        # print("Starting key: ",  entity.key, file)

        dctPrefix = "http://purl.org/dc/terms/"
        item_name = entity.key
        class_name = "InProceedings"

        safe_name = item_name.replace("/", "_")
        safe_name = safe_name.replace("<", "_").replace(">", "_")

        # self.uuid, uuidStr = self.uuidDB.addUUID(biboPrefix + class_name,
        #                                          self.abox_prefix + "Cite_" + item_name)
        # output.append([self.uuid, "Instance", biboPrefix + class_name,
        #                "", "", ""])
        # output.append([  subject, "Instance", self.uuid, predicate, "", ""])

        logging.error(" Invalid function '%s'", class_name)
        err_count += 1

        return output, err_count
        # === end of OntoBibo._csvBtpInProceedings()

    def getCsvArr(self, subject, predicate="", options={}):

        # print(">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> in bib2csv.py")
        if predicate == "":
            short = biboPrefix + "cite"
        elif is_http(predicate):
            short = predicate.split("/")[-1]
        else:
            short = predicate

        if not short.startswith("has"):
            logging.warning(" Predicate in OntoBibo.getCsvArr() is '%s'," +
                            " but expecting it to be 'has" + "%s'.",
                            predicate, "Citation")

        err_count = 0
        output = []

        # output += self._getCsvArrBiblib(subj, predicate, options)
        out, err = self._getCsvArrBibtexParser(subject, predicate, options)
        output += out
        err_count += err

        return output, err_count
        # === end of OntoBib.getCsvArr()

    # === end of class OntoBibo
def getCsvInit( baseName, tPrefix, aPrefix ):
    output = [ ]
    output.append(["Source", "Type", "Target", "Relation", "Value", "Data Type"])

    output.append([baseName, "Ontology", tPrefix, 
                   "http://www.w3.org/2002/07/owl#imports", "", ""])

    output.append([baseName, "Ontology", aPrefix, "base", "", ""])

    return output
    pass  # getCsvInit()

if __name__ == "__main__":
    path = os.path.join("..", "bibfiles", "10.1524_zkri.1988.182.14.1.bib")
    path = os.path.join("bibfiles", "10.1524_zkri.1988.182.14.1.bib")
    path = os.path.join("10.1524_zkri.1988.182.14.1.bib")
    path = "10.1002_anie.200461911.bib"
    path = "bibfiles\\" + "10.1038_nmat2228.bib"
    path = "bibcombine.bib"

    # fp = open(path)
    # p = biblib.bib.Parser().parse(fp, log_fp=sys.stderr).get_entries()
    # p = biblib.bib.resolve_crossrefs(p)

    # print(p)
    # print(vars(p))
    # print(p.getC_entries())
    # print(p.get_entries().keys())
    # print(p.get_entries().values())

    # print(p.keys())
    # print(p.values())
    # print("===============================")
    # for ent in p.values():
    #    for k in ent.keys():
    #        print(k, ":", ent[k])

    """
    b = OntoBibo(path)
    for ent in b.entries.keys():
        #print (ent, ":", b.entries[ent].keys())
        print (ent, ":")
        for k in b.entries[ent].keys():
            print(k, ":", b.entries[ent][k])
    """

    # library = bibtexparser.parse_file(path)

    layers = [
              # Months should be represented as int (0-12):
              btpmiddle.MonthIntMiddleware(True),
              # Co-authors should be separated:
              btpmiddle.SeparateCoAuthors(True),
              # Names should be split into first, von, last, jr parts:
              btpmiddle.SplitNameParts(True)
             ]
    library = bibtexparser.parse_file(path, append_middleware=layers)

    """
    #print("entry: ",library.entries[0])
    print("type: ", library.entries[0].entry_type)
    print("key: ",  library.entries[0].key)
    k = library.entries[0].key
    print(type(library.entries[0].fields))
    #print(library.entries[0].fields)
    for f in library.entries[0].fields:
        if "author" == f.key:
            for a in f.value:
              print(f.key, ":", a.first, a.last)
        else:
            print(f.key, ":", f.value)
    # Need to convert library to a simpler form. Now it is too complicated,
    # with redundant information.
    """

    uuidDB = tools.UuidDB(os.path.join("ontozeolite", "final", "uuid", "biblio.csv"))

    output = getCsvInit("base", tPrefix=crystPrefix, aPrefix=crystPrefix)

    b = OntoBibo(path, abox_prefix=crystPrefix,
                 uuidDB=uuidDB)

                 #tbox_prefix=crystPrefix, uuidDB=uuidDB)

    csv_data, err = b.getCsvArr("", "")
    output += csv_data
    for line in output:
        #print(line)
        pass

    #csv_data = b.getCsvArr("subj", "hasCitation")
    #print("csv =", csv_data)
    tools.writeCsv("onto_bib.csv", output)
    tools.writeCsv(BIB_IRI_FILE, b.doi_iri_list)
    uuidDB.saveDB()

