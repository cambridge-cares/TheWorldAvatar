"""
    Part of ontozeolite package.
    Author: Rutkevych Pavlo, rutkevych.cares@gmail.com
    Date: 2024/04/01
"""

import os

import logging

import tools
import ontocrystal_datatypes as ocdt

#logging.basicConfig(level = logging.DEBUG)
#logging.basicConfig(level = logging.INFO)
logging.basicConfig(level = logging.WARNING)
#logging.basicConfig(level = logging.ERROR)

class XRDSpectrum:
    """ ABox generator for a single XRD spectrum.
    Data for each XRD spectrum is saves in a single file, and
    each file corresponds to UUID.
    A material may have multiple XRD spectra. In this case
    the material may have several xrd files and therefore several spectra.
    The file and the corresponding UUID are saved in file 'xrd_iri_list.csv'.
    """
    def __init__(self, abox_prefix):
        self.crystOntoPrefix = "https://www.theworldavatar.com/kg/ontocrystal/"
        self.abox_prefix = "https://www.theworldavatar.com/kg/ontozeolite/"

        self.uuidDB = tools.UuidDB()
        pass
        # XRDSpectrum.__init__()

    def get_csv_arr(self, subject, predicate, xrd_peak_path, uuid=None):
        """ xrd_peak_path is the path the the file with xrd peaks.
        In case there are several spectra for a given material,
        they should have different UUID.
        If uuid argument it not provided, a new UUID will be generated.
        I uuid is given, it will be used for the XRDSpectrum and its peaks.

        """
        output = []

        if not os.path.isfile(xrd_peak_path):
            logging.error("Missing file with xrd peaks: '%s'. Skipping XRD.",
                          xrd_peak_path)
            return output

        xrd_iri, xrd_uuid = self.uuidDB.addUUID(self.crystOntoPrefix + "XRDSpectrum",
                                                self.abox_prefix + "XRDSpectrum")
        output.append([xrd_iri, "Instance", self.crystOntoPrefix + "XRDSpectrum", "", "", ""])

        if subject != "":
            output.append([subject, "Instance", xrd_iri, predicate, "", ""])

        #print(zeoname, "=====>", xrd[5])
        #print(">>> In arrSpectrum, uuid_xrd =", uuid_xrd)
        #filename = os.path.join("ontozeolite", "zeolite", "data", xrd[5])
        output += self.loadXRDPeaks(xrd_iri,
                                    self.crystOntoPrefix + "hasCharacteristicPeak",
                                    xrd_peak_path, uuid=xrd_uuid)

        return output
        # === end of XRDSpectrum.get_csv_arr()

    def loadXRDPeaks(self, subject, predicate, filename, uuid=None):
        """
        See also
        https://stackoverflow.com/questions/28173128/find-local-maximum-of-data-files-in-gnuplot/56606820#56606820
        """
        if subject.find("XRDSpectrum") < 0:
            logging.warning(" CharacteristicPeak expects a subject 'XRDSpectrum', " +
                            "but got '%s'.", subject)
        if predicate.find("hasCharacteristicPeak") < 0:
            logging.warning(" Spectrum expects a predicate 'CharacteristicPeak', " +
                            "but got '%s'.", predicate)

        output = []

        if not os.path.isfile(filename):
            logging.error(" Missing XRD peaks file '%s' for %s",
                          filename, str(uuid))
            return output

        #f = open(filename, "r", encoding="utf-8")
        with open(filename, "r", encoding="utf-8") as f:
            for il,line in enumerate(f):
                words = line.strip().split()
                #print(il, "=>", line.strip(), "=>", words)

                uuid_peak, _ = self.uuidDB.addUUID(self.crystOntoPrefix + "CharacteristicPeak",
                               self.abox_prefix + "CharacteristicPeak_" + "peak_" + str(il+1))
                output.append([uuid_peak, "Instance",
                               self.crystOntoPrefix + "CharacteristicPeak",
                               "", "", ""])

                output.append([subject, "Instance", uuid_peak, predicate, "", ""])

                output.append([self.crystOntoPrefix + "hasRelativeIntensity",
                               "Data Property", uuid_peak, "", words[6], "decimal"])

                output.append([self.crystOntoPrefix + "isSimulated", "Data Property",
                               uuid_peak, "", True, "boolean"])

                miller = ocdt.OntoVector(
                         class_name = "MillerIndices",
                         item_name  = self.abox_prefix + "MillerIndices_" + "peak_" + str(il+1),
                         uuidDB = self.uuidDB,
                         unit = "om:dimensionOne")

                miller.addComponent(label = "h", value = str(words[0]))
                miller.addComponent(label = "k", value = str(words[1]))
                miller.addComponent(label = "l", value = str(words[2]))
                output += miller.get_csv_arr(uuid_peak, self.crystOntoPrefix + "hasMillerIndices")

                #uuid_peak = tools.getUUID(self.uuidDB, "CharacteristicPeak",
            #            "CharacteristicPeak_" + zeoname + "_peak_" + str(il+1))
                #output.append([uuid_peak, "Instance", "CharacteristicPeak", "", "", ""])
                #output.append([subject, "Instance", uuid_peak, predicate, "", ""])

                output.append([self.crystOntoPrefix + "hasTwoThetaPosition", "Data Property",
                              uuid_peak, "", words[5], "decimal"])

                #output.append([self.crystOntoPrefix + "hasComponentLabel", "Data Property",
                #                   uuid_peak, "", self.value["label"], "string"])

        return output
        # === end of XRDSpectrum.loadXRDPeaks()

    # === end of class XRDSpectrum




