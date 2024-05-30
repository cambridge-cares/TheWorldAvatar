"""
    Part of ontozeolite package.
    Author: Rutkevych Pavlo, rutkevych.cares@gmail.com
    Date: 2024/04/01
"""

import os

import logging

import tools
import ontocrystal_datatypes as ocdt

# Choose the preferred level of logging:
#logging.basicConfig(level = logging.DEBUG)
#logging.basicConfig(level = logging.INFO)
logging.basicConfig(level = logging.WARNING)
#logging.basicConfig(level = logging.ERROR)

crystOntoPrefix = "https://www.theworldavatar.com/kg/ontocrystal/"

class XRDSpectrum:
    """ ABox generator for a single XRD spectrum.
    Data for each XRD spectrum is saves in a single file, and
    each file corresponds to UUID.
    A material may have multiple XRD spectra. In this case
    the material may have several xrd files and therefore several spectra.
    The file and the corresponding UUID are saved in file 'xrd_iri_list.csv'.
    """
    def __init__(self, abox_prefix):
        #self.abox_prefix = "https://www.theworldavatar.com/kg/ontozeolite/"
        self.abox_prefix = abox_prefix

        self.uuidDB = tools.UuidDB()
        self.iri = None
        self.uuid = None
        pass
        # XRDSpectrum.__init__()

    def get_csv_arr(self, subject, predicate, xrd_peak_path, new_uuid=None):
        """ xrd_peak_path is the path the the file with xrd peaks.
        In case there are several spectra for a given material,
        they should have different UUID.
        If new_uuid argument it not provided, a new UUID will be generated.
        I new_uuid is given, it will be used for the XRDSpectrum and its peaks.

        """
        output = []

        if not os.path.isfile(xrd_peak_path):
            logging.error(" Missing file with xrd peaks: '%s'. Skipping XRD.",
                          xrd_peak_path)
            return output

        self.iri, self.uuid = self.uuidDB.addUUID(crystOntoPrefix + "XRDSpectrum",
                                                  self.abox_prefix + "XRDSpectrum")
        output.append([self.iri, "Instance", crystOntoPrefix + "XRDSpectrum", "", "", ""])

        if subject != "":
            output.append([subject, "Instance", self.iri, predicate, "", ""])

        output += self.loadXRDPeaks(self.iri,
                                    crystOntoPrefix + "hasCharacteristicPeak",
                                    xrd_peak_path, new_uuid=self.uuid)

        return output
        # === end of XRDSpectrum.get_csv_arr()

    def loadXRDPeaks(self, subject, predicate, filename, new_uuid=None):
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
                          filename, str(new_uuid))
            return output

        with open(filename, "r", encoding="utf-8") as f:
            for il, line in enumerate(f):
                words = line.strip().split()

                if new_uuid is None:
                    peak_iri, peak_uuid = self.uuidDB.addUUID(
                                          crystOntoPrefix + "CharacteristicPeak",
                                          self.abox_prefix + "CharacteristicPeak_" + \
                                          "peak_" + str(il+1))
                else:
                    peak_uuid = new_uuid
                    peak_iri = self.abox_prefix + "CharacteristicPeak_" + \
                               "peak_" + str(il+1) + "_" + peak_uuid

                output.append([peak_iri, "Instance",
                               crystOntoPrefix + "CharacteristicPeak",
                               "", "", ""])

                output.append([subject, "Instance", peak_iri, predicate, "", ""])

                output.append([crystOntoPrefix + "hasRelativeIntensity",
                               "Data Property", peak_iri, "",
                               words[6], "decimal"])

                output.append([crystOntoPrefix + "isSimulated", "Data Property",
                               peak_iri, "", True, "boolean"])

                miller = ocdt.OntoVector(class_name="MillerIndices",
                                         item_name=self.abox_prefix + 
                                         "MillerIndices_" + "peak_" + str(il+1),
                                         uuidDB=self.uuidDB,
                                         unit="om:dimensionOne")

                miller.addComponent(label = "h", value = str(words[0]))
                miller.addComponent(label = "k", value = str(words[1]))
                miller.addComponent(label = "l", value = str(words[2]))
                output += miller.get_csv_arr(peak_iri,
                                 crystOntoPrefix + "hasMillerIndices",
                                 new_uuid=peak_uuid)

                output.append([crystOntoPrefix + "hasTwoThetaPosition", "Data Property",
                              peak_iri, "", words[5], "decimal"])

                #output.append([crystOntoPrefix + "hasComponentLabel", "Data Property",
                #               peak_iri, "", self.value["label"], "string"])

        return output
        # === end of XRDSpectrum.loadXRDPeaks()

    # === end of class XRDSpectrum

if __name__ == "__main__":
    pass
