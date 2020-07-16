from ..base.base_postprocessor import BasePostprocessor
import pkg_resources

class ArkanePostprocessor(BasePostprocessor):

    def run(self,log,data):
        ARKANE_SPECIES_TEMPLATE = pkg_resources.resource_string(__name__, 'data/arkane_input_species_template.py')
        ARKANE_INPUT_TEMPLATE = pkg_resources.resource_string(__name__, 'data/arkane_input_template.py')
        print(ARKANE_SPECIES_TEMPLATE)
        pass
        #1 load arkane templates
        #2 create input files (streams if possible)
        #3 run Arkane and get output
        #4 postprocess output - 
        #    Empirical formula
        #    Atom counts
        #    Rotational constants (convert these from moments of inertia with Angiras’ help)
        #    Rotational constants value
        #    Rotational constants units
        #    Rotational constants size
        #    Atomic mass value (use Arkane isotopes module?)

#
#def createArkaneFile(self):
#    ARKANE_SPECIES_TEMPLATE = pkg_resources.resource_string('CompChemParser', 'data/arkane_input_species_template.py')
#
#
#def run_Arkane(filePath):
#    species_file_writer(filePath)
#    Arkane_inpfile_writer(filePath)
#    Arkane_sys_call(filePath.replace(".log","_inp.py"))
#
##This function just gets the necessary filenames.
#def get_species_file_name(filePath):
#    with open(filePath, "rt") as gaulogfile:
#        gaufilename = os.path.basename(gaulogfile.name)
#        if ".log" in gaufilename:
#            gaufileroot = gaufilename.replace(".log","")
#            print(gaufileroot)
#        elif ".g09" in gaufilename:
#            gaufileroot = gaufilename.replace(".g09","")
#            print(gaufileroot)
#        gaufilestring = gaufileroot + '.py'
#        return gaufilename, gaufileroot, gaufilestring
#
##This function replaces keywords in the Arkane species template file
##with information parsed from the computational chemistry calculation.
##
##This is the preferred way to write the Arkane species file.
#def fill_arkane_species_template(filePath):
#    fname = get_species_file_name(filePath)[0]
#    inpfile = get_species_file_name(filePath)[2]
#    symm = getSymmetry_gauss(filePath)
#    mult = getMult_gauss(filePath)
#    lot = getLevelOfTheory(filePath)
#    checkWords = ("EXTERNALSYMMETRY","SPINMULTIPLICTY","LEVELOFTHEORY","NAMEOFLOGFILE")
#    repWords = (symm,mult,"'" + lot + "'","'" + fname + "'")
#    with open('arkane_input_myspecies_template.py','r') as f1:
#        with open(inpfile,'w') as f:
#            for line in f1:
#                for check, rep in zip(checkWords, repWords):
#                    line = line.replace(check, rep)
#                f.write(line)
#            f1.close()
#            f.close()
#
##This function replaces keywords in the Arkane input template file
##with information parsed from the computational chemistry calculation.
##
##This is the preferred way to write the Arkane input file.
#def fill_arkane_input_template(filePath):
#    lot = getLevelOfTheory(filePath)
#    froot = get_species_file_name(filePath)[1]
#    pyfname = get_species_file_name(filePath)[2]
#    filename = froot + '_inp.py'
#    checkWords = ("LEVELOFTHEORY","MYSPECIES","MYSPECPY")
#    repWords = ("'" + lot + "'","'" + froot + "'","'" + pyfname + "'")
#    with open('arkane_input_template.py','r') as f1:
#        with open(filename,'w') as f:
#            for line in f1:
#                for check, rep in zip(checkWords, repWords):
#                    line = line.replace(check, rep)
#                f.write(line)
#            f1.close()
#            f.close()
#
##This function uses information parsed from the computational chemistry
##calculation to write the Arkane species without using a template.#
##
##Arkane syntax is obeyed - note optical isomers is assumed to be 1 and
##the molecule is assumed to be non-linear (although Arkane does not care).
##
##This is NOT the preferred way to write the Arkane species file.
#def write_arkane_species_file(filePath):
#    fname = get_species_file_name(filePath)[0]
#    inpfile = get_species_file_name(filePath)[2]
#    symm = getSymmetry_gauss(filePath)
#    mult = getMult_gauss(filePath)
#    lot = getLevelOfTheory(filePath)
#    with open(inpfile,'w') as f:
#        f.write("bonds = {}" + "\n")
#        f.write('\n')
#        f.write('linear = False' + '\n')
#        f.write('\n')
#        f.write('externalSymmetry = ' + symm + '\n')
#        f.write('\n')
#        f.write('spinMultiplicity = ' + mult + '\n')
#        f.write('\n')
#        f.write('opticalIsomers = 1' + '\n')
#        f.write('\n')
#        f.write("energy = {"  + "'" + lot + "'" + ": Log(" + "'" + fname + "'" + "),}"  + "\n")
#        f.write('\n')
#        f.write("geometry = "  +  "Log(" + "'" + fname + "'" + ")" + "\n")
#        f.write('\n')
#        f.write("frequencies = "  +  "Log(" + "'" + fname + "'" + ")" + "\n")
#        f.write('\n')
#        f.close()
#
##This function writes the Arkane input file that will be executed by Arkane.
##Hindered rotors, Bond Corrections, and Atom Corrections are turned off by default.
##
##This is NOT the preferred way to write the Arkane input file.
#def write_arkane_input_file(filePath):
#    lot = getLevelOfTheory(filePath)
#    froot = get_species_file_name(filePath)[1]
#    pyfname = get_species_file_name(filePath)[2]
#    filename = froot + '_inp.py'
#    with open(filename,'w') as f:
#        f.write("modelChemistry = " + "'" + lot + "'"  + "\n")
#        f.write("useHinderedRotors = False" + "\n")
#        f.write("useBondCorrections = False" + "\n")
#        f.write("useAtomCorrections = False" + "\n")
#        f.write("species(" + "'" + froot + "'" + ',' + "'" + pyfname + "'" + ')' + "\n")
#        f.close()
#
##This function executes Arkane using a system call.
#def Arkane_sys_call(inpfilename):
#    os.system('Arkane.py ' + inpfilename)
#
#