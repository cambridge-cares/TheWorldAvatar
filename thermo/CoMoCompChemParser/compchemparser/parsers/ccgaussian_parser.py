import cclib
import os
import sys
from file_read_backwards import FileReadBackwards
import re
import helpers.utils as utils
import helpers.ccutils as ccutils
import helpers.elements_data as eld
from itertools import islice

# Supported keys
#-------------------------------------------------
# group 1 (atoms properties)
EMP_FORM = 'Empirical formula'
ATOM_TYPES = 'Atom types'
ATOM_MASSES = 'Atomic masses'
ATOM_MASSES_UNIT = 'Atomic mass unit'
ATOM_CHARGES = 'Atomic partial charges'
ATOM_CHARGES_UNIT = 'Atomic partial charge unit'
TOTAL_MASS = 'Total mass'
TOTAL_MASS_UNIT = 'Total mass unit'
# group 2 (level of theory)
METHOD = 'Method'
BASIS_SET = 'Basis set'
# group 3 (spin/charge)
SPIN_MULT = 'Spin multiplicity'
FORMAL_CHARGE = 'Formal charge'
FORMAL_CHARGE_UNIT = 'Formal charge unit'
# group 4 (geometry, rot properties)
GEOM = 'Geometry'
GEOM_TYPE = 'Geometry type'
ROT_SYM_NR = 'Rotational symmetry number'
ROT_CONST = 'Rotational constants'
ROT_CONST_NR = 'Rotational constants number'
ROT_CONST_UNIT = 'Rotational constants unit'
# group 5 (frequencies)
FREQ = 'Frequencies'
FREQ_NR = 'Frequencies number'
FREQ_UNIT = 'Frequencies unit'
# group 6 (energy)
TOTAL_ENERGY = 'Total energy'
ELECTRONIC_ENERGY = 'Electronic energy'
ZPE_ENERGY = 'ZPE energy'
# group 7 (program, run date)
PROGRAM_NAME = 'Program name'
PROGRAM_VERSION = 'Program version'
RUN_DATE = 'Run date'
# group 8 (misc)
MISC = 'Misc'

#-------------------------------------------------

# Keys used
#-------------------------------------------------
CCKEYS = [  
            EMP_FORM, ATOM_TYPES, ATOM_MASSES, ATOM_MASSES_UNIT,
            ATOM_CHARGES, ATOM_CHARGES_UNIT, TOTAL_MASS_UNIT, METHOD,
            BASIS_SET, SPIN_MULT, FORMAL_CHARGE, FORMAL_CHARGE_UNIT,
            GEOM, GEOM_TYPE, ROT_SYM_NR, ROT_CONST, ROT_CONST_NR,
            ROT_CONST_UNIT, FREQ, FREQ_NR, FREQ_UNIT, ELECTRONIC_ENERGY,
            ZPE_ENERGY, TOTAL_ENERGY, PROGRAM_NAME, PROGRAM_VERSION,
            RUN_DATE, MISC
        ]
#-------------------------------------------------

EXTRA_FOOTER_LINES_NR = 15

# regex for catching job start string
FSPLIT_RE = re.compile(r"^\s?Entering Link 1 =")
ATOMS_RE = re.compile(r"([a-zA-Z]+)(\d+)")
JOB_SUCCESS_RE = re.compile(r"^\s?Normal termination of Gaussian")
FOOTER_RE = re.compile(r"^\s(1\\1\\|1\|1\|)")

# unit conversion
EV_TO_HARTREE = 0.0367493237908520

class CcGaussianParser():
    def __init__(self):
        """ init the gaussian parser """
        self.parseddata = {key: None for key in CCKEYS}
        self.cclib_data = None

    def parse(self,logFile):
        # 1. check for nr of jobs in a log and if more than one
        #    split the log into the corresponding nr of files
        # 2. Read level of theory if possible
        # 3. Read composite method data

        split_logs = self.split_log_by_jobs(logFile)

        for log in split_logs:
            # move this check to split_logs?
            optdone = ccutils.get_ccattr(log,attr='optdone')
            if optdone:
                self.parse_log_footer(log)
                self.parse_log_body(log)

    def split_log_by_jobs(self,logFile):
        def get_job_success(buffer):
            for line in islice(buffer, len(buffer)-10, len(buffer)):
                if JOB_SUCCESS_RE.match(line):
                    job_success = True
                    break
            return job_success
        # For Gaussian logs, we decided to support logs with multiple jobs in them.
        # The code reads the Gaussian log line by line searching for the "Entering Link 1 ="
        # string which should appear only once per job. If a second instance of that string
        # is found, the code dumps the job content to a temp file.        

        jobs_nr = 0 # nr of found jobs
        log_names = [] # names of created temp files to be parsed by cclib
        job_success = False
        buffer = [] # array to store file blocks (jobs)

        # Open the log file with read only permit
        flog = open(logFile,'r')
        # use readline() to read the first line
        line = flog.readline()
        # use the read line to read further. If the file is not empty keep reading one line
        # at a time, till the file is empty
        while line:
            # use realine() to read next line
            line = flog.readline()            

            if FSPLIT_RE.match(line):
                # job start string found
                jobs_nr = jobs_nr + 1
                # only process the read content and dump it to a temp file
                # from the second job onwoards
                if jobs_nr > 1:
                    # check the line for the job start string
                    job_success = get_job_success(buffer)
                    if job_success:
                        # set temp file name and dump read content to it
                        log_names.append(logFile + '_#' + str(jobs_nr))
                        fjob_log = open(log_names[-1],'w')
                        fjob_log.writelines(buffer[:-2])
                        fjob_log.close()
                    # reset the buffer
                    buffer = []
                    job_success = False
                else:
                    buffer.append(line)
            else:
                buffer.append(line)                
        flog.close()


        job_success = get_job_success(buffer)
        if jobs_nr == 1 and job_success:            
            log_names.append(logFile)

        return log_names


    def parse_log_footer(self,logFile):
        def get_footer(logFile):
            footer = ''
            extra_lines = []
            #
            buffer =[]            
            extra_lines_mode = False
            extra_lines_cnr = 0
    
            with FileReadBackwards(logFile, encoding="utf-8") as frb:
                while True:
                    line = frb.readline()
    
                    if not extra_lines_mode:
                        buffer.append(line.rstrip('\r\n'))
                    mfooter = FOOTER_RE.match(line)
                    if extra_lines_mode:
                        extra_lines_cnr = extra_lines_cnr + 1
                        extra_lines.append(line)
                    if extra_lines_cnr >= EXTRA_FOOTER_LINES_NR-1:
                        break
                    if mfooter:                                      
                        if EXTRA_FOOTER_LINES_NR > 0: extra_lines_mode = True
    
            buffer.reverse()
            footer= ''.join(buffer)
            footer= footer.split('\\')
            extra_lines.reverse()
            return footer, extra_lines

        footer, extra_lines = get_footer(logFile)

        
        # extract footer data
        self.parseddata[MISC] = footer[3].strip()
        self.parseddata[METHOD] = footer[4].strip()
        self.parseddata[BASIS_SET] = footer[5].strip()

        self.parseddata[EMP_FORM] = {}
        for (at, ac) in re.findall(ATOMS_RE, footer[6].strip().split('(')[0]):
            self.parseddata[EMP_FORM][at.upper()] = int(ac)

        self.parseddata[FORMAL_CHARGE] = int(footer[15].strip().split(',')[0])
        self.parseddata[FORMAL_CHARGE_UNIT] = 'atomic'
        self.parseddata[SPIN_MULT] = int(footer[15].strip().split(',')[1])
        #
        rundate = footer[-1].replace('.','').split()
        self.parseddata[RUN_DATE] = ' '.join(rundate[-5:])

        # extract data (if any) from extra n lines above the footer - used for
        # reading composite method total energy
        method = self.parseddata[METHOD][-1]
        for line in extra_lines:
            if 'E(ZPE)' in line and 'E(Thermal)' in line:
                zpe_en = line.split('=')[1]
                zpe_en = zpe_en.split()[0].strip()
                self.parseddata[ZPE_ENERGY] = float(zpe_en)
            if method in line and '(0 K)' in line:
                tot_energy = line.split('=')[1]
                tot_energy = tot_energy.split()[0].strip()
                self.parseddata[TOTAL_ENERGY] = float(tot_energy)
                if self.parseddata[ZPE_ENERGY]:
                    self.parseddata[ELECTRONIC_ENERGY] = self.parseddata[TOTAL_ENERGY]-self.parseddata[ZPE_ENERGY]
                break

    def parse_log_body(self,logFile):
        self.cclib_data = cclib.io.ccread(logFile)

        self.set_coordinates()
        self.set_frequencies()
        self.set_atom_types()
        self.set_atom_masses()
        self.set_partial_charges()
        self.set_ccpackage_info()
        self.set_rotconst_info()
        self.set_geom_type()
        self.set_zpve()
        self.set_energies()
        pass


    def set_coordinates(self):
        # read last coordinates entry from clib atomcoords
        if hasattr(self.cclib_data,'atomcoords'):
            self.parseddata[GEOM] = self.cclib_data.atomcoords[-1]

    def set_frequencies(self):
        # read frequencies entry from clib vibfreqs (only one set stored)
        if hasattr(self.cclib_data,'vibfreqs'):
            self.parseddata[FREQ] = self.cclib_data.vibfreqs
            self.parseddata[FREQ_NR] = len(self.parseddata[FREQ])
            self.parseddata[FREQ_UNIT] = '1/cm'

    def set_atom_types(self):
        # use cclib atomnos entry which stores the atomic number of
        # each element to get the element type from the internal elements data table.
        if hasattr(self.cclib_data,'atomnos'):
            self.parseddata[ATOM_TYPES] = [eld.get_el_symbol_by_atomic_nr(x) for x in self.cclib_data.atomnos]

    def set_atom_masses(self):
        if hasattr(self.cclib_data,'atommasses'):
            # try to read masses of elements from cclib atommasses
            # cclib appends masses to one list, so if these are repeated in the log
            # the list contains more entries that we need. Therefore I only read
            # n entries from the back where n is the number of atom types that is
            # already known
            atwts = self.cclib_data.atommasses[-len(self.parseddata[ATOM_TYPES]):]
            self.parseddata[ATOM_MASSES] = atwts
            self.parseddata[ATOM_MASSES_UNIT] = 'atomic'
            self.parseddata[TOTAL_MASS] = sum(atwts)
            self.parseddata[TOTAL_MASS_UNIT] = 'atomic'
        else:
            # read masses of elements from internal elements data table
            atwts = []
            for at in self.parseddata[ATOM_TYPES]:
                atwti = eld.get_el_wt_by_symbol(at)
                if atwti > 0.0:
                    atwts.append(atwti)
                else:
                    atwts = []
                    break
            if atwts:
                self.parseddata[ATOM_MASSES] = atwts
                self.parseddata[ATOM_MASSES_UNIT] = 'atomic'
                self.parseddata[TOTAL_MASS] = sum(atwts)
                self.parseddata[TOTAL_MASS_UNIT] = 'atomic'

    def set_partial_charges(self):
        if hasattr(self.cclib_data,'atomcharges'): 
            self.parseddata[ATOM_CHARGES] = self.cclib_data.atomcharges
            self.parseddata[ATOM_CHARGES_UNIT] = 'atomic'

    def set_ccpackage_info(self):
        if hasattr(self.cclib_data,'metadata'):
            if 'package' in self.cclib_data.metadata.keys():
                self.parseddata[PROGRAM_NAME] = self.cclib_data.metadata['package']
            if 'package_version' in self.cclib_data.metadata.keys():
                self.parseddata[PROGRAM_VERSION] = self.cclib_data.metadata['package_version']

    def set_rotconst_info(self):
        if self.parseddata[ATOM_MASSES] is not None and self.parseddata[GEOM].any() is not None:
            if self.parseddata[GEOM].shape[0] > 1:
                self.parseddata[ROT_CONST] = utils.getRotConst(self.parseddata[ATOM_MASSES],self.parseddata[GEOM])
                self.parseddata[ROT_CONST_NR] = len(self.parseddata[ROT_CONST])
                self.parseddata[ROT_CONST_UNIT] = 'GHz'

    def set_geom_type(self):
        if self.parseddata[GEOM].shape[0] > 1:
            if self.parseddata[ROT_CONST_NR] > 1:
                self.parseddata[GEOM_TYPE] = 'nonlinear'
            else:
                self.parseddata[GEOM_TYPE] = 'linear'
        else:
            self.parseddata[GEOM_TYPE] = 'atomic'

    def set_zpve(self):
        if hasattr(self.cclib_data,'zpve') and not self.parseddata[ZPE_ENERGY]:
            self.parseddata[ZPE_ENERGY] = self.cclib_data.zpve

    def set_energies(self):
        if hasattr(self.cclib_data,'ccenergies') and not self.parseddata[TOTAL_ENERGY]:
            self.parseddata[TOTAL_ENERGY] = self.cclib_data.ccenergies[-1] * EV_TO_HARTREE
        elif hasattr(self.cclib_data,'scfenergies') and not self.parseddata[ELECTRONIC_ENERGY]:
            self.parseddata[ELECTRONIC_ENERGY] = self.cclib_data.scfenergies[-1] * EV_TO_HARTREE
            pass