import cclib
import os
import sys
from file_read_backwards import FileReadBackwards
import re
import compchemparser.helpers.utils as utils
import compchemparser.helpers.ccutils as ccutils
import compchemparser.helpers.elements_data as eld
from itertools import islice
import json


# Keys/values uploaded to the KG
#-------------------------------------------------
# group 1 (atoms properties)
EMP_FORMULA = 'Empirical formula'
ATOM_COUNTS = 'Atom counts'
ATOM_TYPES = 'Atom types'
ATOM_MASSES = 'Atomic masses'
ATOM_MASSES_UNIT = 'Atomic mass unit'
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
GEOM_UNIT = 'Geometry unit'
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
ELECTRONIC_ZPE_ENERGY = 'Electronic + ZPE energy'
ELECTRONIC_ENERGY = 'Electronic energy'
# group 7 (program, run date)
PROGRAM_NAME = 'Program name'
PROGRAM_VERSION = 'Program version'
RUN_DATE = 'Run date'

# Misc keys, not uploaded to the KG
#-------------------------------------------------
MISC = 'Misc'
ZPE_ENERGY = 'ZPE energy'
CAS_ENERGY = 'CASSCF energy'
CAS_MP2_ENERGY = 'CASSCF MP2 energy'
CI_ENERGY = 'CI energy'
TD_ENERGY = 'TD energy'

#-------------------------------------------------

# Keys used
#-------------------------------------------------
CCKEYS_DATA = [  
            EMP_FORMULA, ATOM_COUNTS, ATOM_TYPES, ATOM_MASSES, ATOM_MASSES_UNIT,
            TOTAL_MASS_UNIT, METHOD, BASIS_SET, SPIN_MULT, FORMAL_CHARGE, FORMAL_CHARGE_UNIT,
            GEOM, GEOM_UNIT, GEOM_TYPE, ROT_SYM_NR, ROT_CONST, ROT_CONST_NR,
            ROT_CONST_UNIT, FREQ, FREQ_NR, FREQ_UNIT, ELECTRONIC_ENERGY,
            ELECTRONIC_ZPE_ENERGY, PROGRAM_NAME, PROGRAM_VERSION,
            RUN_DATE
        ]
CCKEYS_MISC = [ZPE_ENERGY, MISC, CAS_ENERGY, CAS_MP2_ENERGY, CI_ENERGY, TD_ENERGY]
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
        self.cclib_data = None

    def parse(self,logFile):
        # inner functions
        #--------------------------------
        def get_job_success(buffer):
            for line in islice(buffer, len(buffer)-10, len(buffer)):
                if JOB_SUCCESS_RE.match(line):
                    job_success = True
                    break
            return job_success
        def split_log_by_jobs(logFile):
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
            line = 'start'
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
                            log_names.append(logFile + '_#' + str(jobs_nr-1))
                            fjob_log = open(log_names[-1],'w')
                            fjob_log.writelines(buffer[:-2])
                            fjob_log.close()
                        # reset the buffer
                        buffer = buffer[-2:]
                        job_success = False
                    else:
                        buffer.append(line)
                else:
                    buffer.append(line)                
            flog.close()


            job_success = get_job_success(buffer)
            if jobs_nr == 1 and job_success:
                log_names.append(logFile)
            elif jobs_nr > 1 and job_success:
                log_names.append(logFile + '_#' + str(jobs_nr))
                fjob_log = open(log_names[-1],'w')
                fjob_log.writelines(buffer)
                fjob_log.close()

            return log_names
        #--------------------------------

        uploaddata = []
        split_logs = split_log_by_jobs(logFile)

        for log in split_logs:
            parseddata = self.parse_log(log)
            json_data = json.dumps(parseddata)
            uploaddata.append(json_data)

            if log != logFile:
                os.remove(log)
        return uploaddata


    def parse_log(self,logFile):
        #================================================
        # inner functions for getting/setting specific info
        #================================================
        def set_ccpackage_info(data):
            if hasattr(self.cclib_data,'metadata'):
                if 'package' in self.cclib_data.metadata.keys():
                    data[PROGRAM_NAME] = self.cclib_data.metadata['package']
                if 'package_version' in self.cclib_data.metadata.keys():
                    data[PROGRAM_VERSION] = self.cclib_data.metadata['package_version']
        #---------------------------------------------
        def set_geom_type(data):
            if data[ROT_CONST_NR] is not None and \
            data[ATOM_TYPES] is not None:
            
                if len(parseddata[ATOM_TYPES]) == 1:
                    data[GEOM_TYPE] = 'atomic'
                elif data[ROT_CONST_NR] == 1:
                    data[GEOM_TYPE] = 'linear'
                else:
                    data[GEOM_TYPE] = 'nonlinear'
        #---------------------------------------------
        def check_charge_spin_mult(data, cur_line, log_lines):
            line = log_lines[cur_line]
            if 'Charge =' in line and  'Multiplicity =' in line:
                splt_line = line.split('=')
                data[FORMAL_CHARGE] = int(splt_line[1].split()[0])
                data[FORMAL_CHARGE_UNIT] = 'atomic'
                data[SPIN_MULT] = int(splt_line[2])
            return cur_line
        #---------------------------------------------
        def check_geom(data, cur_line,log_lines):
            line = log_lines[cur_line]
            if 'Input orientation:' in line:
                data[GEOM] = []
                data[ATOM_TYPES] = []

                cur_line = cur_line + 2
                line = log_lines[cur_line].strip().split()[4]
                data[GEOM_UNIT] = line.replace('(','').replace(')','')

                cur_line = cur_line + 3
                line = log_lines[cur_line].strip()             
                while '---' not in line:
                    line = line.split()
                    data[GEOM].append([float(line[3]), float(line[4]), float(line[5])])
                    el = eld.get_el_symbol_by_atomic_nr(int(line[1].strip()))
                    data[ATOM_TYPES].append(el)
                    cur_line = cur_line + 1
                    line = log_lines[cur_line].strip()
            return cur_line
        #---------------------------------------------
        def check_elweights(data, cur_line,log_lines):
            line = log_lines[cur_line]
            if 'AtmWgt=' in line:
                data[ATOM_MASSES] = []

                while 'Leave Link' not in line:
                    if 'AtmWgt=' in line:
                        line = line.split()
                        for wt in line[1:]:
                            data[ATOM_MASSES].append(float(wt))
                    cur_line = cur_line + 1
                    line = log_lines[cur_line].strip()
                data[ATOM_MASSES_UNIT] = 'amu'
                data[TOTAL_MASS] = sum(data[ATOM_MASSES])
                data[TOTAL_MASS_UNIT] = 'amu'

            if data[ATOM_MASSES] is None:
                if '- Thermochemistry -' in line:
                    data[ATOM_MASSES] = []
                    while 'Molecular mass:' not in line:
                        if 'Atom' in line and 'has atomic number' in line and 'and mass' in line:
                            line = line.split('and mass')[1]
                            data[ATOM_MASSES].append(float(line))
                        cur_line = cur_line + 1
                        line = log_lines[cur_line].strip()
                    data[ATOM_MASSES_UNIT] = 'amu'
                    data[TOTAL_MASS] = sum(data[ATOM_MASSES])
                    data[TOTAL_MASS_UNIT] = 'amu'
            return cur_line
        #---------------------------------------------
        def check_freq(data, cur_line,log_lines):
            line = log_lines[cur_line]
            if 'Frequencies -- ' in line:
                data[FREQ] = []
                data[FREQ_UNIT] = 'cm^-1'

                while 'Thermochemistry' not in line:
                    if 'Frequencies -- ' in line:
                        line = line.split('--')[1]
                        line = line.split()
                        for f in line:
                            data[FREQ].append(float(f))
   
                    cur_line = cur_line + 1
                    line = log_lines[cur_line].strip()
                data[FREQ_NR] = len(data[FREQ])
            return cur_line
        #---------------------------------------------
        def check_rot_sym_nr(data, cur_line,log_lines):
            line = log_lines[cur_line]
            if 'Rotational symmetry number' in line:
                line = line.split()[-1].replace('.','')
                data[ROT_SYM_NR] = float(line)
            return cur_line
        #---------------------------------------------
        def check_rot_const(data, cur_line,log_lines):
            line = log_lines[cur_line]
            if 'Rotational constant (' in line:
                data[ROT_CONST] = []
                # get unit
                line = line.split('(')[1]
                line = line.split(')')
                data[ROT_CONST_UNIT] = line[0]

                # get rot consts
                line = line[1].replace(':','').split()
                for rc in line:
                    data[ROT_CONST].append(float(rc))
                data[ROT_CONST_NR] = len(data[ROT_CONST])
            return cur_line
        #---------------------------------------------
        def check_zpe(misc, cur_line,log_lines):
            line = log_lines[cur_line]
            if 'Zero-point correction=' in line:
                line = line.split('Zero-point correction=')[1]
                line = line.split()[0]
                misc[ZPE_ENERGY] = float(line)
            return cur_line
        #---------------------------------------------
        def check_E0_zpe(data, cur_line,log_lines):
            line = log_lines[cur_line]
            if 'Sum of electronic and zero-point Energies=' in line:
                line = line.split('Sum of electronic and zero-point Energies=')[1]
                line = line.split()[0]
                data[ELECTRONIC_ZPE_ENERGY] = float(line)
            return cur_line
        #---------------------------------------------
        def check_E0(data, cur_line,log_lines):
            line = log_lines[cur_line]
            if 'SCF Done' in line:
                line = line.split('=')[1]
                line = line.split()[0]
                data[ELECTRONIC_ENERGY] = float(line)
            return cur_line
        #---------------------------------------------
        def check_Casscf_E0(misc, cur_line,log_lines):
            line = log_lines[cur_line]
            if 'ITN=' in line and 'MaxIt=' in line and 'E=' in line and 'DE=' in line and 'Acc=' in line and 'Lan=' in line:
                line = line.split('E=')[1].strip()
                line = line.split()[0]
                misc[CAS_ENERGY] = float(line)
            return cur_line
        #---------------------------------------------
        def check_Casscf_MP2_E0(misc, cur_line,log_lines):
            line = log_lines[cur_line]
            if 'Multireference MP2 with'.upper() in line.upper():
                cur_line = cur_line + 1
                line = log_lines[cur_line].strip()
                while True:
                    if 'E2 =' in line and 'EUMP2 =' in line:
                        line = line.split('=')[-1].strip()
                        line = line.replace('D','E')
                        misc[CAS_MP2_ENERGY] = float(line)
                        break
                    cur_line = cur_line + 1
                    line = log_lines[cur_line].strip()
            return cur_line
        #---------------------------------------------
        def check_CI_E0(misc, cur_line,log_lines):
            line = log_lines[cur_line]
            if 'E(CI'.upper() in line.upper():
                if 'DE(CI'.upper() in line.upper():
                    line = line.split(' E(CI')[1].strip()
                    line = line.split()[1]
                else:
                    line = line.split('=')[1].strip()
                misc[CI_ENERGY] = float(line)
            return cur_line
        #---------------------------------------------
        def check_TD_E0(misc, cur_line,log_lines):
            line = log_lines[cur_line]
            if 'Total Energy, E(TD'.upper() in line.upper():
                line = line.split('=')[1].strip()
                misc[TD_ENERGY] = float(line)
            return cur_line
        #---------------------------------------------
        def correct_Casscf_Mp2_method(data, misc):
            if misc[CAS_MP2_ENERGY] is not None and 'CAS' not in data[METHOD]:
                data[METHOD] = 'CASSCF MP2'
        #---------------------------------------------
        def parse_footer(data, misc, cur_line,log_lines):
            lines_above_footer = log_lines[cur_line-EXTRA_FOOTER_LINES_NR:cur_line]
            footer = log_lines[cur_line:]
            footer = ''.join(footer)
            if '1|1|' in footer[0:5].strip():
                footer= footer.split('|')
            else:
                footer= footer.split('\\')

            # extract footer data
            misc[MISC] = footer[3].strip()
            data[METHOD] = footer[4].strip()
            data[BASIS_SET] = footer[5].strip()
            data[EMP_FORMULA] = footer[6].strip().split('(')[0]
            data[ATOM_COUNTS] = {}
            for (at, ac) in re.findall(ATOMS_RE, data[EMP_FORMULA]):
                data[ATOM_COUNTS][at.upper()] = int(ac)

            #charge = int(footer[15].strip().split(',')[0])
            #spin_mult = int(footer[15].strip().split(',')[1])
            #
            rundate = footer[-1].replace('.','').split()
            data[RUN_DATE] = ' '.join(rundate[-5:])

            #if misc == 'Mixed'
            for line in lines_above_footer:
                if '(0 K)=' in line and 'Energy=' in line:
                    line = line.split('=')[1]
                    data[ELECTRONIC_ZPE_ENERGY] = float(line.split()[0].strip())
                    break
        #---------------------------------------------
        def resolve_energy(data, misc):
            if not 'Mixed' in misc[MISC]:
                if 'CCS' in data[METHOD] or 'QCI' in data[METHOD]:
                    if hasattr(self.cclib_data,'ccenergies'):
                        data[ELECTRONIC_ENERGY] = self.cclib_data.ccenergies[-1] * EV_TO_HARTREE
                elif 'CASSCF MP2' in data[METHOD] and misc[CAS_MP2_ENERGY] is not None:
                    data[ELECTRONIC_ENERGY] = misc[CAS_MP2_ENERGY]
                elif 'CASSCF' in data[METHOD] and misc[CAS_ENERGY] is not None:
                    data[ELECTRONIC_ENERGY] = misc[CAS_ENERGY]
                elif 'MP' in data[METHOD]:
                    if hasattr(self.cclib_data,'mpenergies'): #-75.4186235436
                        emp = self.cclib_data.mpenergies[-1][-1] * EV_TO_HARTREE
                        data[ELECTRONIC_ENERGY] = emp
                elif 'CI' in data[METHOD] and misc[CI_ENERGY] is not None:
                    data[ELECTRONIC_ENERGY] = misc[CI_ENERGY]
                elif 'TD' in data[METHOD] and misc[TD_ENERGY] is not None:
                    data[ELECTRONIC_ENERGY] = misc[TD_ENERGY]
        #================================================

        #================================================
        # parse_log body
        #================================================
        # run cclib first
        parseddata = {key: None for key in CCKEYS_DATA}
        parsedmisc = {key: None for key in CCKEYS_MISC}

        self.cclib_data = cclib.io.ccread(logFile)
        set_ccpackage_info(parseddata)

        # read an entire log into list
        footer_line = -1
        with open(logFile) as flog:
            log_lines = flog.readlines()

        # start parsing
        cur_line = 0
        line = log_lines[cur_line]
        while True:
            cur_line = check_charge_spin_mult(parseddata, cur_line, log_lines)
            cur_line = check_geom(parseddata, cur_line, log_lines)
            cur_line = check_freq(parseddata, cur_line, log_lines)
            cur_line = check_elweights(parseddata, cur_line, log_lines)
            cur_line = check_rot_sym_nr(parseddata, cur_line,log_lines)
            cur_line = check_rot_const(parseddata, cur_line,log_lines)
            cur_line = check_zpe(parsedmisc, cur_line,log_lines)
            cur_line = check_E0_zpe(parseddata, cur_line,log_lines)
            cur_line = check_E0(parseddata, cur_line,log_lines)
            cur_line = check_CI_E0(parsedmisc, cur_line,log_lines)
            cur_line = check_TD_E0(parsedmisc, cur_line,log_lines)
            cur_line = check_Casscf_E0(parsedmisc, cur_line,log_lines)
            cur_line = check_Casscf_MP2_E0(parsedmisc, cur_line,log_lines)

            # check if we have reached the log footer
            # if so, record the line number
            mfooter = FOOTER_RE.match(line)
            if mfooter:
                footer_line = cur_line

            cur_line = cur_line + 1
            if cur_line >= len(log_lines):
                break
            line = log_lines[cur_line]

        # parse the log file footer
        if footer_line > 0:
            parse_footer(parseddata,parsedmisc,footer_line,log_lines)

        set_geom_type(parseddata)        
        correct_Casscf_Mp2_method(parseddata, parsedmisc)
        resolve_energy(parseddata, parsedmisc)

        return parseddata