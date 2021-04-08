import cclib
import os
import sys
import re
import compchemparser.helpers.utils as utils
import compchemparser.helpers.ccutils as ccutils
import compchemparser.helpers.elements_data as eld
from itertools import islice
import json


# keys/values uploaded to the kg
#-------------------------------------------------
# group 1 (atoms properties)
EMP_FORMULA = 'Empirical formula'
ATOM_COUNTS = 'Atom counts'
ATOM_TYPES = 'Atom types'
ATOM_MASSES = 'Atomic masses'
ATOM_MASSES_UNIT = 'Atomic mass unit'
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
ELECTRONIC_ZPE_ENERGY = 'Electronic and ZPE energy'
ELECTRONIC_ENERGY = 'Electronic energy'
HOMO_ENERGY = 'HOMO energy'
HOMO_MIN_1_ENERGY = 'HOMO-1 energy'
HOMO_MIN_2_ENERGY = 'HOMO-2 energy'
LUMO_ENERGY = 'LUMO energy'
LUMO_PLUS_1_ENERGY = 'LUMO+1 energy'
LUMO_PLUS_2_ENERGY = 'LUMO+2 energy'
# group 7 (program, run date, scan specifics)
PROGRAM_NAME = 'Program name'
PROGRAM_VERSION = 'Program version'
RUN_DATE = 'Run date'
SCANFLAG = 'ScanFlag'
SCANPOINTS = 'Scan Points'

# misc keys, not uploaded to the kg
# mostly used for inferring other properties
#-------------------------------------------------
MISC = 'Misc'
ZPE_ENERGY = 'ZPE energy'
CAS_ENERGY = 'CASSCF energy'
CAS_MP2_ENERGY = 'CASSCF MP2 energy'
CI_ENERGY = 'CI energy'
TD_ENERGY = 'TD energy'
#-------------------------------------------------

# collate keys to be uploaded to the kg into a single list
#-------------------------------------------------
CCKEYS_DATA = [
            EMP_FORMULA, ATOM_COUNTS, ATOM_TYPES, ATOM_MASSES, ATOM_MASSES_UNIT,
            METHOD, BASIS_SET, SPIN_MULT, FORMAL_CHARGE, FORMAL_CHARGE_UNIT,
            GEOM, GEOM_UNIT, GEOM_TYPE, ROT_SYM_NR, ROT_CONST, ROT_CONST_NR,
            ROT_CONST_UNIT, FREQ, FREQ_NR, FREQ_UNIT, ELECTRONIC_ENERGY,
            ELECTRONIC_ZPE_ENERGY,HOMO_ENERGY,HOMO_MIN_1_ENERGY ,
            HOMO_MIN_2_ENERGY,LUMO_ENERGY,LUMO_PLUS_1_ENERGY,LUMO_PLUS_2_ENERGY,
            PROGRAM_NAME, PROGRAM_VERSION,
            RUN_DATE, SCANFLAG, SCANPOINTS
        ]

# collate misc keys
CCKEYS_MISC = [ZPE_ENERGY, MISC, CAS_ENERGY, CAS_MP2_ENERGY, CI_ENERGY, TD_ENERGY]
#-------------------------------------------------

# this specifies how many lines above the gaussian log file footer block to read
# used for parsing composite method energies
EXTRA_FOOTER_LINES_NR = 15

# pre-compiled regex for catching various data from Gaussian log files
FSPLIT_RE = re.compile(r"^\s?Entering Link 1 =")
ATOMS_RE = re.compile(r"([a-zA-Z]+)(\d+)")
JOB_SUCCESS_RE = re.compile(r"^\s?Normal termination of Gaussian")
FOOTER_RE = re.compile(r"^\s(1\\1\\|1\|1\|)")
ROT_CONST_RE = re.compile(r"Rotational constants? \(")
HOMO_RE = re.compile(r"Alpha\s*?occ\. eigenvalues \-\-")
LUMO_RE = re.compile(r"Alpha\s*?virt\. eigenvalues \-\-")

# energy conversion factor between cclib eV and Hartrees
EV_TO_HARTREE = 0.0367493237908520

# main Gaussian parser class
class CcGaussianParser():
    def __init__(self):
        """ init the gaussian parser """
        # it is convenient to also store the cclib data
        # the actual parsed data are not stored, though it would
        # be simple to add here
        self.cclib_data = None

    # 1- splits the log file into multiple logs if the log contains linked jobs
    # 2- calls parse_log function to parse split log files
    def parse(self,logFile):
        #================================================
        # inner functions
        #================================================
        def get_job_success(buffer):
            # inspect the last 10 lines from the job to see
            # if it has terminated normally
            job_success = False
            start_slice = len(buffer)-EXTRA_FOOTER_LINES_NR
            if start_slice < 0: start_slice = 0
            for line in islice(buffer, start_slice, len(buffer)):
                if JOB_SUCCESS_RE.match(line):
                    job_success = True
                    break
            return job_success
        #---------------------------------------------
        def split_log_by_jobs(logFile):
            # For Gaussian logs, we decided to support logs with multiple jobs in them.
            # The code reads the Gaussian log line by line searching for the "Entering Link 1 ="
            # string which should appear only once per job. If a second instance of that string
            # is found, the code dumps the job content to a temp file.
            # If not for the fact that cclib requires as an input a path to the log file I would
            # not write temp log files and instead process them using already stored content in
            # memory.

            jobs_nr = 0 # nr of found jobs
            log_names = [] # names of created temp log files to be parsed
             # I do not allow parsing jobs which failed,  each log file MUST HAVE
             # 'Normal termination of Gaussian' string at the end
             # this prevents parsing errors later on
            job_success = False
            buffer = [] # array to store file blocks (jobs)

            # Open the log file with read only permit
            flog = open(logFile,'r')
            line = 'start' # set line to sth non empty so that the whil condition wont fail
            # use the read line to read further. If the file is not empty keep reading one line
            # at a time, till the file is empty
            while line:
                # use realine() to read file line by line
                line = flog.readline()

                if FSPLIT_RE.match(line):
                    # job start string found
                    jobs_nr = jobs_nr + 1
                    # only process the read content and dump it to a temp file
                    # from the second job onwoards, this is because in a non linked
                    # Gaussian log there will be always one job and there is no need
                    # to write this file again to a temp file
                    if jobs_nr > 1:
                        # check if the found job was successful
                        job_success = get_job_success(buffer)
                        print('    PARSER_INFO: Found job '+str(jobs_nr-1)+', job success: '+str(job_success))
                        if job_success:
                            # set temp file name and dump read content to it
                            log_names.append(logFile + '_#' + str(jobs_nr-1))
                            fjob_log = open(log_names[-1],'w')
                            fjob_log.writelines(buffer[:-2])
                            fjob_log.close()
                        # reset the buffer
                        # the buffer now stores two lines for the next job
                        buffer = buffer[-2:]
                        job_success = False
                    else:
                        buffer.append(line)
                else:
                    buffer.append(line)
            flog.close()

            # after the above loop the buffer should contain the last job.
            # this can be either the only job from a non linked Gaussian log
            # or indeed the last job from a linked log
            # we need to again test for its success
            job_success = get_job_success(buffer)
            if jobs_nr == 0:
                print('    PARSER_INFO: No valid jobs found.')
            elif jobs_nr == 1:
                print('    PARSER_INFO: Found job '+str(jobs_nr)+', job success: '+str(job_success))
                # if the jobs number was one, this is a non linked log
                # so there is no need in writing it again to a temp file
                # just add the log name as is to the final log_names list
                if job_success: log_names.append(logFile)
            elif jobs_nr > 1:
                print('    PARSER_INFO: Found job '+str(jobs_nr)+', job success: '+str(job_success))
                # this is the last job from a linked log
                # write this job to a temp file and append its name
                # to the log_names list
                if job_success:
                    log_names.append(logFile + '_#' + str(jobs_nr))
                    fjob_log = open(log_names[-1],'w')
                    fjob_log.writelines(buffer)
                    fjob_log.close()

            return log_names
        #---------------------------------------------
        def split(a, n):
            #This function splits a list into k parts of approximately equal length.
            k, m = divmod(len(a), n)
            return (a[i * k + min(i, m):(i + 1) * k + min(i + 1, m)] for i in range(n))

        #================================================

        #================================================
        # parse body
        #================================================

        uploaddata = [] # final list that would store json data
        # split the log files if multiply jobs found
        split_logs = split_log_by_jobs(logFile)

        # loop thorugh each log and parse it
        for log in split_logs:
            parseddata = self.parse_log(log)
            json_data = json.dumps(parseddata)
            uploaddata.append(json_data)
            #dict_data = json.loads(json_data)
            #with open(log.replace('.log','.json'), 'w') as outfile:
                #json.dump(dict_data, outfile, indent = 4)
            # in case of multiple jobs, remove any
            # temp logs that were created
            # do not remove the original log file
            if log != logFile:
                os.remove(log)
        #print(uploaddata)
        return uploaddata

    # main parse function
    def parse_log(self,logFile):
        #================================================
        # inner functions for getting/setting specific info
        #================================================
        def set_ccpackage_info(data):
            # sets the package and version info using cclib
            if hasattr(self.cclib_data,'metadata'):
                if 'package' in self.cclib_data.metadata.keys():
                    data[PROGRAM_NAME] = self.cclib_data.metadata['package']
                if 'package_version' in self.cclib_data.metadata.keys():
                    data[PROGRAM_VERSION] = self.cclib_data.metadata['package_version']
        #---------------------------------------------
        def set_geom_type(data):
            # sets geometry type based on nr of atoms and rot constants in a molecule
            if data[ROT_CONST_NR] is not None and \
            data[ATOM_TYPES] is not None:            
            
                if data[ROT_CONST_NR] == 1:
                    data[GEOM_TYPE] = 'linear'
                else:
                    data[GEOM_TYPE] = 'nonlinear'
        # If species has one atom then it generates geometry type as atomic. Fixed by Angiras Menon (am2145@cam.ac.uk).     
            elif data[ROT_CONST_NR] is None and \
            data[ATOM_TYPES] is not None:
                if len(parseddata[ATOM_TYPES]) == 1:
                    data[GEOM_TYPE] = 'atomic'
            
        #---------------------------------------------
        def check_charge_spin_mult(data, cur_line, log_lines):
            # tries to extract charge and spin multiplicity from a log file line
            line = log_lines[cur_line]
            if 'Charge =' in line and  'Multiplicity =' in line:
                splt_line = line.split('=')
                data[FORMAL_CHARGE] = int(splt_line[1].split()[0])
                data[FORMAL_CHARGE_UNIT] = 'atomic'
                data[SPIN_MULT] = int(splt_line[2])
            return cur_line
        #---------------------------------------------
        def check_geom(data, cur_line,log_lines):
            # tries to extract geometry from a log file line
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
            elif 'Standard orientation:' in line and data[GEOM] is None:
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
            # tries to extract atoms masses from a log file line
            # checks two places:
            #   1. lines with 'AtmWgt=' (provided an appropriate verbosity was set)
            #   2. lines with '- Thermochemistry -'  (provided freq job was requested)
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
                data[ATOM_MASSES_UNIT] = 'atomic'

            if data[ATOM_MASSES] is None:
                if '- Thermochemistry -' in line:
                    data[ATOM_MASSES] = []
                    while 'Molecular mass:' not in line:
                        if 'Atom' in line and 'has atomic number' in line and 'and mass' in line:
                            line = line.split('and mass')[1]
                            data[ATOM_MASSES].append(float(line))
                        cur_line = cur_line + 1
                        line = log_lines[cur_line].strip()
                    data[ATOM_MASSES_UNIT] = 'atomic'
            return cur_line
        #---------------------------------------------
        def check_freq(data, cur_line,log_lines):
            # tries to extract frequencies from a log file line
            line = log_lines[cur_line]
            if 'Frequencies -- ' in line:
                data[FREQ] = []
                data[FREQ_UNIT] = 'cm^-1'

                while 'Thermochemistry' not in line and 'Zero-point correction' not in line:
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
            # tries to extract rotational symmetry number from a log file line
            line = log_lines[cur_line]
            if 'Rotational symmetry number' in line:
                line = line.split()[-1].replace('.','')
                data[ROT_SYM_NR] = float(line)
            return cur_line
        #---------------------------------------------
        def check_rot_const(data, cur_line,log_lines):
            # tries to extract rotational constants from a log file line
            # it removes duplicated rot constants and rot constants that are zero
            line = log_lines[cur_line]

            if ROT_CONST_RE.search(line):
                data[ROT_CONST] = []
                # get unit
                line = line.split('(')[1]
                line = line.split(')')
                data[ROT_CONST_UNIT] = line[0]

                # get rot consts
                rc = line[1].replace(':','').split()
                rc = list(dict.fromkeys(rc))
                #line below is added based on recommendation Angiras Menon  (am2145@cam.ac.uk) because some Log files have invalid rotational constants values.
                rc = [x for x in rc if x[0] !=  '*']
                data[ROT_CONST] = [float(x) for x in rc if float(x) != 0]

                data[ROT_CONST_NR] = len(data[ROT_CONST])
            return cur_line
        #---------------------------------------------
        def check_homo(data,cur_line,log_lines):

            line = log_lines[cur_line]
            occupied = []
            while HOMO_RE.search(line):
                line = line.strip()
                line = line.split()
                occupied.append(line[4:]) 
                cur_line += 1
                if cur_line >= len(log_lines):
                    break
                line = log_lines[cur_line]
            occupied = sum(occupied, [])
            if occupied: 
                data[HOMO_ENERGY] = float(occupied[-1])
                if len(occupied) > 1 : 
                    data[HOMO_MIN_1_ENERGY] = float(occupied[-2])
                if len(occupied) > 2 : 
                    data[HOMO_MIN_2_ENERGY] = float(occupied[-3])
            return cur_line
        #---------------------------------------------
        def check_lumo(data,cur_line,log_lines):

            line = log_lines[cur_line]
            virtual = []
            while LUMO_RE.search(line):
                line = line.strip()
                line = line.split()
                virtual.append(line[4:]) 
                cur_line += 1
                if cur_line >= len(log_lines):
                    break
                line = log_lines[cur_line]
            virtual = sum(virtual, [])
            if virtual: 
                data[LUMO_ENERGY] = float(virtual[0])
                if len(virtual) > 1 : 
                    data[LUMO_PLUS_1_ENERGY] = float(virtual[1])
                if len(virtual) > 2 : 
                    data[LUMO_PLUS_2_ENERGY] = float(virtual[2])
            return cur_line

        #---------------------------------------------
        def check_zpe(misc, cur_line,log_lines):
            # tries to extract zpe energy from a log file line
            # it goes to the misc object
            line = log_lines[cur_line]
            if 'Zero-point correction=' in line:
                line = line.split('Zero-point correction=')[1]
                line = line.split()[0]
                misc[ZPE_ENERGY] = float(line)
            return cur_line
        #---------------------------------------------
        def check_E0_zpe(data, cur_line,log_lines):
            # tries to extract E + zpe energy from a log file line
            line = log_lines[cur_line]
            if 'Sum of electronic and zero-point Energies=' in line:
                line = line.split('Sum of electronic and zero-point Energies=')[1]
                line = line.split()[0]
                data[ELECTRONIC_ZPE_ENERGY] = float(line)
            return cur_line
        #---------------------------------------------
        def check_E0(data, cur_line,log_lines):
            # tries to extract electronic energy from a log file line
            line = log_lines[cur_line]
            if 'SCF Done' in line:
                line = line.split('=')[1]
                line = line.split()[0]
                data[ELECTRONIC_ENERGY] = float(line)
            return cur_line
        #---------------------------------------------
        def check_Casscf_E0(misc, cur_line,log_lines):
            # tries to extract electronic casscf from a log file line
            # we may try to use a better detection criteria than the one below
            # it goes to the misc object, to be later added to the data to be upload object
            line = log_lines[cur_line]
            if 'ITN=' in line and 'MaxIt=' in line and 'E=' in line and 'DE=' in line and 'Acc=' in line and 'Lan=' in line:
                line = line.split('E=')[1].strip()
                line = line.split()[0]
                misc[CAS_ENERGY] = float(line)
            return cur_line
        #---------------------------------------------
        def check_Casscf_MP2_E0(misc, cur_line,log_lines):
            # tries to extract electronic casscf + mp2 from a log file line
            # we may try to use a better detection criteria than the one below
            # it goes to the misc object, to be later added to the data to be upload object
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
            # tries to extract electronic ci from a log file line
            # we may try to use a better detection criteria than the one below
            # it goes to the misc object, to be later added to the data to be upload object
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
            # tries to extract electronic td from a log file line
            # we may try to use a better detection criteria than the one below
            # it goes to the misc object, to be later added to the data to be upload object
            line = log_lines[cur_line]
            if 'Total Energy, E(TD'.upper() in line.upper():
                line = line.split('=')[1].strip()
                misc[TD_ENERGY] = float(line)
            return cur_line
        #---------------------------------------------
        def correct_Casscf_Mp2_method(data, misc):
            # for Casscf_Mp2 jobs the level of theory on the log footer
            # only contains the MP2 bit, which is not great. so what I am
            # doing here is I manually add 'CASSCF MP2' level of theory
            # if CAS_MP2_ENERGY exist in misc object. This is not great, there
            # may be a better way to do it...
            if misc[CAS_MP2_ENERGY] is not None and 'CAS' not in data[METHOD]:
                data[METHOD] = 'CASSCF MP2'
        #---------------------------------------------
        def parse_footer(data, misc, cur_line,log_lines):
            # this parses the Gaussian footer and extracts:
            #                METHOD  - I read it from the footer rather than from
            #                          the route line at the beginning. This is because
            #                          in certain cases the route line does not contain
            #                          a correct level of theory e.g. if that was taken
            #                          from the checkpoint file, see e.g.
            #                          'gaussian/hf/co2_calcall_g09.log' file
            #             BASIS_SET  - see METHOD above
            #           EMP_FORMULA  - see METHOD above
            #           ATOM_COUNTS  - see METHOD above
            #              RUN_DATE  - see METHOD above
            # ELECTRONIC_ZPE_ENERGY  - only for composite methods, see MISC description
            #                  MISC  - it usually contain extra job info such as SP, Opt
            #                          or Mixed. Mixed is for composite jobs and I use it
            #                          to catch them in below


            # NOTE: I assume that the footer is correct (e.g. contains the required nr of lines)
            # read prescribed nr of lines above the footer
            lines_above_footer = log_lines[cur_line-EXTRA_FOOTER_LINES_NR:cur_line]
            footer = log_lines[cur_line:]
            footer = ''.join(footer)
            # so far I have observed two different Gaussian footer blocks
            # one that uses | to separate data and another one that uses /
            # I am then checking which separator is used in the footer
            # I then split it and assume that the specific data position
            # in the splitted footer list is always the same
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
                #data[ATOM_COUNTS][at.upper()] = int(ac)
                #Line added by Nenad Krdzavac (caresssd@hermes.cam.ac.uk). 
                #Line above is commented because it generates upper case of atom name. That syntax does not mathc periodic table naming atoms.
                 data[ATOM_COUNTS][at] = int(ac)

            # this can also be read from here, though I am not sure
            # if the footer would always contain this info
            # charge = int(footer[15].strip().split(',')[0])
            # spin_mult = int(footer[15].strip().split(',')[1])

            rundate = footer[-1].replace('.','').split()
            data[RUN_DATE] = ' '.join(rundate[-5:])

            #if misc == 'Mixed'
            # read the composite energy from lines above the footer
            for line in lines_above_footer:
                if '(0 K)=' in line and 'Energy=' in line:
                    line = line.split('=')[1]
                    data[ELECTRONIC_ENERGY] = None
                    data[ELECTRONIC_ZPE_ENERGY] = float(line.split()[0].strip())
                    break
        #---------------------------------------------
        def resolve_energy(data, misc):
            # energies for certain levels of theory are not extracted by my parser
            # instead I use cclib to provide these
            # also certain levels of theory energies are stored in the misc data
            # and if that is the case I copy that energy from misc to the final
            # data object
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
        #---------------------------------------------
        def resolve_atom_masses(data):
            # if after parsing the log there are no data on atoms masses
            # i use my own elements lib to set them
            if data[ATOM_MASSES] is None:
                if data[ATOM_TYPES]:
                    data[ATOM_MASSES] = []
                    for at in data[ATOM_TYPES]:
                        data[ATOM_MASSES].append(eld.get_el_wt_by_symbol(at))
                    data[ATOM_MASSES_UNIT] = 'atomic'
        #================================================        
        def check_scan_job(data, cur_line, log_lines):
            # tries to extract electronic energy from a log file line
            line = log_lines[cur_line]
            placeholder_GEOM = None
            placeholder_energy = None
            if "The following ModRedundant input section has been read:".lower() in line.lower():
                data[SCANFLAG] = True
            if data[SCANFLAG] == True:
                data[SCANPOINTS] = self.cclib_data.scanparm
                placeholder_GEOM = self.cclib_data.scancoords
                placeholder_energy = self.cclib_data.scanenergies
            if all(v is None for v in [data[SCANPOINTS],placeholder_GEOM, placeholder_energy]):
                data[SCANFLAG] = None
            else:
                data[GEOM] = placeholder_GEOM
                data[ELECTRONIC_ENERGY] = placeholder_energy
                data[GEOM] = data[GEOM].tolist()
            return cur_line
        #================================================
        # parse_log body
        #================================================
        # init data and misc dict
        parseddata = {key: None for key in CCKEYS_DATA}
        parsedmisc = {key: None for key in CCKEYS_MISC}

        # run cclib first
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
            # sometimes I am passing and retaining cur_line nr to functions
            # that do nothing with it and return it as is. I do it so that
            # in the future we may easily do sth with cur_line in these functions
            cur_line = check_charge_spin_mult(parseddata, cur_line, log_lines)
            cur_line = check_geom(parseddata, cur_line, log_lines)
            cur_line = check_freq(parseddata, cur_line, log_lines)
            cur_line = check_elweights(parseddata, cur_line, log_lines)
            cur_line = check_rot_sym_nr(parseddata, cur_line,log_lines)
            cur_line = check_rot_const(parseddata, cur_line,log_lines)
            cur_line = check_zpe(parsedmisc, cur_line,log_lines)
            cur_line = check_E0_zpe(parseddata, cur_line,log_lines)
            cur_line = check_E0(parseddata, cur_line,log_lines)
            cur_line = check_homo(parseddata,cur_line,log_lines)
            cur_line = check_lumo(parseddata,cur_line,log_lines)
            cur_line = check_CI_E0(parsedmisc, cur_line,log_lines)
            cur_line = check_TD_E0(parsedmisc, cur_line,log_lines)
            cur_line = check_Casscf_E0(parsedmisc, cur_line,log_lines)
            cur_line = check_Casscf_MP2_E0(parsedmisc, cur_line,log_lines)
            cur_line = check_scan_job(parseddata, cur_line,log_lines)

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

        # post-process certain results
        set_geom_type(parseddata)
        correct_Casscf_Mp2_method(parseddata, parsedmisc)
        if parseddata[SCANFLAG] is None:
            resolve_energy(parseddata, parsedmisc)
        resolve_atom_masses(parseddata)

        # remove data with None values
        filtered = {k: v for k, v in parseddata.items() if v is not None}

        return filtered