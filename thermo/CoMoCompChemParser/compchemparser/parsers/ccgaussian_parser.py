import cclib
import os
import sys
from file_read_backwards import FileReadBackwards
import re
import helpers.ccutils as ccutils
from itertools import islice

# Supported keys
#-------------------------------------------------
# group 1 (atoms properties)
EMP_FORM = 'Empirical formula'
ATOM_COUNT = 'Atom counts'
ATOM_TYPE = 'Atom types'
ATOM_MASS = 'Atomic masses'
ATOM_MASS_UNIT = 'Atomic mass unit'
ATOM_CHARGE = 'Atomic partial charges'
ATOM_CHARGE_UNIT = 'Atomic partial charge unit'
# group 2 (level of theory)
METHOD = 'Method'
BASIS_SET = 'Basis set'
# group 3 (spin/charge)
SPIN_MULT = 'Spin multiplicity'
FORMAL_CHARGE = 'Formal charge'
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
# group 7 (program, run date)
PROGRAM_NAME = 'Program name'
PROGRAM_VERSION = 'Program version'
RUN_DATE = 'Run date'
# group 8 (misc)
MISC = 'Misc'

#-------------------------------------------------

# Keys used
#-------------------------------------------------
CCKEYS = [EMP_FORM, ATOM_COUNT, ATOM_TYPE, ATOM_MASS, ATOM_MASS_UNIT, ATOM_CHARGE, ATOM_CHARGE_UNIT, METHOD,
          BASIS_SET, SPIN_MULT, FORMAL_CHARGE, GEOM, GEOM_TYPE, ROT_SYM_NR, ROT_CONST, ROT_CONST_NR, ROT_CONST_UNIT,
          FREQ, FREQ_NR, FREQ_UNIT, TOTAL_ENERGY, PROGRAM_NAME, PROGRAM_VERSION, RUN_DATE, MISC]
#-------------------------------------------------

EXTRA_FOOTER_LINES_NR = 5

# regex for catching job start string
FSPLIT_RE = re.compile(r"^\s?Entering Link 1 =")
ATOMS_RE = re.compile(r"([a-zA-Z]+)(\d+)")
JOB_SUCCESS_RE = re.compile(r"^\s?Normal termination of Gaussian")
FOOTER_RE = re.compile(r"^\s(1\\1\\|1\|1\|)")

class CcGaussianParser():
    def __init__(self):
        """ init the gaussian parser """
        self.parseddata = {key: None for key in CCKEYS}

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
        self.parseddata[EMP_FORM] = footer[6].strip().split('(')[0]
        self.parseddata[FORMAL_CHARGE] = footer[15].strip().split(',')[0]
        self.parseddata[SPIN_MULT] = footer[15].strip().split(',')[1]
        #
        rundate = footer[-1].replace('.','').split()
        self.parseddata[RUN_DATE] = ' '.join(rundate[-5:])
        #
        atom_types= []
        atom_counts= []
        for (at, ac) in re.findall(ATOMS_RE, self.parseddata[EMP_FORM]):
             atom_types.append(at)
             atom_counts.append(ac)
        self.parseddata[ATOM_TYPE] = atom_types
        self.parseddata[ATOM_COUNT] = atom_counts

        # extract data (if any) from extra n lines above the footer - used for
        # reading composite method total energy
        method = self.parseddata[METHOD][-1]
        for line in extra_lines:
            if method in line and '(0 K)' in line:
                tot_energy = line.split('=')[1]
                tot_energy = tot_energy.split()[0].strip()
                self.parseddata[TOTAL_ENERGY] = tot_energy
                break

    def parse_log_body(self,logFile):
        data = cclib.io.ccread(logFile)

        self.parseddata[GEOM] = data.atomcoords[-1]        
        self.parseddata[FREQ] = data.vibfreqs
        self.parseddata[ATOM_MASS] = data.atommasses

        pass