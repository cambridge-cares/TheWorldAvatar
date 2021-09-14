from chemutils.mainutils import xyzReorderToxyz, \
                                xyzToAtomsPositionsWrapper, \
                                xyzToGaussianInputWrapper
from chemutils.obabelutils import obConvertWrapper
from docopt import docopt, DocoptExit

doc = """chemutils
Usage:
    chemutils atomspos <xyzFileOrStr> [--outFile=<out-file-path>
                                       --silent]
    chemutils convert <moleculeFileOrStr> <from> <to> [--convOptions=<conv_opt>
                                                       --outFile=<out-file-path>
                                                       --silent]
    chemutils xyz2xyz <xyzTargetFileOrStr> <xyzRefFileOrStr> [--outFile=<out-file-path>
                                                    --silent]
    chemutils xyz2ginp  <xyzFileOrDir>
             		    [--job_route=<jroute>]
 		                [--charge=<charge>]
 	                    [--spin_multiplicity=<spinmult>]
                        [--memory=<mem>]
 		                [--num_cpus=<ncpus>]
                        [--out-dir=<outdir>]
                        [--silent]
Options
--convOptions          OpenBabe; conversion options
--job_route            Gaussian job route [default: B3LYP/6-311+G(d,p) Opt Freq]
--charge               Molecule's charge in atomic units [default: 0]
--spin_multiplicity    Molecule's spin multiplicity [default: 1]
--memory               Memory to be used for the gaussian job, in GB  [default: 32]
--num_cpus             Number of cpus to be used for the gaussian job  [default: 16]
--out-file-path        Output file path for the generated Gaussian
--out-dir              Output file path for the generated Gaussian
                       input file.
--silent               Silent mode.
"""
#    chemutils xyz2xyzFlexBond <xyzTargetFileOrStr> <xyzRefFileOrStr>
#                                                   <refAtomId1>
#                                                   <refAtomId2>

def start():
    try:
        args = docopt(doc)
    except DocoptExit:
        raise DocoptExit('Error: chemutils called with wrong arguments.')

    if args["atomspos"]:
        output = xyzToAtomsPositionsWrapper(xyzFileOrStr=args['<xyzFileOrStr>'], \
                            silent=args['--silent'], outFile=args['--outFile'])
    elif args["convert"]:
        output = obConvertWrapper(inputMol=args['<moleculeFileOrStr>'], inputMolFormat=args['<from>'], \
            outputMolFormat=args['<to>'], convOptions=args['--convOptions'], outFile=args['--outFile'], \
            silent=args['--silent'])
    elif args["xyz2ginp"]:
        output = xyzToGaussianInputWrapper(xyzFileOrDir=args['<xyzFileOrDir>'], \
             		    job_route= args['--job_route'], \
 		                charge= args['--charge'], \
 	                    spin_multiplicity = args['--spin_multiplicity'], \
                        memory= args['--memory'], \
 		                num_cpus= args['--num_cpus'], \
                        out_dir= args['--out-dir'], \
                        silent= args['--silent'])
    else:
        output = xyzReorderToxyz(args['<xyzTargetFileOrStr>'], args['<xyzRefFileOrStr>'], \
                                   outFile=args['--outFile'])
    #else:
    #    output = xyzReorderToxyzFlexBond(args['<xyzTargetFileOrStr>'], args['<xyzRefFileOrStr>'], \
    #                               args['<refAtomId1>'], args['<refAtomId2>'])
if __name__ == '__main__':
    start()