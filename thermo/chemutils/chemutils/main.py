import chemutils.mainutils.mainutils as mainutils
from docopt import docopt, DocoptExit
import re

doc = """chemutils
Usage:
    chemutils atomspos <xyzFileOrDir>
                       [--out-dir=<outdir>]
                       [--file-extension=<filext>]
    chemutils convert <moleculeFileOrDir> <convertFrom> <convertTo>
                      [--conv-options=<convopts>]
                      [--out-dir=<outdir>]
                      [--file-extension=<filext>]
    chemutils xyz2xyz <xyzTargetFile> <xyzRefFile>
                      [--out-dir=<outdir>]
                      [--file-extension=<filext>]
    chemutils xyz2ginp <xyzFileOrDir>
                       [--charge=<charge>]
                       [--spin-mult=<spinmult>]
                       [--job-route=<jobroute>]
                       [--memory=<mem>]
                       [--num-cpus=<ncpus>]
                       [--out-dir=<outdir>]
                       [--file-extension=<filext>]

Options:
--conv-options=<convopts>     OpenBabel conversion options
--job-route=<jobroute>        Gaussian job route [default: B3LYP/6-311+G(d,p) Opt Freq]
--charge=<charge>             Molecule's charge in atomic units [default: 0]
--spin-mult=<spinmult>        Molecule's spin multiplicity [default: 1]
--memory=<mem>                Memory to be used for the gaussian job, in GB  [default: 32]
--num-cpus=<ncpus>            Number of cpus to be used for the gaussian job  [default: 16]
--out-dir=<outdir>            Output file path for the generated Gaussian.
--file-extension=<filext>     Input file extension.
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
        output = mainutils.xyzToAtomsPositionsWrapper(
                    xyzFileOrDir=args['<xyzFileOrDir>'], \
                    outDir=args['--out-dir'], \
                    fileExt=args['--file-extension'], \
                )
    elif args["convert"]:
        output = mainutils.obConvertWrapper(
                    inputFileOrDir=args['<moleculeFileOrDir>'],
                    convertFrom=args['<convertFrom>'], \
                    convertTo=args['<convertTo>'], \
                    convOptions=args['--conv-options'],
                    outDir=args['--out-dir'], \
                    fileExt=args['--file-extension'], \
                )

    elif args["xyz2ginp"]:
        output = mainutils.xyzToGaussianInputWrapper(xyzFileOrDir=args['<xyzFileOrDir>'], \
             		    jobRoute= args['--job-route'], \
 		                charge= args['--charge'], \
 	                    spinMult = args['--spin-mult'], \
                        memory= args['--memory'], \
 		                numCpus= args['--num-cpus'], \
                        outDir= args['--out-dir'], \
                        fileExt=args['--file-extension'], \
                )
    elif args["xyz2xyz"]:
        output = mainutils.xyzReorderToxyz(
                        xyzTargetFile= args['<xyzTargetFile>'], \
                        xyzRefFile= args['<xyzRefFile>'], \
                        outDir= args['--out-dir'], \
                )
    #else:
    #    output = xyzReorderToxyzFlexBond(args['<xyzTargetFileOrStr>'], args['<xyzRefFileOrStr>'], \
    #                               args['<refAtomId1>'], args['<refAtomId2>'])
if __name__ == '__main__':
    start()