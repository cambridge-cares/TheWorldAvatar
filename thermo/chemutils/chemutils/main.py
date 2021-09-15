import chemutils.mainutils.mainutils as mainutils
from docopt import docopt, DocoptExit
import re

doc = """chemutils
Usage:
    chemutils atomspos <xyzFileOrDir>
                       [--out-dir=<outdir>]
                       [--file-extension=<filext>]
                       [--no-file-output]
    chemutils convert <moleculeFileOrDir> <convertFrom> <convertTo>
                      [--conv-options=<convopts>]
                      [--out-dir=<outdir>]
                      [--file-extension=<filext>]
                      [--no-file-output]
    chemutils xyz2xyz <xyzTargetFile> <xyzRefFile>
                      [--out-dir=<outdir>]
                      [--file-extension=<filext>]
                      [--no-file-output]
    chemutils xyz2ginp <xyzFileOrDir>
                       [--charge=<charge>]
                       [--spin-mult=<spinmult>]
                       [--job-route=<jobroute>]
                       [--memory=<mem>]
                       [--num-cpus=<ncpus>]
                       [--out-dir=<outdir>]
                       [--file-extension=<filext>]
                       [--no-file-output]
    chemutils genconfs <fileOrDir>
                       (--input-format=<inpformat>)
                       [--ret-num-confs=<retnumconfs>]
                       [--gen-num-confs=<gennumconfs>]
                       [--max-iters=<maxiters>]
                       [--mff-variant=<mffvariant>]
                       [--out-dir=<outdir>]
                       [--file-extension=<filext>]
                       [--no-file-output]

Options:
--conv-options=<convopts>       OpenBabel conversion options
--job-route=<jobroute>          Gaussian job route [default: B3LYP/6-311+G(d,p) Opt Freq]
--charge=<charge>               Molecule's charge in atomic units [default: 0]
--spin-mult=<spinmult>          Molecule's spin multiplicity [default: 1]
--memory=<mem>                  Memory to be used for the gaussian job, in GB  [default: 32]
--num-cpus=<ncpus>              Number of cpus to be used for the gaussian job  [default: 16]
--out-dir=<outdir>              Output file path for the generated Gaussian.
--file-extension=<filext>       Input file extension.
--no-file-output                Suppresses the output file generation.
--input-format=<inpformat>      Format of the input file.
--ret-num-confs=<retnumconfs>   Number of conformers to return [default: 5]
--gen-num-confs=<gennumconfs>   Number of conformers to generate [default: 100]
--max-iters=<maxiters>          Max number of force field iterations [default: 1000]
--mff-variant=<mffvariant>      Force field variant [default: MMFF94]
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
                    noOutFile=args['--no-file-output'], \
                )
    elif args["convert"]:
        output = mainutils.obConvertWrapper(
                    inputFileOrDir=args['<moleculeFileOrDir>'],
                    convertFrom=args['<convertFrom>'], \
                    convertTo=args['<convertTo>'], \
                    convOptions=args['--conv-options'],
                    outDir=args['--out-dir'], \
                    fileExt=args['--file-extension'], \
                    noOutFile=args['--no-file-output'], \
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
                        noOutFile=args['--no-file-output'], \
                )
    elif args["xyz2xyz"]:
        output = mainutils.xyzReorderToxyz(
                        xyzTargetFile= args['<xyzTargetFile>'], \
                        xyzRefFile= args['<xyzRefFile>'], \
                        outDir= args['--out-dir'], \
                        noOutFile=args['--no-file-output'], \
                )
    elif args["genconfs"]:
        output= mainutils.getConformersXYZWrapper(
                        moleculeFileOrDir=args['<fileOrDir>'], \
                        inputFormat=args['--input-format'], \
                        retNumConfs=int(args['--ret-num-confs']), \
                        genNumConfs=int(args['--gen-num-confs']), \
                        maxIters=int(args['--max-iters']), \
                        mmffVariant=args['--mff-variant'], \
                        outDir=args['--out-dir'], \
                        fileExt=args['--file-extension'], \
                        noOutFile=args['--no-file-output'], \
                )
    #else:
    #    output = xyzReorderToxyzFlexBond(args['<xyzTargetFileOrStr>'], args['<xyzRefFileOrStr>'], \
    #                               args['<refAtomId1>'], args['<refAtomId2>'])
if __name__ == '__main__':
    start()