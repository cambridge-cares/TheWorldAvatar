# Description


The `chemutils` package has been developed to provide an easy access to a number of most common utility functions from the cheminformatics area. As an example, it allows to perform conversion between variety of molecule formats, re-ordering the molecule's atoms with respect to the other molecule or generation of molecule's conformers using force fields. A full list of supported features is given in the CLI section.

# Requirements

- Python >=3.5. You can install Python by going to the official Python [download page](https://www.python.org/getit/)
- Either [miniconda](https://docs.conda.io/en/latest/miniconda.html) or [miniforge](https://github.com/conda-forge/miniforge/releases) (recommended) package manager

# Installation #
Currently, only the installation from source is possible. A custom bash script has been provided in order to simplify this step:


`(Windows)`

Open miniconda / miniforge command prompt, navigate to the `chemaboxwriters` directory and run:

```cmd
$ install_script_conda.sh -v -e -i
```

`(Linux)`

Open linux bash terminal and run:

```sh
$ install_script_conda.sh -v -e -i
```

The command above will create a separate conda virtual environment `chemutils_venv`, install the `chemutils` package and all its dependencies. Once the installation is done, activate the newly created environment via the following command:

```sh
$ conda activate chemutils_venv
```

If you wish to see the install script help text, simply run `install_script_conda.sh -h`.


# Command line interface #

The command line interface is presented below.

```bash
Usage:
    chemutils atomspos <xyzFileOrDir>
                       [--out-dir=<outdir>]
                       [--out-filename=<outfname>]
                       [--file-extension=<filext>]
                       [--no-file-output]
    chemutils convert <moleculeFileOrDir> <convertFrom> <convertTo>
                      [--conv-options=<convopts>]
                      [--out-dir=<outdir>]
                      [--out-filename=<outfname>]
                      [--file-extension=<filext>]
                      [--no-file-output]
    chemutils xyz2xyz <xyzTargetFile> <xyzRefFile>
                      [--out-dir=<outdir>]
                      [--out-filename=<outfname>]
                      [--file-extension=<filext>]
                      [--no-file-output]
    chemutils xyz2ginp <xyzFileOrDir>
                       [--charge=<charge>]
                       [--spin-mult=<spinmult>]
                       [--job-route=<jobroute>]
                       [--memory=<mem>]
                       [--num-cpus=<ncpus>]
                       [--out-dir=<outdir>]
                       [--out-filename=<outfname>]
                       [--file-extension=<filext>]
                       [--no-file-output]
    chemutils genconfs <moleculeFileOrDir>
                       (--input-format=<inpformat>)
                       [--ret-num-confs=<retnumconfs>]
                       [--gen-num-confs=<gennumconfs>]
                       [--max-iters=<maxiters>]
                       [--mff-variant=<mffvariant>]
                       [--out-dir=<outdir>]
                       [--out-filename=<outfname>]
                       [--file-extension=<filext>]
                       [--no-file-output]

Options:
--conv-options=<convopts>      OpenBabel conversion options. Defaults to none.
--job-route=<jobroute>         Gaussian job route [default: #n B3LYP/6-311+G(d,p) Opt Freq]
--charge=<charge>              Molecule charge in atomic units [default: 0]
--spin-mult=<spinmult>         Molecule spin multiplicity [default: 1]
--memory=<mem>                 Memory to be used for the gaussian job, in GB  [default: 32]
--num-cpus=<ncpus>             Number of cpus to be used for the gaussian job  [default: 16]
--out-dir=<outdir>             Output dir for the generated files. Defaults to:
                                  * input file directory for file input
                                  * input directory for the directory input
--out-filename=<outfname>      Base name of the produced output files.
                               If multiple output files are generated, basename
                               will be appended by an integer number.
--file-extension=<filext>      Input file extension. The following defaults
                               are used:
                                  * xyz for xyzFileOrDir / xyzTargetFile /
                                    xyzRefFile inputs
                                  * --input-format or convertFrom for
                                    moleculeFileOrDir input
--no-file-output               Suppresses the output file(s) generation.
--input-format=<inpformat>     Format of the input file.
--ret-num-confs=<retnumconfs>  Number of the lowest energy conformers to return. [default: 1]
--gen-num-confs=<gennumconfs>  Number of conformers to generate. [default: 100]
                               Larger values would increase a chance of finding
                               the ground state conformer.
--max-iters=<maxiters>         Max number of force field iterations [default: 1000]
--mff-variant=<mffvariant>     Force field variant [default: MMFF94]
```

where:
``sh
chemutils atomspos  - Returns atom positions (order) given a molecule in an xyz format.
                      The atoms order is determined using four invariants, in the
                      following order of precedence:
                        1) Largest sum of distances from all other atoms
                        2) Largest X coordinate
                        3) Largest Y coordinate
                        4) Largest Z coordinate
                      Pros:
                        This is possibly simplest algorithm to canonicalize
                        atoms order in molecules.
                        The algorithm works with single molecules and disconnected
                        molecular fragments out of the box.

                      Cons:
                        Since the algorithm doesnt use any topological information
                        (atoms connectivity) it is sensitive to the quality of the
                        xyz coordinates. This means that atoms canonical order may
                        change if their positions are distorted too much with
                        respect to each other.

                      Use this function to set the atoms positions in a reference
                      molecule. The idea is to assign the positions once and to never
                      change them again.

chemutils convert   - Wrapper for the obConvert function from open babel. Returns a
                      molecule converted to a desired format. The convertFrom,
                      convertTo and convOptions values are those allowed by open babel.

chemutils xyz2xyz   - Matches atom indices from one xyz molecule to the other.
                      Molecules must have the same topology (the same inchis).
                      Outputs the target molecule where the atoms order is the
                      same as in the reference molecule.

chemutils xyz2ginp  - Creates the gaussian input file from the input arguments.

chemutils genconfs  - Generates the requested number of molecule conformers.
                      Supports any input format that is also supported by
                      open babel. Can be used to generate a set of initial xyz
                      coordinates from e.g. inchis or smiles strings.

```

# Authors #
Daniel Nurkowski (danieln@cmclinnovations.com)