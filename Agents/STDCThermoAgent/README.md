# Description #

The `stdc` is a simple calculator of thermodynamic properties of gaseous species based on statistical thermodynamics equations.

# Installation #
These instructions will get you a copy of the project up and running on your local machine for development and testing purposes.
## Requirements

- You need Python >3.5 to run the `stdc`. You can install Python by going to the official Python [download page](https://www.python.org/getit/)
- You also need to install a [Java Runtime Environment version >=8](https://adoptopenjdk.net/?variant=openjdk8&jvmVariant=hotspot)

## Virtual environment setup

It is highly recommended to use a [virtual environment](https://docs.python.org/3/tutorial/venv.html) for the `stdc` installation. The virtual environment can be created as follows:

`(Windows)`

```cmd
$ python -m venv stdc_venv
$ stdc_venv\Scripts\activate.bat
(stdc_venv) $
```

`(Linux)`
```sh
$ python3 -m venv stdc_venv
$ source stdc_venv\bin\activate
(stdc_venv) $
```

The above commands will create and activate the virtual environment `stdc_venv` in the current directory.

## Installation from the version-controlled source (for developers)

This type of installation is only for the developers. To install `stdc` directly from its repository you need to first clone the `TheWorldAvatar` project. Then simply navigate to the *TheWorldAvatar\stdc* directory and execute the following commands:
```bash
# build and install
(stdc_venv) $ python -m pip install .

# or build for in-place development
(stdc_venv) $ python -m  pip install -e .
```

Alternatively, use the provided `install_script_pip.sh` or `install_script_conde.sh` convenience scripts, that can create virtual environment and install the `stdc` in one go:
```bash
# create the environment and install the project
$ install_script_pip.sh -v -i
# create the environment and install the project for in-place development
$ install_script_pip.sh -v -i -e
```
Note that installing the project for in-place development (setting the `-e` flag) also installs the required python packages for development and testing. To test the code, simply run the following commands:

```bash
(stdc_venv) $ pytest tests\test_termocalc.py
```

# How to use #

The calculator can be used as a simple command line tool or as a web agent.
## Command line usage

 The calculator can be run from the command line via the `stdc` command which accepts the following options:

```bash
Usage:
    stdc  (--chem-formula=<chemFormula>)
          (--spin-mult=<spinMult>)
          (--mol-weight=<molWeight>)
          [--sym-number=<symNum>]
          [--rot-constants=<rotConsts>]
          [--frequencies=<freqs>]
          [--freq-scale-factor=<freqScaleFact>]
          [--elec-levels=<elecLvls>]
          [--temperature=<temp>]
          [--pressure=<pres>]
          [--temperature-range=<tempRange>]
          [(--enthalpy-ref=<enthRef> --enthalpy-ref-temp=<enthRefTemp>)]
          [--fit-nasa]
          [--fit-nasa-temperatures=<fitNasaTemps>]

Options:
--chem-formula=<chemFormula>          Chemical formula
--spin-mult=<spinMult>                Spin multiplicity
--mol-weight=<molWeight>              Molecular weight in au
--sym-number=<symNum>                 Symmetry number.
--rot-constants=<rotConsts>           Rotational constants in GHz. Required
                                      only for non-atomic species. Input as
                                      comma separated values: e.g. "rot1,rot2,..."
--frequencies=<freqs>                 Vibrational frequencies in 1/cm. Required only
                                      for non-atomic species. Input as comma
                                      separated values: e.g. "freq1,freq2,..."
--freq-scale-factor=<freqScaleFact>   Vibrational frequencies scaling factor,
                                      [default: 1.0]
--elec-levels=<elecLvls>              Electronic levels and their degeneracies,
                                      Example input: "deg1,lvl1,deg2,lvl2,..."
                                      Levels energies should be in 1/cm
--temperature=<temp>                  Requested temperature in Kelvins to
                                      calculate thermodata, [default: 298.15]
--pressure=<pres>                     Requested pressure in Pascals to
                                      calculate thermodata, [default: 101325.0]
--fit-nasa                            Flag controlling extra Nasa polynomials
                                      output
```

Example usage that calculates thermodynamic properties of SiH4 species:
```bash
(stdc_venv) $ stdc --chem-formula=SiH4 \
     --spin-mult=1 \
     --mol-weight=32.00823
     --rot-constants="85.40101,85.40060,85.40041" \
     --frequencies="921.8178,921.9349,922.1143,980.0678,980.1473,2235.1371,2242.2008,2242.8063,2243.4218" \
     --sym-number=2 \
     --enthalpy-ref=34700.0 \
     --enthalpy-ref-temp=298.15 \
     --pressure=101325 \
     --temperature=298.15 \
     --fit-nasa
```

## Web agent usage

The web agent uses the [TheWorldAvatar](https://github.com/cambridge-cares/TheWorldAvatar) project knowledge graph to retrieve most of the species data needed to calculate its thermodynamic properties. Therefore requesting calculation substantially differs from the command line option. Only four inputs are required in the agent main route:

These are:

/api/thermoagent/calculate?`ontocompchem_IRI=<oc_IRI>`&`ontospecies_IRI=<os_IRI>`&`temperature=<temperature>`&`pressure=<pressure>`

```bash
ontocompchem_IRI=<oc_IRI>   IRI of the species quantum chemistry data ontocompchem entry
ontospecies_IRI=<os_IRI>    IRI of the species ontospecies entry
temperature=<temperature>   Temperature of interest in Kelvins [default: 298.15]
pressure=<pressure>         Pressure of interest in Pascals  [default: 101325.0]
```

In order to use the calculator as a web agent, simply start a server with the following app entry point:

`(Windows)`

```cmd
(stdc_venv) $ set FLASK_APP=stdc\flaskapp\wsgi.py & flask run
```

`(Linux)`
```bash
(stdc_venv) $ export FLASK_APP=stdc\flaskapp\wsgi.py && flask run
```

# Authors #
Daniel Nurkowski (danieln@cmclinnovations.com), 26 August 2021