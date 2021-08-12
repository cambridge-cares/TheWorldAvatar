
import stdc.app as app
from docopt import docopt, DocoptExit


__doc__="""stdc
Usage:
    stdc  (--chem-formula=<chemFormula>)
          (--spin-mult=<spinMult>)
          (--elec-energy=<elecEnergy> | --elecZPE-energy=<elecAndZPEEnergy>)
          (--mol-weight=<molWeight>)
          (--sym-number=<symNum>)
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
--elec-energy=<elecEnergy>            Electronic energy in au
--elecZPE-energy=<elecAndZPEEnergy>   Electronic plus ZPE energy in au
--mol-weight=<molWeight>              Molecular weight in SI
--sym-number=<symNum>                 Symmetry number.
--rot-constants=<rotConsts>           Rotational constants in 1/m. Required 
                                      only for non-atomic species. Input as
                                      comma separated values: e.g. "rot1,rot2,..."
--frequencies=<freqs>                 Vibrational frequencies in 1/cm. Required only
                                      for non-atomic species. Input as comma
                                      separated values: e.g. "freq1,freq2,..."
--freq-scale-factor=<freqScaleFact>   Vibrational frequencies scaling factor,
                                      [default: 1.0]
--elec-levels=<elecLvls>              Electronic levels and their degeneracies,
                                      Example input: "deg1,lvl1,deg2,lvl2,..."
--temperature=<temp>                  Requested temperature in Kelvins to
                                      calculate thermodata, [default: 298.15]
--pressure=<pres>                     Requested pressure in Pascals to
                                      calculate thermodata, [default: 101325]
--fit-nasa                            Flag controlling extra Nasa polynomials
                                      output.
"""

def main():
    try:
        args = docopt(__doc__)
    except DocoptExit:
        raise DocoptExit('Error: stdc called with wrong arguments.')

    app.runThermoCalculator(args)
    pass

if __name__ == "__main__":
   main()