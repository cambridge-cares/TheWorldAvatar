
import stdc.app as app
from docopt import docopt, DocoptExit
import sys

DEV_INPUTS={
    "--dev-nasa-high-coeffs": None,
    "--dev-nasa-low-coeffs": None,
    "--dev-nasa-trange": None,
    "--dev-output-file": None,
    "--dev-enthref-from-nasa": "0",
    "--dev-enthref-nasa-temp": "298.15",
}

__doc__="""stdc
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
--rot-constants=<rotConsts>           Rotational constants in 1/cm. Required 
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
                                      calculate thermodata, [default: 100000.0]
--fit-nasa                            Flag controlling extra Nasa polynomials
                                      output
"""



def _get_developer_args():
    args = sys.argv[1:]
    dev_indices = []
    for dev_input in DEV_INPUTS:
        if dev_input in args:
            dev_input_ind = args.index(dev_input)
            DEV_INPUTS[dev_input]= args[dev_input_ind+1]
            dev_indices.append(dev_input_ind)
            dev_indices.append(dev_input_ind+1)
    args = [arg for j, arg in enumerate(args) if j not in dev_indices]
    return DEV_INPUTS, args

def main():    
    try:        
        dev_args, args = _get_developer_args()
        args = docopt(__doc__, argv=args)
    except DocoptExit:
        raise DocoptExit('Error: stdc called with wrong arguments.')
    except IndexError:
        print('Error: Wrong developer inputs.')
        return

    args = {**args, **dev_args}
    app.runThermoCalculator(args)
    pass

if __name__ == "__main__":
   main()