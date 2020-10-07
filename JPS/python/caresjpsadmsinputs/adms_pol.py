from config import Constants

class AdmsPol(object):
    def __init__(self, pol_name, pol_par_num_deposition_data, pol_par_diameter, pol_par_density, pol_par_mass_fraction,
                 pol_pollutant_type=1, pol_gas_dep_velocity_known=1, pol_gas_deposition_velocity=0, pol_gas_type=1,
                 pol_par_dep_velocity_known=0, pol_par_term_velocity_known=0, pol_par_deposition_velocity=0,
                 pol_par_terminal_velocity=0, pol_wet_washout_known=1, pol_wet_washout=0, pol_wet_washout_a=1.0e-4,
                 pol_wet_washout_b=6.4e-1, pol_conv_factor=1, pol_bkg_level=64.3, pol_bkg_units=Constants.UNIT_UGM3):
        self.PolName = pol_name
        self.PolPollutantType = pol_pollutant_type
        self.PolGasDepVelocityKnown = pol_gas_dep_velocity_known
        self.PolGasDepositionVelocity = pol_gas_deposition_velocity
        self.PolGasType = pol_gas_type
        self.PolParDepVelocityKnown = pol_par_dep_velocity_known
        self.PolParTermVelocityKnown = pol_par_term_velocity_known
        self.PolParNumDepositionData = pol_par_num_deposition_data
        self.PolParDepositionVelocity = [pol_par_deposition_velocity] * pol_par_num_deposition_data
        self.PolParTerminalVelocity = [pol_par_terminal_velocity] * pol_par_num_deposition_data
        self.PolParDiameter = pol_par_diameter
        self.PolParDensity = pol_par_density
        self.PolParMassFraction = pol_par_mass_fraction
        self.PolWetWashoutKnown = pol_wet_washout_known
        self.PolWetWashout = pol_wet_washout
        self.PolWetWashoutA = pol_wet_washout_a
        self.PolWetWashoutB = pol_wet_washout_b
        self.PolConvFactor = pol_conv_factor
        self.PolBkgLevel = pol_bkg_level
        self.PolBkgUnits = pol_bkg_units

    def __repr__(self):
        attrs = self.__dict__.items()

        return '<admsPol {}>'.format(['{} {}'.format(attr, value) for (attr, value) in attrs])

