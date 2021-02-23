import sys

from math import exp, log, sqrt, pi, erf
import json
import torch
import gpytorch
import numpy
import pickle


class ADMSMapSpeedTorqueNOxSoot(object):

    def __init__(self):
        # Default input values.
        self.NO_frac = 0.35
        self.NumParSizeClasses = 4
        self.LogNorm_mu = 2000.0
        # controlls the width of the distribution
        self.LogNorm_sigma = 1.0
        # unpickled surrogate models
        self.nox_surr = self.deserialise_surrogate('NOx_surr.bin')
        self.soot_surr = self.deserialise_surrogate('Soot_surr.bin')

    @staticmethod
    def log_normal(ax, amu, asigma):
        return exp(-((log(ax) - log(amu)) / asigma) ** 2 / 2) / (ax * asigma * sqrt(2 * pi))

    @staticmethod
    def log_normal_cdf(ax, amu, asigma):
        if ax < 1e-20:
            cd = 0.0
        else:
            cd = (1.0 + erf((log(ax) - log(amu)) / (asigma * sqrt(2.0)))) / 2
        return cd

    @staticmethod
    def value_unit_dict(value, unit):
        return {'value': value, 'unit': unit}

    def pollutant_dict(self, name, value, unit, pseudo, molmass, molmass_unit):
        return {'name': name, 'value': value, 'unit': unit,
                'pseudocomponent': pseudo,
                'molmass': self.value_unit_dict(molmass, molmass_unit)}

    def par_size_class_dict(self, diam, diam_unit, dens, dens_unit, massflowrate, massflowrate_unit):
        return {'diameter': self.value_unit_dict(diam, diam_unit),
                'density': self.value_unit_dict(dens, dens_unit),
                'emission_rate': self.value_unit_dict(massflowrate, massflowrate_unit)}

    def mixture_dict(self):
        return {'molmass': self.value_unit_dict(2.8638811393753E-2, 'kg/mol'),
                'cp': self.value_unit_dict(1334.86958348868, 'J/kg/K'),
                'cv': self.value_unit_dict(1043.71109135798, 'J/kg/K'),
                'density': self.value_unit_dict(0.577544330505469, 'kg/m3'),
                'temperature': self.value_unit_dict(889.27736479669, 'K'),
                'pressure': self.value_unit_dict(153934.915475353, 'Pa'),
                'massflux': self.value_unit_dict(1.92143028723584E-2, 'kg/s'),
                'massH2O': self.value_unit_dict(6.4456929510343E-2, '-'),
                'viscosity': self.value_unit_dict(3.9067870880755E-5, 'Pa s'),
                'compressibilityfactor': self.value_unit_dict(1, '-')}

    def pollutants_list(self, nox):
        nox_mfr = nox * 1e-6 * 0.05  # ppmv to kg/s, with rough guesses of density and mass flow rate
        no_mfr = nox_mfr * self.NO_frac
        no2_mfr = nox_mfr * (1.0 - self.NO_frac)
        return [self.pollutant_dict('CO', 4.38018592976711E-4, 'kg/s', 'no', 2.80104E-2, 'kg/mol'),
                self.pollutant_dict('CO2', 1.48566682568053E-2, 'kg/s', 'no', 4.40098E-2, 'kg/mol'),
                self.pollutant_dict('HC', 1.23609160023103E-3, 'kg/s', 'yes', 4.58822245497081E-2, 'kg/mol'),
                self.pollutant_dict('NO', no_mfr, 'kg/s', 'no', 3.0006E-2, 'kg/mol'),
                self.pollutant_dict('NO2', no2_mfr, 'kg/s', 'no', 4.6006E-2, 'kg/mol'),
                self.pollutant_dict('SO2', 8.69367E-6, 'kg/s', 'no', 6.4066E-2, 'kg/mol'),
                self.pollutant_dict('SO3', 3.56659152E-4, 'kg/s', 'no', 8.0066E-2, 'kg/mol'),
                self.pollutant_dict('O3', 3.0319795918367E-8, 'kg/s', 'no', 4.7997E-2, 'kg/mol')]

    def particle_list(self, soot):
        the_list = []
        prev_cdf = 0.0
        for i in range(1, self.NumParSizeClasses + 1):
            # 1e4 ... / 4
            the_size = exp(log(1e4) * i / 4)
            cdf = self.log_normal_cdf(the_size, self.LogNorm_mu, self.LogNorm_sigma)
            the_mfr = soot * (cdf - prev_cdf) / 1000.0 / 3600.0  # g/h to kg/s
            the_list.append(self.par_size_class_dict(the_size, 'nm', 1800, 'kg/m3', the_mfr, 'kg/s'))
            prev_cdf = cdf
        return the_list

    def adms_dict(self, nox, soot):
        return {'mixture': self.mixture_dict(),
                'pollutants': self.pollutants_list(nox),
                'particle': self.particle_list(soot)}

    # NB Need to have activated the MoDS Python_Env for this to work!
    # For some reason it needs to find the module named after the original file
    # in which the surrogate was pickled...

    @staticmethod
    def deserialise_surrogate(filename):
        with open(filename, 'br') as f:
            f.seek(8)  # NB First 64 bits are the length of the binary stream!
            the_surr = pickle.load(f)
        return the_surr

    @staticmethod
    def evaluate_surrogate(the_surr, the_xs):
        # Set model to evaluate mode
        the_surr.eval()
        the_surr.m_likelihood.eval()
        with torch.no_grad(), gpytorch.settings.use_toeplitz(False), gpytorch.settings.debug(False):
            y_pred = the_surr(torch.tensor(the_xs, dtype=torch.float32))

        return y_pred.mean().data.numpy().astype(numpy.float64)

    def evaluate_map(self, speed, torque):
        # Transform inputs.
        speed_transf = 2 * (speed - 800.0) / 1600.0 - 1.0
        torque_transf = 2 * (torque - 30.0) / 470.0 - 1.0
        xs = numpy.array([[speed_transf, torque_transf, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0]])
        # Evaluate surrogates.
        r_nox = self.evaluate_surrogate(self.nox_surr, xs)[0]
        r_soot = self.evaluate_surrogate(self.soot_surr, xs)[0]
        r_soot = exp(r_soot * log(10.0))
        return r_nox, r_soot

    def get_adms_map(self, args):
        speed_load_dict = json.loads(args[1].replace("'", '"'))
        speed = speed_load_dict['speed']['value']
        torque = speed_load_dict['torque']['value']
        nox_resp, soot_resp = self.evaluate_map(speed, torque)
        return self.adms_dict(nox_resp, soot_resp)


def main(args):
    try:
        processor = ADMSMapSpeedTorqueNOxSoot()
        print(json.dumps(processor.get_adms_map(args)))
    except Exception as e:
        print(e)


if __name__ == "__main__":
    main(sys.argv)
