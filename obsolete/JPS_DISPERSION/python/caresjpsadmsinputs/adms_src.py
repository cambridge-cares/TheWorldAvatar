from config import Constants


class AdmsSrc(object):

    def __init__(self, src_name, src_height, src_diameter, src_temperature, src_mol_weight, src_density,
                 src_spec_heat_cap, src_pollutants, src_pol_emission_rate, src_x1 = 0, src_y1 = 0, src_vol_flow_rate=0,
                 src_source_type=0, src_release_at_ntp=0, src_efflux_type=3, src_buoyancy_type=0,
                 src_percent_n_ox_as_no2=5, src_l1=1, src_l2=1, src_fm=1, src_fb=1, src_mass_flux=1, src_angle1=0,
                 src_angle2=0, src_mass_h2_o=0, src_use_var_file=1, src_num_groups=1, src_group=Constants.KEY_GRPTANK,
                 src_num_vertices=0, src_tra_num_traffic_flows=0, src_num_pollutants=1, src_pol_totalemission=1,
                 src_pol_start_time=0, src_pol_duration=0, src_num_isotopes=0):
        self.SrcName = src_name
        self.SrcHeight = src_height
        self.SrcDiameter = src_diameter
        self.SrcVolFlowRate = src_vol_flow_rate
        self.SrcTemperature = src_temperature
        self.SrcMolWeight = src_mol_weight
        self.SrcDensity = src_density
        self.SrcSpecHeatCap = src_spec_heat_cap
        self.SrcSourceType = src_source_type
        self.SrcReleaseAtNTP = src_release_at_ntp
        self.SrcEffluxType = src_efflux_type
        self.SrcBuoyancyType = src_buoyancy_type
        self.SrcPercentNOxAsNO2 = src_percent_n_ox_as_no2
        self.SrcX1 = src_x1
        self.SrcY1 = src_y1
        self.SrcL1 = src_l1
        self.SrcL2 = src_l2
        self.SrcFm = src_fm
        self.SrcFb = src_fb
        self.SrcMassFlux = src_mass_flux
        self.SrcAngle1 = src_angle1
        self.SrcAngle2 = src_angle2
        self.SrcMassH2O = src_mass_h2_o
        self.SrcUseVARFile = src_use_var_file
        self.SrcNumGroups = src_num_groups
        self.SrcGroup = src_group
        self.SrcNumVertices = src_num_vertices
        self.SrcTraNumTrafficFlows = src_tra_num_traffic_flows
        self.SrcNumPollutants = src_num_pollutants
        self.SrcPollutants = src_pollutants
        self.SrcPolEmissionRate = src_pol_emission_rate
        self.SrcPolTotalemission = src_pol_totalemission
        self.SrcPolStartTime = src_pol_start_time
        self.SrcPolDuration = src_pol_duration
        self.SrcNumIsotopes = src_num_isotopes
        if self.SrcNumPollutants > 1:
            self.SrcPolTotalemission = [src_pol_totalemission] * src_num_pollutants
            self.SrcPolStartTime = [src_pol_start_time] * src_num_pollutants
            self.SrcPolDuration = [src_pol_duration] * src_num_pollutants

    def __repr__(self):
        attrs = self.__dict__.items()
        return '<admsSrc {}>'.format(['{} {}'.format(attr, value) for (attr, value) in attrs])

    def set_coordinates(self, coordinates):
        self.SrcX1 = coordinates[0]
        self.SrcY1 = coordinates[1]

    def set_name(self, src_name):
        self.SrcName = src_name
