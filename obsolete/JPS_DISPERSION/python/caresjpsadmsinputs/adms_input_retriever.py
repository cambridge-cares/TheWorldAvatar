import json
import requests
import sys
import geopy.distance
import datetime
from collections import namedtuple
from pyproj import Proj, transform
from rdflib import Graph
from rdflib.plugins.sparql import prepareQuery
from rdflib.plugins.sparql.results.jsonresults import JSONResult
from config import Constants, QueryStrings, ExceptionStrings
from adms_src import AdmsSrc
from adms_pol import AdmsPol
import uuid


class CliInputContext(object):
    """
    The Context defines the interface of interest to clients.
    """

    def __init__(self, args):
        self.entity_type = args[1]

        if self.entity_type == Constants.ENTITY_TYPE_PLANT:
            self._strategy = PlantCliInputStrategy(args)
        elif self.entity_type == Constants.ENTITY_TYPE_SHIP:
            self._strategy = ShipCliInputStrategy(args)

    def get_input(self):
        return self._strategy.extract_data()


class CliInputStrategy(object):

    def __init__(self, args):
        self.sourceCRS = Proj(init=Constants.CRS_EPSG_4326)
        self.entity_type = args[1]
        self.bdn_data = json.loads(args[2].replace("'", '"'))
        self.coor_data = str(args[3]).replace("'", '"')
        self.entity = str(args[4])
        self.working_dir = str(args[5])
        self.coord_sys = args[6][5:]
        self.targetCRS = Proj(init=args[6][:4].lower() + args[6][4:])
        self.input = None
        self.BDN = None
        self.coords = {}
        self.address = None
        self.g = None
        self.query = None
        self.q_method_map = None
        self.raw_src = None
        self.OPT = Constants.TPL_OPT
        self.range = None

    def extract_data(self):
        self.BDN = self.get_bdn(self.bdn_data)
        self.coords = self.get_coordinates(self.coor_data)
        self.range = self.get_range(self.coords)
        self.input = self.retrieve_input()
        self.input[Constants.KEY_BDN] = self.BDN
        self.input[Constants.KEY_COORD_SYS] = int(self.coord_sys)
        self.set_grid_size()
        return self.input

    @staticmethod
    def get_bdn(buildingdata):
        bdn = namedtuple(Constants.BLD_BDN,
                         [Constants.BLD_NUM, Constants.BLD_NAME, Constants.BLD_TYPE, Constants.BLD_X,
                          Constants.BLD_Y, Constants.BLD_HEIGHT, Constants.BLD_LRNGTH, Constants.BLD_WIDTH,
                          Constants.BLD_ANGLE])
        bdn.BldName = buildingdata[Constants.BLD_NAME]
        bdn.BldNumBuildings = len(bdn.BldName)
        bdn.BldType = buildingdata[Constants.BLD_TYPE]
        bdn.BldX = buildingdata[Constants.BLD_X]
        bdn.BldY = buildingdata[Constants.BLD_Y]
        bdn.BldHeight = buildingdata[Constants.BLD_HEIGHT]
        bdn.BldLength = buildingdata[Constants.BLD_LRNGTH]
        bdn.BldWidth = buildingdata[Constants.BLD_WIDTH]
        bdn.BldAngle = buildingdata[Constants.BLD_ANGLE]

        return bdn

    @staticmethod
    def get_coordinates(coordinates):
        coordinates = json.loads(coordinates)
        xmax = coordinates[Constants.COORD_MAX_CORNER][Constants.COORD_MAX_X]
        ymax = coordinates[Constants.COORD_MAX_CORNER][Constants.COORD_MAX_Y]
        xmin = coordinates[Constants.COORD_MIN_CORNER][Constants.COORD_MIN_X]
        ymin = coordinates[Constants.COORD_MIN_CORNER][Constants.COORD_MIN_Y]
        coordinates[Constants.KEY_MIN_X] = xmin
        coordinates[Constants.KEY_MIN_Y] = ymin
        coordinates[Constants.KEY_MAX_X] = xmax
        coordinates[Constants.KEY_MAX_Y] = ymax

        return coordinates

    @staticmethod
    def same_graph(uri1, uri2):
        def trimloc(uri):
            if uri is None:
                return None
            else:
                return uri.split('#')[0]

        return trimloc(uri1) == trimloc(uri2)

    def query_endpoint(self, qstr):
        def literal_tlit(sparqlres):

            if Constants.KEY_RESULTS not in sparqlres:
                return
            for row in sparqlres[Constants.KEY_RESULTS][Constants.KEY_BINDINGS]:
                for name, value in row.items():
                    if value[Constants.KEY_TYPE] == Constants.KEY_LITERAL and Constants.KEY_DATA_TYPE in value:
                        value[Constants.KEY_TYPE] = Constants.KEY_TLITERAL

        resp = requests.get(self.address, params={Constants.KEY_QUERY: qstr}, timeout=1500,
                            headers={Constants.KEY_UAGENT: Constants.KEY_APP_CLIENT})
        result = resp.json()
        literal_tlit(result)
        qres = JSONResult(result)

        return qres

    def query_local_graph(self, qstr):
        qres = self.g.query(qstr)
        return qres

    def connect_db(self, address, connect_type=Constants.KEY_CONN_ENDPOINT):

        def connect_db_actual(addr):
            """
            Actual method to connect to db
            """
            self.address = addr
            if connect_type is Constants.KEY_CONN_PARSE:
                self.g = Graph()
                self.g.parse(addr)

        self.q_method_map = {Constants.KEY_CONN_PARSE: self.query_local_graph,
                             Constants.KEY_CONN_ENDPOINT: self.query_endpoint}

        if not self.same_graph(address, self.address):
            if connect_type not in self.q_method_map:
                raise Exception(ExceptionStrings.DB_CONN_UNKNOWN)

            self.query = self.q_method_map[connect_type]
            connect_db_actual(address)

    def retrieve_input(self):
        pass

    def get_src_data(self):
        """Gets all sourced data.
        returns: list of source data objects for the ADMS_SOURCE_DETAILS section of the APL.
        """
        sources = []

        q1, q2, q3 = self.get_src_queries()

        for src in self.entity:
            new_src = self.get_new_src(src, q1, q2, q3)

            sources.append(new_src)

        return sources

    def get_new_src(self, src, q1, q2, q3):
        iri = self.get_src_iri(src)
        qdata = self.query(q1)
        qdata_c = self.query(q2)
        qdata_erate = self.query(q3)
        aresult, sorteder, pollutantnames = self.get_new_src_data(iri, qdata, qdata_c, qdata_erate)
        return self.make_src(aresult, sorteder, pollutantnames)

    def get_src_queries(self):
        return None, None, None

    def get_src_iri(self, src):
        if src is not None:
            return self.connect_db(src, Constants.KEY_CONN_PARSE)

    def get_new_src_data(self, iri, qdata, qdata_c, qdata_erate):
        return [], [], []

    def make_src(self, aresult, sorteder, pollutantnames):
        return self.make_common_src(aresult, sorteder, pollutantnames)

    @staticmethod
    def make_common_src(aresult, sorteder, pollutantnames):
        src = AdmsSrc(src_name=aresult[Constants.KEY_O].toPython(),
                      src_height=aresult[Constants.KEY_HEIGHT].toPython(),
                      src_diameter=float(aresult[Constants.KEY_DIAMETER].toPython()),
                      src_pol_emission_rate=sorteder,
                      src_pollutants=pollutantnames,
                      src_temperature=aresult[Constants.KEY_TEMP].toPython(),
                      src_mol_weight=aresult[Constants.KEY_MOLE_WEIGHT].toPython(),
                      src_density=float(aresult[Constants.KEY_DENSITY].toPython()),
                      src_spec_heat_cap=aresult[Constants.KEY_HEAT_CAP].toPython(),
                      src_num_pollutants=len(pollutantnames),
                      src_mass_flux=aresult[Constants.KEY_MASS_FLOW].toPython())
        return src

    def get_opt(self, pol_names, src_names):
        num_pol = len(pol_names)

        return self.OPT(num_pol, pol_names, [1] * num_pol, [0] * num_pol, [1] * num_pol, [3] * num_pol, [0] * num_pol,
                        [0] * num_pol, [0] * num_pol, [0] * 80, [0] * 80, [Constants.UNIT_UGM3] * num_pol, 1, 0, 1,
                        Constants.KEY_GRPTANK, src_names, 0)

    @staticmethod
    def get_range(userrange):
        keys = userrange.keys()
        if Constants.KEY_MIN_X in keys and Constants.KEY_MAX_X in keys \
                and Constants.KEY_MIN_Y in keys and Constants.KEY_MAX_Y in keys:
            return ((float(userrange[Constants.KEY_MIN_X]),
                     float(userrange[Constants.KEY_MAX_X])),
                    (float(userrange[Constants.KEY_MIN_Y]),
                     float(userrange[Constants.KEY_MAX_Y])))

    def core_bdn_src(self):
        y_midpoint = (self.range[1][0] + self.range[1][1]) / 2
        x_midpoint = (self.range[0][0] + self.range[0][1]) / 2
        for src in self.raw_src:
            closed, dclosed, first = None, sys.maxsize, True
            for i in range(len(self.BDN.BldX)):
                dx, dy = self.BDN.BldX[i] - x_midpoint, self.BDN.BldY[i] - y_midpoint
                d = dx * dx + dy * dy
                if first:
                    dclosed = d
                    closed = self.BDN.BldName[i]
                    first = False
                if d - dclosed < 0:
                    closed = self.BDN.BldName[i]
                    dclosed = d
            if closed is not None:
                setattr(src, Constants.SRC_MAIN_BLD, closed)
            else:
                raise Exception(ExceptionStrings.BDN_NOT_FOUND + src.SrcName)

    @staticmethod
    def get_weather():
        return Constants.FILEPATH_MET

    @staticmethod
    def get_bkg():
        return Constants.FILEPATH_BKG

    @staticmethod
    def pol_iri_name(pol_iri):
        substances = {
            Constants.OWL_NO2: Constants.POL_NO2,
            Constants.OWL_CO: Constants.POL_CO,
            Constants.OWL_CO2: Constants.POL_CO2,
            Constants.OWL_SO2: Constants.POL_PART_SO2,
            Constants.OWL_O3: Constants.POL_PART_O3,
            Constants.OWL_HC: Constants.POL_HC,
            Constants.OWL_NOx: Constants.POL_NOX,
            Constants.OWL_PART_001: None
        }

        if pol_iri in substances.keys():
            return substances[pol_iri]
        else:
            raise Exception(ExceptionStrings.SUBSTANCE_UNDEFINED)

    def set_grid_size(self):
        """
        Calculate image resolution based on size of the selected area.
        X, Y are equal and  calculated as function of the distance between upper and lower corner coordinates.
        :return: None
        """
        distance = geopy.distance.distance(transform(self.targetCRS, self.sourceCRS, self.coords[Constants.KEY_MAX_X],
                                                     self.coords[Constants.KEY_MAX_Y])[::-1],
                                           transform(self.targetCRS, self.sourceCRS, self.coords[Constants.KEY_MIN_X],
                                                     self.coords[Constants.KEY_MIN_Y])[::-1]).km
        # Rounds to 200 for ~35.5km distance, in case of default HK area size,
        # and to 80 for ~4.5km distance, in case of default SG area size
        rounded = int(round(distance * 4 + 62, -1))
        self.input[Constants.GRD_X] = rounded
        self.input[Constants.GRD_Y] = rounded


class PlantCliInputStrategy(CliInputStrategy):

    def __init__(self, args):
        super().__init__(args)
        self.pollutants = [Constants.POL_CO2, Constants.POL_CO, Constants.POL_NO2, Constants.POL_HC, Constants.POL_NOX]
        self.entity = (self.entity,)

    def retrieve_input(self):
        self.raw_src = self.get_src_data()
        raw_opt = self.get_opt(self.pollutants, [s.SrcName for s in self.raw_src])
        self.core_bdn_src()
        met = self.get_weather()
        xran, yran = self.range
        grd = xran[0], yran[0], xran[1], yran[1]

        return {Constants.KEY_SRC: self.raw_src, Constants.KEY_OPT: raw_opt,
                Constants.KEY_MET: met, Constants.KEY_GRD: grd}

    def get_src_queries(self):
        q1 = prepareQuery(
            QueryStrings.SPARQL_HEIGHT_DIAMETER_CONTENT_X_Y_VELOCITY_MASSFLOW_TEMP_HEATCAPA_DENSITY_MOLEWEIGHT)
        q2 = prepareQuery(QueryStrings.SPARQL_CONTENT)
        q3 = prepareQuery(QueryStrings.SPARQL_ERATE)

        return q1, q2, q3

    def get_new_src_data(self, iri, qdata, qdata_c, qdata_erate):
        aresult, = [row.asdict() for row in qdata]
        contents = [row[Constants.KEY_CONTENT].toPython() for row in qdata_c]
        emissionrates = {row[Constants.KEY_ER].toPython(): row[Constants.KEY_V].toPython() for row in qdata_erate}
        pollutantnames = [self.pol_iri_name(content) for content in contents]

        sorteder = []
        for content in contents:
            name = content.split('#')[1]
            for ername, v in emissionrates.items():
                if name in ername:
                    sorteder.append(v)

        aresult[Constants.KEY_EM_RATES] = sorteder

        return aresult, sorteder, pollutantnames

    def make_src(self, aresult, sorteder, pollutantnames):
        src = self.make_common_src(aresult, sorteder, pollutantnames)
        src.set_coordinates([aresult[Constants.KEY_X].toPython(), aresult[Constants.KEY_Y].toPython()])
        setattr(src, Constants.SRC_VERT_VELOC, aresult[Constants.KEY_VELOCITY].toPython())

        return src


class ShipCliInputStrategy(CliInputStrategy):

    def __init__(self, args):
        super().__init__(args)
        self.precipitation = float(str(args[7]))
        self.pollutants = [Constants.POL_CO2, Constants.POL_CO, Constants.POL_NO2, Constants.POL_HC, Constants.POL_NOX,
                           Constants.POL_PART_SO2, Constants.POL_PART_O3]
        self.ship_coordinates_list = None
        self.coords = None
        self.BDN = None
        self.entity = json.loads(self.entity.replace("'", '"'))
        self.em_rates = {}

    def retrieve_input(self):
        self.ship_coordinates_list = self.get_ship_coordinates()
        pol = self.get_pol()
        self.raw_src = self.get_src_data()
        raw_opt = self.get_opt(self.pollutants, [s.SrcName for s in self.raw_src])
        self.core_bdn_src()
        met = self.get_weather()
        xran, yran = self.range
        grd = xran[0], yran[0], xran[1], yran[1]
        bkg = self.get_bkg()

        return {Constants.KEY_SRC: self.raw_src, Constants.KEY_OPT: raw_opt, Constants.KEY_MET: met,
                Constants.KEY_GRD: grd, Constants.KEY_POL: pol, Constants.KEY_BKG: bkg}

    def extract_data(self):
        self.input = super(ShipCliInputStrategy, self).extract_data()
        self.modify_input_for_ship(self.ship_coordinates_list)
        return self.input

    def get_src_queries(self):
        q1 = prepareQuery(QueryStrings.SPARQL_DIAMETER_TEMP_HEIGHT_MASSFLOW_HEATCAPA_DENSITY_MOLEWEIGHT)
        q2 = prepareQuery(QueryStrings.SPARQL_CONTENT_PARTICULATE)
        q3 = prepareQuery(QueryStrings.SPARQL_ERATE)

        return q1, q2, q3

    def get_new_src(self, src, q1, q2, q3):
        new_src = super(ShipCliInputStrategy, self).get_new_src(src, q1, q2, q3)
        new_src.set_name(str(src[Constants.KEY_MMSI]) + '-' + str(uuid.uuid4()))
        return new_src

    def get_src_iri(self, src):
        return self.connect_chimney_db(src)

    def get_new_src_data(self, iri, qdata, qdata_c, qdata_erate):
        aresult, = [row.asdict() for row in qdata]
        contents = [row[Constants.KEY_CONTENT].toPython() for row in qdata_c]
        pollutantnames = [self.pol_iri_name(content) for content in contents]
        if None in pollutantnames:
            pollutantnames.remove(None)
            pollutantnames = pollutantnames + list(self.em_rates[iri].keys())

        emissionrates = {row[Constants.KEY_ER].toPython(): row[Constants.KEY_V].toPython() for row in qdata_erate}

        sorteder = []
        for content in contents:
            name = content.split('#')[1]
            for ername, v in emissionrates.items():
                if name in ername:
                    if Constants.POL_PART_001 in name.replace('-', ''):
                        sorteder = sorteder + list(self.em_rates[iri].values())
                    else:
                        sorteder.append(v)

        return aresult, sorteder, pollutantnames

    def get_pol(self):
        """
        Prepares data for ADMS_POLLUTANT_DETAILS section of the APL.
        Separates particles to PM10 and PM2.5 categories. Stores emission rates in em_rates
        class member variable for every examined source for later use. @see: get_src_data()
        @return: list of Pol objects.
        """
        limit = 10 - len(self.pollutants)  # No more that 10 pollutants are allowed per ADMS source
        pols = []
        pol_data = []
        diam_dens = set()
        pol_names = {}
        q1 = prepareQuery(QueryStrings.SPARQL_DIAMETER_DENSITY_MASSFRACTION)
        q2 = prepareQuery(QueryStrings.SPARQL_MASSRATE)
        i = 0
        k = 0

        for src in self.entity:
            iri = self.connect_chimney_db(src)
            self.em_rates[iri] = {}
            qb = self.query(q1)
            massrate = self.query(q2).__iter__().__next__()[Constants.KEY_MASS_RATE].toPython()

            for row in qb:
                dd = (row[Constants.KEY_DIAMETER].toPython(), row[Constants.KEY_DENSITY].toPython())
                mf = float(row[Constants.KEY_MASS_FRACTION]) * massrate
                pol_data.append({Constants.KEY_DIAMETER + Constants.KEY_DENSITY: dd,
                                 Constants.KEY_MASS_FLOW: mf,
                                 Constants.KEY_SRC: iri})
                diam_dens.add(dd)
                self.em_rates[iri][dd] = mf

        dd_srt = sorted(diam_dens, key=lambda tup: tup[0], reverse=True)[0:limit]
        for diam, dens in dd_srt:
            name = None
            if diam <= 0.00001:
                name = Constants.POL_PM10 + '-' + str(i)
                i = i + 1
                if diam <= 0.0000025:
                    name = Constants.POL_PM25 + '-' + str(k)
                    k = k + 1
            if name is not None:
                pol_names[(diam, dens)] = name
                pols.append(AdmsPol(name, 1, [diam], [dens], [1.0e+0]))
                self.pollutants.append(name)

        self.update_em_rates(pol_names, limit)

        return pols

    def update_em_rates(self, pol_names, limit):
        for src in self.em_rates:
            srt_rates = sorted(self.em_rates[src].items(), key=lambda kv: kv[0][0], reverse=True)
            mf_acc = 0
            em_rates_ltd = {}
            for p in srt_rates[limit:]:
                mf_acc = mf_acc + p[1]
            i = 1
            for p in srt_rates[:limit]:
                pname = pol_names[p[0]]
                pmflr = p[1]
                if i == limit:
                    pmflr = pmflr + mf_acc
                em_rates_ltd[pname] = pmflr
                i = i + 1
            self.em_rates[src] = em_rates_ltd

    def get_opt(self, pol_names, src_names):
        num_pol = len(pol_names)

        return self.OPT(num_pol, pol_names, [1] * num_pol, [0] * num_pol, [1] * num_pol, [3] * num_pol, [0] * num_pol,
                        [0] * num_pol, [0] * num_pol, [0] * 80, [0] * 80, [Constants.UNIT_UGM3] * num_pol, 0, 0, 1,
                        Constants.KEY_GRPTANK, Constants.KEY_SHIP_OPT_SRCNAME, 0)

    def connect_chimney_db(self, src):
        mmsi = src[Constants.KEY_MMSI]
        iri = Constants.IRI_KB_SHIPS + str(mmsi) + Constants.STR_CHIMNEY
        self.connect_db(iri, Constants.KEY_PARSE)

        return iri

    def get_ship_coordinates(self):
        ship_coordinates_list = []

        for ship in self.entity:
            x_coordinate_value = float(ship[Constants.KEY_LON])
            y_coordinate_value = float(ship[Constants.KEY_LAT])
            ship_coordinates_list.append(
                list(transform(self.sourceCRS, self.targetCRS, x_coordinate_value, y_coordinate_value)))

        return ship_coordinates_list

    def modify_input_for_ship(self, ship_coordinates_list):
        self.set_input_ship_src_geo(ship_coordinates_list)
        self.set_input_ship_indicators()
        self.set_input_ship_night()

        self.input[Constants.KEY_MET] = self.working_dir + Constants.FILENAME_MET
        self.input[Constants.KEY_BKG] = self.working_dir + Constants.FILENAME_BGD

        so2washout, pm10washout = self.get_washouts()
        self.input[Constants.KEY_WASHOUT_SO2] = so2washout
        self.input[Constants.KEY_WASHOUT_PM10] = pm10washout

    def set_input_ship_src_geo(self, ship_coordinates_list):
        for idx in range(len(ship_coordinates_list)):
            self.input[Constants.KEY_SRC][idx].set_coordinates(ship_coordinates_list[idx])

        latitudemid = (float(self.coords[Constants.KEY_MIN_Y]) + float(self.coords[Constants.KEY_MAX_Y])) / 2
        longitudemid = (float(self.coords[Constants.KEY_MIN_X]) + float(self.coords[Constants.KEY_MAX_X])) / 2
        xmid, ymid = transform(self.targetCRS, self.sourceCRS, longitudemid, latitudemid)

        self.input[Constants.KEY_LAT.title()] = ymid

    def set_input_ship_indicators(self):
        if str(2326) in self.coord_sys:
            self.input[Constants.KEY_INDICATOR_TERR] = 1
        else:
            self.input[Constants.KEY_INDICATOR_TERR] = 0

        self.input[Constants.KEY_INDICATOR_CHEM] = 1
        self.input[Constants.KEY_INDICATOR_WET] = 1

    def set_input_ship_night(self):
        now = datetime.datetime.now()
        hournow = now.hour + 1
        if not (6 <= hournow <= 18):
            self.input[Constants.KEY_NIGHT] = 1
            self.input[Constants.KEY_DIR_NIGHT] = Constants.FILEPATH_NIGHT
        else:
            self.input[Constants.KEY_NIGHT] = 0
            self.input[Constants.KEY_DIR_NIGHT] = ""

    def get_washouts(self):
        annual_precipitation = self.precipitation * 365 * 24

        if annual_precipitation < 103:
            so2washout = 0.000001 / 500 * annual_precipitation
        else:
            so2washout = 0.0000019 + annual_precipitation * 0.0000000008

        if self.precipitation < 0.5:
            pm10washout = 0.0016
        elif self.precipitation > 4:
            pm10washout = 0.0072
        else:
            pm10washout = 0.00363

        return so2washout, pm10washout
