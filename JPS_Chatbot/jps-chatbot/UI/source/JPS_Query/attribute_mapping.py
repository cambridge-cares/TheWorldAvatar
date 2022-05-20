from fuzzywuzzy import fuzz


class AttributeMapper:
    def __init__(self):
        self.THERMO_ATTRIBUTES = ['hasPolarizability', 'hasLennardJonesWellDepth', 'hasDipoleMoment',
                                  'hasRotationalRelaxationCollisionNumber']
        self.QUANTUM_ATTRIBUTES = ['hasRotationalConstants', 'hasFrequencies', 'hasRotationalSymmetryNumber',
                                   'hasOutputFile', 'hasSpinMultiplicity', 'hasFormalCharge', 'hasElectronicEnergy',
                                   'hasGeometryType']

        self.query_attribute_map = {'hasRotationalConstants': 'rotational_constants',
                                    'hasFrequencies': 'vibration_frequency',
                                    'hasRotationalSymmetryNumber': 'symmetry_number',
                                    'hasOutputFile': 'guassian_file', 'hasSpinMultiplicity': 'spin_multiplicity',
                                    'hasFormalCharge': 'formal_charge', 'hasElectronicEnergy': 'electronic_energy',
                                    'hasGeometryType': 'geometry_type'}

    def find_closest_attribute(self, intent, attribute):
        # TODO: increase the threshold
        lookup_threshold = 35
        if intent == 'query_thermodynamic':
            score_map = sorted([(word, fuzz.ratio(attribute, word)) for word in self.THERMO_ATTRIBUTES if fuzz.ratio(attribute, word) > 34],
                               key=lambda x: x[1], reverse=True)
            return score_map[0][0]

        elif intent == 'query_quantum_chemistry':
            score_map = sorted([(word, fuzz.ratio(attribute, word)) for word in self.QUANTUM_ATTRIBUTES if fuzz.ratio(attribute, word) > 34],
                               key=lambda x: x[1], reverse=True)
            return score_map[0][0]

    def map_to_quantum_queries(self, attribute_iri):

        return self.query_attribute_map[attribute_iri]
