from .MarieLogger import MarieIOLog


def getHighestIntentScore(result):
    first_intent = result['intent']
    name = first_intent['name']
    print('intent name:', name)
    score = first_intent['confidence']
    return score

if __name__ == "__main__":
    rst = {'intent': {'name': 'query_quantum_chemistry', 'confidence': 0.7796140909194946}, 'entities': [
        {'entity': 'attribute', 'start': 0, 'end': 16, 'confidence_entity': 0.5236995705636405,
         'value': 'geometry c=c=c=c', 'extractor': 'CRFEntityExtractor'}],
           'intent_ranking': [{'name': 'query_quantum_chemistry', 'confidence': 0.7796140909194946},
                              {'name': 'geometry_type', 'confidence': 0.12144190818071365},
                              {'name': 'electronic_energy', 'confidence': 0.02252243459224701},
                              {'name': 'spin_multiplicity', 'confidence': 0.022378213703632355},
                              {'name': 'symmetry_number', 'confidence': 0.017157595604658127},
                              {'name': 'rotational_relaxation_collision', 'confidence': 0.009460683912038803},
                              {'name': 'vibration_frequency', 'confidence': 0.007247160654515028},
                              {'name': 'rotational_constants', 'confidence': 0.007164621260017157},
                              {'name': 'guassian_file', 'confidence': 0.006535339634865522},
                              {'name': 'query_thermodynamic', 'confidence': 0.006477926392108202}],
           'text': 'geometry c=c=c=c\n'}
