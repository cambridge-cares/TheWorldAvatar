import re
import pytest

# get the JVM module view (via jpsBaseLibGateWay instance) from the jpsSingletons module
from gasgridagent.jpsSingletons import jpsBaseLibView

# Import module under test from gasgridagent
import gasgridagent.output_flow_data as term_out


@pytest.fixture()
def get_sample_gasflow_history():
    # Create sample test data as returned by get_gasflow_history() from triple store and postgres database
    # FORMAT: {terminal name: [terminal IRI, measurement IRI, Java time series object], ...}

    ts_data = {}

    # Create sample data of length n
    n = 4
    terminal_name = 'test_terminal'
    terminal_iri = 'test_terminal_iri'
    data_iri = 'test_data_iri'
    values = [[1.0 for j in (range(n))]]
    t = jpsBaseLibView.java.time.Instant.now()
    times = []
    for k in range(n):
        times.append(t.plusSeconds(k))

    # Construct time series object from sample data
    ts = jpsBaseLibView.TimeSeries(times, [data_iri], values)
    # Add to dictionary
    ts_data[terminal_name] = [terminal_iri, data_iri, ts]

    return ts_data


def test_onSuccess(get_sample_gasflow_history):

    # Retrieve converted data
    converted_data = term_out.onSuccess(get_sample_gasflow_history)

    # Remove trailing [] and split individual entries
    conv = converted_data.strip('[]')
    conv = conv.replace('}, {', '},  {')
    conv = conv.split(',  ')

    for c in range(len(conv)):

        assert conv[c].startswith('{"s": "test_terminal_iri", "UTC": "')
        assert conv[c].endswith('", "num_val": "1.0", "label": "test_terminal"}')

        s = len('{"s": "test_terminal_iri", "UTC": "')
        e = len('", "num_val": "1.0", "label": "test_terminal"}')
        assert len(conv[c]) == s + e + len('2021-09-24T14:55:15.000Z')

        date = conv[c][s : s+len('2021-09-24T14:55:15.000Z')]
        regexp = re.compile(r'[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}:[0-9]{2}:[0-9]{2}.[0-9]{3}Z')
        assert bool(regexp.search(date))



