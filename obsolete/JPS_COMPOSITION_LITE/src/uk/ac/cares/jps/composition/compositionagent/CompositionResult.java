package uk.ac.cares.jps.composition.compositionagent;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;

public class CompositionResult {

	public Map<String, Map<String, String>> input_mapping = new HashMap<String, Map<String, String>>();
	public Map<String, Map<String, String>> output_mapping = new HashMap<String, Map<String, String>>();
	public ArrayList<ArrayList<String>> layers = new ArrayList<ArrayList<String>>();

	public CompositionResult() {

	}

	public CompositionResult(Map<String, Map<String, String>> _input_mapping,
			Map<String, Map<String, String>> _output_mapping, ArrayList<ArrayList<String>> _layers) {

		this.input_mapping = _input_mapping;
		this.output_mapping = _output_mapping;
		this.layers = _layers;

	}

}
