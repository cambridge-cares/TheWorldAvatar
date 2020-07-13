package uk.ac.cam.cares.jps.composition.util;

import static org.junit.Assert.assertEquals;

import java.io.IOException;
import java.net.URISyntaxException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.apache.commons.lang3.ArrayUtils;
import org.json.JSONException;
import org.json.JSONObject;

import com.fasterxml.jackson.core.JsonParseException;
import com.fasterxml.jackson.databind.JsonMappingException;

import uk.ac.cam.cares.jps.composition.enginemodel.Edge;
import uk.ac.cam.cares.jps.composition.enginemodel.Graph;
import uk.ac.cam.cares.jps.composition.servicemodel.MessageContent;
import uk.ac.cam.cares.jps.composition.servicemodel.MessagePart;
import uk.ac.cam.cares.jps.composition.servicemodel.Service;

public class Optimization {

	
	public Optimization() {
		
	}

	// For details of the implementation, please go to test case: TestOptimization
	public Set<Service> start_optimization(String graph_in_string) throws JsonParseException, JsonMappingException, URISyntaxException, IOException, JSONException {
		
		
		Set<Edge> edge_elimination_set = new HashSet<Edge>();
		Set<Service> service_elimination_set = new HashSet<Service>();
		JSONObject GraphInJSON = new JSONObject(graph_in_string);
		Graph graph = FormatTranslator.convertGraphJSONTOJavaClass(GraphInJSON.toString());
		Map<String, Service> agent_map = new HashMap<String, Service>();
		agent_map = make_input_to_agent_map(graph);
		ArrayList<Edge> edges = graph.edges;
		
		edges = remove_duplicated_edges(edges);
		
		Map<String, ArrayList<Edge>> edges_group = find_redundant_edges(edges);
		
		
		if (edges_group == null) {
			System.out.println("The end of the optimization, nothing to optimize with");
		}
		else {
			
			for (Map.Entry<String, ArrayList<Edge>> entry : edges_group.entrySet())
			{
				
				int max_score = 0;
			    // This map groups edges pointing to the same input into same groups... 
				// Do optimization in each group first ... 
				ArrayList<Edge> one_edge_group = entry.getValue();
				if(one_edge_group.size() > 1) {
					Edge the_selected_edge = one_edge_group.get(0);
					for (Edge edge : one_edge_group) {
						String edge_key = "";
						for (int i : edge.fromOutput) {
							edge_key += String.valueOf(i);
						}
						//Service upstream_service = agent_map.get(edge_key);
						if(edge_key.startsWith("-1")) {
							// This means its an inital input, this edge is automatically the selected edge... 
							the_selected_edge = edge; 
							for(Edge other_edge : one_edge_group) {
								if(other_edge != the_selected_edge) {
									edge_elimination_set.add(other_edge); // Edges other than the selected edge will be eliminated...  
								}
							}
							break;
						}
						else {
							// Here is the situation for comparing a group of agents with scores. 
							Service upstream_service = agent_map.get(edge_key);
							Long[] score_array = graph.scoreMap.get(upstream_service.getUri().toASCIIString()); 
							int score = (int) (score_array[0] / score_array[1]);
							if(score >= max_score) {
								max_score = score;
								the_selected_edge = edge;
							}
						}
						
					}
					// The end of an iteration. 
					for(Edge other_edge : one_edge_group) {
						if(other_edge != the_selected_edge) {
							edge_elimination_set.add(other_edge); // Edges other than the selected edge will be eliminated...  
						}
					}
				}
			}
		}
		 
			
			for(Edge edge_to_remove : edge_elimination_set) {
				Service service_to_remove = locate_agent(edge_to_remove.fromOutput, graph, agent_map); // The final result, the services to eliminate... 
				service_elimination_set.add(service_to_remove);
			}
			
			return service_elimination_set;
		
		
	}
	
	private Service locate_agent(int[] inputs, Graph graph, Map<String, Service> agent_map) {
		 
		String key = "";
		for (int i : inputs) {
			key += String.valueOf(i);
		}
		System.out.println("---- located ----" + agent_map.get(key).getUri().toASCIIString());
		return agent_map.get(key);
	}
	
	
	private Map<String, ArrayList<Edge>> find_redundant_edges(ArrayList<Edge> edges) {
		// 1. Group all the edges by target ... 
		Boolean has_redundant = false;
		// 2. This boolean variable indicates whether there is any thing to eliminate at all. 
		
		Map<String, ArrayList<Edge>> map = new HashMap<String, ArrayList<Edge>>();
		for (Edge edge : edges) {
			String key = "";
        	for(int n : edge.toInput) {
        		key += String.valueOf(n);
        	}
        	
         	if(map.containsKey(key)) {
        		// append the edge
        		has_redundant = true;
        		map.get(key).add(edge);
        	}
        	else {
        		// create new ArrayList of edges 
        		ArrayList<Edge> new_edge_group = new ArrayList<Edge>(); 
        		new_edge_group.add(edge);
        		map.put(key, new_edge_group);
        	}
        	

		}
		
		if(!has_redundant) {
			return null;
		}
		else {
			return map;		
		}
	} 
	
	
	
	private ArrayList<Edge> remove_duplicated_edges(ArrayList<Edge> _edges) {
		
		// Compare one edge with all the other edge, if there is a duplicate, put the index in index_to_remove
			ArrayList<Edge> clean_edges = new ArrayList<Edge>();
			Map<String,Edge> edge_map = new HashMap<String,Edge>();
			Iterator<Edge> itr = _edges.iterator(); 
	        while (itr.hasNext()) {
	        	Edge this_edge = (Edge) itr.next();
	        	int[] key_array = (int[])ArrayUtils.addAll(this_edge.fromOutput, this_edge.toInput);
	        	String key = "";
	        	for(int n : key_array) {
	        		key += String.valueOf(n);
	        	}
	        	
	        	edge_map.put(key, this_edge);
	        }
	        
	        for (Map.Entry<String, Edge> entry : edge_map.entrySet())
	        {
	        	clean_edges.add(entry.getValue());
	        }
	        
		return clean_edges; 
	}
	
	
	private Map<String, Service> make_input_to_agent_map(Graph graph) {
		
		Map<String, Service> agent_map = new HashMap<String, Service>();
		for (Service service : graph.servicePool) {
			List<MessageContent> outputs = service.getOperations().get(0).getOutputs();
			Set<String> index_list = new HashSet<String>();
			for (MessageContent output : outputs) {
				for(MessagePart part : output.getMandatoryParts()) {
					for(Edge edge : part.outputEdges) {
						String key_from_edge = "";
					 	for(int n : edge.fromOutput) {
					 		key_from_edge += String.valueOf(n);
			        	}
					 	
					 	index_list.add(key_from_edge);
					}
				}
			}
		  
			if((index_list.contains("000") && index_list.contains("020")) && index_list.size() > 1) {
				index_list.remove("000");
			}
			//System.out.println("index_list" + index_list);
			if(index_list.size() != 0) {
				Iterator<String> iter = index_list.iterator();
				agent_map.put((String) iter.next(), service);
			}
		}

		return agent_map;
	 
	}
}
