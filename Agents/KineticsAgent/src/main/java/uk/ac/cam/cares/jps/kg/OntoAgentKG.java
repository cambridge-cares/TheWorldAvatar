package uk.ac.cam.cares.jps.kg;

import java.io.IOException;
import java.net.URLEncoder;

import com.cmclinnovations.jps.agent.http.Request;
import com.cmclinnovations.jps.agent.quantum.calculation.Property;

/**
 * This class manages the query of hardware requirement for Kinetics Agent from</br>
 *  the OntoAgent repository available on any Fusaki triple store.
 * 
 * @author Feroz Farazi (msff2@cam.ac.uk)
 *
 */
public class OntoAgentKG {

	/**
	 * Reads the number of cores required for a quantum job. 
	 * 
	 * @return
	 * @throws IOException
	 */
	public static String getNumberOfCores() throws IOException{
		String query = formNumberOfCoreQuery(Property.PREFIX_BINDING_MSM.getPropertyName(),
				Property.PREFIX_MSM.getPropertyName(), Property.Kinetics_AGENT_IRI.getPropertyName());
		System.out.println("Query:"+query);
		String httpURL = Property.FUSAKI_URL_FOR_WORLD_AVATAR.getPropertyName().concat(URLEncoder.encode(query, "UTF-8"));
		return Request.get(httpURL);
	}

	/**
	 * Reads the size of RAM required for a quantum job. 
	 * 
	 * @return
	 * @throws IOException
	 */
	public static String getMemorySize() throws IOException{
		String query = OntoAgentKG.formRAMSizeQuery(Property.PREFIX_BINDING_MSM.getPropertyName(),
				Property.PREFIX_MSM.getPropertyName(), Property.PREFIX_BINDING_RAM.getPropertyName(),
				Property.PREFIX_RAM.getPropertyName(), Property.Kinetics_AGENT_IRI.getPropertyName());
		System.out.println("Query:"+query);
		String httpURL = Property.FUSAKI_URL_FOR_WORLD_AVATAR.getPropertyName().concat(URLEncoder.encode(query, "UTF-8"));
		return Request.get(httpURL);
	}

	
	/**
	 * Builds a query to retrieve the number of cores needed to run a quantum job.</br>
	 * 
	 * @param prefixBindingMSM
	 * @param prefix
	 * @param kineticsAgent
	 * @return
	 */
	public static String formNumberOfCoreQuery(String prefixBindingMSM, String prefix, String kineticsAgentIRI){
		String queryString = prefixBindingMSM+"\n";
		queryString = queryString.concat("SELECT ?core \n");
		queryString = queryString.concat("WHERE { \n");
		queryString = queryString.concat("    ").concat(kineticsAgentIRI).concat(" ").concat(prefix).concat(":hasNumberOfCore").concat(" ?core . \n");
		queryString = queryString.concat("      }");
		return queryString;
	}

	/**
	 * Builds a query to retrieve the size of RAM needed to run a qunatum job.</br>
	 * 
	 * @param prefixBindingMSM
	 * @param prefix
	 * @param kineticsAgent
	 * @return
	 */
	public static String formRAMSizeQuery(String prefixBindingMSM, String prefixMSM, String prefixBindingRAM, String prefixRAM, String kineticsAgentIRI){
		String queryString = prefixBindingMSM+"\n";
		queryString = queryString + prefixBindingRAM+"\n";
		queryString = queryString.concat("SELECT * \n");
		queryString = queryString.concat("WHERE { \n");
//		queryString = queryString.concat("    ").concat(kineticsAgentIRI).concat(" ").concat(prefixMSM).concat(":hasNumberOfCore").concat(" ?core . \n");		
		queryString = queryString.concat("    ").concat(kineticsAgentIRI).concat(" ").concat(prefixMSM).concat(":hasMemoryRequirement").concat(" ?memReq . \n");
//		queryString = queryString.concat("    ").concat("?memReq ").concat(prefixMSM).concat(":hasUnit").concat(" ?units . \n");
//		queryString = queryString.concat("    ").concat("?memReq ").concat(prefixRAM).concat(":ram_size").concat(" ?memSize . \n");
		queryString = queryString.concat("      }");
		return queryString;
	}
}
