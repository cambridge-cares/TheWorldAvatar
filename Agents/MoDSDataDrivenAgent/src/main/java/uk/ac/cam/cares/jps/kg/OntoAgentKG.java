package uk.ac.cam.cares.jps.kg;

import java.io.IOException;
import java.net.URLEncoder;

import uk.ac.cam.cares.jps.agent.http.Request;
import uk.ac.cam.cares.jps.agent.mechanism.datadriven.Property;

public class OntoAgentKG {
	/**
	 * Reads the number of cores required for a calibration job. 
	 * 
	 * @return
	 * @throws IOException
	 */
	public static String getNumberOfCores() throws IOException {
		String query = formNumberOfCoreQuery(Property.PREFIX_BINDING_MSM.getPropertyName(),
				Property.PREFIX_MSM.getPropertyName(), Property.MODS_AGENT_IRI.getPropertyName());
		System.out.println("Query:"+query);
		String httpURL = Property.FUSAKI_URL_FOR_WORLD_AVATAR.getPropertyName().concat(URLEncoder.encode(query, "UTF-8"));
		return Request.get(httpURL);
	}
	
	/**
	 * Reads the size of RAM required for a calibration job.
	 * 
	 * @return
	 * @throws IOException
	 */
	public static String getMemorySize() throws IOException {
		String query = OntoAgentKG.formRAMSizeQuery(Property.PREFIX_BINDING_MSM.getPropertyName(),
				Property.PREFIX_MSM.getPropertyName(), Property.PREFIX_BINDING_RAM.getPropertyName(),
				Property.PREFIX_RAM.getPropertyName(), Property.MODS_AGENT_IRI.getPropertyName());
		System.out.println("Query:"+query);
		String httpURL = Property.FUSAKI_URL_FOR_WORLD_AVATAR.getPropertyName().concat(URLEncoder.encode(query, "UTF-8"));
		return Request.get(httpURL);
	}
	
	/**
	 * Builds a query to retrieve the number of cores needed to run a calibration job. 
	 * 
	 * @param prefixBindingMSM
	 * @param prefix
	 * @param modsAgentIRI
	 * @return
	 */
	public static String formNumberOfCoreQuery(String prefixBindingMSM, String prefix, String modsAgentIRI) {
		String queryString = prefixBindingMSM+"\n";
		queryString = queryString.concat("SELECT ?core \n");
		queryString = queryString.concat("WHERE { \n");
		queryString = queryString.concat("    ").concat(modsAgentIRI).concat(" ").concat(prefix).concat(":hasNumberOfCore").concat(" ?core . \n");
		queryString = queryString.concat("      }");
		return queryString;
	}
	
	/**
	 * Builds a query to retrieve the size of RAM needed to run a calibration job. 
	 * 
	 * @param prefixBindingMSM
	 * @param prefixMSM
	 * @param prefixBindingRAM
	 * @param prefixRAM
	 * @param modsAgentIRI
	 * @return
	 */
	public static String formRAMSizeQuery(String prefixBindingMSM, String prefixMSM, String prefixBindingRAM, String prefixRAM, String modsAgentIRI) {
		String queryString = prefixBindingMSM+"\n";
		queryString = queryString + prefixBindingRAM+"\n";
		queryString = queryString.concat("SELECT * \n");
		queryString = queryString.concat("WHERE { \n");
		queryString = queryString.concat("    ").concat(modsAgentIRI).concat(" ").concat(prefixMSM).concat(":hasMemoryRequirement").concat(" ?memReq . \n");
		queryString = queryString.concat("      }");
		return queryString;
	}
}
