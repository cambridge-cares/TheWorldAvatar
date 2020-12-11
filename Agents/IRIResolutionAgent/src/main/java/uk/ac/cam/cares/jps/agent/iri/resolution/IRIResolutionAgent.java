package uk.ac.cam.cares.jps.agent.iri.resolution;

import javax.servlet.http.HttpServletRequest;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.ResponseBody;
import uk.ac.cam.cares.jps.base.agent.JPSAgent;

/**
 * IRI Resolution Agent is developed to ensure the resolvability of all IRIs<br>
 * created to identify entities/resources in the JPS knowledge graph.<br>
 * 
 * The approach followed in this implementation is the detection of a repo-<br>
 * sitory/namespace that contains the resource connected to the current IRI<br>
 * to query information which are directly connected to this resource.  
 * 
 * @author Feroz Farazi (msff2@cam.ac.uk)
 *
 */
@Controller
public class IRIResolutionAgent extends JPSAgent{
	/**
	 * Extracts the name of repository or namespace from the current IRI<br>
	 * and returns it to the calling method. 
	 * 
	 * @param iri the URI of request
	 * @return
	 */
	public String retrieveRepositoryOrNamespace(String iri){
		String[] tokens = iri.split("/");
		if(tokens.length>1 && tokens[0].length()>5){
			return tokens[0];
		}else{
			return null;
		}
	}
}
