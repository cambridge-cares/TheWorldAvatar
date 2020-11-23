package uk.ac.cam.cares.jps.ontomatch.alignment;
/**
 * Helper class that contains: Namespace definition in Alignment ontology
 *
 * @author shaocong zhang
 * @version 1.0
 * @since 2020-09-08
 */
public class AlignmentNamespace {
	
	public static final String IRI = "http://knowledgeweb.semanticweb.org/heterogeneity/alignment#";
	public static final String PREFIX = "alignment";
	public static final String ENTITY1 = PREFIX+":entity1";
	public static final String ENTITY2 = PREFIX+":entity2";
	public static final String MEASURE = PREFIX+":measure";
	public static final String RELATION = PREFIX+":relation";
	public static final String ONTO1 = PREFIX+":onto1";
	public static final String ALIGNMENT = PREFIX+":Alignment";
	public static final String ONTO2 = PREFIX+":onto2";
	public static final String ONTOLOGY = PREFIX+":ontology";
	public static final String LOCATION = PREFIX+":location";
	public static final String CELL = PREFIX+":Cell";
	public static final String EQUAL_RELATION ="\"=\"";
	public static final String map = PREFIX+":map";

	public static final String fullIRI(String shortname) {
		String[] namearr = shortname.split(":");
		return IRI+namearr[1];
		
	}
}
