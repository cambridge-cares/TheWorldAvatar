package uk.ac.cam.cares.jps.building;

// TODO-AE; move to JPS_BASE, maybe write a SPARQL Query Builder
public interface SparqlConstants {

	public static final String PREFIX_XSD = "PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>\n";  
	public static final String PREFIX_ONTOCAPE_SYS = "PREFIX sys: <http://www.theworldavatar.com/OntoCAPE/OntoCAPE/upper_level/system.owl#>\n";
	public static final String PREFIX_ONTOCAPE_SPACE_AND_TIME_EXTENDED = "PREFIX space_and_time_extended: <http://www.theworldavatar.com/OntoCAPE/OntoCAPE/supporting_concepts/space_and_time/space_and_time_extended.owl#>\n";
	public static final String PREFIX_CITYGML = "PREFIX citygml:<http://www.theworldavatar.com/CityGMLOntology.owl#>\n";
	
	String CITYGML_HASCOORDINATES_XY = 		
			"?coordinates space_and_time_extended:hasProjectedCoordinate_x ?xe .\n" + 
			"?xe sys:hasValue ?xv .\n" + 
			"?xv sys:numericalValue ?x .\n" + 
			"?coordinates space_and_time_extended:hasProjectedCoordinate_y ?ye .\n" + 
			"?ye sys:hasValue ?yv .\n" + 
			"?yv sys:numericalValue ?y .\n";
	
	String CITYGML_HASCOORDINATES_XYZ = 		
			"?coordinates space_and_time_extended:hasProjectedCoordinate_x ?xe .\n" + 
			"?xe sys:hasValue ?xv .\n" + 
			"?xv sys:numericalValue ?x .\n" + 
			"?coordinates space_and_time_extended:hasProjectedCoordinate_y ?ye .\n" + 
			"?ye sys:hasValue ?yv .\n" + 
			"?yv sys:numericalValue ?y .\n" + 
			"?coordinates space_and_time_extended:hasProjectedCoordinate_z ?ze .\n" + 
			"?ze sys:hasValue ?zv .\n" + 
			"?zv sys:numericalValue ?z .\n";
}
