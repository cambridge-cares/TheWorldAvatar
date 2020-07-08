import java.util.List;

//import uk.ac.cam.cares.jps.base.query.JenaResultSetFormatter;
//import uk.ac.cam.cares.jps.base.query.QueryBroker;
//import uk.ac.cam.cares.jps.base.util.MatrixConverter;

//import uk.ac.cam.cares.jps.powsys.nuclear.IriMapper;
//import uk.ac.cam.cares.jps.powsys.nuclear.QueryBroker;

public class Matlab_ipgen {
	
public static void main (String[] args) {
		
	String lotiri = "http://localhost:8080/kb/sgp/singapore/district/building/user/BO-001.owl";
	
	String baseURL = "/Users/gourab/Eclipse-Workspace/Output/";
	
	
	Matlab_ipgen main1 = new Matlab_ipgen();
	main1.prepareCSVLandlot(lotiri, baseURL);
	
	}

    public void prepareCSVLandlot(String lotiri, String baseUrl) {

        /* String lotsInfo = "PREFIX j1:<http://www.theworldavatar.com/ontology/ontoland/OntoLand.owl#> "
                + "PREFIX j2:<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#> "
                + "PREFIX j3:<http://www.theworldavatar.com/ontology/ontocape/upper_level/technical_system.owl#> "
                + "PREFIX j4:<http://www.theworldavatar.com/ontology/meta_model/topology/topology.owl#> "
                + "PREFIX j5:<http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/geometry/geometry.owl#> "
                + "PREFIX j6:<http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_behavior/behavior.owl#> "
                + "PREFIX j7:<http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#> "
                + "PREFIX j8:<http://www.theworldavatar.com/ontology/ontocape/material/phase_system/phase_system.owl#> "
                + "SELECT ?entity ?xvalue ?yvalue ?areavalue ?distancevalue "

                + "WHERE {?entity  a  j1:Landlot  ."
                + "?entity   j5:hasSurfaceGeometry ?sur ."
                + "?sur   j5:has_area ?surarea ."
                + "?surarea   j2:hasValue ?vsurarea ."
                + "?vsurarea   j2:numericalValue ?areavalue ."

                + "?entity   j7:hasGISCoordinateSystem ?coorsys ."
                + "?coorsys   j7:hasProjectedCoordinate_x ?x ."
                + "?x   j2:hasValue ?xval ."
                + "?xval   j2:numericalValue ?xvalue ."
                + "?coorsys   j7:hasProjectedCoordinate_y ?y ."
                + "?y   j2:hasValue ?yval ."
                + "?yval   j2:numericalValue ?yvalue ."

                + "?entity   j1:hasDistanceToClosestWaterSources ?distance ."
                + "?distance   j2:hasValue ?distval ."
                + "?distval   j2:numericalValue ?distancevalue ."

                + "}";*/
    	
    	String lotsInfo = "SELECT ?s WHERE { ?s ?p ?o . } ;";

    	System.out.println(lotsInfo);
//        QueryBroker broker = new QueryBroker();

//        String result = broker.queryFile(lotiri, lotsInfo);
//    	String[] keys = {"entity", "yvalue", "xvalue", "areavalue", "distancevalue"};
//        List<String[]> resultList = JenaResultSetFormatter.convertToListofStringArrays(result, keys);
        
//        String power_output = MatrixConverter.fromArraytoCsv(resultList);
//        System.out.println(power_output);

       // logger.info("number of queried lot entities = " + resultList.size());

       /* IriMapper mapper = new IriMapper();
        for (int i = 0; i < resultList.size(); i++) {
            String[] current = resultList.get(i);
            String id = "s" + (i + 1);
            mapper.add(current[0], id, "lot");
            current[0] = id;
        }
        */

    }
}