package uk.ac.cam.cares.jps.misc.powerplants.obda;

import static java.util.stream.Collectors.joining;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import org.semanticweb.owlapi.io.ToStringRenderer;
import org.semanticweb.owlapi.model.OWLException;
import org.semanticweb.owlapi.model.OWLObject;

import it.unibz.inf.ontop.injection.OntopSQLOWLAPIConfiguration;
import it.unibz.inf.ontop.owlapi.OntopOWLFactory;
import it.unibz.inf.ontop.owlapi.OntopOWLReasoner;
import it.unibz.inf.ontop.owlapi.connection.OntopOWLConnection;
import it.unibz.inf.ontop.owlapi.connection.OntopOWLStatement;
import it.unibz.inf.ontop.owlapi.resultset.OWLBinding;
import it.unibz.inf.ontop.owlapi.resultset.OWLBindingSet;
import it.unibz.inf.ontop.owlapi.resultset.TupleOWLResultSet;
import junit.framework.TestCase;
import uk.ac.cam.cares.jps.misc.powerplants.performance.PowerPlantQueries;

public class OntopOWLReasonerNativeMappingExample extends TestCase {

	private static final String RES = "C:/Users/Andreas/my/JPSWorkspace/JParkSimulator-git/JPS_MISC/res";
	private static final String owlFile = RES + "/powerplants.owl";
    private static final String obdaFile = RES + "/powerplants.obda";
    private static final String propertyFile = RES + "/powerplants.properties";
    
    public TupleOWLResultSet performQuery(String sparqlQuery) throws OWLException, IOException  {

        OntopOWLFactory factory = OntopOWLFactory.defaultFactory();
        
        File file = new File(obdaFile);
        File oFile = new File(owlFile);
        File propFile = new File(propertyFile);

        OntopSQLOWLAPIConfiguration config = (OntopSQLOWLAPIConfiguration) OntopSQLOWLAPIConfiguration.defaultBuilder()
                .nativeOntopMappingFile(file)
                .ontologyFile(oFile)
                .propertyFile(propFile)
                .enableTestMode()
                .build();

        OntopOWLReasoner reasoner = factory.createReasoner(config);
        
        System.out.println();
        System.out.println("The input SPARQL query:");
        System.out.println("=======================");
        System.out.println(sparqlQuery);
        System.out.println();
        long start = System.currentTimeMillis();

  
        OntopOWLConnection conn = reasoner.getConnection();
        OntopOWLStatement st = conn.createStatement();
        TupleOWLResultSet rs = st.executeSelectQuery(sparqlQuery);
 
        System.out.println(rs.getSignature().stream().collect(joining(",")));
        System.out.println("------------------------------------------------------------------------------------------");

       
        long queried = System.currentTimeMillis();    
        long diff = queried - start;
        System.out.println("queried - start = " + diff);
        System.out.println();

        // Only for debugging purpose, not for end users: this will redo the query reformulation, which can be expensive
        //final SQLExecutableQuery sqlExecutableQuery = (SQLExecutableQuery) st.getExecutableQuery(sparqlQuery);
        //String sqlQuery = sqlExecutableQuery.getSQL();

        //System.out.println("The output SQL query:");
        //System.out.println("=====================");
        //System.out.println(sqlQuery);
            
        return rs;
    }
    
	public void testQueryAllPowerPlants() throws OWLException, IOException {
		queryAllPowerplants();
	}
	
	public List<String> queryAllPowerplants() throws OWLException, IOException {
		
		List<String> plants = new ArrayList<String>();
		
		TupleOWLResultSet rs = performQuery(PowerPlantQueries.SPARQL_ALL_PLANTS);
    	
        int i=0;
        while (rs.hasNext()) {
        	i++;
        	// System.out.println("\nresult " + i);
            final OWLBindingSet bindingSet = rs.next();
            for (OWLBinding binding : bindingSet) {
                OWLObject value = binding.getValue();
                String plant = ToStringRenderer.getInstance().getRendering(value);
                plant = plant.substring(1, plant.length()-1);
                plants.add(plant);
                System.out.println(plant);
            }          
        }
    	
        rs.close();
        
		System.out.println("number of power plants = " + plants.size());
        
        return plants;
	}
	
	public void testqueryEmissions() throws OWLException, IOException {		
		String iri = "http://www.theworldavatar.com/kb/powerplants/powerplant_1001";
		iri = "http://www.theworldavatar.com/kb/powerplants/powerplant_532";
		queryEmission(iri);
	}
	
	public double queryEmission(String iri) throws OWLException, IOException {

		double result = 0.0;
		
		String query = String.format(PowerPlantQueries.SPARQL_PLANT, iri, iri);

		TupleOWLResultSet rs = performQuery(query);
 
        while (rs.hasNext()) {
            final OWLBindingSet bindingSet = rs.next();
            for (OWLBinding binding : bindingSet) {
                OWLObject value = binding.getValue();
                String renderedValue = ToStringRenderer.getInstance().getRendering(value);     
                int j = renderedValue.indexOf("^^xsd:double");
                if (j>=0) {
                	result = Double.valueOf(renderedValue.substring(1, j-1));
                }
                System.out.println("result: " + result + ", renderedValue" + renderedValue);
            }          
        }
    	
        rs.close();
        
        return result;
	}
		
	public void testLoopOnPlants() throws OWLException, IOException {
		
		int numberPlants = 10;
		
		List<String> plants = queryAllPowerplants();

		long start = System.currentTimeMillis();
		
		double sum = 0;
		int i = 0;
		for (String current : plants) {
			i++;
			System.out.println("my plant " + i + ": " + current);

			double queried = queryEmission(current);
			sum += queried;

			System.out.println("my emission " + i + ": " + queried);
			
			if (i == numberPlants) {
				break;
			}
		}
		
		long stop = System.currentTimeMillis();

		System.out.println("elapsed time in milli = " + (stop - start));
		System.out.println("number of queried or updated plants = " + i);
		System.out.println("sum of queried capacity (after possible update) = " + sum);
	}
}