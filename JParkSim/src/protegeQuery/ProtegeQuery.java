package protegeQuery;

import java.util.logging.Level;
import java.util.logging.Logger;

import edu.stanford.smi.protege.exception.OntologyLoadException;
import edu.stanford.smi.protegex.owl.ProtegeOWL;
import edu.stanford.smi.protegex.owl.model.OWLIndividual;
import edu.stanford.smi.protegex.owl.model.OWLModel;

public class ProtegeQuery {
	static double x;
			
	public static void main(final String[] args){
		
		
		try{			
			String uri = "File:/D:/OntoCAPE/BiodieselPlant.owl";
	        OWLModel owlModel = ProtegeOWL.createJenaOWLModelFromURI(uri);
	        
	        OWLIndividual x1 = owlModel.getOWLIndividual("http://www.jparksimulator.com/BiodieselPlant.owl#F_OIL_V");                       
            String x1value = x1.getPropertyValueLiteral(owlModel.getOWLProperty("system:numericalValue")).getString();             
            x = Double.valueOf(x1value)/807.3;              //molar flowrate	

            System.out.println("x=" +x);
	}catch (OntologyLoadException ex) {
	    Logger.getLogger(ProtegeQuery.class.getName()).log(Level.SEVERE, null, ex); 
	    System.out.println(ex);
	 }
	}
}
	
	
	
