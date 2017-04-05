/** this package extracts information from a CSV file and then write it into an .owl file*/
package autoKBDev;

import java.io.BufferedReader;
import java.io.FileInputStream;
import java.io.FileReader;
import java.io.InputStreamReader;
import java.io.Reader;
import java.net.URI;

import edu.stanford.smi.protegex.owl.ProtegeOWL;
import edu.stanford.smi.protegex.owl.jena.JenaOWLModel;
import edu.stanford.smi.protegex.owl.model.OWLDatatypeProperty;
import edu.stanford.smi.protegex.owl.model.OWLNamedClass;
import edu.stanford.smi.protegex.owl.model.OWLObjectProperty;
import edu.stanford.smi.protegex.owl.model.RDFIndividual;

public class AutoKBupdate {
	public static String Reactor = new String("D:/TheWorldSimulator/Reactor_example.CSV"); //  CSV file that contains information 

	public static void main(String[] args) throws Exception {

		   /**get model from an owl file*/
		   String filePath = "D:\\TheWorldSimulator/Reactor.owl";
		   FileInputStream inFile= new FileInputStream(filePath);
		   Reader in = new InputStreamReader(inFile,"UTF-8");
		   JenaOWLModel jenaOwlModel = ProtegeOWL.createJenaOWLModelFromReader(in);
		   
		   /**get an NamedClass from the model*/
		   OWLNamedClass StirredTank = jenaOwlModel.getOWLNamedClass("file:/C:/OntoCAPE/OntoCAPE/chemical_process_system/CPS_realization/plant_equipment/apparatus.owl#StirredTank");
		   OWLNamedClass SystemFunction = jenaOwlModel.getOWLNamedClass("file:/C:/OntoCAPE/OntoCAPE/upper_level/technical_system.owl#SystemFunction");
		   OWLNamedClass Temperature = jenaOwlModel.getOWLNamedClass("file:/C:/OntoCAPE/OntoCAPE/material/phase_system/phase_system.owl#Temperature");
		   OWLNamedClass ScalarValue = jenaOwlModel.getOWLNamedClass("file:/C:/OntoCAPE/OntoCAPE/upper_level/system.owl#ScalarValue");
		   OWLNamedClass CoordinateValue = jenaOwlModel.getOWLNamedClass("file:/C:/OntoCAPE/OntoCAPE/upper_level/coordinate_system.owl#CoordinateValue");
		   
		   /**get Unit - Celsius from certain partial model*/
//		   RDFIndividual Celsius = jenaOwlModel.getOWLIndividual("file:/C:/OntoCAPE/OntoCAPE/supporting_concepts/SI_unit/derived_SI_units.owl#Celsius");
		   
		   /**get named individual - the temporal coordinate from partial model space_and_time.owl*/
		   RDFIndividual CoordinatedUT = jenaOwlModel.getOWLIndividual("file:/C:/OntoCAPE/OntoCAPE/supporting_concepts/space_and_time/space_and_time.owl#CoordinatedUniversalTime");
		   
		   /**get corresponding object properties that are to be used to associate the relevant information*/
		   OWLObjectProperty hasInsideDiameter = jenaOwlModel.getOWLObjectProperty("file:/C:/OntoCAPE/OntoCAPE/chemical_process_system/CPS_realization/plant.owl#hasInsideDiameter");
		   OWLObjectProperty realizes = jenaOwlModel.getOWLObjectProperty("file:/C:/OntoCAPE/OntoCAPE/upper_level/technical_system.owl#realizes");		 
		   OWLObjectProperty hasProperty = jenaOwlModel.getOWLObjectProperty("file:/C:/OntoCAPE/OntoCAPE/upper_level/system.owl#hasProperty");
		   OWLObjectProperty hasValue = jenaOwlModel.getOWLObjectProperty("file:/C:/OntoCAPE/OntoCAPE/upper_level/system.owl#hasValue");
		   OWLObjectProperty hasUnitOfMeasure = jenaOwlModel.getOWLObjectProperty("file:/C:/OntoCAPE/OntoCAPE/upper_level/system.owl#hasUnitOfMeasure");
		   OWLObjectProperty isObservedAgainstBackdrop = jenaOwlModel.getOWLObjectProperty("file:/C:/OntoCAPE/OntoCAPE/upper_level/system.owl#isObservedAgainstBackdrop");
		   
		   /**get corresponding data properties that are to be used to associate the relevant information*/
		   OWLDatatypeProperty numericalValue = jenaOwlModel.getOWLDatatypeProperty("file:/C:/OntoCAPE/OntoCAPE/upper_level/system.owl#numericalValue");
		   		   		   		   
		   /**create individuals and define their relationship*/
		   RDFIndividual Tank_A = StirredTank.createRDFIndividual("http://www.jparksimulator.com/BiodieselPlant.owl#Tank_A");
		   RDFIndividual Esterification = SystemFunction.createRDFIndividual("file:/C:/OntoCAPE/OntoCAPE/upper_level/technical_system.owl#Esterification");
		   RDFIndividual Temp = Temperature.createRDFIndividual("file:/C:/OntoCAPE/OntoCAPE/material/phase_system/phase_system.owl#Temp");	
		   
		   Tank_A.setPropertyValue(hasInsideDiameter, new Float(0.8));
		   Tank_A.setPropertyValue(realizes, Esterification);
		   Tank_A.setPropertyValue(hasProperty, Temp);
		   
		   /** start a loop to read temperature data from a CSV/XML file and write into a new owl file*/
		   BufferedReader fileReader = null;
		   String line = null;
		   fileReader = new BufferedReader(new FileReader(Reactor));   //input CSV file
		   fileReader.readLine();       // Read the CSV flie header to skip it
		   
		   int i = 1;
		   while ((line = fileReader.readLine()) != null) {			   
				String[] data = line.split(",");
				System.out.println("data= " + data);
  
				/**create instances for temperature values and temporal coordinate*/
				RDFIndividual v_Temp = ScalarValue.createRDFIndividual("file:/C:/OntoCAPE/OntoCAPE/upper_level/system.owl#v_Temp_" + i);
				RDFIndividual t = CoordinateValue.createRDFIndividual("file:/C:/OntoCAPE/OntoCAPE/upper_level/coordinate_system.owl#t_" + i);
				
				Temp.addPropertyValue(hasValue, v_Temp);	
				v_Temp.setPropertyValue(numericalValue, new Float(data[0]));
				v_Temp.setPropertyValue(hasUnitOfMeasure, data[1]);
                CoordinatedUT.setPropertyValue(hasValue, t);
				t.setPropertyValue(numericalValue, data[2]);
				   
				v_Temp.setPropertyValue(isObservedAgainstBackdrop, t);
				i++;
			}
		    fileReader.close();
/**		   
		   Temp.setPropertyValue(hasValue, v_Temp);		   
		   v_Temp.setPropertyValue(numericalValue, new Float(50.0));
		   v_Temp.setPropertyValue(hasUnitOfMeasure, Celsius);
		   
		   CoordinatedUT.setPropertyValue(hasValue, t);
		   t.setPropertyValue(numericalValue, "05/04/2017");
		   
		   v_Temp.setPropertyValue(isObservedAgainstBackdrop, t);
*/		   
		   /**save the model to another owl file*/
		   URI file=URI.create("file:///D:/TheWorldSimulator/Reactor_test.owl");
		   
		   jenaOwlModel.save(file);
		   System.out.println(file);

		}
}
