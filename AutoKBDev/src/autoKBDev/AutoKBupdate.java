package autoKBDev;

import java.io.FileInputStream;
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

	public static void main(String[] args) throws Exception {
		   // TODO Auto-generated method stub

		   //get model from an owl file
		   String filePath = "D:\\TheWorldSimulator/Reactor.owl";
		   FileInputStream inFile= new FileInputStream(filePath);
		   Reader in = new InputStreamReader(inFile,"UTF-8");
		   JenaOWLModel jenaOwlModel = ProtegeOWL.createJenaOWLModelFromReader(in);
		   
		   //get an NamedClass from the model
		   OWLNamedClass StirredTank=jenaOwlModel.getOWLNamedClass("file:/C:/OntoCAPE/OntoCAPE/chemical_process_system/CPS_realization/plant_equipment/apparatus.owl#StirredTank");
		   OWLNamedClass SystemFunction=jenaOwlModel.getOWLNamedClass("file:/C:/OntoCAPE/OntoCAPE/upper_level/technical_system.owl#SystemFunction");
		   OWLNamedClass Temperature=jenaOwlModel.getOWLNamedClass("file:/C:/OntoCAPE/OntoCAPE/material/phase_system/phase_system.owl#Temperature");
		   OWLNamedClass ScalarValue=jenaOwlModel.getOWLNamedClass("file:/C:/OntoCAPE/OntoCAPE/upper_level/system.owl#ScalarValue");
		   
		   //get named individual
		   RDFIndividual Celsius = jenaOwlModel.getOWLIndividual("file:/C:/OntoCAPE/OntoCAPE/supporting_concepts/SI_unit/derived_SI_units.owl#Celsius");
		   //get the temporal coordinate
		   RDFIndividual CoordinatedUT = jenaOwlModel.getOWLIndividual("file:/C:/OntoCAPE/OntoCAPE/supporting_concepts/space_and_time/space_and_time.owl#CoordinatedUniversalTime");
		   
		   //set object property
		   OWLObjectProperty hasInsideDiameter = jenaOwlModel.getOWLObjectProperty("file:/C:/OntoCAPE/OntoCAPE/chemical_process_system/CPS_realization/plant.owl#hasInsideDiameter");
		   OWLObjectProperty realizes = jenaOwlModel.getOWLObjectProperty("file:/C:/OntoCAPE/OntoCAPE/upper_level/technical_system.owl#realizes");		 
		   OWLObjectProperty hasProperty = jenaOwlModel.getOWLObjectProperty("file:/C:/OntoCAPE/OntoCAPE/upper_level/system.owl#hasProperty");
		   OWLObjectProperty hasValue = jenaOwlModel.getOWLObjectProperty("file:/C:/OntoCAPE/OntoCAPE/upper_level/system.owl#hasValue");
		   OWLObjectProperty  hasUnitOfMeasure= jenaOwlModel.getOWLObjectProperty("file:/C:/OntoCAPE/OntoCAPE/upper_level/system.owl#hasUnitOfMeasure");
		   OWLObjectProperty  isObservedAgainstBackdrop= jenaOwlModel.getOWLObjectProperty("file:/C:/OntoCAPE/OntoCAPE/upper_level/system.owl#isObservedAgainstBackdrop");
		   
		   OWLDatatypeProperty numericalValue = jenaOwlModel.getOWLDatatypeProperty("file:/C:/OntoCAPE/OntoCAPE/upper_level/system.owl#numericalValue");
		   
/**		   
		 //set individual
		   RDFIndividual Esterification = SystemFunction.createRDFIndividual("file:/C:/OntoCAPE/OntoCAPE/upper_level/technical_system.owl#Esterification");
		   RDFIndividual Temp = Temperature.createRDFIndividual("file:/C:/OntoCAPE/OntoCAPE/material/phase_system/phase_system.owl#Temp");
		   RDFIndividual v_Temp = ScalarValue.createRDFIndividual("file:/C:/OntoCAPE/OntoCAPE/upper_level/system.owl#v_Temp");
		   
		   		   
		 //set individual
		   RDFIndividual Tank_A = StirredTank.createRDFIndividual("http://www.jparksimulator.com/BiodieselPlant.owl#Tank_A");
		   Tank_A.setPropertyValue(hasInsideDiameter, new Float(0.8));
		   Tank_A.setPropertyValue(realizes, Esterification);
		   Tank_A.setPropertyValue(hasProperty, Temp);
		   Temp.setPropertyValue(hasValue, v_Temp);
		   v_Temp.setPropertyValue(numericalValue, new Float(50.0));
		   v_Temp.setPropertyValue(hasUnitOfMeasure, Celsius);
*/		   
		   
		   //save the model to another owl file
		   URI file=URI.create("file:///D:/TheWorldSimulator/Reactor_test__.owl");
		   
		   jenaOwlModel.save(file);
		   System.out.println(file);

		}
}
