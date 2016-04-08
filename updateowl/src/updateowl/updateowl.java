package updateowl;
import java.io.FileInputStream;
import java.io.InputStreamReader;
import java.io.Reader;
import java.net.URI;

//import org.semanticweb.owl.model.OWLObjectProperty;


import edu.stanford.smi.protegex.owl.ProtegeOWL;
import edu.stanford.smi.protegex.owl.jena.JenaOWLModel;
import edu.stanford.smi.protegex.owl.model.OWLDatatypeProperty;
import edu.stanford.smi.protegex.owl.model.OWLIndividual;
import edu.stanford.smi.protegex.owl.model.OWLObjectProperty;
import edu.stanford.smi.protegex.owl.model.OWLNamedClass;
import edu.stanford.smi.protegex.owl.model.RDFIndividual;
import edu.stanford.smi.protegex.owl.model.RDFResource;

public class updateowl {



public static void main(String[] args) throws Exception {
   // TODO Auto-generated method stub

   //get model from an owl file
   String filePath = "D:\\OntoCAPE/BiodieselPlant.owl";
   FileInputStream inFile= new FileInputStream(filePath);
   Reader in = new InputStreamReader(inFile,"UTF-8");
   JenaOWLModel jenaOwlModel = ProtegeOWL.createJenaOWLModelFromReader(in);
   
   //get an class from the model
   RDFResource oneClass=jenaOwlModel.getRDFResource("http://www.jparksimulator.com/BiodieselPlant.owl#Material");
   
   RDFResource oneDataProperty=jenaOwlModel.getRDFResource("http://www.jparksimulator.com/BiodieselPlant.owl#hasTrayNumber");
   
   //delete the resource
   oneClass.delete();
   oneDataProperty.delete();
   
   //make new class
   
   OWLNamedClass personClass = jenaOwlModel.createOWLNamedClass("http://www.jparksimulator.com/BiodieselPlant.owl#person");

   // Create subclass (complicated version)
   OWLNamedClass brotherClass = jenaOwlModel.createOWLNamedClass("http://www.jparksimulator.com/BiodieselPlant.owl#brother");
   brotherClass.addSuperclass(personClass);
   brotherClass.removeSuperclass(jenaOwlModel.getOWLThingClass());
   
   //set data property
   OWLDatatypeProperty ageProperty = jenaOwlModel.createOWLDatatypeProperty("http://www.jparksimulator.com/BiodieselPlant.owl#hasAge");
   ageProperty.setRange(jenaOwlModel.getXSDint());
   ageProperty.setDomain(personClass);

 //set object property
   OWLObjectProperty childrenProperty = jenaOwlModel.createOWLObjectProperty("http://www.jparksimulator.com/BiodieselPlant.owl#hasChildren");
   childrenProperty.setRange(personClass);
   childrenProperty.setDomain(personClass);
   
 //set individual
   RDFIndividual darwin = personClass.createRDFIndividual("http://www.jparksimulator.com/BiodieselPlant.owl#Darwin");
   darwin.setPropertyValue(ageProperty, new Integer(0));
   
   RDFIndividual holgi = personClass.createRDFIndividual("http://www.jparksimulator.com/BiodieselPlant.owl#Holger");
   holgi.setPropertyValue(childrenProperty, darwin);
   holgi.setPropertyValue(ageProperty, new Integer(33));
   
   
//update the value of an individual
   
   OWLIndividual pump = jenaOwlModel.getOWLIndividual("http://www.jparksimulator.com/BiodieselPlant.owl#pump_1");
   OWLDatatypeProperty hasName = jenaOwlModel.getOWLDatatypeProperty("http://www.jparksimulator.com/BiodieselPlant.owl#hasName");
   pump.setPropertyValue(hasName, new String("10P05"));
   
   //save the model to another owl file
   URI file=URI.create("file:///D:/OntoCAPE/BiodieselPlantchange2.owl");
   
   jenaOwlModel.save(file);
   System.out.println(file);
   //System.out.println(oneClass);

}

}