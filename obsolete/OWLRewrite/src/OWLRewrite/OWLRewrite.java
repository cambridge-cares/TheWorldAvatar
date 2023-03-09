package OWLRewrite;

import java.io.BufferedReader;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.FileReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.io.Reader;
import java.net.URI;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import com.hp.hpl.jena.iri.IRI;
import com.hp.hpl.jena.util.FileUtils;

import edu.stanford.smi.protegex.owl.ProtegeOWL;
import edu.stanford.smi.protegex.owl.jena.JenaOWLModel;
import edu.stanford.smi.protegex.owl.model.OWLDatatypeProperty;
import edu.stanford.smi.protegex.owl.model.OWLIndividual;
import edu.stanford.smi.protegex.owl.model.OWLModel;
import edu.stanford.smi.protegex.owl.model.OWLNamedClass;
import edu.stanford.smi.protegex.owl.model.OWLObjectProperty;
import edu.stanford.smi.protegex.owl.model.RDFIndividual;

public class OWLRewrite {
	static String [] data;
	 static String [] data2;
	 public static ArrayList<String> list2 = new ArrayList<String>();
	
	public static void main(String[] args) throws Exception {
		
		String csvFile = "D:\\Landlots.csv";
        String line = "";
        String cvsSplitBy = ",";

        
        
        try (BufferedReader br = new BufferedReader(new FileReader(csvFile))) {
        	int i=0;
            while ((line = br.readLine()) != null) {
            	
                // use comma as separator
                String[] data = line.split(cvsSplitBy);

                //System.out.println("CO2 landlot1=  " + data[38]);
                
                list2.add(data[38]);
               // data2[i]=data[38];
                
                 i++ ;
            }

        } catch (IOException e) {
            e.printStackTrace();
        }
		
        //System.out.println("The list is " + list2);
        
        
        /*System.out.println("Item no 5" + list2.get(5));
        
        for(String item : list2)
        {
        	System.out.println("Item -- " + item);
        }*/
        
        
		   // TODO Auto-generated method stub
for (int a=1; a<=219;a++){
	String i=String.format("%03d", a); //function to make 1 become 001 and 54 become 054
		   //get model from an owl file
		   String baseURL = "D:\\johnelectricalontology/fromserver/";
		   String filePath = baseURL + "MGDL-"+i+".owl";
		   FileInputStream inFile= new FileInputStream(filePath);
		   Reader in = new InputStreamReader(inFile,"UTF-8");
		   JenaOWLModel jenaOwlModel = ProtegeOWL.createJenaOWLModelFromReader(in);
		   
		   //String uri = "http://www.jparksimulator.com/BiodieselPlant3.owl";
		    //OWLModel owlModel = ProtegeOWL.createJenaOWLModelFromURI(uri);
		   
		   
		 /* String[] devicegroup={"E-401","E-402","E-404","E-403","E-405","R-401","M-401","M-402","P-401","P-402",
				  "V-401","X-401"};*/
		  
		  

		  
		   OWLObjectProperty hasaverageactivepower = jenaOwlModel.getOWLObjectProperty("http://www.theworldavatar.com/OntoElectricalNet/EN_behavior.owl#hasAverageActivePower");
		   OWLObjectProperty hasaveragereactivepower = jenaOwlModel.getOWLObjectProperty("http://www.theworldavatar.com/OntoElectricalNet/EN_behavior.owl#hasAverageReactivePower");
		   OWLObjectProperty hasapparentactivepower = jenaOwlModel.getOWLObjectProperty("http://www.theworldavatar.com/OntoElectricalNet/EN_behavior.owl#hasAverageApparentPower");
		   OWLObjectProperty hasProjectedCoordinate_y = jenaOwlModel.getOWLObjectProperty("file:/C:/OntoCAPE/OntoCAPE/supporting_concepts/space_and_time/space_and_time_extended.owl#hasProjectedCoordinate_y");
 		OWLObjectProperty hasvalue = jenaOwlModel.getOWLObjectProperty("http://www.theworldavatar.com/OntoCAPE/OntoCAPE/upper_level/system.owl#hasValue");
 		OWLObjectProperty isrelatedto = jenaOwlModel.getOWLObjectProperty("file:/C:/OntoCAPE/OntoCAPE/upper_level/system.owl#isRelatedTo");
 		OWLObjectProperty hasUnitOfMeasure = jenaOwlModel.getOWLObjectProperty("http://www.theworldavatar.com/OntoCAPE/OntoCAPE/upper_level/system.owl#hasUnitOfMeasure");
 		   OWLNamedClass pavgClass = jenaOwlModel.getOWLNamedClass("http://www.theworldavatar.com/OntoElectricalNet/EN_behavior.owl#AverageActivePower");
 		  OWLNamedClass qavgClass = jenaOwlModel.getOWLNamedClass("http://www.theworldavatar.com/OntoElectricalNet/EN_behavior.owl#AverageReactivePower");
 		 OWLNamedClass savgClass = jenaOwlModel.getOWLNamedClass("http://www.theworldavatar.com/OntoElectricalNet/EN_behavior.owl#AverageApparentPower");
 		   OWLNamedClass scalarvalue = jenaOwlModel.getOWLNamedClass("http://www.theworldavatar.com/OntoCAPE/OntoCAPE/upper_level/system.owl#ScalarValue");
		   
 		  OWLDatatypeProperty numericalvalue = jenaOwlModel.getOWLDatatypeProperty("http://www.theworldavatar.com/OntoCAPE/OntoCAPE/upper_level/system.owl#numericalValue");
 		 OWLIndividual MW = jenaOwlModel.getOWLIndividual("http://www.theworldavatar.com/OntoCAPE/OntoCAPE/supporting_concepts/SI_unit/derived_SI_units.owl#MW");
 		OWLIndividual MVar = jenaOwlModel.getOWLIndividual("http://www.theworldavatar.com/OntoCAPE/OntoCAPE/supporting_concepts/SI_unit/derived_SI_units.owl#Mvar");
 		OWLIndividual MVA = jenaOwlModel.getOWLIndividual("http://www.theworldavatar.com/OntoCAPE/OntoCAPE/supporting_concepts/SI_unit/derived_SI_units.owl#MVA");
		  
 		 /* OWLIndividual pump = jenaOwlModel.getOWLIndividual("http://www.jparksimulator.com/BiodieselPlant.owl#Pump_P101");
		   OWLIndividual pump_x = jenaOwlModel.getOWLIndividual("http://www.jparksimulator.com/BiodieselPlant.owl#x_P101");
		 pump.removePropertyValue(hasProperty, pump_x);*/
		   
		  // for (int x=0;x<465;x++)
	//{
		   
	//OWLIndividual devices = jenaOwlModel.getOWLIndividual("https://jparksimulator.com/ZeonPlant.owl#"+devicegroup[x]);
			   OWLIndividual ploss = jenaOwlModel.getOWLIndividual("http://www.theworldavatar.com/MGDL-"+i+".owl#V_Ploss_MGDL-"+i);	   
			   OWLIndividual qloss = jenaOwlModel.getOWLIndividual("http://www.theworldavatar.com/MGDL-"+i+".owl#V_Qloss_MGDL-"+i);
			   qloss.setPropertyValue(numericalvalue, new Float(0));
			   ploss.setPropertyValue(numericalvalue, new Float(0));
			   
			   OWLIndividual mgdl = jenaOwlModel.getOWLIndividual("http://www.theworldavatar.com/MGDL-"+i+".owl#MGDL-"+i);
			   
			   RDFIndividual Pavg = pavgClass.createRDFIndividual("http://www.theworldavatar.com/MGDL-"+i+".owl#Pavg_MGDL-"+i);
			   RDFIndividual Qavg = qavgClass.createRDFIndividual("http://www.theworldavatar.com/MGDL-"+i+".owl#Qavg_MGDL-"+i);
			   RDFIndividual Savg = savgClass.createRDFIndividual("http://www.theworldavatar.com/MGDL-"+i+".owl#Savg_MGDL-"+i);
			   RDFIndividual vPavg = scalarvalue.createRDFIndividual("http://www.theworldavatar.com/MGDL-"+i+".owl#V_Pavg_MGDL-"+i);
			   RDFIndividual vQavg = scalarvalue.createRDFIndividual("http://www.theworldavatar.com/MGDL-"+i+".owl#V_Qavg_MGDL-"+i);
			   RDFIndividual vSavg = scalarvalue.createRDFIndividual("http://www.theworldavatar.com/MGDL-"+i+".owl#V_Savg_MGDL-"+i);
				
			   mgdl.addPropertyValue(hasaverageactivepower, Pavg);
			   mgdl.addPropertyValue(hasaveragereactivepower, Qavg);
			   mgdl.addPropertyValue(hasapparentactivepower,Savg);
			   
			   Pavg.addPropertyValue(hasvalue, vPavg);
			   Qavg.addPropertyValue(hasvalue, vQavg);
			   Savg.addPropertyValue(hasvalue,vSavg);
			   
			   vPavg.setPropertyValue(numericalvalue, new Float(0));
			   vQavg.setPropertyValue(numericalvalue, new Float(0));
			   vSavg.setPropertyValue(numericalvalue, new Float(0));
			   
			   vPavg.setPropertyValue(hasUnitOfMeasure, MW);
			   vQavg.setPropertyValue(hasUnitOfMeasure, MVar);
			   vSavg.setPropertyValue(hasUnitOfMeasure, MVA);
	//OWLIndividual devices_x = jenaOwlModel.getOWLIndividual("https://jparksimulator.com/ZeonPlant.owl#x_"+devicegroup[x]);
	//OWLIndividual devices_y = jenaOwlModel.getOWLIndividual("https://jparksimulator.com/ZeonPlant.owl#y_"+devicegroup[x]);
 
	//OWLIndividual devices_xvalue = jenaOwlModel.getOWLIndividual("http://www.jparksimulator.com/AirSeparationPlant.owl#ValueOf_x_"+devicegroup[x]);
	//OWLIndividual devices_yvalue = jenaOwlModel.getOWLIndividual("http://www.jparksimulator.com/AirSeparationPlant.owl#ValueOf_y_"+devicegroup[x]);
	 
	//OWLIndividual devices_coordinate = jenaOwlModel.getOWLIndividual("https://jparksimulator.com/ZeonPlant.owl#"+devicegroup[x]+"_Coordinates");
	//task to do
	
	//RDFIndividual device_coordinate = giscoordinatesysClass.createRDFIndividual("http://www.jparksimulator.com/AirSeparationPlant.owl#CoordinateSystemOf"+devicegroup[x]);
	
	
	//devices.removePropertyValue(isrelatedto, devices_coordinate);
	
	
	
	
	/*device_coordinate.setPropertyValue(hasProjectedCoordinate_y, devices_y);
	device_coordinate.setPropertyValue(hasProjectedCoordinate_x, devices_x);
		
	
	devices_x.removePropertyValue(hasvalue, devices_yvalue);
	devices_y.removePropertyValue(hasvalue, devices_xvalue);
	
	devices_x.setPropertyValue(hasvalue, devices_xvalue);
	devices_y.setPropertyValue(hasvalue, devices_yvalue);*/
	
	/*devices_xvalue.setPropertyValue(numericalvalue, new Float(34.2));
	devices_yvalue.setPropertyValue(numericalvalue, new Float(34.2));*/
	
	
		//   URI file=URI.create("file:///D:/OntoCAPE/BiodieselPlantchange2.owl");
		   
 		 		  
 		 /**save the updated model file*/
 			Collection errors = new ArrayList();
 			jenaOwlModel.save(new URI("file:////" + filePath.replace("\\", "/")), FileUtils.langXMLAbbrev, errors, jenaOwlModel.getOntModel());
 			System.out.println("File saved with " + errors.size() + " errors."); 
	}  
 		 

		   
		  
	}
	   
}
