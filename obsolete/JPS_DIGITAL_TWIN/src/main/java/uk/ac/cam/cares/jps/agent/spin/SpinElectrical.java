package uk.ac.cam.cares.jps.agent.spin;

import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.util.List;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServletRequest;
import org.apache.jena.ontology.OntModel;
import org.apache.jena.ontology.OntModelSpec;
import org.apache.jena.rdf.model.Model;
import org.apache.jena.rdf.model.ModelFactory;
import org.json.JSONObject;
import org.topbraid.spin.inference.SPINInferences;
import org.topbraid.spin.system.SPINModuleRegistry;
import com.opencsv.CSVReader;
import uk.ac.cam.cares.jps.agent.matlab.JPSMatlabAgent;
import uk.ac.cam.cares.jps.base.agent.JPSAgent;
import uk.ac.cam.cares.jps.base.discovery.AgentCaller;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;

@WebServlet("/SpinElectrical")
public class SpinElectrical extends JPSAgent {
  private static final long serialVersionUID = 1L;
  public static final double MAX = 50.2;
  public static final double MIN = 49.8;
  public static int LINENUMBER = 0;
  static String ontologyURL =
      "http://www.theworldavatar.com/ontology/ontopowsys/OntoPowSysSPIN.ttl";

  /** To check the spin constraints. */
  public JSONObject processRequestParameters(JSONObject requestParams, HttpServletRequest request) {
    JSONObject jo = AgentCaller.readJsonParameter(request);
    String frequencyFilePath = null;
    String agentiri = JPSMatlabAgent.MATLAB_AGENT_URL;
    List<String> lst = null;
    Class c = SpinChemical.class;
    try {
		Object obj = c.newInstance();
		Method m = null;
		try {
			m = c.getDeclaredMethod("queryRDF4J", new Class[]{String.class,List.class});
		} catch (NoSuchMethodException e1) {
			throw new JPSRuntimeException(e1.getMessage());
		} catch (SecurityException e1) {
			throw new JPSRuntimeException(e1.getMessage());
		}
		m.setAccessible(true);
		try {
			m.invoke(obj, agentiri, lst);
		} catch (IllegalArgumentException e) {
			throw new JPSRuntimeException(e.getMessage());
		} catch (InvocationTargetException e) {
			throw new JPSRuntimeException(e.getMessage());
		}
	} catch (InstantiationException e2) {
		throw new JPSRuntimeException(e2.getMessage());
	} catch (IllegalAccessException e2) {
		throw new JPSRuntimeException(e2.getMessage());
	}  
    CSVReader reader = null;
    String str = requestParams.getString("inputfile");
    try {
      reader = new CSVReader(new FileReader(frequencyFilePath));
    } catch (FileNotFoundException e1) {
      throw new JPSRuntimeException(e1.getMessage());
    }
    try {
      reader.close();
    } catch (IOException e1) {
      throw new JPSRuntimeException(e1.getMessage());
    }
    String[] nextLine;
    double min = MIN;
    double max = MAX;
    try {
      while ((nextLine = reader.readNext()) != null) {
        LINENUMBER++;
        double cmp = 50;
        try {
          cmp = Double.parseDouble(nextLine[1]);
        } catch (NumberFormatException e) {
          throw new JPSRuntimeException(e.getMessage());
        }
        if (max < cmp) {
          max = cmp;
        }
        if (min > cmp) {
          min = cmp;
        }
      }
      reader.close();
    } catch (IOException e) {
      throw new JPSRuntimeException(e.getMessage());
    }
    System.out.println("Max = " + max + " , Min = " + min);
    System.out.println(str);
    /** Initialize SPIN system functions and templates. */
    SPINModuleRegistry.get().init();

    /** Create ontology Model with imports */
    OntModel ontModel =
        ModelFactory.createOntologyModel(
            OntModelSpec.OWL_MEM, new SpinElectrical().getBaseModel(ontologyURL));
    Model inferrenceTriples = new SpinElectrical().inferredTriples(ontModel);

    /** Lists inferred triples. */
    Object obj1 = null;
	try {
		obj1 = c.newInstance();
	} catch (InstantiationException e) {
		throw new JPSRuntimeException(e.getMessage());
	} catch (IllegalAccessException e) {
		throw new JPSRuntimeException(e.getMessage());
	}
	Method m1 = null;
	try {
		m1 = c.getDeclaredMethod("getInferredTriples", new Class[] {Model.class});
	} catch (NoSuchMethodException e) {
		throw new JPSRuntimeException(e.getMessage());
	} catch (SecurityException e) {
		throw new JPSRuntimeException(e.getMessage());
	} 
	m1.setAccessible(true);  
	try {
		m1.invoke(obj1,inferrenceTriples);
	} catch (IllegalAccessException e) {
		throw new JPSRuntimeException(e.getMessage());
	} catch (IllegalArgumentException e) {
		throw new JPSRuntimeException(e.getMessage());
	} catch (InvocationTargetException e) {
		throw new JPSRuntimeException(e.getMessage());
	} 

    /** Lists all constraint violations. */
    Object obj2 = null;
	try {
		obj2 = c.newInstance();
	} catch (InstantiationException e) {
		throw new JPSRuntimeException(e.getMessage());
	} catch (IllegalAccessException e) {
		throw new JPSRuntimeException(e.getMessage());
	}
	Method m2 = null;
	try {
		m2 = c.getDeclaredMethod("getAllConstraintViolations", new Class[] {OntModel.class});
	} catch (NoSuchMethodException e) {
		throw new JPSRuntimeException(e.getMessage());
	} catch (SecurityException e) {
		throw new JPSRuntimeException(e.getMessage());
	} 
	m2.setAccessible(true);  
	try {
		m2.invoke(obj2,ontModel);
	} catch (IllegalAccessException e) {
		throw new JPSRuntimeException(e.getMessage());
	} catch (IllegalArgumentException e) {
		throw new JPSRuntimeException(e.getMessage());
	} catch (InvocationTargetException e) {
		throw new JPSRuntimeException(e.getMessage());
	}
    return jo;
  }

  public Model getBaseModel(String ontologyURL) {
    /** Load ontology file to Jena model. */
    Model baseModel = ModelFactory.createDefaultModel();
    baseModel.read(ontologyURL);
    return baseModel;
  }

  public Model inferredTriples(OntModel ontologyModel) {
    /** Add a Model to inferred triples */
    Model inferredTriples = ModelFactory.createDefaultModel();
    ontologyModel.addSubModel(inferredTriples);

    /** Register locally defined functions */
    SPINModuleRegistry.get().registerAll((org.apache.jena.rdf.model.Model) ontologyModel, null);

    /** Run SPIN inference engine to infer new triples */
    SPINInferences.run(ontologyModel, inferredTriples, null, null, false, null);
    return inferredTriples;
  }
}
