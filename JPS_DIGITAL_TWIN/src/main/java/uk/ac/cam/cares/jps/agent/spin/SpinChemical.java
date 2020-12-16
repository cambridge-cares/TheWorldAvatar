package uk.ac.cam.cares.jps.agent.spin;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.util.List;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServletRequest;
import org.apache.jena.ontology.OntModel;
import org.apache.jena.ontology.OntModelSpec;
import org.apache.jena.rdf.model.Model;
import org.apache.jena.rdf.model.ModelFactory;
import org.apache.jena.rdf.model.Resource;
import org.apache.jena.rdf.model.Statement;
import org.apache.jena.rdf.model.StmtIterator;
import org.json.JSONObject;
import org.topbraid.spin.constraints.ConstraintViolation;
import org.topbraid.spin.constraints.SPINConstraints;
import org.topbraid.spin.inference.SPINInferences;
import org.topbraid.spin.system.SPINLabels;
import org.topbraid.spin.system.SPINModuleRegistry;
import com.opencsv.CSVReader;

import uk.ac.cam.cares.jps.agent.gPROMS.gPROMSAgent;
import uk.ac.cam.cares.jps.base.agent.JPSAgent;
import uk.ac.cam.cares.jps.base.annotate.MetaDataQuery;
import uk.ac.cam.cares.jps.base.discovery.AgentCaller;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.query.JenaResultSetFormatter;

@WebServlet("/SpinChemical")
public class SpinChemical extends JPSAgent {
  private static final long serialVersionUID = 1L;
  public static final double MAX = 1.0;
  public static final double MIN = 0.973;
  static String ontologyURL =
      "http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_function/processSPIN.ttl";

  public JSONObject processRequestParameters(JSONObject requestParams, HttpServletRequest request) {
    JSONObject jo = AgentCaller.readJsonParameter(request);
    String current = System.getProperty("user.home");
    System.out.println("\nThis is Spin agent:" + current);
    String moleFractionFilePath = null;
    String agentiri = gPROMSAgent.GPROMS_AGENT_URL ;
    String str= requestParams.getString("inputFile");
    List<String> lst = null;
    SpinChemical iri = new SpinChemical();
    moleFractionFilePath = iri.queryRDF4J(agentiri, lst);
    System.out.println("\nThis is moleFractionFilePath: " + moleFractionFilePath);
    CSVReader reader = null;
    try {
      reader = new CSVReader(new FileReader(moleFractionFilePath));
    } catch (FileNotFoundException e1) {
      throw new JPSRuntimeException(e1.getMessage());
    }
    String[] nextLine;
    int lineNumber = 0;
    double min = MIN;
    double max = MAX;
    try {
      while ((nextLine = reader.readNext()) != null) {
        lineNumber++;
        double cmp = .975;
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
    /**
     * Initialize SPIN system functions and templates.
     */
    SPINModuleRegistry.get().init();

    /**
     * Create ontology Model with imports
     */
    OntModel ontModel = ModelFactory.createOntologyModel(OntModelSpec.OWL_MEM,
        new SpinChemical().getBaseModel(ontologyURL));

    Model inferrenceTriples = new SpinChemical().inferredTriples(ontModel);
    /**
     * Lists inferred triples.
     */
    new SpinChemical().getInferredTriples(inferrenceTriples);

    /**
     * Lists all constraint violations.
     */
    new SpinChemical().getAllConstraintViolations(ontModel);
    return jo;
  }

  /**
   * Query RDF4J for chemical system output IRI.
   */
  public String queryRDF4J(String agentiri, List<String> lst) {
    String csvFilePath = null;
    String resultFromRDF4J =
        MetaDataQuery.queryResources(null, null, null, agentiri, null, null, null, lst);
    String[] keys = JenaResultSetFormatter.getKeys(resultFromRDF4J);
    List<String[]> listmap =
        JenaResultSetFormatter.convertToListofStringArrays(resultFromRDF4J, keys);
    for (String[] str : listmap) {
      for (String s : str) {
        if (isFile(s)) {
          csvFilePath = s;
          System.out.println("\nThis is csvFilePath: " + csvFilePath);
          break;
        }
      }
      break;
    }
    return (csvFilePath);
  }

  /**
   * Validate file path.
   */
  private boolean isFile(String path) {
    if (path == null) {
      return true;
    }
    return new File(path).isFile();
  }

  public Model getBaseModel(String ontologyURL) {

    /**
     * Load ontology file to Jena model.
     */
    Model baseModel = ModelFactory.createDefaultModel();
    baseModel.read(ontologyURL);
    return baseModel;
  }

  public Model inferredTriples(OntModel ontologyModel) {
    /**
     * Add a Model to inferred triples
     */
    Model inferredTriples = ModelFactory.createDefaultModel();
    ontologyModel.addSubModel(inferredTriples);

    /**
     * Register locally defined functions
     */
    SPINModuleRegistry.get().registerAll((org.apache.jena.rdf.model.Model) ontologyModel, null);

    /**
     * Run SPIN inference engine to infer new triples
     */
    SPINInferences.run(ontologyModel, inferredTriples, null, null, false, null);
    return inferredTriples;

  }

  /**
   * 
   * @param inferrenceTriples model that represents inferred triples.
   */
  public void getInferredTriples(Model inferrenceTriples) {

    System.out.println("Inferred triples size: " + inferrenceTriples.size());
    System.out.println("Prints inferred triples: ");
    StmtIterator stmtIterator = inferrenceTriples.listStatements();
    int i = 1;
    while (stmtIterator.hasNext()) {

      Statement statement = stmtIterator.next();

      System.out.println(i + ". ( " + statement.getSubject().getLocalName() + " , "
          + statement.getPredicate().getLocalName() + " , " + statement.getObject().toString()
          + " )");
      i++;
    }

  }

  /**
   * Lists all constraint violations
   * 
   * @param ontModel the ontology model
   */
  public void getAllConstraintViolations(OntModel ontModel) {

    /**
     * Run all constraints
     */

    List<ConstraintViolation> cvs = SPINConstraints.check(ontModel, null);
    System.out.println("Constraint violations:");
    for (ConstraintViolation cv : cvs) {
      System.out
          .println(" - at " + SPINLabels.get().getLabel(cv.getRoot()) + ": " + cv.getMessage());
    }

    /**
     * Run constraints on a single instance only
     */
    Resource person = cvs.get(0).getRoot();
    List<ConstraintViolation> localCVS = SPINConstraints.check(person, null);
    System.out.println(
        "Constraint violations for " + SPINLabels.get().getLabel(person) + ": " + localCVS.size());

  }
}
