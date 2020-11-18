package com.cmclinnovations.ontochem.model.converter.owl;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.util.ArrayList;

import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBException;
import javax.xml.bind.Marshaller;

import org.apache.commons.io.output.FileWriterWithEncoding;
import org.semanticweb.owlapi.model.IRI;
import org.semanticweb.owlapi.model.OWLOntology;
import org.semanticweb.owlapi.model.OWLOntologyCreationException;
import org.semanticweb.owlapi.model.OWLOntologyManager;
import org.semanticweb.owlapi.reasoner.OWLReasoner;
import org.semanticweb.owlapi.reasoner.structural.StructuralReasonerFactory;
import org.slf4j.Logger;

import com.cmclinnovations.ontochem.model.CtmlConverterState;
import com.cmclinnovations.ontochem.model.IInitCtmlConverter;
import com.cmclinnovations.ontochem.model.InitCtmlConverter;
import com.cmclinnovations.ontokin.model.data.structure.ctml.Ctml;
import com.cmclinnovations.ontochem.model.exception.OntoException;
import com.cmclinnovations.ontochem.model.utils.CtmlConverterUtils;
import com.cmclinnovations.ontochem.model.utils.OwlConverterUtils;

import de.derivo.sparqldlapi.Query;
import de.derivo.sparqldlapi.QueryEngine;
import de.derivo.sparqldlapi.QueryResult;
import de.derivo.sparqldlapi.exceptions.QueryEngineException;
import de.derivo.sparqldlapi.exceptions.QueryParserException;

/**
 * A converter that can convert a set of OWL files each containing chemical
 * a mechanism into CTML files. When it receives a request for conversion, 
 * in each iteration, it loads an OWL file into memory as an ontology model. 
 * The model is then processed to retrieve the CTML instance and its 
 * properties i.e. CMCL version number and comment. It also retrieves phase, 
 * species data and reaction data. As soon as it extracts an OWL class, 
 * an instance or a property it codifies this in CTML. Finally, in the last
 * step it serializes the CTML content and saves this to the disk.
 * 
 * @author Feroz Farazi (msff2@cam.ac.uk)
 *
 */
public class OwlConverter extends CtmlConverterState implements IOwlConverter{

	static Logger logger = org.slf4j.LoggerFactory.getLogger(OwlConverter.class);

	public static void main(String[] args) throws OWLOntologyCreationException, OntoException{
		ArrayList<String> owlFiles= new  ArrayList<String>();
		String ctmlFilePath = "file:\\C:\\Users\\Zhou%20Li\\Feroz_s_Work\\Development\\TestResults\\chemical_mechanism.owl";
		owlFiles.add("file:/C:/Users/Zhou%20Li/Feroz_s_Work/Development/TestResults/chemical_mechanism.owl");
		new OwlConverter().convert(owlFiles, ctmlFilePath);
	}
	
	/**
	 * Converts a set of mechanism OWL ontologies into CTML.
	 * 
	 * @param owlFiles absolute paths including names of the OWL ontology files 
	 * @param ctmlFilePath the path to the CTML files being generated
	 * @throws OntoException
	 * @throws OWLOntologyCreationException
	 */
	public void convert(ArrayList<String> owlFiles, String ctmlFilePath)
			throws OntoException, OWLOntologyCreationException {	
		if (ctmlFilePath == null) {
			logger.error("The ctml file path is null.");
			throw new OntoException("The ctml file path is null.");
		}
		for (String owlFile : owlFiles) {
			// Assigns the name of an OWL file to the global variable
			// owlFileName. It helps us to show ontology specific comments,
			// which is very important while converting a set of ontologies
			// to CTML. In particular when problem arises, it helps to locate
			// a bug easily so potentially help reduce the fixing time.
			 owlFileName = owlFile; 
			// Initialise the instances of the classes that hold CTML metadata
			// and data as well as parsing status information thereof.
			// Also initialises the instances of the classes that read
			// configuration parameters using Spring framework annotations.
			IInitCtmlConverter iCOConverter = new InitCtmlConverter();
			iCOConverter.init();
			convertOwlFile(ctmlFilePath, owlFile);
		}
	}
	
	/**
	 * Converts a mechanism OWL ontology file into CTML. 
	 * 
	 * @param ctmlFilePath the path (without the file name) where a 
	 * CTML file being stored
	 * @param owlFile the path plus file name of an OWL file
	 * @throws OntoException
	 * @throws OWLOntologyCreationException
	 */
	private void convertOwlFile(String ctmlFilePath, String owlFile) throws OntoException, OWLOntologyCreationException{
		// OWL file URL needs to create the ontology IRI
		String owlFileURL = CtmlConverterUtils.formOwlUrl(owlFile);
		ontologyIRI = IRI.create(owlFileURL);
		if (ontologyIRI == null) {
			logger.error("An IRI for the following OWL ontology could not be created:"+owlFile);
			throw new OntoException("An IRI for the following OWL ontology could not be created:"+owlFile);
		}
		// Loads the ontology located at a IRI
		ontology = loadOntology(ontologyIRI);
		// Replaces the previous ontology IRI with the one retrieved
		// from the OWL file
		ontologyIRI = CtmlConverterUtils.readOntologyIRI(ontology);
		// Creates an instance of query engine and assigns it to the 
        // member variable engine 
        engine = createQueryEngine();
        // Creates an absolute path of the CTML file being created
        ctmlFilePath = OwlConverterUtils.formCtmlAbsoultePath(owlFile, ctmlFilePath);
		// Extracts the mechanism name from a mechanism OWL ontology file name
        // mechanismName = OwlConverterUtils.extractOwlMechanismName(owlFile);
        mechanismName = readMechanismName();
        // Converts a mechanism OWL ontology into CTML
		convertOwlFile(ctmlFilePath);
	}
	
	/**
	 * Retrieves the OWL instance id of the current mechanism using the 
	 * mechanism class name. Following this retrieval, it calls the queryLabel
	 * method that retrieves the name of the mechanism using the mechanism
	 * OWL instance id.
	 * 
	 * @return mechanism name the name of the mechanism available in the OWL
	 * file currently being processed 
	 */
	private String readMechanismName() throws OntoException{
		queryInstance(appConfigOntokin.getOntokinMechanism());
		ArrayList<String> results = queryResult;
		queryResult = new ArrayList<>();
		String mechanismLabel = null;
		for(String result:results){
			if(result==null || result.trim().isEmpty()){
				logger.error("No mechanism has been found");
				throw new OntoException("No mechanism has been found");
			}
			mechanismLabel = queryLabel(result);
		}
		return mechanismLabel;
	}

	/**
	 * Retrieves the name of a mechanism using its OWL instance id.
	 * 
	 * @param mechanismInstanceId
	 * @return
	 * @throws OntoException
	 */
	private String queryLabel(String mechanismInstanceId) throws OntoException{
		String label = readLabel(mechanismInstanceId);
		if(label==null || label.trim().isEmpty()){
			logger.error("Mechanism OWL instance does not have a label (name).");
			throw new OntoException("Mechanism OWL instance does not have a label (name).");
		}
		return label;
	}
	
	/**
	 * Loads a mechanism OWL ontology into memory.
	 * 
	 * @param ontologyIRI
	 * @return
	 * @throws OntoException
	 * @throws OWLOntologyCreationException
	 */
	private OWLOntology loadOntology(IRI ontologyIRI) throws OntoException, OWLOntologyCreationException{
		// Loads the ontology located at a IRI
		ontology = manager.loadOntology(ontologyIRI);
		if (ontology == null) {
			logger.error("The requested ontology could not be loaded into memory.");
			throw new OntoException("The requested ontology could not be loaded into memory.");
		}
		return ontology;
	}
	
	/**
	 * Converts an OWL ontology into CTML. The conversion process reads the OWL
	 * ontology and looks for the CTML metadata such as version and comment.
	 * It also looks for the species and reaction data about the mechanism
	 * which is represented in CTML.
	 * 
	 * @param ctmlFilePath
	 * @throws OntoException
	 */
	private void convertOwlFile(String ctmlFilePath)throws OntoException {
		iMetadataQuery.query();
		iPhaseQuery.query();
		iElementQuery.query();
		iSpeciesQuery.query();
		iReactionQuery.query();
		try {
			// Forms the absolute URL of the CTML file being generated
			// using the MechanismViewer compatible
			FileWriterWithEncoding file = new FileWriterWithEncoding(ctmlFilePath, "UTF-8");
			JAXBContext jaxbContext = JAXBContext.newInstance(Ctml.class);
			Marshaller jaxbMarshaller = jaxbContext.createMarshaller();

			// output pretty printed
			jaxbMarshaller.setProperty(Marshaller.JAXB_FORMATTED_OUTPUT, true);

			jaxbMarshaller.marshal(ctmlMD, file);
			addSourceCommentToCtmlObjects(ctmlFilePath);
//			jaxbMarshaller.marshal(ctmlMD, System.out);

		} catch (JAXBException e) {
			logger.error("JAXBException occurred.");
			e.printStackTrace();
		} catch (IOException e) {
			logger.error("IOException occurred.");
			e.printStackTrace();
		}
	}
	
	/**
	 * Creates a new file by adding source comments to the OntoKin generated 
	 * CTML file which does not contain any comment.
	 * 
	 * @param filePathPlusName
	 */
	private void addSourceCommentToCtmlObjects(String filePathPlusName) {
		try {
			BufferedWriter fileCommented = new BufferedWriter(new OutputStreamWriter(
					new FileOutputStream(filePathPlusName.replace(".xml", "_with_comment.xml")), "UTF-8"));
			BufferedReader br = CtmlConverterUtils.openSourceFile(filePathPlusName);
			String line;
			initObjectSequences();
			while ((line = br.readLine()) != null) {
				addSourceCommentToCtmlObjects(line, fileCommented);
				if(line.trim().equals("<comment></comment>")){
					fileCommented.write(line.replace("<comment></comment>", "<comment/>").concat("\n"));
				}else {
					fileCommented.write(line.concat("\n"));
				}
			}
			fileCommented.close();
			br.close();
		} catch (IOException e) {
			logger.error("OntoKin generated CTML file read error.");
			e.printStackTrace();
		}
	}
	
	/**
	 * Initialises the variables that maintain the sequence numbers of CTML 
	 * objects for the codification of source CTML comments into the OntoKin
	 * generated CTML.
	 */
	public void initObjectSequences(){
		phaseSequence = 0;
		elementDataSequence = 0;
		elementSequence = 0;
		speciesDataSequence = 0;
		speciesSequence = 0;
		reactionDataSequence = 0;
		reactionSequence = 0;
	}
	
	/**
	 * Forwards the calls to the methods that add source comments to CTML.
	 * 
	 * @param line
	 * @param fileCommented
	 * @throws IOException
	 */
	private void addSourceCommentToCtmlObjects(String line, BufferedWriter fileCommented) throws IOException{
		addSourceCommentToCtmlPhase(line, fileCommented);
		addSourceCommentToCtmlElementData(line, fileCommented);
		addSourceCommentToCtmlElement(line, fileCommented);
		addSourceCommentToCtmlSpeciesData(line, fileCommented);
		addSourceCommentToCtmlSpecies(line, fileCommented);
		addSourceCommentToCtmlReactionData(line, fileCommented);
		addSourceCommentToCtmlReaction(line, fileCommented);
	}
	
	/**
	 * Adds the source comments about phases to the OntoKin generated CTML.
	 * 
	 * @param line
	 * @param fileCommented
	 * @throws IOException
	 */
	private void addSourceCommentToCtmlPhase(String line, BufferedWriter fileCommented) throws IOException{
		if(line.contains("<phase ")){
			if(objectVsSourceComment.containsKey("phase".concat(Integer.toString(++phaseSequence)))){
				fileCommented.write(
						"\n    <!-- ".concat(objectVsSourceComment.get("phase".concat(Integer.toString(phaseSequence))))
								.concat(" -->").concat("\n"));
			}
		}
	}
	
	/**
	 * Adds the source comments about the elementData to the OntoKin generated 
	 * CTML.
	 * 
	 * @param line
	 * @param fileCommented
	 * @throws IOException
	 */
	private void addSourceCommentToCtmlElementData(String line, BufferedWriter fileCommented) throws IOException{
		if(line.contains("<elementData")){
			if(objectVsSourceComment.containsKey("elementData".concat(Integer.toString(++elementDataSequence)))){
				fileCommented.write(
						"\n    <!-- ".concat(objectVsSourceComment.get("elementData".concat(Integer.toString(elementDataSequence))))
								.concat(" -->").concat("\n"));
			}
		}
	}
	
	/**
	 * Adds source comments about elements to the OntoKin generated CTML.
	 * 
	 * @param line
	 * @param fileCommented
	 * @throws IOException
	 */
	private void addSourceCommentToCtmlElement(String line, BufferedWriter fileCommented) throws IOException{
		if(line.contains("<element ")){
			if(objectVsSourceComment.containsKey("element".concat(Integer.toString(++elementSequence)))){
				fileCommented.write(
						"        <!-- ".concat(objectVsSourceComment.get("element".concat(Integer.toString(elementSequence))))
								.concat(" -->").concat("\n"));
			}
		}
	}
	
	/**
	 * Adds source comments about speciesData to the OntoKin generated CTML.
	 * 
	 * @param line
	 * @param fileCommented
	 * @throws IOException
	 */
	private void addSourceCommentToCtmlSpeciesData(String line, BufferedWriter fileCommented) throws IOException{
		if(line.contains("<speciesData")){
			if(objectVsSourceComment.containsKey("speciesData".concat(Integer.toString(++speciesDataSequence)))){
				fileCommented.write(
						"\n    <!-- ".concat(objectVsSourceComment.get("speciesData".concat(Integer.toString(speciesDataSequence))))
								.concat(" -->").concat("\n"));
			}
		}
	}
	
	/**
	 * Adds source comments about species to the OntoKin generated CTML.
	 * 
	 * @param line
	 * @param fileCommented
	 * @throws IOException
	 */
	private void addSourceCommentToCtmlSpecies(String line, BufferedWriter fileCommented) throws IOException{
		if(line.contains("<species ")){
			if(objectVsSourceComment.containsKey("species".concat(Integer.toString(++speciesSequence)))){
				fileCommented.write(
						"  \n        <!-- ".concat(objectVsSourceComment.get("species".concat(Integer.toString(speciesSequence))))
								.concat(" -->").concat("\n"));
			}
		}
	}
	
	/**
	 * Adds the source comments about reactionData to the OntoKin generated CTML.
	 * 
	 * @param line
	 * @param fileCommented
	 * @throws IOException
	 */
	private void addSourceCommentToCtmlReactionData(String line, BufferedWriter fileCommented) throws IOException{
		if(line.contains("<reactionData")){
			if(objectVsSourceComment.containsKey("reactionData".concat(Integer.toString(++reactionDataSequence)))){
				fileCommented.write(
						"\n    <!-- ".concat(objectVsSourceComment.get("reactionData".concat(Integer.toString(reactionDataSequence))))
								.concat(" -->").concat("\n"));
			}
		}
	}
	
	/**
	 * Adds source comments about reactions to the OntoKin generated CTML.
	 * 
	 * @param line
	 * @param fileCommented
	 * @throws IOException
	 */
	private void addSourceCommentToCtmlReaction(String line, BufferedWriter fileCommented) throws IOException{
		if(line.contains("<reaction ")){
			if(objectVsSourceComment.containsKey("reaction".concat(Integer.toString(++reactionSequence)))){
				fileCommented.write(
						"  \n        <!-- ".concat(objectVsSourceComment.get("reaction".concat(Integer.toString(reactionSequence))))
								.concat(" -->").concat("\n"));
			}
		}
	}
	
	/**
	 * Performs a SPARQL query
	 * 
	 * @param q a SPARQL query
	 * @return the result of the SPARQL query
	 */
	public String performQuery(String q, int type){
		String filteredResult = "";
		// Create a query object from it's string representation
		try {
		Query query = Query.create(q);
			// Execute the query and generate the result set
			QueryResult result = engine.execute(query);
			if(result.isEmpty()){
				return null;				
			}
			// Calls filterResult method to filter out the unwanted
			// part of the result
			filteredResult = filterResult(result.toString(), type);
		} catch (QueryParserException e) {
			// TODO Auto-generated catch block
			return null;
		} catch (QueryEngineException e) {
			// TODO Auto-generated catch block
			return null;
		}
		return filteredResult;
	}

	/**
	 * Performs a SPARQL query
	 * 
	 * @param q a SPARQL query
	 * @param type the type of value expected to return.
	 * It can be a class or property value
	 * @return the result of the SPARQL query
	 */
	public void performMultilineAnswerQuery(String q, int type){
		// Create a query object from it's string representation
		try {
			Query query = Query.create(q);
			// Execute the query and generate the result set
			QueryResult result = engine.execute(query);
			// Calls filterResult method to filter out the unwanted
			// part of the result
			filterMultilineResult(result.toString(), type);
		} catch (QueryParserException e) {
			return;
		} catch (QueryEngineException e) {
			return;
		} catch(Exception e){
			return;
		}
	}

	/**
	 * Separates the actual result of a query from the 
	 * SPARQLDLAPI part (e.g. ?de.derivo.sparqldlapi.Var@76 = ?),
	 * which appears at the beginning of the result and the
	 * datatype, which appears at the end.
	 * 
	 * @param result contains SPARQLDLAPI part + result + datatype
	 * (e.g. ^^xsd:string)
	 * @return only the result of a query
	 */
	private String filterResult(String result, int type){
		String onlyResult = "";
		if (type == 1) {
			return filterPropertyValue(result, result.length());
		} else if (type == 2) {
			return filterClassType(result, result.length());
		}
		return onlyResult;
	}
	
	/**
	 * Removes unwanted texts from a multi-line result.
	 * 
	 * @param result
	 * @param type
	 */
	private void filterMultilineResult(String result, int type){
		try{
		if(result.contains("\n")){
			for(String res:result.split("\n")){
				// To reduce the amount of message passing all the multiline
				// results query use queryResult global vairable. It helps
				// reduce the amount of message passing.
				String str = filterResult(res, type);
				if(str!=null && !str.isEmpty()){
					queryResult.add(str);
				}
				//queryResult.add(filterResult(res, type));
			}
		}
		}catch(Exception e){
			e.printStackTrace();
		}
	}
	
	/**
	 * Produces the expected result by filtering out the unwanted parts from
	 * the value of a property.
	 * 
	 * @param result
	 * @param resultLength
	 * @return
	 */
	private String filterPropertyValue(String result, int resultLength){
		try{
		if (resultLength > 48) {
			if(result.contains("^^xsd:string")){
				return result.toString().substring(34, resultLength - 14);
			} else if(result.contains("#")){
				String[] tokens = result.split("#");
				if(tokens.length>1){
					return tokens[1];
				}
			}
		}
		}catch(Exception e){
			e.printStackTrace();
		}
		return EMPTY;
	}
	
	/**
	 * Produces the expected result by filtering out the unwanted parts from
	 * the class of an instance.
	 *  
	 * @param result
	 * @param resultLength
	 * @return
	 */
	private String filterClassType(String result, int resultLength){
		try{
		if (resultLength > 32) {
			if(result.substring(32)!=null){
				if(result.substring(32).contains(HASH)){
					String[] resultParts = result.substring(32).split(HASH);
					if(resultParts.length>1){
						return resultParts[1];
					}
				}
			}
		}
		}catch(Exception e){
			e.printStackTrace();
		}
		return EMPTY;
	}
	
	/**
	 * Queries the instances of a class.
	 * 
	 * @param claz the class to which instances belong to
	 */
	/**
	 * 
	 * @param claz the class to which instances belong to
	 * @param type the type represents the 
	 */
	public void queryInstance(String claz){
		String q = formQueryWithAType(RDF, RDF_URL, RDF_TYPE, claz);
		// This method call will store result in a global variable called
		// queryResult, therefore, the line following the call will extract
		// results from it. 
		performMultilineAnswerQuery(q, 2);
	}
	
	/**
	 * Forms and then performs a SPARQL query to read the rdfs:label value of 
	 * any object. 
	 * 
	 * @param instanceOwlId the instance id in the OWL representation of 
	 * a mechanism
	 * @return
	 */
	public String readLabel(String instanceOwlId) {
		// Calls the method that forms a SPARQL query
		String q = formQueryWithAStandardVocabulary(RDFS, RDFS_URL, instanceOwlId, RDFS_LABEL);
		// Performs the query q and returns the result
		return performQuery(q, 1);
	}
	
	/**
	 * Forms the SPARQL query to retrieve the comment about a mechanism.
	 * 
	 * @param ontologyIRI ontologyIRI the IRI of the ontology is being queried
	 * @param mechanismName the name of a mechanism
	 * @return
	 */
	public String formQueryWithAStandardVocabulary(String vocabulary, String vocabURL, String object, String property) {
		String q = "PREFIX base: <".concat(ontologyIRI.toString()).concat("#>\n")
				.concat("PREFIX ").concat(vocabulary).concat(": <").concat(vocabURL).concat(">\n")
				.concat("SELECT ?v WHERE {\n")
				.concat("PropertyValue(base:").concat(object)
				.concat(", ").concat(vocabulary).concat(":").concat(property).concat(", ?v)\n")
				.concat("}");
		return q;
	}
	
	/**
	 * Forms the SPARQL query to retrieve the comment about a mechanism.
	 * 
	 * @param ontologyIRI ontologyIRI the IRI of the ontology is being queried
	 * @param mechanismName the name of a mechanism
	 * @return
	 */
	public String formSubjectRetrievalQuery(String object, String property) {
		String q = "PREFIX base: <".concat(ontologyIRI.toString()).concat("#>\n")
				.concat("SELECT ?v WHERE {\n")
				.concat("PropertyValue(?v, base:").concat(property).concat(", base:")
				.concat(object).concat(")\n")
				.concat("}");
		return q;
	}
	
	/**
	 * Forms the SPARQL query to retrieve the requirement of reaction 
	 * data validation.
	 * 
	 * @param ontologyIRI ontologyIRI the IRI of the ontology is being queried
	 * @param mechanismName the name of a mechanism
	 * @return
	 */
	public String formQueryWithBaseURL(String object, String queryItem){
		String q = "PREFIX base: <".concat(ontologyIRI.toString()).concat("#>\n")
				.concat("SELECT ?v WHERE {\n")
				.concat("PropertyValue(base:").concat(object)
				.concat(", base:").concat(queryItem).concat(", ?v)\n").concat("}");
		return q;
	}
	
	/**
	 * Forms the SPARQL query to retrieve the instance belonging to a type 
	 * when the type is given.
	 * 
	 * @param ontologyIRI ontologyIRI the IRI of the ontology is being queried
	 * @param mechanismName the name of a mechanism
	 * @return
	 */
	public String formQueryWithAType(String vocabulary, String vocabURL, String property, String type) {
		String q = "PREFIX base: <".concat(ontologyIRI.toString()).concat("#>\n")
				.concat("SELECT ?v WHERE {\n")
				.concat("Type(?v, ")
				.concat("base:").concat(type)
				.concat(")\n}");
		return q;
	}
	
	/**
	 * Forms the SPARQL query to retrieve the types of an instance.
	 * 
	 * @param ontologyIRI ontologyIRI the IRI of the ontology is being queried
	 * @param mechanismName the name of a mechanism
	 * @return
	 */
	public String formTypeQueryWithAnInstance(String instance) {
		String q = "PREFIX base: <".concat(ontologyIRI.toString()).concat("#>\n")
				.concat("SELECT ?v WHERE {\n")
				.concat("Type(base:").concat(instance).concat(", ")
				.concat("?v")
				.concat(")\n}");
		return q;
	}
	
	
	/**
	 * Forms and then performs a SPARQL query to read any comment that is
	 * codified using rdfs:comment vocabulary term. 
	 * 
	 * @return the comment about a species
	 */
	public String readComment(String instance) {
		// Calls the method that forms a SPARQL query
		String q = formQueryWithAStandardVocabulary(RDFS, RDFS_URL, instance, RDFS_COMMENT);
		// Performs the query q and returns the result
		return performQuery(q, 1);
	}
	
	/**
	 * Creates an instance of query engine and returns it.
	 * 
	 * @return
	 * @throws OntoException
	 * @throws OWLOntologyCreationException
	 */
	private QueryEngine createQueryEngine() throws OntoException, OWLOntologyCreationException{
		// Creates a reasoner
        OWLReasoner reasoner = reasonerFactory.createReasoner(ontology);
		// Creates a query engine
		return QueryEngine.create(manager, reasoner, true);
	}
}
