package uk.ac.cam.cares.jps.base.converter;
 
import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.swing.filechooser.FileFilter;

import org.apache.commons.validator.routines.UrlValidator;
import org.jfree.ui.ExtensionFileFilter;
import org.semanticweb.owlapi.model.IRI;
import org.semanticweb.owlapi.model.OWLOntologyCreationException;
import org.semanticweb.owlapi.model.OWLOntologyStorageException;
import org.slf4j.Logger;
import org.springframework.context.ApplicationContext;
import org.springframework.context.annotation.AnnotationConfigApplicationContext;

import com.opencsv.CSVReader;

import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.util.Dialogs;
import uk.ac.cam.cares.jps.base.util.FileUtil;

/**
 * This class implemented the methods that were provided in the ITBoxGeneration</br> 
 * interface, namely following methods were implemented:</br>
 * 1. generateClass</br>
 * 2. generateSubClass</br>
 * 3. generateDataProperty</br>
 * 4. generateObjectProperty</br>
 * 5. generateUnionOfRanges</br>
 * 6. generateUnionOfDomains</br>
 * 7. readTBoxTemplate</br>
 * 
 * @author msff2
 *
 */
 public class TBoxGeneration implements ITBoxGeneration{
	static Logger logger = org.slf4j.LoggerFactory.getLogger(TBoxGeneration.class);
	ITBoxManagement iTBoxManagement;
	public static String owlFilePath; 
	public static final String HAS_IRI = "https://www.w3.org/2007/05/powder-s#hasIRI";
	public static final String VERSION_INFO = "http://www.w3.org/2002/07/owl#versionInfo";
	public static final String RDFS_COMMENT = "http://www.w3.org/2000/01/rdf-schema#comment";
	public static final String OWL_IMPORTS = "http://www.w3.org/2002/07/owl#imports";
	public static TBoxConfiguration tBoxConfig;
	public static ApplicationContext applicationContext;
	
	/**
	 * Stores the mapping between a child class and its parents.
	 */
	Map<String, List<String>> childParentMap = new HashMap<String, List<String>>();
	/**
	 * Stores the mapping between a domain class and its relations.
	 */
	Map<String, List<String>> domainRelationMap = new HashMap<String, List<String>>();
	/**
	 * Stores the mapping between a range class and its relations.
	 */
	Map<String, List<String>> rangeRelationMap = new HashMap<String, List<String>>();

	
	public static void main(String[] args) {
		File folder = Dialogs.selectFileDialog(new File(System.getProperty("user.home")), new FileFilter[]{new ExtensionFileFilter("Comma-separated Value", "csv")}, false);
		if (folder == null) {
		} else if (!folder.exists()) {
			Dialogs.showErrorDialog("Selected folder does not exist.", "Read");
		} else{
			ITBoxGeneration iTBoxGeneration = new TBoxGeneration();
			iTBoxGeneration.generateTBox(folder.toString());
		}
	}
	
	/**
	 * Takes the path plus name of a CSV file containing the TBox template.</br>
	 * 
	 * @param csvFileNamePlusPath
	 */
	public void generateTBox(String csvFileNamePlusPath) {
		if (csvFileNamePlusPath == null || csvFileNamePlusPath.isEmpty()) {
			logger.error("No file has been found in the path specied.");
		}
		logger.info("Ontokin TBox generator started running...");
		owlFilePath = csvFileNamePlusPath.replace(".csv", ".owl");
		iTBoxManagement = new TBoxManagement();
		try {
			iTBoxManagement.init();
			readCSVTemplate(csvFileNamePlusPath);
			iTBoxManagement.saveOntology(owlFilePath);
			logger.info("Ontokin TBox generation FINISHED.");
		} catch (IOException e) {
			logger.error("IOException occured.");
			throw new JPSRuntimeException(e);
		} catch (JPSRuntimeException e) {
			logger.error("JPSRuntimeException occured.");
			throw new JPSRuntimeException(e);
		} catch (OWLOntologyCreationException e) {
			logger.error("OWLOntologyCreationException occured.");
			throw new JPSRuntimeException(e);
		} catch (OWLOntologyStorageException e) {
			logger.error("OWLOntologyStorageException occured.");
			throw new JPSRuntimeException(e);
		}
	}
	
	public void generateTBox(String csvFileNamePlusPath, String outputFilePlusPath) {
		if (csvFileNamePlusPath == null || csvFileNamePlusPath.isEmpty()) {
			logger.error("No file has been found in the path specied.");
		}
		logger.info("Ontokin TBox generator started running...");
		owlFilePath = outputFilePlusPath;
		iTBoxManagement = new TBoxManagement();
		try {
			iTBoxManagement.init();
			readCSVTemplate(csvFileNamePlusPath);
			iTBoxManagement.saveOntology(owlFilePath);
			logger.info("Ontokin TBox generation FINISHED.");
		} catch (IOException e) {
			logger.error("IOException occured.");
			throw new JPSRuntimeException(e);
		} catch (JPSRuntimeException e) {
			logger.error("JPSRuntimeException occured.");
			throw new JPSRuntimeException(e);
		} catch (OWLOntologyCreationException e) {
			logger.error("OWLOntologyCreationException occured.");
			throw new JPSRuntimeException(e);
		} catch (OWLOntologyStorageException e) {
			logger.error("OWLOntologyStorageException occured.");
			throw new JPSRuntimeException(e);
		}
	}

	/**
	 * Reads a CSV template with inputs for creating TBoxes.
	 * 
	 * @param csvFileNamePlusPath
	 * @throws IOException
	 * @throws OWLOntologyCreationException 
	 */
	private void readCSVTemplate(String csvFileNamePlusPath) throws IOException, JPSRuntimeException, OWLOntologyCreationException{
		storeRelationships(csvFileNamePlusPath);
		List<List<String>> brSourceCtml = FileUtil.openCSVSourceFile(csvFileNamePlusPath);
		int countLine = 0;
		for(List<String> singleLine:brSourceCtml){
			if(++countLine<=1){
				processHeader(singleLine);
			}else{
				processLine(singleLine);
			}
		}
	}
	
	/**
	 * Reads a CSV template with inputs to produce the following:
	 * 1. the mapping between a child class and its parents.
	 * 2. the mapping between a domain class and its relations.
	 * 3. the mapping between a range class and its relations.
	 * 
	 * @param csvFileNamePlusPath
	 * @throws IOException
	 * @throws OWLOntologyCreationException 
	 */
	private void storeRelationships(String csvFileNamePlusPath) throws IOException, JPSRuntimeException, OWLOntologyCreationException{
		List<List<String>> brSourceCtml = FileUtil.openCSVSourceFile(csvFileNamePlusPath);
		int rowCount = 0;
		for(List<String> singleLine:brSourceCtml){
			rowCount++;
			// Creates the mapping between a class and its parents
			String source = singleLine.get(tBoxConfig.getIndexOfSourceColumn());
			String type = singleLine.get(tBoxConfig.getIndexOfTypeColumn());
			String target = singleLine.get(tBoxConfig.getIndexOfTargetColumn());
			String relation = singleLine.get(tBoxConfig.getIndexOfRelationColumn());
			if (type == null || type.trim().equals("")) {
				logger.error("The ontology was not created as the Type is null or empty in the CSV file in row: "+rowCount);
				throw new JPSRuntimeException("The ontology was not created as the Type is null or empty in the CSV file in row: "+rowCount);
			}
			if (source == null || source.trim().equals("")) {
				logger.error("The ontology was not created as the Source is null or empty in the CSV file in row: "+rowCount);
				throw new JPSRuntimeException("The ontology was not created as the Source is null or empty in the CSV file in row: "+rowCount);
			}
			source = source.replaceAll("\\s+", "").toLowerCase();
			type = type.trim().toLowerCase();
			target = target.replaceAll("\\s+", "").toLowerCase();
			if (singleLine.size() > tBoxConfig.getIndexOfRelationColumn() 
					&& type.equalsIgnoreCase(tBoxConfig.getElementTypeClass()) 
					&& relation.equalsIgnoreCase(tBoxConfig.getIsARelation())) {
				if (childParentMap.containsKey(source)) {
					childParentMap.get(type).add(target);
				} else {
					List<String> parents = new ArrayList<String>();
					parents.add(target);
					childParentMap.put(source, parents);
				}
			}
			
			// Creates the mapping between a domain class and relations associated with it.
			String domain = singleLine.get(tBoxConfig.getIndexOfDomainColumn());
			if (singleLine.size() > tBoxConfig.getIndexOfDomainColumn()
					&& domain != null
					&& !domain.trim().equals("")) {
				String[] domainClasses = null;
				if (domain.contains("UNION")) {
					domainClasses = domain.split("UNION");
				} else if (domain.contains("INTERSECTION")) {
					domainClasses = domain.split("INTERSECTION");
				}
				if (domainClasses != null){
					for(String domainClass:domainClasses) {
						domainClass = domainClass.replaceAll("\\s+", "").toLowerCase();
						if (domainRelationMap.containsKey(domainClass)) {
							domainRelationMap.get(domainClass).add(source);
						} else {
							List<String> relations = new ArrayList<String>();
							relations.add(source);
							domainRelationMap.put(domainClass, relations);
						}
					}
				}
			}
			
			// Creates the mapping between a range class and relations associated with it.
			String range = singleLine.get(tBoxConfig.getIndexOfRangeColumn());
			if (singleLine.size() > tBoxConfig.getIndexOfRangeColumn() 
					&& range!= null
					&& !range.equals("")) {
				String[] rangeClasses = null;
				if(range.contains("UNION")){
					rangeClasses = range.split("UNION");
				} else if (range.contains("INTERSECTION")) {
					rangeClasses = range.split("INTERSECTION");
				}
				if (rangeClasses != null) {
					for(String rangeClass:rangeClasses) {
						rangeClass = rangeClass.replaceAll("\\s+", "").toLowerCase();
						if (rangeRelationMap.containsKey(rangeClass)) {
							rangeRelationMap.get(rangeClass).add(source);
						} else {
							List<String> relations = new ArrayList<String>();
							relations.add(source);
							rangeRelationMap.put(rangeClass, relations);
						}
					}
				}
			}
		}
		
//		System.out.println("Child-parent relationships:");
//		for(String key: childParentMap.keySet()) {
//			System.out.println("Child:"+key);
//			for(String parent:childParentMap.get(key)) {
//				System.out.println("   Parent:"+parent);
//			}
//		}
//
//		System.out.println("Domain and its associated relations:");
//		for(String key: domainRelationMap.keySet()) {
//			System.out.println("Domain:"+key);
//			for(String relation:domainRelationMap.get(key)) {
//				System.out.println("   Relation:"+relation);
//			}
//		}
//		System.out.println("Range and its associated relations:");
//		for(String key: rangeRelationMap.keySet()) {
//			System.out.println("Range:"+key);
//			for(String relation:rangeRelationMap.get(key)) {
//				System.out.println("   Relation:"+relation);
//			}
//		}
	}


	/**
	 * Processes the header of the CSV file being read.
	 * 
	 * @param line represents the header of the CSV file
	 * @throws IOException
	 */
	private void processHeader(List<String> line) throws IOException, JPSRuntimeException{
		if(line.isEmpty()){
			logger.error("The header is empty.");
			throw new IOException("TBox generation stopped proceeding as the header is empty.");
		}
		String[] tokens = line.toArray(new String[line.size()]);
		processHeader(tokens);
	}
	
	/**
	 * Checks if the header contains all the construct name. If it does</br>
	 * not find the right construct in the right position, it throws an</br>
	 * exception. It expects the following position, construct pairs:
	 * 0 - source
	 * 1 - type
	 * 2 - target
	 * 3 - relation
	 * 4 - domain
	 * 5 - range
	 * 6 - quantifier
	 * 7 - comment
	 * 8 - defined by
	 * 
	 * @param tokens
	 */
	private void processHeader(String[] tokens) throws JPSRuntimeException{
		int tokenNumber = 0;
		for (String token : tokens) {
			switch (++tokenNumber) {
			case 1:
				checkIfSourceAppears(token);
				break;
			case 2:
				checkIfTypeAppears(token);
				break;
			case 3:
				checkIfTargetAppears(token);
				break;
			case 4:
				checkIfRelationAppears(token);
				break;
			case 5:
				checkIfDomainAppears(token);
				break;
			case 6:
				checkIfRangeAppears(token);
				break;
			case 9:
				checkIfDefinedByAppears(token);
			}
		}
	}
	
	/**
	 * Checks if the source column appears in the header. If it does not</br>
	 * appear in the header, it throws an exception.
	 * 
	 * @param token
	 * @throws JPSRuntimeException
	 */
	private void checkIfSourceAppears(String token) throws JPSRuntimeException{
		if (!token.equalsIgnoreCase("source")){
			throw new JPSRuntimeException("The source column is missing in the header.");
		}
	}
	
	/**
	 * Checks if the type column appears in the header. If it does not</br>
	 * appear in the header, it throws an exception.
	 * 
	 * @param token
	 * @throws JPSRuntimeException
	 */
	private void checkIfTypeAppears(String token) throws JPSRuntimeException{
		if (!token.equalsIgnoreCase("type")){
			throw new JPSRuntimeException("The type column is missing in the header.");
		}
	}
	
	/**
	 * Checks if the target column appears in the header. If it does not</br>
	 * appear in the header, it throws an exception.
	 * 
	 * @param token
	 * @throws JPSRuntimeException
	 */
	private void checkIfTargetAppears(String token) throws JPSRuntimeException{
		if (!token.equalsIgnoreCase("target")){
			throw new JPSRuntimeException("The target column is missing in the header.");
		}
	}
	
	/**
	 * Checks if the relation column appears in the header. If it does not</br>
	 * appear in the header, it throws an exception.
	 * 
	 * @param token
	 * @throws JPSRuntimeException
	 */
	private void checkIfRelationAppears(String token) throws JPSRuntimeException{
		if (!token.equalsIgnoreCase("relation")){
			throw new JPSRuntimeException("The relation column is missing in the header.");
		}
	}
	
	/**
	 * Checks if the domain column appears in the header. If it does not</br>
	 * appear in the header, it throws an exception.
	 * 
	 * @param token
	 * @throws JPSRuntimeException
	 */
	private void checkIfDomainAppears(String token) throws JPSRuntimeException{
		if (!token.equalsIgnoreCase("domain")){
			throw new JPSRuntimeException("The domain column is missing in the header.");
		}
	}
	
	/**
	 * Checks if the range column appears in the header. If it does not</br>
	 * appear in the header, it throws an exception.
	 * 
	 * @param token
	 * @throws JPSRuntimeException
	 */
	private void checkIfRangeAppears(String token) throws JPSRuntimeException{
		if (!token.equalsIgnoreCase("range")){
			throw new JPSRuntimeException("The range column is missing in the header.");
		}
	}
	
	/**
	 * Checks if the defined by column appears in the header. If it does not</br>
	 * appear in the header, it throws an exception.
	 * 
	 * @param token
	 * @throws JPSRuntimeException
	 */
	private void checkIfDefinedByAppears(String token) throws JPSRuntimeException{
		if (!token.equalsIgnoreCase("defined by")){
			throw new JPSRuntimeException("The defined by column is missing in the header.");
		}
	}
	
	/**
	 * Processes a line that contains either a class with or without the name
	 * of its parent, a data property or an object property. Both properties
	 * may or may not come with domain(s) and range(s).
	 * 
	 * @param line represents a TBox originating from the CSV file
	 * @throws IOException
	 * @throws JPSRuntimeException
	 * @throws OWLOntologyCreationException 
	 */
	private void processLine(List<String> line) throws IOException, JPSRuntimeException, OWLOntologyCreationException{
		if(line.isEmpty()){
			logger.info("It encountered an empty line.");
			return;
		}
		readEachColumn(line);
	}
	
	/**
	 * Reads each column and determines the action whether to create a class,
	 * a data property or an object property.
	 * 
	 * @param line represents a TBox originating from the CSV file
	 * @throws IOException
	 * @throws JPSRuntimeException 
	 * @throws OWLOntologyCreationException 
	 */
	private void readEachColumn(List<String> line) throws IOException, JPSRuntimeException, OWLOntologyCreationException{
		String[] tokens = line.toArray(new String[line.size()]);
		int tokenNumber = 0;
		for(String token: tokens){
			switch (++tokenNumber) {
			case 1:
				readSourceColumn(tokens[tokenNumber - 1]);
				break;
			case 2: readTypeColumn(tokens, tokenNumber - 1);
				break;
			}
		}
	}
	
	/**
	 * Reads the first column and checks if it is empty. In case it is empty it
	 * throws an exception, otherwise it allows proceeding to the next column.
	 * 
	 * @param firstColumn represents the value of the source in any row
	 * except the header
	 * @throws IOException
	 * @throws JPSRuntimeException
	 */
	private void readSourceColumn(String firstColumn) throws IOException, JPSRuntimeException{
		if(firstColumn == null || firstColumn.isEmpty()){
			throw new IOException("The first column of a row is empty.");
		}
	}
	
	/**
	 * Reads the second column and checks if it is empty. In case it is empty it
	 * throws an exception, otherwise it allows proceeding to the next column.
	 * 
	 * @param secondColumn represents the value of the type of source in any
	 * row except the header
	 * @throws IOException
	 * @throws JPSRuntimeException
	 * @throws OWLOntologyCreationException 
	 */
	private void readTypeColumn(String[] tokens, int tokenNumber) throws IOException, JPSRuntimeException, OWLOntologyCreationException{
		if(tokens[tokenNumber] == null || tokens[tokenNumber].isEmpty()){
			throw new IOException("The second column of a row is empty.");
		}
		decideTypeOfConstruct(tokens, tokenNumber);
	}
	
	/**
	 * Decides whether to create a class, data property, object property or TBox property.</br>
	 * Following the decision it calls the appropriate method to do it.</br>
	 * For creating a data property, it passes 1 as the first parameter of the</br>
	 * generateProperty method.</br>
	 * For creating an object property, it passes 2 as the first parameter of</br>
	 * the generateProperty method. 
	 * 
	 * @param tokens
	 * @param tokenNumber
	 * @throws IOException
	 * @throws JPSRuntimeException
	 * @throws OWLOntologyCreationException 
	 */
	private void decideTypeOfConstruct(String[] tokens, int tokenNumber) throws IOException, JPSRuntimeException, OWLOntologyCreationException{
		if(tokens[tokenNumber].toLowerCase().equalsIgnoreCase("class")){
			if(tokens.length>tokenNumber+1 && !tokens[tokenNumber+1].isEmpty() && tokens.length>tokenNumber+2 && !tokens[tokenNumber+2].isEmpty()){
				generateClass(tokens[tokenNumber-1], tokens[tokenNumber+1], tokens[tokenNumber+2]);
			} else{
				generateClass(tokens[tokenNumber-1], null, null);
			}
			generateClassMetadata(tokens);
		} else if(tokens[tokenNumber].toLowerCase().split(",")[0].equalsIgnoreCase("data property")){
			generateProperty(1, tokens);
			generateDataPropertyMetadata(tokens);
		} else if(tokens[tokenNumber].toLowerCase().split(",")[0].equalsIgnoreCase("object property")){
			generateProperty(2, tokens);
			generateObjectPropertyMetadata(tokens);
		} else if(tokens[tokenNumber].toLowerCase().equalsIgnoreCase("tbox")){
			generateTBoxProperty(tokens);
		} 
	}
	
	/**
	 * Processes and extracts the following info to send this to the 
	 * callPropertyGenerateor method:
	 * 1. Property name;</br>
	 * 2. Target name;</br>
	 * 3. Relation;</br>
	 * 2. Domain, which can have multiple values; </br> 
	 * 3. Range, which can have multiple values.
	 * 
	 * @param tokens
	 * @throws IOException
	 * @throws JPSRuntimeException
	 */
	private void generateProperty(int propertyTypeID, String[] tokens) throws IOException, JPSRuntimeException {
		int i = 0;
		String propertyName = "";
		String type = "";
		String targetName = "";
		String relation = "";
		String domain = "";
		String range = "";
		String quantifier = "";
		for(String token: tokens){
			if(++i==1){
				propertyName = token;
			} if(i==2){
				type = token;
			} if(i==3){
				targetName = token;
			} if(i==4){
				relation = token;
			} if(i==5){
				domain = token;
			} if (i==6){
				range = token;
			} if(i==7){
				quantifier = token;
			}
		}
		callPropertyGenerateor(propertyTypeID, propertyName, type, targetName, relation, domain, range, quantifier);
	}

	/**
	 * Processes and extracts the following info to send this to the 
	 * callPropertyGenerateor method:
	 * 1. Property name;</br>
	 * 2. Target name;</br>
	 * 3. Relation;</br>
	 * 2. Domain, which can have multiple values; </br> 
	 * 3. Range, which can have multiple values.
	 * 
	 * @param tokens
	 * @throws IOException
	 * @throws JPSRuntimeException
	 * @throws OWLOntologyCreationException 
	 */
	private void generateTBoxProperty(String[] tokens) throws IOException, JPSRuntimeException, OWLOntologyCreationException {
		int i = 0;
		String tBoxName = "";
		String target = "";
		String relation = "";
		for(String token: tokens){
			if(++i==1){
				tBoxName = token;
			} if(i==3){
				target = token;
			} if(i==4){
				relation = token;
			}
		}
		assignTBoxProperty(tBoxName, target, relation);
	}

	
	/**
	 * Calls either the data property generator or the object property</br> 
	 * generator based on the value of the property type. If its value</br>
	 * is 1, it calls the generateDataProperty method and if its value</br>
	 * is 2, it calls the generateObjectProperty method.
	 * 
	 * @param propertyType
	 * @param strings
	 * @throws IOException
	 * @throws JPSRuntimeException
	 */
	private void callPropertyGenerateor(int propertyType, String...strings) throws IOException, JPSRuntimeException {
		if(propertyType==1){
			generateDataProperty(strings[0], strings[1], strings[2], strings[3], strings[4], strings[5]);
		}
		if(propertyType==2){
			generateObjectProperty(strings[0], strings[1], strings[2], strings[3], strings[4], strings[5], strings[6]);
		}
	}

	/**
	 * Assigns each TBox property to the corresponding setter method. 
	 * 
	 * @param tBoxName
	 * @param target
	 * @param relation
	 * @throws IOException
	 * @throws JPSRuntimeException
	 * @throws OWLOntologyCreationException 
	 */
	private void assignTBoxProperty(String tBoxName, String target, String relation)
			throws IOException, JPSRuntimeException, OWLOntologyCreationException {
		if (tBoxName != null && !tBoxName.isEmpty() && relation != null && !relation.isEmpty()) {
			if (relation.trim().equals(HAS_IRI) && target != null && new UrlValidator().isValid(target)) {
				TBoxManagement.tBoxConfig.settBoxIri(target);
				iTBoxManagement.instantiateOntologyModel();
			} else if (relation.trim().equals(VERSION_INFO) && target != null && !target.isEmpty()) {
				TBoxManagement.tBoxConfig.settBoxVersion(target);
			} else if (relation.trim().equals(RDFS_COMMENT) && target != null && !target.isEmpty()) {
				TBoxManagement.tBoxConfig.settBoxComment(target);
			} else if (relation.trim().equals(OWL_IMPORTS) && target != null && !target.isEmpty()) {
				TBoxManagement.tBoxConfig.settBoxImport(target);
			}
		}
	}
	
	/**
	 * Generates an OWL class with the name originated from the current</br>
	 * template CSV file. If the parent name is also provided, it generates</br>
	 * a subclass of relation between the class and its parent.
	 * 
	 * @param className
	 * @param targetName
	 * @param relation
	 * @throw IOException
	 * @throws JPSRuntimeException 
	 *
	 */
	public void generateClass(String className, String targetName, String relation) throws IOException, JPSRuntimeException{
		iTBoxManagement.createOWLClass(className, targetName, relation);
	}
	
	/**
	 * Adds following metadata to a class.</br>
	 * - the class definition (as a comment)
	 * - the defined by property
	 * - a label
	 * 
	 * @param tokens
	 * @throws IOException
	 * @throws JPSRuntimeException
	 */
	public void generateClassMetadata(String[] tokens) throws IOException, JPSRuntimeException{
		if(tokens.length>7){
			iTBoxManagement.addDefinitionToOWLClass(tokens[0], tokens[7]);
		}
		
		if(tokens.length>8){
			iTBoxManagement.addDefinedByToClass(tokens[0], tokens[8]);
		}
		
		if(tokens.length>9){
			iTBoxManagement.addLabelToOWLClass(tokens[0], tokens[9]);
		}
	}
	
	/**
	 * Adds following metadata to an object property.</br>
	 * - the property definition (as a comment)
	 * - the defined by property
	 * - the domain and range
	 * - formulas
	 * - a label
	 * 
	 * @param tokens
	 * @throws IOException
	 * @throws JPSRuntimeException
	 */
	public void generateObjectPropertyMetadata(String[] tokens) throws IOException, JPSRuntimeException{
		if(tokens.length>7){
			iTBoxManagement.addDefinitionToObjectProperty(tokens[0], tokens[7]);
		}
		if(tokens.length>8){
			iTBoxManagement.addDefinedByToObjectProperty(tokens[0], tokens[8]);
		}
		if(tokens.length>6){
			if(tokens[5]!=null && !tokens[5].isEmpty()){
				iTBoxManagement.addLogicalFormulaToObjectProperty(tokens[0], tokens[6], tokens[4], tokens[5]);
			}
		}
		if(tokens.length>9){
			iTBoxManagement.addLabelToObjectProperty(tokens[0], tokens[9]);
		}

	}
	
	/**
	 * Adds following metadata to a data property.</br>
	 * - the property definition (as a comment)
	 * - the defined by property
	 * - the domain and range
	 * - a label
	 * 
	 * @param tokens
	 * @throws IOException
	 * @throws JPSRuntimeException
	 */
	public void generateDataPropertyMetadata(String[] tokens) throws IOException, JPSRuntimeException{
		if(tokens.length>7){
			iTBoxManagement.addDefinitionToDataProperty(tokens[0], tokens[7]);
		}
		if(tokens.length>8){
			iTBoxManagement.addDefinedByToDataProperty(tokens[0], tokens[8]);
		}
		if(tokens.length>9){
			iTBoxManagement.addLabelToDataProperty(tokens[0], tokens[9]);
		}
	}
	
	/**
	 * Generates an OWL data property with the name originated from the current</br>
	 * template CSV file. If the domain(s) and range(s) are provided, it</br>
	 * generates them as well. If the target name that refer to the target</br>
	 * property and relation are provided, it creates the relation. 
	 * 
	 * @param propertyName
	 * @param type
	 * @param targetName
	 * @param relation
	 * @param domain
	 * @param range
	 */
	public void generateDataProperty(String propertyName, String type, String targetName, String relation, String domain, String range) throws IOException, JPSRuntimeException{
		iTBoxManagement.createOWLDataProperty(propertyName, type, targetName, relation, domain, range);
	}
	
	/**
	 * Generates an OWL object property with the name originated from the current</br>
	 * template CSV file. If the domain(s) and range(s) are provided, it</br>
	 * generates them as well.
	 * 
	 * @param propertyName
	 * @param type
	 * @param targetName
	 * @param relation
	 * @param domain
	 * @param range
	 * @param quantifier
	 */
	public void generateObjectProperty(String propertyName, String type, String targetName, String relation, String domain, String range, String quantifier) throws IOException, JPSRuntimeException{
		iTBoxManagement.createOWLObjectProperty(propertyName, type, targetName, relation, domain, range, quantifier);
	}
	
	/**
	 * Initialise variables for reading configuration properties.
	 */
	public void init() throws JPSRuntimeException, OWLOntologyCreationException{
		applicationContext = new AnnotationConfigApplicationContext(SpringConfiguration.class);
		tBoxConfig = applicationContext.getBean(TBoxConfiguration.class);
	}
 }