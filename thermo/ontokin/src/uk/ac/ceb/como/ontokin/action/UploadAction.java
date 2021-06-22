package uk.ac.ceb.como.ontokin.action;

import java.io.File;
//import java.io.FileWriter;
import java.io.IOException;
//import java.net.URL;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Properties;
import java.util.Set;

import org.apache.log4j.Logger;
import org.semanticweb.owlapi.model.OWLOntologyCreationException;

import com.cmclinnovations.ontochem.model.converter.ctml.CtmlConverter;
import com.cmclinnovations.ontochem.model.exception.OntoException;
import com.opensymphony.xwork2.ActionSupport;
import com.opensymphony.xwork2.ValidationAware;

import uk.ac.cam.cares.jps.blazegraph.KnowledgeRepository;
//import uk.ac.cam.ceb.como.compchem.ontology.InconsistencyExplanation;
import uk.ac.ceb.como.ontokin.bean.ChemkinUploadReport;
import uk.ac.ceb.como.ontokin.model.FolderManager;
import uk.ac.ceb.como.ontokin.model.PropertiesManager;

/**
 * Uploads a CHEMKIN mechanism consisting of the following files:</br>
 * 1. Mechanism file.
 * 2. Thermo Chemistry file.
 * 3. Surface Chemistry file.(optional)
 * 4. Transport data file. (optional)
 * 
 * @author msff2
 *
 */
public class UploadAction extends ActionSupport implements ValidationAware {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = 1L;

	/** The Constant logger. */
	final static Logger logger = Logger.getLogger(UploadAction.class.getName());
	/**
	 * The name used to refer to a user uploaded mechanism file.
	 */
	final static String FILE_NAME_MECH = "mech.dat";
	/**
	 * The name used to refer to a user uploaded thermo data file.
	 */	
	final static String FILE_NAME_THERMO = "thermo.dat";
	/**
	 * The name used to refer to a user uploaded surface chemistry file.
	 */	
	final static String FILE_NAME_SURFACE = "surface.dat";
	/**
	 * The name used to refer to a user uploaded transport data file.
	 */	
	final static String FILE_NAME_TRANSPORT = "transport.dat";

	Properties ontokinPropreties = PropertiesManager.loadProperties(UploadAction.class.getClassLoader().getResourceAsStream("ontokin.management.properties"));

	private String dataFolderPath = ontokinPropreties.getProperty("data.folder.path").toString();
	
	private String generatedXMLFileName = ontokinPropreties.getProperty("generated.xml.file.name").toString();

	private String kbFolderPath = ontokinPropreties.getProperty("kb.folder.path").toString();
	
	private String kbMechanismFolderPath = ontokinPropreties.getProperty("kb.mechanism.folder.path").toString();
	
	private String kineticsConverterFilePath = ontokinPropreties.getProperty("kinetics.converter.file.path").toString();

	private final String XML_FILENAME = "mechanism.xml";
	
	private final String LOG_FILE_NAME = "mech_dat.log";
	
	/**
	 * 
	 * @author NK510 Adds kb properties such as: OntoCompChem URI, RDF4J server URL.
	 * 
	 */
	Properties kbProperties = PropertiesManager.loadProperties(UploadAction.class.getClassLoader().getResourceAsStream("kb.ontokin.management.properties"));

	private final String ONTOKIN_KB_URL = kbProperties.getProperty("ontokin.kb.uri").toString();

	private String serverURL = kbProperties.getProperty("ontokin.kb.local.rdf4j.server.url").toString();
	
	private String dataURL = kbProperties.getProperty("ontokin.data.folder.url").toString();
	
	private final String REPOSITORY_ID = kbProperties.getProperty("ontokin.repository.id").toString();

	/** The files. */
	private List<File> files = new ArrayList<File>();

	/** The upload file name. */
	private String[] uploadFileName;

	/** The upload content type. */
	private String[] uploadContentType;

	private File myMechFile;
	
	private String myMechFileContentType;
	
	private String myMechFileFileName;
	
	private File myThermoFile;
	
	private String myThermoFileContentType;
	
	private String myThermoFileFileName;

	private File mySurfaceFile;
	
	private String mySurfaceFileContentType;
	
	private String mySurfaceFileFileName;

	private File myTransportFile;
	
	private String myTransportFileContentType;
	
	private String myTransportFileFileName;
	
	private String myMechanismName;
	
	private boolean myChemkinValidationReport;
	
	private String myChemkinValidationReportFile;
	
	private boolean myOWLConsistencyReport;
	
	private List<String> column = new ArrayList<String>();
	private String column1 = new String();
	private String column2 = new String();
	private String column3 = new String();
	private String column4 = new String();
	private String column5 = new String();
	private String column6 = new String();
	private String column7 = new String();
	

	/** The CHEMKIN upload report. */
	private ChemkinUploadReport gaussianUploadReport;

	/** The upload report list. */
	private List<ChemkinUploadReport> uploadReportList = new ArrayList<ChemkinUploadReport>();
	
	@Override
	public String execute() throws Exception {
		if (!(getMyMechFile() == null || getMyMechFile().getName() == null || getMyMechFile().getName().isEmpty() 
				|| getMyThermoFile() == null  || getMyThermoFile().getName() == null || getMyThermoFile().getName().isEmpty()
				|| getMyMechanismName() == null  || getMyMechanismName().isEmpty())) {
			column.add("Mechanism name");
			setColumn1("Mechanism name");
			setColumn2("CHEMKIN mechanism file name");
			setColumn3("CHEMKIN thermo data file name");
			setColumn4("CHEMKIN surface chemistry file name");
			setColumn5("CHEMKIN transport data file name");
			setColumn6("CHEMKIN files found valid");
			setColumn7("OWL file passed consistency check");
		}

		if (getMyMechFile() == null || getMyMechFile().getName()==null || getMyMechFile().getName().isEmpty() 
				|| getMyThermoFile() == null  || getMyThermoFile().getName()==null || getMyThermoFile().getName().isEmpty()
				|| getMyMechanismName() == null  || getMyMechanismName().isEmpty()) {

			addActionMessage(
					"Please select a mechanism file and thermo chemistry file first and then press the 'Upload' button.");

		}

		if (!(getMyMechFile() == null || getMyMechFile().getName() == null || getMyMechFile().getName().isEmpty() 
				|| getMyThermoFile() == null  || getMyThermoFile().getName() == null || getMyThermoFile().getName().isEmpty()
				|| getMyMechanismName() == null  || getMyMechanismName().isEmpty())) {

		File folder = new File(dataFolderPath);
		File[] listOfDirectories = folder.listFiles();
		// Defined to produce the list of directory names under the folder of data. 
		Set<String> directoryNames = new HashSet<String>(); 
		// Adding the name of each OWL file to the list
		for (int i = 0; i < listOfDirectories.length; i++) {
			if (listOfDirectories[i].isDirectory()) {
				directoryNames.add(listOfDirectories[i].getName());
			}
		}
		// If user proposed mechanism name already exists in the repository, 
		// this part will generate and propose a new name
		for(int i = 0; i < listOfDirectories.length; i++){
			if(i==0){
				if(!directoryNames.contains(getMyMechanismName())){
					break;
				}
			}else{
				if(!directoryNames.contains(getMyMechanismName()+"_"+i)){
					setMyMechanismName(getMyMechanismName()+"_"+i);
					break;
				}
			}
		}
		
		String uuidFolderName = getMyMechanismName();
		
		File inputMechFile = new File(dataFolderPath + uuidFolderName + "/" + FILE_NAME_MECH);
		File inputThermoFile = new File(dataFolderPath + uuidFolderName + "/" + FILE_NAME_THERMO);
		File inputSurfaceFile = new File(dataFolderPath + uuidFolderName + "/" + FILE_NAME_SURFACE);
		File inputTransportFile = new File(dataFolderPath + uuidFolderName + "/" + FILE_NAME_TRANSPORT);

		FolderManager.createFolder(dataFolderPath + uuidFolderName);

		FolderManager.saveFileInFolder(inputMechFile, getMyMechFile().getAbsolutePath());
		FolderManager.saveFileInFolder(inputThermoFile, getMyThermoFile().getAbsolutePath());
		// A surface file can be empty, so the following statement skips
		// the creating and saving of an empty surface file.
		if (getMySurfaceFile() != null) {
			FolderManager.saveFileInFolder(inputSurfaceFile, getMySurfaceFile().getAbsolutePath());
		}
		// A transport file can be empty, so the following statement skips
		// the creating and saving of an empty transport file.
		if (getMyTransportFile() != null) {
			FolderManager.saveFileInFolder(inputTransportFile, getMyTransportFile().getAbsolutePath());
		}
		// Calls the method to convert from CHEMKIN to the intermediate XML
		// format
		boolean chemKinValidation = convertChemkinToXML(kineticsConverterFilePath,
				dataFolderPath + uuidFolderName + "\\", inputMechFile.getAbsolutePath(),
				inputThermoFile.getAbsolutePath(), inputSurfaceFile.getAbsolutePath(),
				inputTransportFile.getAbsolutePath());
		setMyChemkinValidationReport(chemKinValidation);
		setMyChemkinValidationReportFile(dataURL+ uuidFolderName + "/" + LOG_FILE_NAME);
		// Calls the method to convert from the intermediate XML format to OWL
		boolean owlConversionDone = convertXMLToOWL(kbFolderPath,
				dataFolderPath + uuidFolderName + "\\" + generatedXMLFileName);
		if(!owlConversionDone){
			return ERROR;
		}
		String outputOwlFile = kbFolderPath + "\\" + uuidFolderName + ".owl";
//		boolean owlConversionDone = convertXMLToOWL(ontokinBatchFilePath, ontokinJarFilePath, kbFolderPath,
//		dataFolderPath + uuidFolderName + "\\" + generatedXMLFileName);
//		String outputOwlFile = kbFolderPath + "\\" + uuidFolderName + ".owl";

		boolean consistency = true; //InconsistencyExplanation.getConsistencyOWLFile(outputOwlFile);
		setMyOWLConsistencyReport(consistency);
		gaussianUploadReport = new ChemkinUploadReport(getMyMechanismName(), getMyMechFileFileName(),
				getMyThermoFileFileName(), getMySurfaceFileFileName(), getMyTransportFileFileName(), chemKinValidation,
				dataFolderPath + uuidFolderName + "\\" + LOG_FILE_NAME, consistency);

		uploadReportList.add(gaussianUploadReport);

		if (consistency) {
			loadOntology(serverURL, uuidFolderName + ".owl", kbMechanismFolderPath, serverURL + uuidFolderName + ".owl",
					REPOSITORY_ID);
		}

		if (!consistency) {
			addFieldError("term.name", "Ontology '" + new File(outputOwlFile).getName()
					+ "' is not consistent. Owl file is not loaded into the Ontokin triple store.");
			return ERROR;

		}
		}
		return SUCCESS;

	}
	
	/**
	 * Gets the upload.
	 *
	 * @return the upload
	 */

	public List<File> getUpload() {
		return files;
	}

	/**
	 * Sets the upload.
	 *
	 * @param upload the new upload
	 */
	public void setUpload(List<File> upload) {
		this.files = upload;
	}

	/**
	 * Gets the upload file name.
	 *
	 * @return the upload file name
	 */
	public String[] getUploadFileName() {
		return uploadFileName;
	}

	/**
	 * Sets the upload file name.
	 *
	 * @param uploadFileName the new upload file name
	 */

	public void setUploadFileName(String[] uploadFileName) {
		this.uploadFileName = uploadFileName;
	}

	/**
	 * Gets the upload report list.
	 *
	 * @return the upload report list
	 */
	public List<ChemkinUploadReport> getUploadReportList() {
		return uploadReportList;
	}

	/**
	 * Sets the upload report list.
	 *
	 * @param uploadReportList the new upload report list
	 */
	public void setUploadReportList(List<ChemkinUploadReport> uploadReportList) {
		this.uploadReportList = uploadReportList;
	}

	/**
	 * Gets the gaussian upload report.
	 *
	 * @return the gaussian upload report
	 */
	public ChemkinUploadReport getGaussianUploadReport() {
		return gaussianUploadReport;
	}

	/**
	 * Sets the gaussian upload report.
	 *
	 * @param gaussianUploadReport the new gaussian upload report
	 */
	public void setGaussianUploadReport(ChemkinUploadReport gaussianUploadReport) {
		this.gaussianUploadReport = gaussianUploadReport;
	}

	/**
	 * Gets the column.
	 *
	 * @return the column
	 */

	public List<String> getColumn() {
		return column;
	}

	/**
	 * Sets the column.
	 *
	 * @param column the new column
	 */

	public void setColumn(List<String> column) {

		/**
		 * @author nk510 Assigns names for each column in table report.
		 */

		this.column = column;
	}
	
	public String getColumn1() {
		return column1;
	}

	public void setColumn1(String column1) {
		this.column1 = column1;
	}

	public String getColumn2() {
		return column2;
	}

	public void setColumn2(String column2) {
		this.column2 = column2;
	}

	public String getColumn3() {
		return column3;
	}

	public void setColumn3(String column3) {
		this.column3 = column3;
	}

	public String getColumn4() {
		return column4;
	}

	public void setColumn4(String column4) {
		this.column4 = column4;
	}

	public String getColumn5() {
		return column5;
	}

	public void setColumn5(String column5) {
		this.column5 = column5;
	}

	public String getColumn6() {
		return column6;
	}

	public void setColumn6(String column6) {
		this.column6 = column6;
	}
	
	public String getColumn7() {
		return column7;
	}

	public void setColumn7(String column7) {
		this.column7 = column7;
	}

	/**
	 * Gets the files.
	 *
	 * @return the files
	 */
	public List<File> getFiles() {
		return files;
	}

	/**
	 * Sets the files.
	 *
	 * @param files the new files
	 */
	public void setFiles(List<File> files) {
		this.files = files;
	}

	/**
	 * Gets the upload content type.
	 *
	 * @return the upload content type
	 */
	public String[] getUploadContentType() {
		return uploadContentType;
	}

	/**
	 * Sets the upload content type.
	 *
	 * @param uploadContentType the new upload content type
	 */
	public void setUploadContentType(String[] uploadContentType) {
		this.uploadContentType = uploadContentType;
	}
	
	public File getMyMechFile() {
		return myMechFile;
	}

	public void setMyMechFile(File myMechFile) {
		this.myMechFile = myMechFile;
	}

	public String getMyMechFileContentType() {
		return myMechFileContentType;
	}

	public void setMyMechFileContentType(String myMechFileContentType) {
		this.myMechFileContentType = myMechFileContentType;
	}

	public String getMyMechFileFileName() {
		return myMechFileFileName;
	}

	public void setMyMechFileFileName(String myMechFileFileName) {
		this.myMechFileFileName = myMechFileFileName;
	}
	
	public File getMyThermoFile() {
		return myThermoFile;
	}

	public void setMyThermoFile(File myThermoFile) {
		this.myThermoFile = myThermoFile;
	}

	public String getMyThermoFileContentType() {
		return myThermoFileContentType;
	}

	public void setMyThermoFileContentType(String myThermoFileContentType) {
		this.myThermoFileContentType = myThermoFileContentType;
	}

	public String getMyThermoFileFileName() {
		return myThermoFileFileName;
	}

	public void setMyThermoFileFileName(String myThermoFileFileName) {
		this.myThermoFileFileName = myThermoFileFileName;
	}

	public File getMySurfaceFile() {
		return mySurfaceFile;
	}

	public void setMySurfaceFile(File mySurfaceFile) {
		this.mySurfaceFile = mySurfaceFile;
	}

	public String getMySurfaceFileContentType() {
		return mySurfaceFileContentType;
	}

	public void setMySurfaceFileContentType(String mySurfaceFileContentType) {
		this.mySurfaceFileContentType = mySurfaceFileContentType;
	}

	public String getMySurfaceFileFileName() {
		return mySurfaceFileFileName;
	}

	public void setMySurfaceFileFileName(String mySurfaceFileFileName) {
		this.mySurfaceFileFileName = mySurfaceFileFileName;
	}

	public File getMyTransportFile() {
		return myTransportFile;
	}

	public void setMyTransportFile(File myTransportFile) {
		this.myTransportFile = myTransportFile;
	}

	public String getMyTransportFileContentType() {
		return myTransportFileContentType;
	}

	public void setMyTransportFileContentType(String myTransportFileContentType) {
		this.myTransportFileContentType = myTransportFileContentType;
	}

	public String getMyTransportFileFileName() {
		return myTransportFileFileName;
	}

	public void setMyTransportFileFileName(String myTransportFileFileName) {
		this.myTransportFileFileName = myTransportFileFileName;
	}
	
    public String getMyMechanismName() {
		return myMechanismName;
	}

	public void setMyMechanismName(String myMechanismName) {
		this.myMechanismName = myMechanismName;
	}
	
    public boolean isMyChemkinValidationReport() {
		return myChemkinValidationReport;
	}

	public void setMyChemkinValidationReport(boolean myChemkinValidationReport) {
		this.myChemkinValidationReport = myChemkinValidationReport;
	}
	
	public String getMyChemkinValidationReportFile() {
		return myChemkinValidationReportFile;
	}

	public void setMyChemkinValidationReportFile(String myChemkinValidationReportFile) {
		this.myChemkinValidationReportFile = myChemkinValidationReportFile;
	}

	public boolean isMyOWLConsistencyReport() {
		return myOWLConsistencyReport;
	}

	public void setMyOWLConsistencyReport(boolean myOWLConsistencyReport) {
		this.myOWLConsistencyReport = myOWLConsistencyReport;
	}

	/**
     * Adds double quotes to the name of the current input file regardless</br>
     * of its availability.
     *
     * @param parameter
     * @return
     */
    private String addDoubleQuoteToFileName(String parameter) {
    	if(parameter!=null){
    		parameter = "\"" + parameter + "\"";
        }
    	return parameter;
    }
    
	/**
	 * Converts a Chemkin mechanism into an intermediate XML format.
	 * 
	 */
	private boolean convertChemkinToXML(String converter, String dataFolderPath, String mechFile, String thermoFile,
			String surfaceFile, String transportFile) {
		try {
			String command = converter + " -c -i=c -o=x -p=" + dataFolderPath + " " + mechFile + " " + thermoFile + " "
					+ surfaceFile + " " + transportFile;
			if (converter != null && new File(mechFile).exists() && new File(thermoFile).exists()) {
				mechFile = addDoubleQuoteToFileName(mechFile);
				thermoFile = addDoubleQuoteToFileName(thermoFile);
				if (!(new File(surfaceFile).exists())) {
					surfaceFile = addDoubleQuoteToFileName("");
				}
				if (!(new File(transportFile).exists())) {
					transportFile = addDoubleQuoteToFileName("");
				}
				command = converter + " -c -i=c -o=x -p=" + dataFolderPath + " " + mechFile + " " + thermoFile + " "
						+ surfaceFile + " " + transportFile;
				Process p = Runtime.getRuntime().exec(command);
				p.waitFor();
				if(!new File(dataFolderPath+XML_FILENAME).exists()){
					return false;
				}
			} else {
				if (converter == null) {
					logger.error("The Chemkin to XML converter does not exist");
				}
				if (!(new File(mechFile).exists())) {
					logger.error("The following mechanism file does not exist: " + mechFile);
				}
				if (!(new File(thermoFile).exists())) {
					logger.error("The following thermo file does not exist: " + thermoFile);
				}
				logger.info("Therefore, the tool could not finish the conversion.");
			}
		} catch (IOException e) {
			logger.error("Failed to convert to an intermediate XML format.");
			e.printStackTrace();
			return false;
		} catch (InterruptedException e) {
			logger.error("Failed to convert to an intermediate XML format.");
			e.printStackTrace();
			return false;
		}
		return true;
	}

	/**
	 * Converts the intermidate XML format to OWL.
	 * 
	 */	
	private boolean convertXMLToOWL(String owlFolderPath, String xmlFilePath) {
		try {
			if (new File(xmlFilePath).exists()) {
				ArrayList<String> ctmlFiles = new ArrayList<String>();
				ctmlFiles.add(xmlFilePath);
				new CtmlConverter().convert(ctmlFiles, owlFolderPath);
			} else {
				if (!(new File(xmlFilePath).exists())) {
					logger.error("The following XML file does not exist: " + xmlFilePath);
				}
				logger.info("Therefore, the tool could not finish the conversion.");
				return false;
			}
		} catch (OWLOntologyCreationException e) {
			logger.error("Failed to create the corresponding OntoKin OWL ontology.");
			e.printStackTrace();
			return false;
		} catch (OntoException e) {
			logger.error("Failed to convert to the OntoKin OWL format.");
			e.printStackTrace();
			return false;
		}
		return true;
	}
	
	/**
	 * Loads an ontology to the Ontokin KB repository. It also creates</br>
	 * a context, which is a necessary feature to delete the mechanism</br>
	 * if user wants.
	 * 
	 * @param serverURL
	 * @param mechanismName
	 * @param mechanismFilePath
	 * @param baseURI
	 * @param repositoryID
	 * @throws OntoException
	 */
	public void loadOntology(String serverURL, String mechanismName, String mechanismFilePath, String baseURI, String repositoryID) throws Exception{
		KnowledgeRepository kr = new KnowledgeRepository();
		System.out.println("Started uploading the converted OWL file...");
		kr.uploadOntology(serverURL, repositoryID, mechanismFilePath+mechanismName);
		System.out.println("Finished uploading the converted OWL file...");
	}
}