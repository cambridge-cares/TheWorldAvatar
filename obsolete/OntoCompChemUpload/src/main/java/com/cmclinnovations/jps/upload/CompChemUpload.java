package com.cmclinnovations.jps.upload;
/**
 * This class includes functionalities to upload ontologies to the<br>
 * OntoCompChem repository. 
 * 
 * @author msff2
 *
 */

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.net.URL;
import java.util.Properties;

import okhttp3.MediaType;
import okhttp3.MultipartBody;
import okhttp3.OkHttpClient;
import okhttp3.Request;
import okhttp3.RequestBody;
import okhttp3.Response;


public class CompChemUpload {
	/**
	 * The name of calculation file.
	 */
	private String calculationFileName;
	
	/**
	 * Absolute path to the file including the file name, e.g.<br>
	 * C:/Users/msff2/DFTAgent_435827288195609/login-skylake.hpc.cam.ac.uk_113420049902577/login-skylake.hpc.cam.ac.uk_113420049902577.log 
	 */
	private String calculationFilePath;
	
	/**
	 * The IRI of species that corresponds to the current calculation file.
	 */
	private String ontoSpeciesIRI;
	/**
	 * Reads the property file. 
	 */
	Properties prop;
	/**
	 * The default constructor.
	 */
	public CompChemUpload(){
	}
	
	/**
	 * A constructor with all parameters required to call the upload method.
	 * 
	 * @param calculationFileName the name of calculation file, e.g.<br> 
	 * login-skylake.hpc.cam.ac.uk_113420049902577.log
	 * @param calculationFilePath the absolute path to the calculation file<br>
	 * including the file name, e.g.<br>
	 * C:/Users/msff2/DFTAgent_435827288195609/login-skylake.hpc.cam.ac.uk_113420049902577/login-skylake.hpc.cam.ac.uk_113420049902577.log
	 * @param ontoSpeciesIRI the IRI of species (in OntoSpecies) to which the current<br>
	 * calculation is connected to, e.g.<br>
	 * http://www.theworldavatar.com/kb/ontospecies/3ad49265-0d58-3827-ba0f-18201693f82b.owl#3ad49265-0d58-3827-ba0f-18201693f82b
	 */
	public CompChemUpload(String calculationFileName, String calculationFilePath, String ontoSpeciesIRI){
		this.calculationFileName = calculationFileName;
		this.calculationFilePath = calculationFilePath;
		this.ontoSpeciesIRI = ontoSpeciesIRI;
	}
	
	/**
	 * When the following three parameters are set, it uploads the<br>
	 * corresponding calculation to the OntoCompChem repository:
	 * - calculationFileName
	 * - calculationFilePath
	 * - ontoSpeciesIRI
	 * 
	 * @throws IOException
	 */
	public String upload() throws Exception {
		init();
		if(getCalculationFileName() == null || getCalculationFileName().trim().isEmpty()){
			throw new Exception("Claculation File Name is not provided.");
		}
		if(getCalculationFilePath() == null || getCalculationFilePath().trim().isEmpty()){
			throw new Exception("Claculation File Path is not provided.");
		}
		if(prop == null){
			throw new IOException("Calculation upload property file could not be initialised.");
		}
		String molhubUploadURL = prop.getProperty("molhub.upload.url");
		OkHttpClient client = new OkHttpClient().newBuilder().build();
		RequestBody body = null;
		// Uploads without the IRI of species in OntoSpecies
		if(getOntoSpeciesIRI() == null || getOntoSpeciesIRI().trim().isEmpty()){
			body = uploadWithoutOntoSpeciesIRI(getCalculationFileName(), getCalculationFilePath());
		} else{ // Uploads with the IRI of species in OntoSpecies
			checkURLValidity(getOntoSpeciesIRI());
			body = uploadWithOntoSpeciesIRI(getCalculationFileName(), getCalculationFilePath(), getOntoSpeciesIRI());
		}
		Response response = null;
		if(body!=null){
			Request request = new Request.Builder().url(molhubUploadURL).method("POST", body)
					.addHeader("Content-Type", "multipart/form-data").build();
			response = client.newCall(request).execute();
			InputStream inputStream = response.body().byteStream();
			int data = inputStream.read();
			String line = "";
			while(data != -1) {
				data = inputStream.read();
				line = line + (char)data;
				if(data == 13){
					if(line.trim().startsWith("<td")){
						if(line.trim().startsWith("<td class=\"nowrap\">") && line.trim().endsWith("</td>")){
							line = line.trim().replace("<td class=\"nowrap\">", "");
							line = line.trim().replace("</td>", "");
							return line;
						}
					}
					line = "";
				}
			}
			inputStream.close();
		}
		return null;
	}
	/**
	 * Initialises the property file reading facility and loads the file. 
	 * 
	 * @throws IOException
	 */
	private void init() throws IOException{
		if(prop == null){
			prop = new Properties();
			InputStream stream = CompChemUpload.class.getClassLoader().getResourceAsStream("compchem-upload.properties");
			prop.load(stream);
		}
	}
	
	/**
	 * It uploads a calculation and establishes a link between the<br>
	 * calculation OWL file and the corresponding OntoSpecies IRI.
	 * 
	 * @param calculationFileName
	 * @param calculationFilePath
	 * @param ontoSpeciesIRI
	 * @return
	 */
	private RequestBody uploadWithOntoSpeciesIRI(String calculationFileName, String calculationFilePath, String ontoSpeciesIRI){
		return new MultipartBody.Builder().setType(MultipartBody.FORM)
				.addFormDataPart("upload", calculationFileName,
						RequestBody.create(MediaType.parse("application/octet-stream"),
								new File(calculationFilePath)))
				.addFormDataPart("ontoSpeciesIRI", ontoSpeciesIRI)
				.build();
	}
	
	/**
	 * It uploads a calculation and doesn't include the corresponding<br>
	 * OntoSpecies IRI.
	 * 
	 * @param calculationFileName
	 * @param calculationPath
	 * @return
	 */
	private RequestBody uploadWithoutOntoSpeciesIRI(String calculationFileName, String calculationPath){
		return new MultipartBody.Builder().setType(MultipartBody.FORM)
				.addFormDataPart("upload", calculationFileName,
						RequestBody.create(MediaType.parse("application/octet-stream"),
								new File(calculationPath)))
				.build();
	}

	public String getCalculationFileName() {
		return calculationFileName;
	}

	public void setCalculationFileName(String calculationFileName) {
		this.calculationFileName = calculationFileName;
	}

	public String getCalculationFilePath() {
		return calculationFilePath;
	}

	public void setCalculationFilePath(String calculationFilePath) {
		this.calculationFilePath = calculationFilePath;
	}

	public String getOntoSpeciesIRI() {
		return ontoSpeciesIRI;
	}

	public void setOntoSpeciesIRI(String ontoSpeciesIRI) {
		this.ontoSpeciesIRI = ontoSpeciesIRI;
	}
	
	/**
	 * Checks the validity of a URL.
	 * 
	 * @param iri
	 * @throws OntoException
	 */
	private void checkURLValidity(String iri) throws Exception{
		if(iri==null){
				throw new Exception("Provided IRI is null.");
		}
		if(iri.isEmpty()){
			throw new Exception("Provided IRI is empty.");
		}
		try{
			URL url = new URL(iri);
			url.toURI();
		}catch(Exception e){
			throw new Exception("Provided IRI is not a valid URL.");
		}
	}
}
