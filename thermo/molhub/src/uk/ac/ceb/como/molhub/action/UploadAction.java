/*
 * Licensed to the Apache Software Foundation (ASF) under one
 * or more contributor license agreements.  See the NOTICE file
 * distributed with this work for additional information
 * regarding copyright ownership.  The ASF licenses this file
 * to you under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance
 * with the License.  You may obtain a copy of the License at
 *
 *  http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied.  See the License for the
 * specific language governing permissions and limitations
 * under the License.
 */

package uk.ac.ceb.como.molhub.action;

import com.opensymphony.xwork2.ActionSupport;

import uk.ac.cam.ceb.como.compchem.xslt.Transformation;
import uk.ac.cam.ceb.como.io.chem.file.jaxb.Module;
import uk.ac.cam.ceb.como.jaxb.xml.generation.GenerateXml;
import uk.ac.ceb.como.molhub.controler.ConnectionToTripleStore;
import uk.ac.ceb.como.molhub.model.FolderManager;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;

import javax.xml.transform.stream.StreamSource;

import org.eclipse.rdf4j.repository.RepositoryConnection;
import org.eclipse.rdf4j.repository.RepositoryException;
import org.eclipse.rdf4j.rio.RDFFormat;

// TODO: Auto-generated Javadoc
/**
 * @author nk510 The Class UploadAction: Uploads one of more selected Gaussian
 *         files (g09) on server and generates XML and ontology files.
 * 
 */
public class UploadAction extends ActionSupport {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = 1L;

	/** The catalina folder path. */
	private String catalinaFolderPath = System.getProperty("catalina.home");

	/** The xslt. */
	private String xslt = catalinaFolderPath + "/conf/Catalina/xslt/ontochem_rdf.xsl";

	/** The files. */
	private File[] files;

	/** The upload file name. */
	private String[] uploadFileName;

	/** The uri. */
	private String uri = "http://como.cheng.cam.ac.uk/molhub/compchem/";

	/** The rdf4j server url (localhost). */
	private String serverUrl = "http://localhost:8080/rdf4j-server/";

	/*
	 * (non-Javadoc)
	 * 
	 * @see com.opensymphony.xwork2.ActionSupport#execute()
	 */
	@Override
	public String execute() throws Exception {

		int fileNumber = 0;

		/**
		 * 
		 * @author nk510 Iterates over selected file names.
		 * 
		 */

		for (File f : files) {

			Module rootModule = new Module();

			/**
			 * @author nk510 Creates unique folder name for each uploaded Gaussian file
			 *         (g09).
			 */

			String folderName = FolderManager.generateUniqueFolderName(f.getName(), catalinaFolderPath);
			File inputG09File = new File(folderName + "/" + uploadFileName[fileNumber]);
			File outputXMLFile = new File(
					folderName + "/" + uploadFileName[fileNumber].replaceAll(".g09", "") + ".xml");

			String outputOwlPath = folderName + "/" + uploadFileName[fileNumber].replaceAll(".g09", "").toString()
					+ ".owl";

			File owlFile = new File(outputOwlPath);

			FolderManager.createFolder(folderName);

			FolderManager.saveFileInFolder(inputG09File, f.getAbsolutePath());

			GenerateXml.generateRootModule(inputG09File, outputXMLFile, rootModule);

			/**
			 * 
			 * @author nk510 Runs Xslt transformation.
			 * 
			 */
			Transformation.trasnformation(new FileInputStream(outputXMLFile.getPath()), new FileOutputStream(owlFile),
					new StreamSource(xslt));

			/**
			 * 
			 * @author nk510 Adding generated ontology files (owl) into RDF4J triple store.
			 * 
			 */
			try (RepositoryConnection connection = ConnectionToTripleStore.getRepositoryConnection(serverUrl,
					"compchemkb")) {
				/**
				 * @author nk510 Beginning of transaction.
				 */
				connection.begin();
				try {
					/**
					 * @author nk510 Each generated owl file will be stored in RDF4J triple store.
					 */
					connection.add(owlFile, uri, RDFFormat.RDFXML);
					connection.commit();
				} catch (RepositoryException e) {
					/**
					 * If something is wrong during the transaction, it will return a message about
					 * it.
					 */
					connection.rollback();
				}

				connection.close();
			}

			fileNumber++;

		}

		addActionMessage("Uploading files (g09, xml, owl) successfully completed.");

		return INPUT;

	}

	/**
	 * Gets the upload.
	 *
	 * @return the upload
	 */
	public File[] getUpload() {
		return files;
	}

	/**
	 * Sets the upload.
	 *
	 * @param upload
	 *            the new upload
	 */
	public void setUpload(File[] upload) {
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
	 * @param uploadFileName
	 *            the new upload file name
	 */
	public void setUploadFileName(String[] uploadFileName) {
		this.uploadFileName = uploadFileName;
	}
}