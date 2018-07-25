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
import uk.ac.ceb.como.molhub.model.FolderManager;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;

import javax.xml.transform.stream.StreamSource;

public class UploadAction extends ActionSupport {

	private static final long serialVersionUID = 1L;
	private String catalicaFolderPath = System.getProperty("catalina.home");
	private String xslt = catalicaFolderPath + "/conf/Catalina/xslt/ontochem_rdf.xsl";

	private File[] files;
	private String[] uploadFileName;
	

	@Override
	public String execute() throws Exception {

		int fileNumber = 0;

		/**
		 * @author nk510 Iterates over selected file names.
		 */

		for (File f : files) {

			Module rootModule = new Module();

			/**
			 * @author nk510 Creates unique folder name for each uploaded Gaussian file
			 *         (g09).
			 */
			String folderName = FolderManager.generateUniqueFolderName(f.getName(), catalicaFolderPath);
			File inputG09File = new File(folderName + "/" + uploadFileName[fileNumber]);
			File outputXMLFile = new File(
					folderName + "/" + uploadFileName[fileNumber].replaceAll(".g09", "") + ".xml");

			String outputOwlFile = folderName + "/" + uploadFileName[fileNumber].replaceAll(".g09", "").toString()
					+ ".owl";

			FolderManager.createFolder(folderName);

			FolderManager.saveFileInFolder(inputG09File, f.getAbsolutePath());

			GenerateXml.generateRootModule(inputG09File, outputXMLFile, rootModule);

			/**
			 * @author nk510 Runs Xslt transformation.
			 */
			Transformation.trasnformation(new FileInputStream(outputXMLFile.getPath()),
					new FileOutputStream(new File(outputOwlFile)), new StreamSource(xslt));

			fileNumber++;

		}

		addActionMessage("Uploading Gaussian files successfully completed.");

		return INPUT;
	}

	public void validate() {

		setUpload(files);
		setUploadFileName(uploadFileName);
		/**
		 * @author nk510 Checks whether there are no selected Gaussian files (g09) for parsing.
		 */

		if (this.getUploadFileName().length==0) {

			addFieldError("uploadFileName.length", "No Gaussian files (g09) are selected.");
		}
	}

	public File[] getUpload() {
		return files;
	}

	public void setUpload(File[] upload) {
		this.files = upload;
	}

	public String[] getUploadFileName() {
		return uploadFileName;
	}

	public void setUploadFileName(String[] uploadFileName) {
		this.uploadFileName = uploadFileName;
	}
}