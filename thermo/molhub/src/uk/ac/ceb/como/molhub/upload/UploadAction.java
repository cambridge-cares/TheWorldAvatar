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

package uk.ac.ceb.como.molhub.upload;

import com.opensymphony.xwork2.ActionSupport;

import uk.ac.cam.ceb.como.io.chem.file.jaxb.Module;
import uk.ac.cam.ceb.como.jaxb.xml.generation.GenerateCompChemXML;

import java.io.BufferedOutputStream;
import java.io.File;
import java.io.FileOutputStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;

import javax.servlet.ServletContext;

import org.apache.struts2.util.ServletContextAware;

public class UploadAction extends ActionSupport implements ServletContextAware{

	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	
	private File[] files;
	private String[] uploadFileName;
	private String[] uploadContentType;

	private String filesPath;
	
	private ServletContext context;

	@Override
	public String execute() throws Exception {
		 
		String catalicaFolderPath = System.getProperty("catalina.home");
		
		for(File f: files) {
		
		Module rootModule = new Module();
	
		String fileName = f.getName().replaceAll(".tmp","");
		
		File inputFile = new File(catalicaFolderPath + "/conf/Catalina/" + fileName + ".g09");
		
		System.out.println("outputFile.getAbsolutePath() : " +  inputFile.getAbsolutePath());
		
		Path path = Paths.get(f.getAbsolutePath());
		
		byte[] data = Files.readAllBytes(path);
		
		BufferedOutputStream stream = new BufferedOutputStream(new FileOutputStream(inputFile));
		
		stream.write(data);
        stream.close();
        
		File outputFile = new File(catalicaFolderPath + "/conf/Catalina/" + uploadFileName[0] + ".xml");
        
        GenerateCompChemXML.generateRootModule(inputFile, outputFile, rootModule);	
        
		
		}  
		return INPUT;
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

	public String[] getUploadContentType() {
		return uploadContentType;
	}

	public void setUploadContentType(String[] uploadContentType) {
		this.uploadContentType = uploadContentType;
	}
	
	public String getFilesPath() {
		return filesPath;
	}

	public void setFilesPath(String filesPath) {
		this.filesPath = filesPath;
	}

	public ServletContext getContext() {
		return context;
	}

	public void setContext(ServletContext context) {
		this.context = context;
	}


	@Override
	public void setServletContext(ServletContext arg0) {
		// TODO Auto-generated method stub
		
	}

}
