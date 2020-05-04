package com.cmclinnovations.jps.agent.file_management.mods.files;

import java.util.ArrayList;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;

import com.cmclinnovations.jps.agent.file_management.mods.parameters.InitialRead;
import com.cmclinnovations.jps.agent.file_management.mods.parameters.WorkingWrite;

@XmlAccessorType(XmlAccessType.FIELD)
public class FileS {
	@XmlElement(name = "file")
	private ArrayList<File> fileList;

	public ArrayList<File> getFile() {
		return fileList;
	}

	public void setFile(ArrayList<File> fileList) {
		this.fileList = fileList;
	}

	@XmlElement(name = "initial_read")
	private InitialRead initialRead;

	public InitialRead getInitialRead() {
		return initialRead;
	}

	public void setInitialRead(InitialRead initialRead) {
		this.initialRead = initialRead;
	}

	@XmlElement(name = "working_write")
	private WorkingWrite workingWrite;

	public WorkingWrite getWorkingWrite() {
		return workingWrite;
	}

	public void setWorkingWrite(WorkingWrite workingWrite) {
		this.workingWrite = workingWrite;
	}
}
