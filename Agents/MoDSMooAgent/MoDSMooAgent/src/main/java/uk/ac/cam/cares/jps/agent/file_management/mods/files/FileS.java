package uk.ac.cam.cares.jps.agent.file_management.mods.files;

import java.util.ArrayList;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;

import com.fasterxml.jackson.annotation.JsonProperty;

import uk.ac.cam.cares.jps.agent.file_management.mods.parameters.InitialRead;
import uk.ac.cam.cares.jps.agent.file_management.mods.parameters.WorkingRead;
import uk.ac.cam.cares.jps.agent.file_management.mods.parameters.WorkingWrite;

@XmlAccessorType(XmlAccessType.FIELD)
public class FileS {
	@JsonProperty("file")
	@XmlElement(name = "file")
	private ArrayList<File> fileList;

	public ArrayList<File> getFile() {
		return fileList;
	}

	public void setFile(ArrayList<File> fileList) {
		this.fileList = fileList;
	}
	
	@JsonProperty("initialRead")
	@XmlElement(name = "initial_read")
	private InitialRead initialRead;

	public InitialRead getInitialRead() {
		return initialRead;
	}

	public void setInitialRead(InitialRead initialRead) {
		this.initialRead = initialRead;
	}
	
	@JsonProperty("workingRead")
	@XmlElement(name = "working_read")
	private WorkingRead workingRead;

	public WorkingRead getWorkingRead() {
		return workingRead;
	}

	public void setWorkingRead(WorkingRead workingRead) {
		this.workingRead = workingRead;
	}
	
	@JsonProperty("workingWrite")
	@XmlElement(name = "working_write")
	private WorkingWrite workingWrite;

	public WorkingWrite getWorkingWrite() {
		return workingWrite;
	}

	public void setWorkingWrite(WorkingWrite workingWrite) {
		this.workingWrite = workingWrite;
	}
}
