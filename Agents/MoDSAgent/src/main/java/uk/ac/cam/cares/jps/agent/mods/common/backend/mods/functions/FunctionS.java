package uk.ac.cam.cares.jps.agent.mods.common.backend.mods.functions;

import java.util.ArrayList;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;

@XmlAccessorType(XmlAccessType.FIELD)
public class FunctionS {
	@XmlElement(name = "function")
	private ArrayList<Function> functionList;
	
	public ArrayList<Function> getFunction() {
		return functionList;
	}
	
	public void setFunction(ArrayList<Function> functionList) {
		this.functionList = functionList;
	}
}
