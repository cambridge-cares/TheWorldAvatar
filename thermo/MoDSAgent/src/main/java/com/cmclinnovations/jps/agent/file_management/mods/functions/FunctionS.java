package com.cmclinnovations.jps.agent.file_management.mods.functions;

import java.util.ArrayList;

import javax.xml.bind.annotation.XmlElement;

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
