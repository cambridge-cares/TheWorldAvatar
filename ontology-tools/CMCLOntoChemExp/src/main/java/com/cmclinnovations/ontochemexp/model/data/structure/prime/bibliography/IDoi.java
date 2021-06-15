package com.cmclinnovations.ontochemexp.model.data.structure.prime.bibliography;

import java.util.List;

import com.cmclinnovations.ontochemexp.model.exception.OntoChemExpException;

public interface IDoi {
	public void extractContent() throws OntoChemExpException;
	public void add(String doi);
}
