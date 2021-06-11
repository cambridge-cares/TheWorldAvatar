package com.cmclinnovations.ontochemexp.model.configuration;

import java.util.Map;

import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.PropertySource;

/**
 * Reads many to one mapping between all PrIMe Property and OntoChemExp 
 * Dimension Quantity classes provided in the 
 * ontochemexp.dimensional.quantity.mapping.properties file. 
 * 
 * This will empower users to use OntoChemExp, if some of its Dimensional 
 * Quantity classes, or name spaces change at a later stage, without changing 
 * its source code.
 * 
 * @author Jiaru Bai (jb2197@cam.ac.uk)
 *
 */
@Configuration
@PropertySource("classpath:ontochemexp.dimensional.quantity.mapping.properties")
public class DimensionalQuantityMapping {
	@Value("#{${map.between.prime.and.ontochemexp}}")
	private Map<String, String> dimensionalQuantityMapping;

	public Map<String, String> getDimensionalQuantityMapping() {
		return dimensionalQuantityMapping;
	}

	public void setDimensionalQuantityMapping(Map<String, String> dimensionalQuantityMapping) {
		this.dimensionalQuantityMapping = dimensionalQuantityMapping;
	}
	
	public String getOntoChemExpDimensionalQuantityEntry(String key) {
		return dimensionalQuantityMapping.get(key);
	}	
}
