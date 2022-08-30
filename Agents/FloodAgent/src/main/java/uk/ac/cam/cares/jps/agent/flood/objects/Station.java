package uk.ac.cam.cares.jps.agent.flood.objects;

import java.time.Instant;
import java.time.temporal.ChronoUnit;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Comparator;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.NoSuchElementException;
import java.util.Optional;
import java.util.stream.Collectors;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import uk.ac.cam.cares.jps.base.timeseries.TimeSeries;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeriesClient;

public class Station {
	private static final Logger LOGGER = LogManager.getLogger(Station.class);
	
    private String iri;
    private int visId;
    private double lat;
    private double lon;
    private String identifier;
    private String label;
    private String river;
    private String catchment;
    private String town;
    private String dateOpened;
    private Map<String, String> displayProperties;
    private List<TimeSeries<Instant>> tsList;
	private List<Measure> measures;

	private Station downstream;
	private Station upstream;

	private Double stageUpper = null;
	private Double stageLower = null;
	private Double downstageUpper = null;
	private Double downstageLower = null;

    // icons to use
    static Map<String, String> icons = new HashMap<>();
	private static final String WATER_LEVEL = "Water Level";
	static {
		icons.put(WATER_LEVEL, "ea-water-level");
		icons.put("Flow", "ea-flow");
		icons.put("Rainfall", "ea-rainfall");
		icons.put("Wind", "ea-wind");
		icons.put("Temperature", "ea-temperature");
	}
    
    public Station(String iri) {
    	this.iri = iri;
    	this.identifier = "";
    	this.label = "";
    	this.river = "";
    	this.catchment = "";
    	this.town = "";
    	this.dateOpened = "";
    	this.displayProperties = new HashMap<>();
    	this.tsList = new ArrayList<>();
    	this.measures = new ArrayList<>();
    }
    
	public void addMeasure(Measure measure) {
		boolean duplicateExists = false;
		for (Measure localmeasure : this.measures) {
			if (localmeasure.getIri().contentEquals(measure.getIri())) {
				duplicateExists = true;
				LOGGER.warn("Duplicate measure detected in station");
			}
		}
		if (!duplicateExists) {
			this.measures.add(measure);
		}
	}

	// return measures in this station
    public List<Measure> getMeasures() {
    	return this.measures; 
    }
    
    public void setLabel(String label) {
    	this.label = label;
    	this.displayProperties.put("Name", label);
    }
    
    public String getLabel() {
    	return this.label;
    }
    
    public void setRiver(String river) {
    	this.river = river;
    	this.displayProperties.put("River", river);
    }
    
    public String getRiver() {
    	return this.river;
    }
    
    public void setCatchment(String catchment) {
    	this.catchment = catchment;
    	this.displayProperties.put("Catchment", catchment);
    }
    
    public String getCatchment() {
    	return this.catchment;
    }
    
    public void setTown(String town) {
    	this.town = town;
    	this.displayProperties.put("Town", town);
    }
    
    public String getTown() {
    	return this.town;
    }
    
    public void setDateOpened(String dateOpened) {
    	this.dateOpened = dateOpened;
    	this.displayProperties.put("Date opened", dateOpened);
    }
    
    public String getDateOpened() {
    	return this.dateOpened;
    }
    
    public void setVisId(int visId) {
    	this.visId = visId;
    }
    
    public void setLat(double lat) {
    	this.lat = lat;
    	this.displayProperties.put("Latitude", String.valueOf(lat));
    }
    
    public void setLon(double lon) {
    	this.lon = lon;
    	this.displayProperties.put("Longitude", String.valueOf(lon));
    }
    
    public void setIdentifier(String identifier) {
    	this.identifier = identifier;
    	this.displayProperties.put("Identifier", identifier);
    }
    
    public String getIri() {
    	return this.iri;
    }
    
    public String getIdentifier() {
    	return this.identifier;
    }
    
    public double getLat() {
    	return this.lat;
    }
    
    public double getLon() {
    	return this.lon;
    }
    
    public int getVisId() {
    	return this.visId;
    }
    
    public Map<String, String> getDisplayProperties() {
    	return this.displayProperties;
    }
    
    public void addTimeseries(TimeSeries<Instant> ts) {
    	this.tsList.add(ts);
    }
    
    public List<TimeSeries<Instant>> getTimeSeriesList() {
    	return this.tsList;
    }
    
    // combine time series list into a single time series object
    // time series client is needed to query if value needed is from the day before
    public TimeSeries<Instant> getCombinedTimeSeries(TimeSeriesClient<Instant> tsClient) {
    	if (this.tsList.size() > 1) {
    		// this will sort in ascending order
    		List<TimeSeries<Instant>> tsSorted = this.tsList.stream().sorted(Comparator.comparing(ts -> ts.getTimes().size())).collect(Collectors.toList());
    		List<Instant> longestTimeList = tsSorted.get(tsSorted.size()-1).getTimes();
    		
    		List<List<?>> valuesList = new ArrayList<>();
    		List<String> dataIRIs = new ArrayList<>();
    		
    		for (int i = 0; i < tsSorted.size() ; i++) {
    			TimeSeries<Instant> ts = tsSorted.get(i);
    			List<Double> values = new ArrayList<>();
    			// each time series has one column
    			String dataIRI = ts.getDataIRIs().get(0);
    			
    			// need to query data from the day before
    			Double valueBefore = null;
    			if (ts.getTimes().get(0).isAfter(longestTimeList.get(0))) {
    				TimeSeries<Instant> extraInfo = tsClient.getTimeSeriesWithinBounds(Arrays.asList(dataIRI), longestTimeList.get(0).minus(1, ChronoUnit.DAYS), longestTimeList.get(0));
    				
    				if (extraInfo.getTimes().isEmpty()) {
    					// could potentially implement a while loop here
    					LOGGER.warn("getCombinedTimeSeries: no extra data obtained");
    					valueBefore = 0.0;
    				} else {
    					// get final value in the list
    					valueBefore = extraInfo.getValuesAsDouble(dataIRI).get(extraInfo.getValuesAsDouble(dataIRI).size()-1);
    				}
    			}
    			for (Instant time : longestTimeList) {
    				int index; // work out which index to extract value from
    				if (ts.getTimes().contains(time)) {
    					index = ts.getTimes().indexOf(time);
    					
    				} else if (time.isAfter(ts.getTimes().get(ts.getTimes().size()-1))) {
    					// get final index
    					index = ts.getTimes().size() - 1;
    				
    				} else {
    					// this gives the element right after the time point
    					Instant t1;
						Optional<Instant> optionalTime = ts.getTimes().stream().filter(t -> t.isAfter(time)).findFirst();
						if (optionalTime.isPresent()) {
							t1 = optionalTime.get();
						} else {
							throw new NoSuchElementException("Error in getCombinedTimeSeries");
						}
    					index = ts.getTimes().indexOf(t1) - 1;
    				}
    				
    				if (index < 0) {
    					values.add(valueBefore);
    				} else {
    					values.add(ts.getValuesAsDouble(dataIRI).get(index));
    				}
    			}
    			
    			dataIRIs.add(dataIRI);
    			valuesList.add(values);
    		}
    		
    		return new TimeSeries<>(longestTimeList, dataIRIs, valuesList);
    	} else {
    		return this.tsList.get(0);
    	}
    }
    
    // some stations measure more than 1 property, at the moment icon is only determined from one of them
    public String getIconImage() {
		if (!measures.isEmpty()) {
			if (icons.containsKey(this.measures.get(0).getParameterName())) {
				return icons.get(this.measures.get(0).getParameterName());
			} else {
				return icons.get(WATER_LEVEL);
			}
		} else {
			return icons.get(WATER_LEVEL);
		}
    }
    
    /**
     * this is the text that appears below the display name
     * @return
     */
    public String getDescription() {
    	return "The Environmental Agency stations primarily measure river properties such as water level and flowrate. "
		+ "Some stations measure rainfall, wind and temperature.";
    }

	public void setStageUpper(double stageUpper) {
		this.stageUpper = stageUpper;
	}

	public void setStageLower(double stageLower) {
		this.stageLower = stageLower;
	}

	public Double getStageUpper() {
		return this.stageUpper;
	}

	public Double getStageLower() {
		return this.stageLower;
	}

	public void setDownstageUpper(double downstageUpper) {
		this.downstageUpper = downstageUpper;
	}

	public void setDownstageLower(double downstageLower) {
		this.downstageLower = downstageLower;
	}

	public Double getDownstageUpper() {
		return this.downstageUpper;
	}

	public Double getDownstageLower() {
		return this.downstageLower;
	}

	public void setDownstream(Station downstream) {
		this.downstream = downstream;
	}

	public Station getDownstream() {
		return this.downstream;
	}

	public void setUpstream(Station upstream) {
		this.upstream = upstream;
	}

	public Station getUpstream() {
		return this.upstream;
	}
}
