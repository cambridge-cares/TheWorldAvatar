package com.cmclinnovations.ontochem.model.reference;

import java.util.ArrayList;
import java.util.List;

/**
 * Parses a reference text to extract DOIs. 
 * 
 * @author Feroz Farazi(msff2@cam.ac.uk)
 *
 */
public class DoiParser implements IDoiParser{
	/**
	 * The default constructor.
	 */
	public DoiParser(){
	}
	
	private static String comment = "CRECK Modeling Group (Politecnico di Milano)\n"+
		    "http://creckmodeling.chem.polimi.it/\n"+
		    "POLIMI_TOT_NOx_1412 kinetic mechanism\n"+
		    "Version 1412, December 2014\n"+
		    "Detailed and lumped mechanism of pyrolysis, partial oxidation and combustion of\n"+
		    "hydrocarbon and oxygenated fuels, with NOx. This mechanism includes all the\n"+
		    "other sub-schemes of the same version available on the website\n"+
		    "http://creckmodeling.chem.polimi.it/.\n"+
		    "High and Low Temperature Kinetic scheme: 484 species 19341 reactions\n"+
		    "References\n"+
		    "E. Ranzi, A. Frassoldati, R. Grana, A. Cuoci, T. Faravelli, A.P. Kelley, C.K.\n"+
		    "Law, Hierarchical and comparative kinetic modeling of laminar flame speeds of\n"+
		    "hydrocarbon and oxygenated fuels,\n"+
		    "Progress in Energy and Combustion Science, 38 (4), pp. 468-501 (2012), DOI:\n"+
		    "10.1016/j.pecs.2012.03.004\n"+
		    "T. Faravelli, A.Frassoldati and E. Ranzi, Kinetic modeling of the interactions\n"+
		    "between NO and hydrocarbons in the oxidation of hydrocarbons at low\n"+
		    "temperatures,\n"+
		    "Combustion and Flame, 132(1-2), pp. 188 - 207 (2003),\n"+
		    "doi:10.1016/S0010-2180(02)00437-6\n"+
		    "A. Frassoldati, T. Faravelli and E. Ranzi, Kinetic modeling of the interactions\n"+
		    "between NO and hydrocarbons at high temperature,\n"+
		    "Combustion and Flame 135, pp. 97-112 (2003), doi:10.1016/S0010-2180(03)00152-4\n"+
		    "A. Cuoci, A. Frassoldati, T. Faravelli, E. Ranzi, Formation of soot and\n"+
		    "nitrogen oxides in unsteady counterflow diffusion flames,\n"+
		    "Combustion and Flame 156 (10), pp. 2010-2022 (2009),\n"+
		    "doi:10.1016/j.combustflame.2009.06.023";
			
			/**
			 * The main method of the class containing a sample code to run</br>
			 * run the extractDOI() method.
			 * 
			 * @param args
			 */
			public static void main(String[] args) {
				List<String> dois = new DoiParser().extractDoi(comment);
				for(String doi:dois){
					System.out.println(doi);
				}
			}
			
			/**
			 * Extracts dois from a chunk of text.
			 * 
			 * @param text
			 * @return
			 */
			public List<String> extractDoi(String text){
				List<String> dois = new ArrayList<>();
				if (text != null) {
					text = text.toLowerCase();
					while (text.contains("doi")) {
						text = text.substring(text.indexOf("doi"));
						if (text.startsWith("doi:?")) {
							text = extractDoi(text, "doi:?", dois);
						} else if (text.startsWith("doi:\n")) {
							text = extractDoi(text, "doi:\n", dois);
						} else if (text.startsWith("doi:")) {
							text = extractDoi(text, "doi:", dois);
						} else if (text.contains("doi.org/")) {
							text = extractDoi(text, "doi.org/", dois);
						}
					}
				}
				return dois;
			}
			
			/**
			 * Extracts the doi that appears first in a chunk of text. 
			 * 
			 * @param text
			 * @param doiLiteral
			 * @param dois
			 * @return
			 */
			private String extractDoi(String text, String doiLiteral, List<String> dois){
				String doi = text.substring(text.indexOf(doiLiteral)+doiLiteral.length(), text.length());
				if(doi.contains("\n")){
					doi = doi.substring(0, doi.indexOf("\n"));
				}
				doi = doi.trim();
				dois.add(doi);
				System.out.println("DOI:"+doi);
				return text.substring(text.indexOf(doi)+doi.length());
			}
}
