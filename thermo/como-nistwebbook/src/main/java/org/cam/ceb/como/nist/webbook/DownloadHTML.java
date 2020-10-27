package org.cam.ceb.como.nist.webbook;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStreamReader;
import java.net.URL;
import java.net.URLConnection;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.TimeUnit;

import org.cam.ceb.como.chem.filemgmt.filter.HTMLFileFilter;
import org.cam.ceb.como.nist.webbook.filter.NISTSpeciesFilterByComposition;
import org.cam.ceb.como.nist.webbook.info.NISTInfoReader;
import org.cam.ceb.como.nist.webbook.info.NISTSpeciesId;
import org.cam.ceb.como.nist.webbook.info.NISTSpeciesInfo;
import org.cam.ceb.como.nist.webbook.parser.NISTSpeciesList;
import org.cam.ceb.como.nist.webbook.parser.NISTSpeciesListParser;
import org.cam.ceb.como.tools.file.writer.StringWriter;

/**
 * 
 * @author NK510
 * Examples of downloading data from NIST Webbook, and parsing these data stored in html format.
 *
 */
public class DownloadHTML {

	public static void main(String[] args) throws Exception {

		/**
		 * A map created to store the type of parsed file vs information extracted from the file. 
		 */
		Map<String, NISTSpeciesInfo> data = new HashMap<String, NISTSpeciesInfo>();
		parsingHTML("D:\\msff2\\Documents\\Data\\NIST\\ChemSpecies\\html\\", data);
	}
	
	static int speciesNo = 0;
	
	public static void parsingHTML(String htmlFolderPath, Map<String, NISTSpeciesInfo> data) throws Exception {

		// TODO Auto-generated method stub

		HTMLFileFilter htmlFilter = new HTMLFileFilter();

		// read the html files
		Collection<File> htmlFiles = htmlFilter.getValidFiles(htmlFolderPath, true);

		for (File f : htmlFiles) {

			NISTInfoReader reader = new NISTInfoReader();
			
			reader.setPath(f.getAbsolutePath());
			reader.parse();

			data.put(f.getName().replace(".html", ""), (NISTSpeciesInfo) reader.get());
			++speciesNo;
			System.out.println("###########################");
			System.out.println("Parsed file:"+ speciesNo + " / " + htmlFiles.size());
			System.out.println("###########################");
			System.out.print("Formula elements: ");
//			display(data, htmlFiles);
		}
	}

	public static void display(Map<String, NISTSpeciesInfo> data, Collection<File> htmlFiles){
		for (Map.Entry<String, NISTSpeciesInfo> speciesData : data.entrySet()) {
			
			NISTSpeciesInfo value = speciesData.getValue();
			
			String formula = value.getFormula().getFormula();

			Collection<String> formulaElements = value.getFormula().getElements();

			for (String formulaElement : formulaElements) {

				System.out.print(formulaElement + "  ");
			}

			System.out.println("");

			System.out.println("formula: " + formula);

			System.out.println("Molecular weight: " + value.getMolecularWeight());

			if (value.getInChIKey() != "") {
				
				System.out.println("InChIKey : " + value.getInChIKey());
				
			}

			if (value.getInChIKey() != "") {
				
				System.out.println("InChI: " + value.getInChI());
				
			}

			System.out.println("CAS Reg Number " + value.getCASRegNr());

			Collection<String> otherNames = value.getOtherNames();
			
			// other names
			if (!otherNames.isEmpty()) {

				System.out.print("Other names: ");

				for (String otherName : otherNames) {

					System.out.print(otherName + ", ");
				}
			}
			
			System.out.println();
			
			if(!value.getPermanentLink().isEmpty()) {
				
				System.out.println("Permanent lik: " + value.getPermanentLink().trim());
			}

			Collection<String> isotopologuesCollection = value.getIsotopologues();

			if (!isotopologuesCollection.isEmpty()) {

				System.out.println("Isotopologues :");

				for (String isotop : isotopologuesCollection) {

					System.out.print(isotop + " ");

				}
			}
			System.out.println("==============================================================");

		}
	}
	
	public static void display(NISTSpeciesInfo value){
			
			String formula = value.getFormula().getFormula();

			Collection<String> formulaElements = value.getFormula().getElements();

			System.out.print("Formula elements: ");

			for (String formulaElement : formulaElements) {

				System.out.print(formulaElement + "  ");
			}

			System.out.println("");

			System.out.println("formula: " + formula);

			System.out.println("Molecular weight: " + value.getMolecularWeight());

			if (value.getInChIKey() != "") {
				
				System.out.println("InChIKey : " + value.getInChIKey());
				
			}

			if (value.getInChIKey() != "") {
				
				System.out.println("InChI: " + value.getInChI());
				
			}

			System.out.println("CAS Reg Number " + value.getCASRegNr());

			Collection<String> otherNames = value.getOtherNames();
			
			// other names
			if (!otherNames.isEmpty()) {

				System.out.print("Other names: ");

				for (String otherName : otherNames) {

					System.out.print(otherName + ", ");
				}
			}
			
			System.out.println();
			
			if(!value.getPermanentLink().isEmpty()) {
				
				System.out.println("Permanent lik: " + value.getPermanentLink().trim());
			}

			Collection<String> isotopologuesCollection = value.getIsotopologues();

			if (!isotopologuesCollection.isEmpty()) {

				System.out.println("Isotopologues :");

				for (String isotop : isotopologuesCollection) {

					System.out.print(isotop + " ");

				}
			}
			System.out.println("");
	}
	
	public static void downloadSdfAndMolFiles(String speciesFilePath, String urlForSdfFile, String urlForMolFile,
			String destinationFolderPath) throws FileNotFoundException, IOException {

		NISTSpeciesListParser nistSpeciesListParser = new NISTSpeciesListParser();

		nistSpeciesListParser.setPath(speciesFilePath);

		nistSpeciesListParser.parse();

		NISTSpeciesList nistSpeciesList = nistSpeciesListParser.getNISTSpeciesList();

		List<NISTSpeciesId> listNistSpeciesId = nistSpeciesList.get();

		int count = 0;

		for (NISTSpeciesId nistSpeciesId : listNistSpeciesId) {

			if (!nistSpeciesId.getCASRegNr().matches("N/A")) {

				System.out.println(nistSpeciesId.getName() + " " + nistSpeciesId.getFormula() + ", "
						+ nistSpeciesId.getCASRegNr());

				count++;

				URLDownload.fileUrl(urlForSdfFile + nistSpeciesId.getCASRegNr(), nistSpeciesId.getCASRegNr() + ".sdf",
						destinationFolderPath);

				URLDownload.fileUrl(urlForMolFile + nistSpeciesId.getCASRegNr(), nistSpeciesId.getCASRegNr() + ".mol",
						destinationFolderPath);

			}

		}

		System.out.println("- - -  - - - - -  - - - - - - ");

		System.out.println("Number of species: " + count);

	}

	public static void writeHTMLFile(String casRegNr, File dest, String category)
			throws FileNotFoundException, IOException, Exception {

		URL oracle = new URL("https://webbook.nist.gov/cgi/cbook.cgi?ID=C" + casRegNr.replace("-", "") + "&Units=SI&Mask=1" + category); // Registry Number Not Found

		URLConnection yc = oracle.openConnection();

		BufferedReader in = new BufferedReader(new InputStreamReader(yc.getInputStream()));

		String inputLine;

		String content = "";

		while ((inputLine = in.readLine()) != null) {

			content += inputLine + System.getProperty("line.separator");
		}

		in.close();

		StringWriter writer = new StringWriter();

		writer.setContent(content);

		writer.setOverwrite(false);

		writer.setPath(dest.getAbsolutePath());

		writer.write();
	}

	public static void downloadHTMLFile(String speciesListFilePath, String destinationFolderPath) throws Exception {

		NISTSpeciesListParser nistSpeciesListParser = new NISTSpeciesListParser();

		nistSpeciesListParser.setPath(speciesListFilePath);

		nistSpeciesListParser.parse();

		NISTSpeciesList nistSpeciesList = nistSpeciesListParser.getNISTSpeciesList();

		NISTSpeciesList nistSpeciesListWithCAS = new NISTSpeciesList();

		NISTSpeciesId nistSpeciesId;

		while ((nistSpeciesId = nistSpeciesList.next()) != null) {

			if (!nistSpeciesId.getCASRegNr().equals("N/A") & nistSpeciesId.getCASRegNr().contains("-")) {

				nistSpeciesListWithCAS.add(nistSpeciesId);
			}
		}

		int ctr = 0;

		while ((nistSpeciesId = nistSpeciesListWithCAS.next()) != null) {

			ctr++;

			System.out.println("Retrieving species data from " + nistSpeciesId.getCASRegNr() + " - " + ctr + " / "
					+ nistSpeciesListWithCAS.get().size());

			writeHTMLFile(nistSpeciesId.getCASRegNr(),
					new File(destinationFolderPath + "html\\" + nistSpeciesId.getCASRegNr() + ".html"), "#Thermo-Gas");

			Thread.sleep(500);

			System.out.println(ctr + " Files created!");

			Thread.sleep(500);

		}

	}

	public static void downloadFileredSpeciesHTMLFile(String speciesListFilePath, String destinationFolderPath)
			throws FileNotFoundException, IOException, Exception {

		NISTSpeciesListParser parser = new NISTSpeciesListParser();
		parser.setPath(speciesListFilePath);
		parser.parse();

		NISTSpeciesList speciesList = parser.getNISTSpeciesList();
		NISTSpeciesList speciesWithCAS = new NISTSpeciesList();

		HashMap<String, Integer> validElements = new HashMap<String, Integer>();

		validElements.put("C", 1);
		validElements.put("H", 4);

		NISTSpeciesFilterByComposition filter = new NISTSpeciesFilterByComposition(validElements, speciesList, false,
				false);

		filter.filter();

		NISTSpeciesList filteredList = filter.getFilteredList();

		NISTSpeciesId id;

//  System.out.println("Processing species list...");

		while ((id = filteredList.next()) != null) {

			if (!id.getCASRegNr().equals("N/A") && id.getCASRegNr().contains("-")) {

				speciesWithCAS.add(id);

			}
		}

		List<NISTSpeciesId> nistSpeciesId = speciesWithCAS.get();

		System.out.println("Listing species list: ");

		for (NISTSpeciesId species : nistSpeciesId) {

			System.out.println(species.getName() + " " + species.getFormula() + " " + species.getCASRegNr() + " | ");
		}

		int ctr = 0;

		while ((id = speciesWithCAS.next()) != null) {

			ctr++;

			System.out.println("Retrieving species data from " + id.getCASRegNr() + " - " + ctr + " / "
					+ speciesWithCAS.get().size());

			writeHTMLFile(id.getCASRegNr(), new File(destinationFolderPath + "html\\" + id.getCASRegNr() + ".html"),
					"#Thermo-Gas");

			Thread.sleep(500);

			System.out.println(ctr + " Files created!");

			Thread.sleep(500);
		}
	}

}
