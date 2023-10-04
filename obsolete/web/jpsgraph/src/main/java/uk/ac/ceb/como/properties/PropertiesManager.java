package uk.ac.ceb.como.properties;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.io.InputStream;

import java.net.MalformedURLException;
import java.net.URL;
import java.net.URLConnection;
import java.nio.file.Files;
import java.nio.file.attribute.BasicFileAttributes;
import java.text.ParseException;
import java.text.SimpleDateFormat;

import java.time.LocalDate;
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Properties;
import java.util.TreeMap;

import org.eclipse.rdf4j.repository.RepositoryConnection;

import uk.ac.ceb.como.query.QueryManager;

public class PropertiesManager {

	/**
	 * @author NK510
	 * @param inputStream the Input Stream.
	 * @return Properties (key, value) given in .properties file for self growing
	 *         knowledge graph statistics project
	 */
	public static Properties loadProperties(InputStream inputStream) {

		Properties properties = new Properties();

		try {

			if (inputStream == null) {

				return properties;
			}

			// load a properties file from class path.
			properties.load(inputStream);

		} catch (IOException ex) {

			ex.printStackTrace();

		} finally {

			if (inputStream != null) {

				try {

					inputStream.close();

				} catch (IOException e) {

					e.printStackTrace();

				}
			}
		}

		return properties;

	}

	/**
	 * @author NK510
	 * @param filesList  list of Files stored in given folder including sub folders.
	 * @param level      the level of a folder in folder hierarchy
	 * @param speciesMap the species hash map that contains unique OWL file name
	 * @throws IOException
	 */
	static void getAllFiles(File[] filesList, int level, LinkedHashMap<String, String> speciesMap) throws IOException {

		for (File f : filesList) {
			if (f.isFile()) {

				BasicFileAttributes basicAttribute = Files.readAttributes(f.getAbsoluteFile().toPath(),
						BasicFileAttributes.class);

				String[] date = basicAttribute.creationTime().toString().split("T");

				speciesMap.put(f.getName(), date[0]);

			} else if (f.isDirectory()) {

				getAllFiles(f.listFiles(), level + 1, speciesMap);

			}

		}

	}

	/**
	 * @author NK510
	 * @param folderPath the folder that contains OWL files uploaded to JPS
	 *                   knowledge graph.
	 * @return the map that contains unique OWL file name and created date (uploaded
	 *         date)
	 * @throws MalformedURLException the ex
	 * @throws IOException
	 * @throws InterruptedException
	 */

	public LinkedHashMap<String, String> getFrequencyOfSpeciesPerDate(String folderPath) throws IOException {

		LinkedHashMap<String, String> speciesMap = new LinkedHashMap<String, String>();

		File maindir = new File(folderPath);

		if (maindir.exists() && maindir.isDirectory()) {
			File fileList[] = maindir.listFiles();

			getAllFiles(fileList, 0, speciesMap);
		}

		LinkedHashMap<String, String> speciesFrequecniesPerDate = new LinkedHashMap<String, String>();

		for (Map.Entry<String, String> map : speciesMap.entrySet()) {

			int count = Collections.frequency(new ArrayList<String>(speciesMap.values()), map.getValue());

			if (!speciesFrequecniesPerDate.containsKey(map.getValue())) {

				speciesFrequecniesPerDate.put(map.getValue(), String.valueOf(count));
			}
		}

		Map<String, String> treeMap = new TreeMap<String, String>(speciesFrequecniesPerDate);

		LinkedHashMap<String, String> linkedHashMap = new LinkedHashMap<String, String>(treeMap);

		return linkedHashMap;
	}

	

	/**
	 * @author NK510 (caresssd@hermes.cam.ac.uk)
	 * 
	 * @param todaysDate
	 * @param numberOfMonths
	 * @return
	 * @throws ParseException
	 */
	public static LocalDate getDateThreeMonthsEarlierFromTodaysDate(String todaysDate, int numberOfMonths)
			throws ParseException {

		Date currentDate = new SimpleDateFormat("yyyy-MM-dd").parse(todaysDate);
		String formattedDate = new SimpleDateFormat("yyyy-MM-dd").format(currentDate);

		LocalDate statedDate = LocalDate.parse(formattedDate);
		String threeMonthEarlierDate = statedDate.minusMonths(numberOfMonths).toString();

		Date threeMonthsBeforeDate = new SimpleDateFormat("yyyy-MM-dd").parse(threeMonthEarlierDate);
		String formattedThreeMonthsBeforeDate = new SimpleDateFormat("yyyy-MM-dd").format(threeMonthsBeforeDate);

		LocalDate statedThreeMonthBeforeDate = LocalDate.parse(formattedThreeMonthsBeforeDate);

		return statedThreeMonthBeforeDate;
	}

	/**
	 * @author NK510 (caresssd@hermes.cam.ac.uk)
	 * 
	 * @param startingDate the current date. It is last date of a year i.e. today's
	 *                     date.
	 * @param endDate      the date that is seven days earlier from starting date,
	 *                     including that date.
	 * @return the hash map of seven days range where key is starting date and value
	 *         is end date seven days earlier than starting date. Implementation of
	 *         this method can be easly change to support range between date to any
	 *         number of days. For example, 5 or 10 days.
	 */
	public LinkedHashMap<LocalDate, LocalDate> getWeeklyDatesBetweenTwoDates(String startingDate, String endDate) {

		LocalDate start = LocalDate.parse(startingDate);
		LocalDate end = LocalDate.parse(endDate);

		LinkedList<LocalDate> listOfDates = new LinkedList<LocalDate>();

		while (!start.isBefore(end)) {

			listOfDates.add(start);

			/**
			 * In line below change number of days that we want to have as difference
			 * between dates in a range of dates on bar chart label. For example, if we use
			 * -5 instead of -5 then difference between each range in bar chart label will
			 * be 5 days.
			 */
			start = start.plusDays(-7);

			LocalDate dayAfter = start.plusDays(1);

			if (!dayAfter.isBefore(end) || (dayAfter.isEqual(end))) {
				listOfDates.add(dayAfter);
			}

		}
		listOfDates.add(end);

		int k = 0;

		/**
		 * Printing all dates from start date to the date that is three month earlier with seven days difference.  
		 */
		for (LocalDate d : listOfDates) {

			DateTimeFormatter formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd");

			String formattedString = d.format(formatter);

			System.out.println(k + " " + formattedString);

			k++;
		}

		LinkedHashMap<LocalDate, LocalDate> pairsDateMap = new LinkedHashMap<LocalDate, LocalDate>();
		System.out.println("Pairs of dates:");
		int s = 0;
		for (int i = 0; i < listOfDates.size(); i++) {
			if (2 * i < listOfDates.size()) {
				System.out.println(s + ". " + listOfDates.get(i * 2) + "  " + listOfDates.get((1 + i) * 2 - 1));
				pairsDateMap.put(listOfDates.get(i * 2), listOfDates.get((1 + i) * 2 - 1));
				s++;
			}
		}

		return pairsDateMap;
	}

	/**
	 * @author NK510 (caresssd@hermes.cam.ac.uk)
	 * 
	 * @param pairsOfDatesMap pairs of dates between two dates. Difference between
	 *                        these two days in each pair is seven days, including
	 *                        start and end date in every pair.
	 * @param speciesMap      the map that contains a pair of dates (taken from
	 *                        pairsOfDatesMap) and sum of uploaded species between
	 *                        these two days in a pair.
	 * @return the hash map that contains pair of dates and sum of upladed species.
	 */
	public LinkedHashMap<String, String> getSpeciesStatisticsWeekly(LinkedHashMap<LocalDate, LocalDate> pairsOfDatesMap,
			LinkedHashMap<String, String> speciesMap) {

		LinkedHashMap<String, String> updatedSpeciesMap = new LinkedHashMap<String, String>();

		for (Map.Entry<LocalDate, LocalDate> m : pairsOfDatesMap.entrySet()) {

			int sum = 0;

			for (Map.Entry<String, String> map : speciesMap.entrySet()) {

				LocalDate date = LocalDate.parse(map.getKey().toString());

				int value = Integer.parseInt(map.getValue());

				if ((date.isBefore(m.getKey()) || date.isEqual(m.getKey()))
						&& (date.isAfter(m.getValue()) || date.isEqual(m.getValue()))) {

					System.out.println(date + " is between " + m.getKey() + " and " + m.getValue());

					/**
					 * sums number of uploaded species for dates that are after and before given
					 * range of seven days.
					 */
					sum = sum + value;

					updatedSpeciesMap.put(m.getKey().toString() + " to " + m.getValue().toString(),String.valueOf(sum));
				} else {
					/**
					 * If there is not uploaded species for given dates then sum of uploaded species
					 * is zero.
					 */

					value = 0;
					sum = sum + value;
					updatedSpeciesMap.put(m.getKey().toString() + " to " + m.getValue().toString(),String.valueOf(sum));
				}
			}
		}

		return updatedSpeciesMap;
	}

}