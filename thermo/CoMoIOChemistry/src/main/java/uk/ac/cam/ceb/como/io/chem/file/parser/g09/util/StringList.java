package uk.ac.cam.ceb.como.io.chem.file.parser.g09.util;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

public class StringList extends ArrayList<String> {

	/**
	 * @author nk510 Added serialVersionUID as suggested by Java VM.
	 */

	private static final long serialVersionUID = 1L;
		
	public StringList(List<String> lines) {

		/**
		 * @author nk510 <p>If one adds Collections.addAll(lines) in order to avoid
		 *         java.lang.NullPointerException at this point, it may cause breaking
		 *         code in other places.
		 *         Date: MAY-17th-2018. </p>
		 * 
		 *        
		 */

		addAll(lines);

	}

	public StringList() {
	}

	/**
	 * Remove blank lines at the beginning of list
	 */

	public void trim() {
		for (Iterator<String> it = this.iterator(); it.hasNext();) {
			if (it.next().trim().isEmpty()) {
				it.remove();
			} else {
				break;
			}
		}
	}

	/**
	 * get the line until it doesn't match the given pattern
	 * 
	 * @param from_line
	 * @param regex
	 * @param regexes
	 * @return
	 */

	public List<String> getLines(int from_line, String regex, String... regexes) {
		List<String> lines = new ArrayList<String>();
		for (int i = from_line; i < size(); i++) {
			int mathcedPos = getFirstMatchPosition(i, regex, regexes);
			if (mathcedPos == i) {
				lines.addAll(subList(i, i + regexes.length + 1)); // +1 for default regex
				i = i + regexes.length; // +0 for default regex
			} else {
				break;
			}
		}
		return lines;
	}

	/**
	 * get the line until it match the given pattern
	 * 
	 * @param from_line
	 * @param regex
	 * @param regexes
	 * @return
	 */
	
	public List<String> getLinesUntil(int from_line, String regex, String... regexes) {
		int pos = getFirstMatchPosition(from_line, regex, regexes);
		if (pos != -1) {
			return subList(from_line, pos);
		} else {
			return new ArrayList<String>();
		}
	}

	public int getFirstMatchPosition(int from_line, String regex, String... regexes) {
		if (from_line < 0) {
			throw new IndexOutOfBoundsException("Accessing index (" + from_line + ") is forbidden.");
		}
		for (int i = from_line; i < size(); i++) {
			boolean match = get(i).matches(regex);
			if (i + regexes.length + 1 <= size()) { // does not overflow. +1 for default regex
				for (int j = 0; j < regexes.length; j++) {
					match = match && get(i + j + 1).matches(regexes[j]);
				}
			} else {
				match = false;
			}
			if (match) {
				return i;
			}
		}
		return -1;
	}

	public int getLastMatchPosition(int from_line, String regex, String... regexes) {
		if (from_line < 0) {
			throw new IndexOutOfBoundsException("Accessing index (" + from_line + ") is forbidden.");
		}
		int pos = -1;
		for (int i = from_line; i < size(); i++) {
			boolean match = get(i).matches(regex);
			if (i + regexes.length + 1 <= size()) { // does not overflow. +1 for default regex
				for (int j = 0; j < regexes.length; j++) {
					match = match && get(i + j + 1).matches(regexes[j]);
				}
			} else {
				match = false;
			}
			if (match) {
				pos = i;
			}
		}
		return pos;
	}

}
