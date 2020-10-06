package uk.ac.cam.cares.jps.ontomatch;

import java.nio.file.InvalidPathException;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import org.apache.commons.validator.routines.DoubleValidator;
import org.apache.commons.validator.routines.UrlValidator;
import org.json.JSONArray;
import org.json.JSONObject;

import uk.ac.cam.cares.jps.ontomatch.ElementMatcher.MATCHERTYPE;
import uk.ac.cam.cares.jps.ontomatch.ElementMatcher.MATCHING_TYPE;

public class ParamsValidateHelper {

	public enum CUSTOMVALUETYPE {
		URL, PATH, WEIGHTS, MATCHING_TYPE, MATCHERTYPE, THRESHOLD
	}

	public static <K, V> boolean validateALLParams(JSONObject obj, Map<String, CUSTOMVALUETYPE> keyTypeMap) {
		for (Map.Entry<String, CUSTOMVALUETYPE> entry : keyTypeMap.entrySet()) {
			String paramName = entry.getKey();
			CUSTOMVALUETYPE expectedType = entry.getValue();
			if (!validateSingleParamByType(obj, paramName, expectedType)) {
				return false;
			}
		}
		return true;
	}

	public static boolean validateSingleParamByType(JSONObject obj, String name, CUSTOMVALUETYPE ctype) {
		switch (ctype) {
		case URL:
			try {
				String value = obj.getString(name);
				return isValidUrl(value);
			} catch (Exception ex) {
				return false;
			}

		case PATH:
			try {
				String value = obj.getString(name);
				return isValidFilePath(value);
			} catch (Exception ex) {
				return false;
			}
		case WEIGHTS:	
			try {
				JSONArray jweight = obj.getJSONArray(name);
				List<Double> lweight = new ArrayList<Double>();
				for (int i = 0; i < jweight.length(); i++) {
					lweight.add(jweight.getDouble(i));
				}
				Double[] weights = new Double[lweight.size()];
				lweight.toArray(weights);
				return isValidWeights(weights);
			} catch (Exception ex) {
				return false;
			}
			
		case MATCHING_TYPE:
			try {
				String value = obj.getString(name);
				return isValidMatchingType(value);
			} catch (Exception ex) {
				return false;
			}
			
		case MATCHERTYPE:
			try {
				String value = obj.getString(name);
				return isValidMatcherType(value);
			} catch (Exception ex) {
				return false;
			}
			
		case THRESHOLD:
			try {
				double value = obj.getDouble(name);
				return isValidThreshold(value);
			} catch (Exception ex) {
				return false;
			}
		default:
			return false;
		}

	}

	public static boolean isValidUrl(String url) {
		String[] schemes = { "http", "https" };
		UrlValidator urlValidator = new UrlValidator(schemes);
		if (urlValidator.isValid(url)) {
			return true;
		} else {
			return false;
		}
	}

	// TODO
	public static boolean isValidFilePath(String path) {
		try {
			Paths.get(path);
		} catch (InvalidPathException | NullPointerException ex) {
			return false;
		}
		return true;
	}

	public static boolean isValidMatchingType(String mtstr) {
		try {
			MATCHING_TYPE.valueOf(mtstr);
		} catch (IllegalArgumentException | NullPointerException ex) {
			return false;
		}
		return false;
	}

	public static boolean isValidMatcherType(String mstr) {
		try {
			MATCHERTYPE.valueOf(mstr);
		} catch (IllegalArgumentException | NullPointerException ex) {
			return false;
		}
		return false;
	}

	public static boolean isValidWeights(Double[] weights) {
		DoubleValidator db = new DoubleValidator();
		double sum = 0;
		for (double w : weights) {
			sum += w;
			if (!db.isInRange(w, 0, 1)) {
				return false;
			}
		}
		if (Math.abs(sum - 1.0) > Math.ulp(1.0)) {
			return false;
		}
		return true;
	}

	public static boolean isValidThreshold(double t) {
		DoubleValidator db = new DoubleValidator();
		if (!db.isInRange(t, 0, 1)) {
			return false;
		}
		return true;
	}
}
