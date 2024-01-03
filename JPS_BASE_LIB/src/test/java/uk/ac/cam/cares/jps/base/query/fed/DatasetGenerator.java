package uk.ac.cam.cares.jps.base.query.fed;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.UUID;
import java.util.function.Supplier;

public class DatasetGenerator {
	
	public class Pair<S,T> {
		public S x1;
		public T x2;
		public Pair(S x1, T x2) {
			this.x1 = x1;
			this.x2 = x2;
		}
	}
		
	private String prefixes = null;
	private List<String> triplePatterns = new ArrayList<String>();
	List<Pair<String, Supplier<Object>>> generators = new ArrayList<Pair<String, Supplier<Object>>>();
	private List<Map<String, String>> generatedValues = new ArrayList<Map<String,String>>();
	private StringBuffer b = new StringBuffer();
	
	public DatasetGenerator(String prefixes) {
		this.prefixes = prefixes;
	}
	
	public DatasetGenerator pattern(String triplePattern) {
		triplePatterns.add(triplePattern);
		return this;
	}
	
	public DatasetGenerator generator(String variableName, Supplier<Object> dataGenerator) {
		generators.add(new Pair<String, Supplier<Object>>(variableName, dataGenerator));
		return this;
	}
	
	public DatasetGenerator generator(String variableName, Supplier<Object> dataGenerator, int maxBeforeNull) {
		if (maxBeforeNull < 0) {
			return generator(variableName, dataGenerator);
		}
		Supplier<Object> optionalSupplier = supplierOptional(dataGenerator, maxBeforeNull);
		generators.add(new Pair<String, Supplier<Object>>(variableName, optionalSupplier));
		return this;
	}
	
	private Map<String, String> generateVariableValues() {
		Map<String, String> name2value = new HashMap<String, String>();
		for (Pair<String, Supplier<Object>> current : generators) {
			String name = current.x1;
			Supplier<Object> supplier = current.x2;
			Object value = supplier.get();
			String valueString = (value == null)? null : value.toString();
			name2value.put(name, valueString);
		}
		return name2value;
	}

	public DatasetGenerator generateVariableValues(int number) {
		for (int i=0; i<number; i++) {
			Map<String, String> generated = generateVariableValues();
			generatedValues.add(generated);
		}
		return this;
	}
	
	public List<Object> getGeneratedValues(String variableName, int maxNumber) {
		List<Object> result = new ArrayList<Object>();
		for (Map<String, String> current : generatedValues) {
			String value = current.get(variableName);
			result.add(value);
			if (result.size() == maxNumber) {
				break;
			}
		}
		return result;
	}
	
	public List<String> getOrderedVariableNames() {
		
		List<String> varNames = new ArrayList<String>();
		for (Pair<String, Supplier<Object>> current : generators) {
			varNames.add(current.x1);
		}
		
		// variable names such as "?scfEnergy" "?scfEnergyABCValue" must be ordered 
		// by decreasing String length such that "?scfEnergyABCValue" is replaced before "?scfEnergy";
		// otherwise we would obtain e.g. "-123.1ABCValue"
		Comparator<String> comparator = new Comparator<String>() {

			@Override
			public int compare(String o1, String o2) {
				return o2.length() - o1.length();
			}
		};
		Collections.sort(varNames, comparator);
		return varNames;
	}
	
	public String build() {
		b.append(prefixes);
		b.append("\r\n");
		b.append("\r\n");
		
		List<String> varNames = getOrderedVariableNames();
		
		for (Map<String, String> current : generatedValues) {
			
			for (String triplePattern : triplePatterns) {
				for (String variableName : varNames) {
					String value = current.get(variableName);
					if (value != null) {
						triplePattern = triplePattern.replace(variableName, value);
					}
				}
				if (triplePattern.indexOf("?") == -1) {
					// i.e. all variables are bounded 
					b.append(triplePattern).append(" .\r\n");
				}
			}
			b.append("\r\n");
		}
		return b.toString();
	}
	
	public static Supplier<Object> supplierConstant(Object constant) {
		Supplier<Object> supplier = () -> constant;
		return supplier;
	}
	
	public static Supplier<Object> supplierUUID(String firstPart, String lastPart) {
		Supplier<Object> supplier = () -> firstPart + UUID.randomUUID() + lastPart;
		return supplier;
	}
	
	public static Supplier<Object> supplierList(List<Object> list) {
		Supplier<Object> supplier = new Supplier<Object>() {

			private int index = -1;
			
			@Override
			public Object get() {
				index++;
				return list.get(index);
			}
		};
		return supplier;
	}
	
	public static Supplier<Object> supplierJoin(List<Object> list, Supplier<Object> supplier) {
		Supplier<Object> joinSupplier = new Supplier<Object>() {

			private int index = -1;
			
			@Override
			public Object get() {
				index++;
				if (index < list.size()) {
					return list.get(index);
				}
				return supplier.get();
			}
		};
		return joinSupplier;
	}
	
	public static Supplier<Object> supplierInt(int min, int max) {
		int span = max - min + 1;
		Supplier<Object> supplier = () -> (int) (min + Math.floor(Math.random() * span));
		return supplier;
	}
	
	public static Supplier<Object> supplierOptional(Supplier<Object> supplier, int maxBeforeNull ) {
		Supplier<Object> optionalSupplier = new Supplier<Object>() {

			private int count = 0;
			
			@Override
			public Object get() {
				count++;
				if (count > maxBeforeNull) {
					return null;
				}
				return supplier.get();
			}
		};
		return optionalSupplier;
	}
}
