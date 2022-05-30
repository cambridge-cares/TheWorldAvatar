package uk.ac.cam.cares.jps.base.query.fed.test;

import java.util.ArrayList;
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
	private String triplePatterns = null;
	private List<Pair<String, Supplier<Object>>> generators = new ArrayList<Pair<String, Supplier<Object>>>();
	private List<Map<String, String>> generatedValues = new ArrayList<Map<String,String>>();
	
	public DatasetGenerator(String prefixes, String triplePatterns) {
		this.prefixes = prefixes;
		this.triplePatterns = triplePatterns;
	}
	
	public void bindVariable(String variableName, Supplier<Object> dataGenerator) {
		generators.add(new Pair<String, Supplier<Object>>(variableName, dataGenerator));
	}
	
	private Map<String, String> generateVariableValues() {
		Map<String, String> name2value = new HashMap<String, String>();
		for (Pair<String, Supplier<Object>> current : generators) {
			String name = current.x1;
			Supplier<Object> supplier = current.x2;
			String value = supplier.get().toString();
			name2value.put(name, value);
		}
		return name2value;
	}

	public void generateVariableValues(int number) {
		for (int i=0; i<number; i++) {
			Map<String, String> generated = generateVariableValues();
			generatedValues.add(generated);
		}
	}
	
	public void build(StringBuffer b) {
		b.append(prefixes);
		
		for (Map<String, String> current : generatedValues) {
			
			String data = triplePatterns;
			for (String variableName : current.keySet()) {
				String value = current.get(variableName);
				data = data.replace(variableName, value);
			}
			b.append("\r\n");
			b.append(data);
		}
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
}
