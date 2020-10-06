package uk.ac.cam.cares.jps.dispersion.sensor;

public class PSITable {
	
	private int[] pm25a= {0,12};
	private int[] pm25b= {13,55};
	private int[] pm25c= {56,150};
	private int[] pm25d= {151,250};
	private int[] pm25e= {251,350};
	private int[] pm25f= {401,500};
	private int[] pm10a= {0,50};
	private int[] pm10b= {51,150};
	private int[] pm10c= {151,350};
	private int[] pm10d= {351,420};
	private int[] pm10e= {421,500};
	private int[] pm10f= {501,600};
	private int[] so2a= {0,80};
	private int[] so2b= {81,365};
	private int[] so2c= {366,800};
	private int[] so2d= {801,1600};
	private int[] so2e= {1601,2100};
	private int[] so2f= {2101,2620};
	private int[] no2a= {0,0};
	private int[] no2b= {0,0};
	private int[] no2c= {1130,1130};
	private int[] no2d= {1131,2260};
	private int[] no2e= {2261,3000};
	private int[] no2f= {3001,3750};
	private int[] o3a= {0,118};
	private int[] o3b= {119,157};
	private int[] o3c= {158,235};
	private int[] o3d= {236,785};
	private int[] o3e= {786,980};
	private int[] o3f= {981,1180};
	private double[]coa= {0.0,5.0};
	private double[]cob= {5.1,10.0};
	private double[]coc= {10.1,17.0};
	private double[]cod= {17.1,34.0};
	private double[]coe= {34.1,46.0};
	private double[]cof= {46.1,57.5};
	public int[] getPm10a() {
		return pm10a;
	}
	public void setPm10a(int[] pm10a) {
		this.pm10a = pm10a;
	}
	public int[] getPm10b() {
		return pm10b;
	}
	public void setPm10b(int[] pm10b) {
		this.pm10b = pm10b;
	}
	public int[] getPm10c() {
		return pm10c;
	}
	public void setPm10c(int[] pm10c) {
		this.pm10c = pm10c;
	}
	public int[] getPm10d() {
		return pm10d;
	}
	public void setPm10d(int[] pm10d) {
		this.pm10d = pm10d;
	}
	public int[] getPm10e() {
		return pm10e;
	}
	public void setPm10e(int[] pm10e) {
		this.pm10e = pm10e;
	}
	public int[] getPm10f() {
		return pm10f;
	}
	public void setPm10f(int[] pm10f) {
		this.pm10f = pm10f;
	}
	public int[] getSo2a() {
		return so2a;
	}
	public void setSo2a(int[] so2a) {
		this.so2a = so2a;
	}
	public int[] getSo2b() {
		return so2b;
	}
	public void setSo2b(int[] so2b) {
		this.so2b = so2b;
	}
	public int[] getSo2c() {
		return so2c;
	}
	public void setSo2c(int[] so2c) {
		this.so2c = so2c;
	}
	public int[] getSo2d() {
		return so2d;
	}
	public void setSo2d(int[] so2d) {
		this.so2d = so2d;
	}
	public int[] getSo2e() {
		return so2e;
	}
	public void setSo2e(int[] so2e) {
		this.so2e = so2e;
	}
	public int[] getSo2f() {
		return so2f;
	}
	public void setSo2f(int[] so2f) {
		this.so2f = so2f;
	}
	public int[] getNo2a() {
		return no2a;
	}
	public void setNo2a(int[] no2a) {
		this.no2a = no2a;
	}
	public int[] getNo2b() {
		return no2b;
	}
	public void setNo2b(int[] no2b) {
		this.no2b = no2b;
	}
	public int[] getNo2c() {
		return no2c;
	}
	public void setNo2c(int[] no2c) {
		this.no2c = no2c;
	}
	public int[] getNo2d() {
		return no2d;
	}
	public void setNo2d(int[] no2d) {
		this.no2d = no2d;
	}
	public int[] getNo2e() {
		return no2e;
	}
	public void setNo2e(int[] no2e) {
		this.no2e = no2e;
	}
	public int[] getNo2f() {
		return no2f;
	}
	public void setNo2f(int[] no2f) {
		this.no2f = no2f;
	}
	public int[] getO3a() {
		return o3a;
	}
	public void setO3a(int[] o3a) {
		this.o3a = o3a;
	}
	public int[] getO3b() {
		return o3b;
	}
	public void setO3b(int[] o3b) {
		this.o3b = o3b;
	}
	public int[] getO3c() {
		return o3c;
	}
	public void setO3c(int[] o3c) {
		this.o3c = o3c;
	}
	public int[] getO3d() {
		return o3d;
	}
	public void setO3d(int[] o3d) {
		this.o3d = o3d;
	}
	public int[] getO3e() {
		return o3e;
	}
	public void setO3e(int[] o3e) {
		this.o3e = o3e;
	}
	public int[] getO3f() {
		return o3f;
	}
	public void setO3f(int[] o3f) {
		this.o3f = o3f;
	}
	public double[] getCoa() {
		return coa;
	}
	public void setCoa(double[] coa) {
		this.coa = coa;
	}
	public double[] getCob() {
		return cob;
	}
	public void setCob(double[] cob) {
		this.cob = cob;
	}
	public double[] getCoc() {
		return coc;
	}
	public void setCoc(double[] coc) {
		this.coc = coc;
	}
	public double[] getCod() {
		return cod;
	}
	public void setCod(double[] cod) {
		this.cod = cod;
	}
	public double[] getCoe() {
		return coe;
	}
	public void setCoe(double[] coe) {
		this.coe = coe;
	}
	public double[] getCof() {
		return cof;
	}
	public void setCof(double[] cof) {
		this.cof = cof;
	}

	public int[] getPm25a() {
		return pm25a;
	}
	public void setPm25a(int[] pm25a) {
		this.pm25a = pm25a;
	}
	public int[] getPm25b() {
		return pm25b;
	}
	public void setPm25b(int[] pm25b) {
		this.pm25b = pm25b;
	}
	public int[] getPm25c() {
		return pm25c;
	}
	public void setPm25c(int[] pm25c) {
		this.pm25c = pm25c;
	}
	public int[] getPm25d() {
		return pm25d;
	}
	public void setPm25d(int[] pm25d) {
		this.pm25d = pm25d;
	}
	public int[] getPm25e() {
		return pm25e;
	}
	public void setPm25e(int[] pm25e) {
		this.pm25e = pm25e;
	}
	public int[] getPm25f() {
		return pm25f;
	}
	public void setPm25f(int[] pm25f) {
		this.pm25f = pm25f;
	}
	
	

}
