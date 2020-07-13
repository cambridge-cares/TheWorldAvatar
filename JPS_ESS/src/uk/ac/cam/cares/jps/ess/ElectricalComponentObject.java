package uk.ac.cam.cares.jps.ess;

public class ElectricalComponentObject {
	 
			private String bus="";
			private double x = 0;
			private double y = 0;
			private double env = 0;
			private double cost = 0;
			private double matur = 0;
			private double pthigh = 0;
			private double ptlow = 0;
			private double dthigh = 0;
			private double dtlow = 0;
			
			public ElectricalComponentObject(String bus) {
				this.bus = bus;
			}
			
			public String getObjectIRI() {
				return bus;
			}
			
			public double getx() {
				return x;
			}
			
			public void setx(double x) {
				this.x = x;
			}
			
			public double gety() {
				return y;
			}
			
			public void sety(double y) {
				this.y = y;
			}
			
			public double getenv() {
				return env;
			}
			
			public void setenv(double env) {
				this.env = env;
			}
			
			public double getcost() {
				return cost;
			}
			
			public void setcost(double cost) {
				this.cost = cost;
			}
			public double getmatur() {
				return matur;
			}
			
			public void setmatur(double matur) {
				this.matur = matur;
			}
			public double getpthigh() {
				return pthigh;
			}
			
			public void setpthigh(double pthigh) {
				this.pthigh = pthigh;
			}
			
			public double getptlow() {
				return ptlow;
			}
			
			public void setptlow(double ptlow) {
				this.ptlow = ptlow;
			}
			public double getdthigh() {
				return dthigh;
			}
			
			public void setdthigh(double dthigh) {
				this.dthigh = dthigh;
			}
			
			public double getdtlow() {
				return dtlow;
			}
			
			public void setdtlow(double dtlow) {
				this.dtlow = dtlow;
			}
			
			
			

			
			

		

}
