package uk.ac.cam.cares.jps.ess;

public class ElectricalComponentObject {
	 
			private String bus="";
			private double x = 0;
			private double y = 0;
			
			public ElectricalComponentObject(String bus) {
				this.bus = bus;
			}
			
			public String getbus() {
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
			

			
			

		

}
