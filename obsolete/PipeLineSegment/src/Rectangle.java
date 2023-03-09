

import com.esri.core.geometry.Point;

public class Rectangle {

	private Point[] vertice = new Point[4];

	private Point UpperLeft, UpperRight, LowerLeft, LowerRight;
	
	public Point getUpperLeft() {
		return UpperLeft;
	}




	public Point getUpperRight() {
		return UpperRight;
	}




	public Point getLowerLeft() {
		return LowerLeft;
	}




	public Point getLowerRight() {
		return LowerRight;
	}




	public Rectangle(Point[] vertice){
		
		if(vertice.length !=4){
			System.out.println("ERR: not a rectangle");
		return;
		}


		this.vertice = vertice;

	if(!identifyVertice()){
		System.out.println("ERR: rectangle vertices can not be identified");
		return;
	}
	
	}

		
	public Rectangle(double upperLeftX, double upperLeftY, double width, double height){
		this.UpperLeft = new Point(upperLeftX, upperLeftY);
		this.UpperRight = new Point(upperLeftX+width, upperLeftY);
		this.LowerLeft  = new Point(upperLeftX, upperLeftY - height);
		this.LowerRight = new Point(upperLeftX + width, upperLeftY-height);
	}
	
	public Rectangle(Point aVertex, Point diagVertex){
		
		vertice[0] = aVertex;
		vertice[1] = diagVertex;
		vertice[2] = new Point(aVertex.getX(), diagVertex.getY());
		vertice[3] = new Point(diagVertex.getX(), aVertex.getY());
		if(!identifyVertice()){
			System.out.println("ERR: Not be able to identify rect vertices, constructor err");
		}
	}
	
	private boolean identifyVertice(){
		
		Point first = vertice[0];
		double lowerC = first.getY();
		double leftC = first.getX();
		boolean isZeroX = true;//special cases, step may be a horizontal/Vertice, in which case the blocking rectangle will be with height/width ZERO
		boolean isZeroY = true;
		for(Point vertex : vertice){
			
	
			if(vertex.getX() != leftC){
				isZeroX = false;
				if(vertex.getX() < leftC) {
					leftC = vertex.getX();
					}
			}
			
			if(vertex.getY() != lowerC){
				isZeroY = false;
				if(vertex.getY() < lowerC) {
					lowerC = vertex.getY();
					}
			}
			
		}
		
		if(isZeroX){
			boolean firstLower = true, firstUpper = true;
			
			for (Point vertex: vertice){
				if(vertex.getY() == lowerC){
					
					if(firstLower){
						this.LowerLeft = vertex;
						firstLower = false;
					} else{
						this.LowerRight = vertex;
					}
				} else{
					
					if(firstUpper){
						this.UpperLeft = vertex;
						firstUpper = false;
					} else{
						this.UpperRight = vertex;
					}
					
				}
				
			}
			
		} else 		if(isZeroY){
			boolean firstLeft = true, firstRight = true;
			
			for (Point vertex: vertice){
				if(vertex.getX() == leftC){
					
					if(firstLeft){
						this.LowerLeft = vertex;
						firstLeft = false;
					} else{
						this.UpperLeft = vertex;
					}
				} else{
					
					if(firstRight){
						this.LowerRight = vertex;
						firstRight = false;
					} else{
						this.UpperRight = vertex;
					}
					
				}
				
			}
			
		}
		
		for(Point vertex : vertice){
		if(vertex.getY() == lowerC && vertex.getX() == leftC){
			LowerLeft = vertex;
		} else if( vertex.getY() == lowerC && vertex.getX() != leftC){
			LowerRight = vertex;
		} else if(vertex.getY()!= lowerC && vertex.getX() == leftC){
			UpperLeft =vertex;
		} else {
			UpperRight = vertex;
		}
		
		}
		//test if all 4 points are assigned
		
		
		return (UpperRight!=null && UpperLeft!= null && LowerRight!=null && LowerLeft!=null);
	}




	public double getHeight() {

		return UpperLeft.getY() - LowerLeft.getY();
	}








	public double getWidth() {
		return UpperRight.getX() - UpperLeft.getX();
	}



  public double getLeft(){
	  return UpperLeft.getX();
  }
  
  public double getRight(){
	  return UpperRight.getX();
  }
  
  public double getLower(){
	  return LowerLeft.getY();
  }
  
  public double getUpper(){
	  return UpperLeft.getY();
  }

	
	}
	
	
	

