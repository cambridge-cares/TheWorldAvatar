package cam.dev.zhouxiaochi;

/**A* for line Arrangement : grid class. defines griding of a map.
 * Deprecated. Kept only for reference.
 * ***/
public class Grid {

	private int gridRowNum, gridColNum;
	private double mapWidth, mapHeight;
	
	public Grid(int rows, int cols, double mapWidth, double mapHeight){
		
		gridRowNum = rows;

		gridColNum = cols;
		this.mapWidth = mapWidth;
		this.mapHeight = mapHeight;
		
	}
	public int getGridRowNum() {
		return gridRowNum;
	}


	public void setGridRowNum(int gridRowNum) {
		this.gridRowNum = gridRowNum;
	}


     
	


	public int getGridColNum() {
		return gridColNum;
	}


	public void setGridColNum(int gridColNum) {
		this.gridColNum = gridColNum;
	}
	
	public double getCellHeight(){
		return mapHeight/gridRowNum;
	}
	public double getCellWidth(){
		return mapWidth/gridColNum;
	}
	

	public double getCellLeft(int rowIndex, int colIndex){
		return rowIndex * getCellWidth();
		
	}
	public double getCellRight(int rowIndex, int colIndex){
		return (rowIndex+1) * getCellWidth();

	}
	public double getCellLower(int rowIndex, int colIndex){
		return rowIndex * getCellHeight();

	}

	public double getCellUpper(int rowIndex, int colIndex){
		return (rowIndex+1) * getCellHeight();

	}


}
