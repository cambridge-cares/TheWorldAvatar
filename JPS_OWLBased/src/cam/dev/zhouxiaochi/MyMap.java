package cam.dev.zhouxiaochi;

import java.awt.Color;
import java.awt.Graphics;
import java.util.ArrayList;

import com.esri.core.geometry.Point;

public class MyMap implements TileBasedMap {

	private double width;
	private double height;

	private ArrayList <Rectangle> obstacles;
	
	private Grid grid;
	
	private boolean[][] visited;
	private boolean[][] isBlocked ;
	private boolean[][] isLine ;


	


	
	
	public MyMap(double width, double height, int gridRowNum, int gridColNum, ArrayList<Rectangle> obstacles) {
		super();
		this.width = width;
		this.height = height;

		this.obstacles = obstacles;
		grid = new Grid(gridRowNum,gridColNum, width, height);//draw grid for testing


		//save map for blocked information
        initBlocking();
		
	}

	
	public void initBlocking(){
		int gridRowNum = grid.getGridRowNum();
		int gridColNum = grid.getGridColNum();
		System.out.println("new row num"+gridRowNum);
		visited = new boolean[gridRowNum][gridColNum];
		isBlocked = new boolean[gridRowNum][gridColNum];
		isLine = new boolean[gridRowNum][gridColNum];

		for (int x=0;x<gridRowNum;x++) {
			for (int y=0;y<gridRowNum;y++) {
				visited[x][y] = false;
				isBlocked[x][y] = false;
				isLine[x][y] = false;

			}
		}
		for(Rectangle obstacle : obstacles){
			//check if obstacles are indeed within map range
			if(obstacle.getLeft() < 0 || obstacle.getRight() > width|| obstacle.getLower() < 0 || obstacle.getUpper() > height){
				System.out.println("ERR: Blocking Rectangle outside Map");//TODO:ERR MSG STANDARLIZE
			}
			
			
			//populate blocked flags
			int startXIndex =getCellRowIdx(obstacle.getLeft());
			int endXIndex = getCellRowIdx(obstacle.getRight());
			int endYIndex = getCellColIdx(obstacle.getUpper());

			int startYIndex = getCellColIdx(obstacle.getLower());

			for(int idx = startXIndex; idx <= endXIndex; idx++){
				for(int idy = startYIndex; idy <= endYIndex; idy++){
					isBlocked[idx][idy] = true;
				}
			}
		}
	}
	
	
	public void changeGrid(int newRowNum, int newColNum){
		grid.setGridRowNum(newRowNum);
		grid.setGridColNum(newColNum);

		initBlocking();
	}
	
	@Override
	public int getWidthInTiles() {
		return grid.getGridRowNum();
	}

	@Override
	public int getHeightInTiles() {
		return grid.getGridColNum();
	}

	@Override
	public void pathFinderVisited(int x, int y) {
		visited[x][y] = true;
	}

	@Override
	public boolean blocked(int x, int y) {

	
		
	return isBlocked[x][y];
	}

	@Override
	public float getCost(int sx, int sy, int tx, int ty) {
		return 1;
	}
	
	public boolean isLine(int x, int y){
		
     return isLine[x][y];		
	}
	
	public void setLine(int x, int y){
		isLine[x][y] = true;
	}
	
	/**
	public void draw(Graphics g){
		  
	         // Your custom painting codes. For example,
	         // Drawing primitive shapes
	        
		
		     int r = 0, gr=0 , b= 0,idxRect = 0;
	    
	         for(Rectangle obstacle : obstacles){
		         g.setColor(new Color(r, gr,b));       // change the drawing color

		         if(r <= 235){
		        	 r+=30;
		         } else if(gr <=235){
		        	 gr+=30;
		         } else{
		        	 b+=30;
		         }
		         
	        	 Point transformedUpperLeft = invertCoordinatesForJCanvas(obstacle.getUpperLeft());
	        	 g.fillRect((int)transformedUpperLeft.getX(), (int)transformedUpperLeft.getY(), (int)obstacle.getWidth(), (int)obstacle.getHeight());
	         g.drawString(idxRect+"", (int)transformedUpperLeft.getX(), (int)transformedUpperLeft.getY());
	         idxRect++;
	         }
	         
	         g.setColor(Color.BLUE);       // change the drawing color
	         grid.drawOnCanvas(g);
	}
	
	
	///TODO: no longer needed after test!
	public Point invertCoordinatesForJCanvas(Point ori){
		return new Point(ori.getX(), height-ori.getY());
		
	}
	***/
	public int getCellRowIdx(double xCoor){
		
		return (int) Math.floor(xCoor/grid.getCellWidth());
	}
	
	public int getCellColIdx(double yCoor){
		
		return (int) Math.floor(yCoor/grid.getCellHeight());
	}
	
	public void block(int x, int y){
		isBlocked[x][y] = true;
	}
	
	public void unblock(int x, int y){
		isBlocked[x][y] = false;
	}
	public Point getCellCenterCoordiFromIdx(int idxRow, int idxCol){
		double x = (idxRow+0.5) * grid.getCellWidth();
		double y = (idxCol+0.5) * grid.getCellHeight();
		
		
		return new Point(x,y);
	}

}
