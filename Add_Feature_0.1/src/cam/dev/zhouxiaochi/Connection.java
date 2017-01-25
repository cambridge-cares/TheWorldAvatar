package cam.dev.zhouxiaochi;

import com.esri.core.geometry.Point;
/**A* for line Arrangement : Connection data class.
 * 
 * @author Shaocong
 *
 */
public class Connection {
	
	public Connection(Point start, Point end) {
		this.start = start;
		this.end = end;
	}

	public Point start ;
	

	
	public Point end;
	
	

}
