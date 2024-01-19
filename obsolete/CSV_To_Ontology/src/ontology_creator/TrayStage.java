package ontology_creator;

import java.util.HashSet;
import java.util.Set;

public class TrayStage {
	private TrayColumn column;
	private int stageNumber;
	private Set<String> feedStreams = new HashSet<String>();
	private Set<String> drawStreams = new HashSet<String>();
	
	TrayStage(TrayColumn column, int stageNumber) {
		this.column = column;
		this.stageNumber = stageNumber;
	}
	
	public TrayColumn getColumn() {
		return column;
	}
	public int getStageNumber() {
		return stageNumber;
	}
	public Set<String> getFeedStreams() {
		return feedStreams;
	}
	public Set<String> getDrawStreams() {
		return drawStreams;
	}
	public void setColumn(TrayColumn column) {
		this.column = column;
	}
	public void setStageNumber(int stageNumber) {
		this.stageNumber = stageNumber;
	}
	public void setFeedStreams(Set<String> feedStreams) {
		this.feedStreams = feedStreams;
	}
	public void addFeedStream(String feedStream) {
		feedStreams.add(feedStream);
	}
	public void setDrawStreams(Set<String> drawStreams) {
		this.drawStreams = drawStreams;
	}
	public void addDrawStream(String drawStream) {
		drawStreams.add(drawStream);
	}
	
}
