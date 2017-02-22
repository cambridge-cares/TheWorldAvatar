package cam.dev.zhouxiaochi;

import java.util.ArrayList;
import java.util.List;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;


	
public class Tree{
	  final static Logger logger = LoggerFactory.getLogger(OWLReader.class);

	private TreeNode root;
	
	public TreeNode getRoot() {
		return root;
	}



	public Tree (String rootData){
		root = new TreeNode(rootData);
	}
	
	
	private void expandPrint(TreeNode mNode){
		logger.info(mNode.nodeData);
		List<TreeNode> children = mNode.getChildren();
		for(TreeNode child : children){
			expandPrint(child);
		}
		
	}
	public void printTree(){
		expandPrint(root);
	}
	
	
	public  class TreeNode{
		

		private String nodeData;
		public String getNodeData() {
			return nodeData;
		}

		private TreeNode parent;

		private List<TreeNode> children;
		public List<TreeNode> getChildren() {
			return children;
		}

		public TreeNode(String data) {
			
			this.nodeData = data;
			this.children =  new ArrayList<TreeNode>();

		}
		
		public TreeNode getParent() {
			return parent;
		}
		public void setParent(TreeNode parent) {
			this.parent = parent;
		}
		
		public TreeNode addChild(String childData){
			
			TreeNode childNode =new TreeNode(childData);
			childNode.setParent(this);
			this.children.add(childNode);
			return childNode;
			
	}
	
	}
}

