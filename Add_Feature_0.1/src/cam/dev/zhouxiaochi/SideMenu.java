package cam.dev.zhouxiaochi;

import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTree;
import javax.swing.tree.DefaultTreeModel;

import cam.dev.zhouxiaochi.EntityTree.TreeNode;

public class SideMenu extends JPanel{

	public SideMenu() throws IOException, Exception{		
		super();
		//set length height
		
        JTree tree = new JTree();  
           
        EntityTree entityNameTree = OWLReader.getEntityListFromOWLAsTree();
        
        //TODO: Manually construct tree to accomodate some layers not in the structure yet
        TreeNode root = entityNameTree.getRoot();
        root.addChild(new EntityInfo("waterline",""));
        root.addChild(new EntityInfo("gasline",""));
        root.addChild(new EntityInfo("materialline",""));
        root.addChild(new EntityInfo("airline",""));
        
        TreeNode subTranNode = entityNameTree.getNodeVName("SubTransmissionLines");//TODO: TESTING!
        System.out.println(subTranNode.getEntityInfo().getName());
        subTranNode.addChild(new EntityInfo("EHTLines","owl/updated electrical network.owl" ));
        subTranNode.addChild(new EntityInfo("HTLines","owl/updated electrical network.owl" ));
        TreeNode disNode = entityNameTree.getNodeVName("DistributionLines");
        disNode.addChild(new EntityInfo("TLPlant(22kV-11kV)","owl/updated electrical network.owl" ));
        disNode.addChild(new EntityInfo("TLPlant(22kV-3.4kV)","owl/updated electrical network.owl" ));
        disNode.addChild(new EntityInfo("TLPlant(3.4kV-3kV)","owl/updated electrical network.owl" ));
        disNode.addChild(new EntityInfo("TLPlant(3kV-0.4kV)","owl/updated electrical network.owl" ));
        disNode.addChild(new EntityInfo("TLPlant(main-22kV)","owl/updated electrical network.owl" ));

///////////////////////////////////////////////////////////////////////////////////////
        
        //////construct Display JTree
        TreeNode rootData  = entityNameTree.getRoot();
        CheckBoxTreeNode rootNode = new CheckBoxTreeNode(rootData.getEntityInfo().getName());  
        constructNodeModel(rootNode, rootData);
        DefaultTreeModel model = new DefaultTreeModel(rootNode);  
        tree.addMouseListener(new CheckBoxTreeNodeSelectionListener());  
        tree.setModel(model);  
        tree.setCellRenderer(new CheckBoxTreeCellRenderer());  

        rootNode.setSelected(true);
        
       
        
        
        
       //tranverse through tree and add children to nodeModel       
		this.add(tree);
		
	}
	
	
    private void constructNodeModel(CheckBoxTreeNode pNode, TreeNode pDataNode){
    
    	List<TreeNode> childrenData =  pDataNode.getChildren();
    	
    	for(TreeNode childData: childrenData){
            CheckBoxTreeNode cNode = new CheckBoxTreeNode(childData.getEntityInfo().getName());  

            pNode.add(cNode);
            constructNodeModel(cNode, childData);
    	}
    	
    }
	
}
