package cam.dev.zhouxiaochi;

import java.awt.BorderLayout;

import javax.swing.JFrame;
import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.DefaultTreeModel;
import javax.swing.tree.TreeNode;
import javax.swing.tree.TreePath;

public class TreeVision extends JFrame {

    private static final long serialVersionUID = 4648172894076113183L;

    public TreeVision() {
        super();
        setSize(500, 500);
        this.getContentPane().setLayout(new BorderLayout());
        
 
      
     
        
        final TreeMenu cbt = new TreeMenu();
         
        
        this.getContentPane().add(cbt);
        cbt.addCheckChangeEventListener(new TreeMenu.CheckChangeEventListener() {
            public void checkStateChanged(TreeMenu.CheckChangeEvent event) {
                System.out.println("event");
                TreePath[] paths = cbt.getCheckedPaths();
                for (TreePath tp : paths) {
                    for (Object pathPart : tp.getPath()) {
                        System.out.print(pathPart + ",");
                    }                   
                    System.out.println();
                }
            }           
        });         
        this.setDefaultCloseOperation(EXIT_ON_CLOSE);
    }

    public static void main(String args[]) {
    	TreeVision m = new TreeVision();
        m.setVisible(true);
    }
}