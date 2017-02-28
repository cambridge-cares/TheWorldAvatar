package cam.dev.zhouxiaochi;

import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;

import javax.swing.JTree;
import javax.swing.tree.TreePath;
import javax.swing.tree.DefaultTreeModel;

/**
 * Listener for tree selection event, extending mouseAdapter
 * @author Shaocong
 *
 */
public class CheckBoxTreeNodeSelectionListener extends MouseAdapter {

	public static final int XBOUND = 20;// bound for checking box so only
										// clicking on checkbox area will check
										// the layer
										// approximately width of one checkbox

	@Override
	public void mouseClicked(MouseEvent event) {
		int mBound;
		JTree tree = (JTree) event.getSource();
		int x = event.getX();
		int y = event.getY();
		System.out.println(x + ", " + y);

		int row = tree.getRowForLocation(x, y); // get selected row number
		TreePath path = tree.getPathForRow(row);

		if (path != null) {
			int depth = path.getPathCount();// depth of this element(hierachy
											// level), to determine bound
			System.out.println("depth:" + depth);
			mBound = XBOUND * depth;
			if (x < mBound) {
				CheckBoxTreeNode node = (CheckBoxTreeNode) path.getLastPathComponent();
				if (node != null) {
					boolean isSelected = !node.isSelected(); // toggle selection
					node.setSelected(isSelected);
					((DefaultTreeModel) tree.getModel()).nodeStructureChanged(node);
				}
			}
		}
	}
}