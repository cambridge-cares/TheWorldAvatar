/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package org.cam.ceb.como.enthalpy.calculator.reaction.selector;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;
import org.cam.ceb.como.enthalpy.calculator.reaction.Reaction;
import org.cam.ceb.como.enthalpy.calculator.reaction.ReactionList;

/**
 *
 * @author pb556
 */
public class RangeBasedReactionSelector extends ReactionSelector {

    protected int numBlocks = 10;
    protected int regardedBlocks = 3;
    protected double percentage = 0.0;
    protected boolean blocks = true;

    public RangeBasedReactionSelector() {
        super();
    }

    public RangeBasedReactionSelector(int numBlocks, int regardedBlocks) {
        super();
        this.numBlocks = numBlocks;
        this.regardedBlocks = regardedBlocks;
        blocks = true;
    }

    public RangeBasedReactionSelector(int numBlocks, double percentage) {
        super();
        this.numBlocks = numBlocks;
        this.percentage = percentage;
        blocks = false;
    }

    @Override
    public ReactionList select(ReactionList reactions) {
        if (blocks) {
            return selectUsingNumBlocks(reactions);
        } else {
            return selectUsingPercentage(reactions);
        }
    }

    protected ReactionList selectUsingNumBlocks(ReactionList reactions) {
        Map<Integer, ReactionList> sorted = organise(reactions);
        int centre = getCentre(sorted);
        ReactionList rList = new ReactionList();
        ArrayList<Integer> ordered = getOrderedBlockList(sorted);
        // only neighbours will be regarded
        int n1 = centre - 1;
        int n2 = centre + 1;
        for (int i = 0; i < regardedBlocks; i++) {
            for (int j = 0; j < ordered.size(); j++) {
                if (ordered.get(j) == n1) {
                    rList.addAll(sorted.get(n1));
                    n1--;
                    break;
                }
                if (ordered.get(j) == n2) {
                    rList.addAll(sorted.get(n2));
                    n2++;
                    break;
                }
            }
        }

        System.out.println("Remaining reactions:");
        for (int i = 0; i < rList.size(); i++) {
            System.out.println(rList.get(i).calculateHf());
        }
        return rList;
    }

    protected ReactionList selectUsingNumReactions(ReactionList reactions) {
        Map<Integer, ReactionList> sorted = organise(reactions);
        Map<Integer, ReactionList> sortedAndOrdered = new HashMap<Integer, ReactionList>();
        for (Integer i : sorted.keySet()) {
            sortedAndOrdered.put(i, getOrderedReactionList(sorted.get(i)));
        }
        int centre = getCentre(sortedAndOrdered);
        ReactionList rList = new ReactionList();

        int numReactions = 10;
        ArrayList<Integer> ordered = getOrderedBlockList(sorted);
        // only neighbours will be regarded
        int n1 = centre - 1;
        int n2 = centre + 1;
        int ctrAdded = 0;

        ReactionList completeRList = new ReactionList(sortedAndOrdered.get(centre));




        for (int i = 0; i < regardedBlocks; i++) {
            for (int j = 0; j < ordered.size(); j++) {
                if (ordered.get(j) == n1) {
                    rList.addAll(sortedAndOrdered.get(n1));
                    n1--;
                    break;
                }
                if (ordered.get(j) == n2) {
                    rList.addAll(sortedAndOrdered.get(n2));
                    n2++;
                    break;
                }
            }
        }
        return rList;
    }

    protected ReactionList selectUsingPercentage(ReactionList reactions) {
        Map<Integer, ReactionList> sorted = organise(reactions);
        int centre = getCentre(sorted);

        return null;
    }

    protected ReactionList getOrderedReactionList(ReactionList list) {
        ReactionList orderedReactionList = new ReactionList(list);
        for (int i = 0; i < list.size(); i++) {
            for (int j = i + 1; j < list.size(); j++) {
                if (list.get(i).calculateHf() > list.get(j).calculateHf()) {
                    Reaction r = list.get(i);
                    list.set(i, list.get(j));
                    list.set(j, r);
                }
            }
        }
        return orderedReactionList;
    }

    protected ArrayList<Integer> getOrderedBlockList(Map<Integer, ReactionList> sorted) {
        ArrayList<Integer> orderedBlocks = new ArrayList<Integer>();
        for (int i = 0; i < numBlocks; i++) {
            orderedBlocks.add(i);
        }
        for (int i = 0; i < numBlocks; i++) {
            for (int j = i + 1; j < numBlocks; j++) {
                if (sorted.get(i).size() < sorted.get(j).size()) {
                    int z = orderedBlocks.get(i);
                    orderedBlocks.set(i, orderedBlocks.get(j));
                    orderedBlocks.set(j, z);
                }
            }
        }
        return orderedBlocks;
    }

    protected Map<Integer, ReactionList> organise(ReactionList reactions) {
        HashMap<Integer, ReactionList> sorted = new HashMap<Integer, ReactionList>();
        double min = Double.MAX_VALUE;
        double max = Double.MIN_VALUE;
        for (int i = 0; i < reactions.size(); i++) {
            double hf = reactions.get(i).calculateHf();
            if (hf < min) {
                min = hf;
            }
            if (hf > max) {
                max = hf;
            }
        }
        double delta = (max - min) / numBlocks;
        for (int i = 0; i < numBlocks; i++) {
            sorted.put(i, new ReactionList());
        }
        for (int i = 0; i < reactions.size(); i++) {
            double hf = reactions.get(i).calculateHf();
            int block = -1;
            if (hf <= max && hf >= max - delta) {
                block = numBlocks - 1;
            } else {
                for (int j = 0; j < numBlocks - 1; j++) {
                    if (hf >= (j) * delta + min && hf < (j + 1) * delta + min) {
                        block = j;
                        break;
                    }
                }
            }
            if (block < 0) {
                System.out.println("ERROR!");
            }
            try {
                sorted.get(block).add(reactions.get(i));
            } catch (NullPointerException npe) {
                System.out.println();
            }
        }
        return sorted;
    }

    protected int getCentre(Map<Integer, ReactionList> sorted) {
        // get the reactions itself
        int maxBlock = -1;
        int ctrMaxBlock = -1;
        for (Integer i : sorted.keySet()) {
            if (sorted.get(i).size() > ctrMaxBlock) {
                maxBlock = i;
                ctrMaxBlock = sorted.get(i).size();
            }
        }
        return maxBlock;
    }
}
