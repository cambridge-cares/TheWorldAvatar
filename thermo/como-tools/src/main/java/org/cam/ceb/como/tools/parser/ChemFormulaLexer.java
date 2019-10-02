/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package org.cam.ceb.como.tools.parser;

import org.cam.ceb.como.tools.pattern.composite.Component;
import org.cam.ceb.como.tools.pattern.composite.Composite;
import org.cam.ceb.como.tools.pattern.composite.Leaf;
import java.util.ArrayList;
import java.util.List;

/**
 *
 * @author pb556
 */
public class ChemFormulaLexer extends Lexer {

    // product
    protected Component root = new CompositeToken();

    @Override
    public void build() throws Exception {
        if (this.obj == null) {
            throw new Exception("No value is defined!");
        }

        // split into lines!
        List<String> commands = this.getCommands();
        this.root = new CompositeToken();
        root.set(this.obj);
        for (String cmd : commands) {
            ((CompositeToken) this.root).add(next(new Token(cmd, 0)));
        }
    }

    protected List<String> getCommands() {
        String[] lines = ((String) this.obj).split("\n");
        List<String> commands = new ArrayList<String>();
        if (!((String) this.obj).contains("\\.")) {
            commands.add(lines[0]);
        } else {
            for (String line : lines) {
                String[] items = line.split("\\.");
                if (items.length > 1) {
                    String cmd = "";
                    for (int i = 1; i < items.length; i++) {
                        cmd += items[i];
                    }
                    commands.add(cmd.trim());
                }
            }
        }
        return commands;
    }

    protected Component next(Token root) {
        List<Token> extractedTokens = this.extractTokens(root.getChemicalFormula());
        if (extractedTokens.size() > 1) {
            // create different levels
            // identify tokens on the current level
            // identify tokens on the next level
            List<Token> currentLevel = new ArrayList<Token>();
            List<Token> nextLevel = new ArrayList<Token>();
            for (Token token : extractedTokens) {
                if (token.getNumberOfBranches() == 0) {
                    currentLevel.add(token);
                } else if (token.getNumberOfBranches() > 0) {
                    nextLevel.add(token);
                }
            }

            // combine the ones
            Composite composite = new CompositeToken();
            composite.set(currentLevel);
            ((CompositeToken) composite).setDescription(root.chemFormula);
            // all tokens are new branches
            for (int i = 0; i < nextLevel.size(); i++) {
                Component comp = next(nextLevel.get(i));
                if (comp != null) {
                    composite.add(comp);
                }
            }
            return composite;
        }

        Leaf token = new Token();
        token.set(root);
        return token;
    }

    //"([A-Z][a-z]?)(\d*)"
    protected List<Token> extractTokens(String value) {
        List<Token> identifiedTokens = new ArrayList<Token>();
        String currentValue = "";
        int ctrOpen = 0;
        for (int i = 0; i < value.length(); i++) {
            currentValue += value.charAt(i);
            if (value.charAt(i) == '(') {
                ctrOpen++;
                if (currentValue.length() != 1) {
                    // check for numbers
                    identifiedTokens.add(new Token(this.shrinkBrackets(currentValue.substring(0, currentValue.length() - 1)), 0));
                    currentValue = "" + value.charAt(i);
                }
                continue;
            } else if (value.charAt(i) == ')') {
                ctrOpen--;
                if (ctrOpen == 0) {
                    if (currentValue.length() != 0) {
                        // check for numbers
                        String number = "";
                        for (int j = i + 1; j < value.length(); j++) {
                            if (!this.isNumber(value.charAt(j))) {
                                break;
                            } else {
                                number += value.charAt(j);
                                i++;
                            }
                        }
                        if (number.length() != 0) {
                            identifiedTokens.add(new Token(this.shrinkBrackets(currentValue), Integer.parseInt(number)));
                        } else {
                            identifiedTokens.add(new Token(this.shrinkBrackets(currentValue), 1));
                        }
                        currentValue = "";
                    }
                    continue;
                }
            }
        }
        if (currentValue.length() != 0) {
            identifiedTokens.add(new Token(this.shrinkBrackets(currentValue), 0));
        }
        return identifiedTokens;
    }

    private String shrinkBrackets(String str) {
        String newStr = str;
        boolean remBrackets = true;
        while (remBrackets) {
            if (newStr.startsWith("(") && newStr.endsWith(")")) {
                newStr = newStr.substring(1, newStr.length() - 1);
            } else {
                remBrackets = false;
            }
        }
        return newStr;
    }

    private boolean isNumber(char c) {
        switch (c) {
            case '0':
                return true;
            case '1':
                return true;
            case '2':
                return true;
            case '3':
                return true;
            case '4':
                return true;
            case '5':
                return true;
            case '6':
                return true;
            case '7':
                return true;
            case '8':
                return true;
            case '9':
                return true;
        }
        return false;
    }

    @Override
    public Object getProduct() {
        return this.root;
    }
}