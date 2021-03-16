/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package uk.ac.cam.ceb.como.tools.util;

import uk.ac.cam.ceb.como.tools.util.StringUtil;
import java.util.ArrayList;
import java.util.Arrays;
import org.junit.Test;

/**
 *
 * @author pb556
 */
public class StringUtilTest {

    @Test
    public void isLetterTest() {
        // char
        assert (StringUtil.isLetter('a'));
        assert (StringUtil.isLetter('b'));
        assert (StringUtil.isLetter('c'));
        assert (StringUtil.isLetter('d'));
        assert (StringUtil.isLetter('e'));
        assert (StringUtil.isLetter('f'));
        assert (StringUtil.isLetter('g'));
        assert (StringUtil.isLetter('h'));
        assert (StringUtil.isLetter('i'));
        assert (StringUtil.isLetter('j'));
        assert (StringUtil.isLetter('k'));
        assert (StringUtil.isLetter('l'));
        assert (StringUtil.isLetter('m'));
        assert (StringUtil.isLetter('n'));
        assert (StringUtil.isLetter('o'));
        assert (StringUtil.isLetter('p'));
        assert (StringUtil.isLetter('q'));
        assert (StringUtil.isLetter('r'));
        assert (StringUtil.isLetter('s'));
        assert (StringUtil.isLetter('t'));
        assert (StringUtil.isLetter('u'));
        assert (StringUtil.isLetter('v'));
        assert (StringUtil.isLetter('w'));
        assert (StringUtil.isLetter('x'));
        assert (StringUtil.isLetter('y'));
        assert (StringUtil.isLetter('z'));

        assert (StringUtil.isLetter('A'));
        assert (StringUtil.isLetter('B'));
        assert (StringUtil.isLetter('C'));
        assert (StringUtil.isLetter('D'));
        assert (StringUtil.isLetter('E'));
        assert (StringUtil.isLetter('F'));
        assert (StringUtil.isLetter('G'));
        assert (StringUtil.isLetter('H'));
        assert (StringUtil.isLetter('I'));
        assert (StringUtil.isLetter('J'));
        assert (StringUtil.isLetter('K'));
        assert (StringUtil.isLetter('L'));
        assert (StringUtil.isLetter('M'));
        assert (StringUtil.isLetter('N'));
        assert (StringUtil.isLetter('O'));
        assert (StringUtil.isLetter('P'));
        assert (StringUtil.isLetter('Q'));
        assert (StringUtil.isLetter('R'));
        assert (StringUtil.isLetter('S'));
        assert (StringUtil.isLetter('T'));
        assert (StringUtil.isLetter('U'));
        assert (StringUtil.isLetter('V'));
        assert (StringUtil.isLetter('W'));
        assert (StringUtil.isLetter('X'));
        assert (StringUtil.isLetter('Y'));
        assert (StringUtil.isLetter('Z'));

        assert (!StringUtil.isLetter('1'));
        assert (!StringUtil.isLetter('.'));
        assert (!StringUtil.isLetter(','));
        assert (!StringUtil.isLetter(']'));
        assert (!StringUtil.isLetter('3'));
        assert (!StringUtil.isLetter('%'));
        assert (!StringUtil.isLetter(')'));

        // string
        assert (StringUtil.areLetters("abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"));

        assert (!StringUtil.areLetters("32abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"));
        assert (!StringUtil.areLetters("}abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"));
        assert (!StringUtil.areLetters("&2abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"));
        assert (!StringUtil.areLetters("abcdefghijklmnopqrstuvwxyzA5BCDEFGHIJKLMNOPQRSTUVWXYZ"));
    }

    @Test
    public void isNumberTest() {
        assert (StringUtil.isNumber('0'));
        assert (StringUtil.isNumber('1'));
        assert (StringUtil.isNumber('2'));
        assert (StringUtil.isNumber('3'));
        assert (StringUtil.isNumber('4'));
        assert (StringUtil.isNumber('5'));
        assert (StringUtil.isNumber('6'));
        assert (StringUtil.isNumber('7'));
        assert (StringUtil.isNumber('8'));
        assert (StringUtil.isNumber('9'));

        assert (!StringUtil.isNumber(','));
        assert (!StringUtil.isNumber('.'));

        assert (StringUtil.isNumber("0"));
        assert (StringUtil.isNumber("1"));
        assert (StringUtil.isNumber("2"));
        assert (StringUtil.isNumber("3"));
        assert (StringUtil.isNumber("4"));
        assert (StringUtil.isNumber("5"));
        assert (StringUtil.isNumber("6"));
        assert (StringUtil.isNumber("7"));
        assert (StringUtil.isNumber("8"));
        assert (StringUtil.isNumber("9"));

        assert (!StringUtil.isNumber(","));
        assert (!StringUtil.isNumber("."));

    }

    @Test
    public void isLowerCaseTest() {
        assert (StringUtil.isLowerCase('a'));
        assert (StringUtil.isLowerCase('b'));
        assert (StringUtil.isLowerCase('c'));
        assert (StringUtil.isLowerCase('d'));
        assert (StringUtil.isLowerCase('e'));
        assert (StringUtil.isLowerCase('f'));
        assert (StringUtil.isLowerCase('g'));
        assert (StringUtil.isLowerCase('h'));
        assert (StringUtil.isLowerCase('i'));
        assert (StringUtil.isLowerCase('j'));
        assert (StringUtil.isLowerCase('k'));
        assert (StringUtil.isLowerCase('l'));
        assert (StringUtil.isLowerCase('m'));
        assert (StringUtil.isLowerCase('n'));
        assert (StringUtil.isLowerCase('o'));
        assert (StringUtil.isLowerCase('p'));
        assert (StringUtil.isLowerCase('q'));
        assert (StringUtil.isLowerCase('r'));
        assert (StringUtil.isLowerCase('s'));
        assert (StringUtil.isLowerCase('t'));
        assert (StringUtil.isLowerCase('u'));
        assert (StringUtil.isLowerCase('v'));
        assert (StringUtil.isLowerCase('w'));
        assert (StringUtil.isLowerCase('x'));
        assert (StringUtil.isLowerCase('y'));
        assert (StringUtil.isLowerCase('z'));

        assert (!StringUtil.isLowerCase('A'));
        assert (!StringUtil.isLowerCase('B'));
        assert (!StringUtil.isLowerCase('C'));
        assert (!StringUtil.isLowerCase('D'));
        assert (!StringUtil.isLowerCase('E'));
        assert (!StringUtil.isLowerCase('F'));
        assert (!StringUtil.isLowerCase('G'));
        assert (!StringUtil.isLowerCase('H'));
        assert (!StringUtil.isLowerCase('I'));
        assert (!StringUtil.isLowerCase('J'));
        assert (!StringUtil.isLowerCase('K'));
        assert (!StringUtil.isLowerCase('L'));
        assert (!StringUtil.isLowerCase('M'));
        assert (!StringUtil.isLowerCase('N'));
        assert (!StringUtil.isLowerCase('O'));
        assert (!StringUtil.isLowerCase('P'));
        assert (!StringUtil.isLowerCase('Q'));
        assert (!StringUtil.isLowerCase('R'));
        assert (!StringUtil.isLowerCase('S'));
        assert (!StringUtil.isLowerCase('T'));
        assert (!StringUtil.isLowerCase('U'));
        assert (!StringUtil.isLowerCase('V'));
        assert (!StringUtil.isLowerCase('W'));
        assert (!StringUtil.isLowerCase('X'));
        assert (!StringUtil.isLowerCase('Y'));
        assert (!StringUtil.isLowerCase('Z'));

        assert (!StringUtil.isLowerCase('1'));
        assert (!StringUtil.isLowerCase('.'));
        assert (!StringUtil.isLowerCase(','));
        assert (!StringUtil.isLowerCase(']'));
        assert (!StringUtil.isLowerCase('3'));
        assert (!StringUtil.isLowerCase('%'));
        assert (!StringUtil.isLowerCase(')'));

        assert (StringUtil.isLowerCase("abcdefghijklmnopqrstuvwxyz"));
        assert (!StringUtil.isLowerCase("abcdefghijklmnopqrstu58vwxyz"));
        assert (!StringUtil.isLowerCase("abcd%efghijklmnopqrstuvwxyz"));
    }

    @Test
    public void isUpperCaseTest() {
        assert (!StringUtil.isUpperCase('a'));
        assert (!StringUtil.isUpperCase('b'));
        assert (!StringUtil.isUpperCase('c'));
        assert (!StringUtil.isUpperCase('d'));
        assert (!StringUtil.isUpperCase('e'));
        assert (!StringUtil.isUpperCase('f'));
        assert (!StringUtil.isUpperCase('g'));
        assert (!StringUtil.isUpperCase('h'));
        assert (!StringUtil.isUpperCase('i'));
        assert (!StringUtil.isUpperCase('j'));
        assert (!StringUtil.isUpperCase('k'));
        assert (!StringUtil.isUpperCase('l'));
        assert (!StringUtil.isUpperCase('m'));
        assert (!StringUtil.isUpperCase('n'));
        assert (!StringUtil.isUpperCase('o'));
        assert (!StringUtil.isUpperCase('p'));
        assert (!StringUtil.isUpperCase('q'));
        assert (!StringUtil.isUpperCase('r'));
        assert (!StringUtil.isUpperCase('s'));
        assert (!StringUtil.isUpperCase('t'));
        assert (!StringUtil.isUpperCase('u'));
        assert (!StringUtil.isUpperCase('v'));
        assert (!StringUtil.isUpperCase('w'));
        assert (!StringUtil.isUpperCase('x'));
        assert (!StringUtil.isUpperCase('y'));
        assert (!StringUtil.isUpperCase('z'));

        assert (StringUtil.isUpperCase('A'));
        assert (StringUtil.isUpperCase('B'));
        assert (StringUtil.isUpperCase('C'));
        assert (StringUtil.isUpperCase('D'));
        assert (StringUtil.isUpperCase('E'));
        assert (StringUtil.isUpperCase('F'));
        assert (StringUtil.isUpperCase('G'));
        assert (StringUtil.isUpperCase('H'));
        assert (StringUtil.isUpperCase('I'));
        assert (StringUtil.isUpperCase('J'));
        assert (StringUtil.isUpperCase('K'));
        assert (StringUtil.isUpperCase('L'));
        assert (StringUtil.isUpperCase('M'));
        assert (StringUtil.isUpperCase('N'));
        assert (StringUtil.isUpperCase('O'));
        assert (StringUtil.isUpperCase('P'));
        assert (StringUtil.isUpperCase('Q'));
        assert (StringUtil.isUpperCase('R'));
        assert (StringUtil.isUpperCase('S'));
        assert (StringUtil.isUpperCase('T'));
        assert (StringUtil.isUpperCase('U'));
        assert (StringUtil.isUpperCase('V'));
        assert (StringUtil.isUpperCase('W'));
        assert (StringUtil.isUpperCase('X'));
        assert (StringUtil.isUpperCase('Y'));
        assert (StringUtil.isUpperCase('Z'));

        assert (!StringUtil.isUpperCase('1'));
        assert (!StringUtil.isUpperCase('.'));
        assert (!StringUtil.isUpperCase(','));
        assert (!StringUtil.isUpperCase(']'));
        assert (!StringUtil.isUpperCase('3'));
        assert (!StringUtil.isUpperCase('%'));
        assert (!StringUtil.isUpperCase(')'));

        assert (StringUtil.isUpperCase("ABCDEFGHIJKLMNOPQRSTUVWXYZ"));
        assert (!StringUtil.isUpperCase("ABCD4EFGHIJKLMNOPQRSTUVWXYZ"));
        assert (!StringUtil.isUpperCase(".ABCDEFGHIJKLMNOPQRSTUVWXYZ"));
    }

    @Test
    public void isSpecialCharacterTest() {

        assert (!StringUtil.isSpecialCharacter('1'));
        assert (StringUtil.isSpecialCharacter('.'));
        assert (StringUtil.isSpecialCharacter(','));
        assert (StringUtil.isSpecialCharacter(']'));
        assert (!StringUtil.isSpecialCharacter('3'));
        assert (!StringUtil.isSpecialCharacter('a'));
        assert (!StringUtil.isSpecialCharacter('I'));
        assert (StringUtil.isSpecialCharacter('^'));
        assert (StringUtil.isSpecialCharacter('$'));
        assert (StringUtil.isSpecialCharacter('!'));
        assert (StringUtil.isSpecialCharacter('"'));
        assert (StringUtil.isSpecialCharacter('%'));
        assert (StringUtil.isSpecialCharacter(')'));

        assert (StringUtil.isSpecialCharacter("`¬¦!\"£$%^&*()_+=-€|\\,./<>?;'#:@~[]{}/"));
        assert (!StringUtil.isSpecialCharacter("`¬¦!\"£$%^&*()_+=-€|\\,./<>?;'#:@~[]{s}/"));
        assert (!StringUtil.isSpecialCharacter("`¬¦!\"£$%^&*()_+=-€|\\,./<>?;'#:@~[6]{}/"));
    }

    @Test
    public void containsLowerCaseTest() {
        assert (StringUtil.containsLowerCase("abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"));
        assert (StringUtil.containsLowerCase("726374986328795631948aABCDEFGHIJKLMNOPQRSTUVWXYZ"));
        assert (StringUtil.containsLowerCase("`¬¦!\"£$%^&*()_+=-€|\\,./<>?;'#:@~[]{s}/"));
        assert (!StringUtil.containsLowerCase("`¬¦!\"£$%^&*()_+=-€|\\,./<>?;'#:@~[6]{}/"));
    }

    @Test
    public void containsUpperCaseTest() {
        assert (StringUtil.containsUpperCase("abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"));
        assert (StringUtil.containsUpperCase("726374986328795631948asdfasdfdfgdsfgsdfDF"));
        assert (!StringUtil.containsUpperCase("`¬¦!\"£$%^&*()_+=-€|\\,./<>?;'#:@~[]{s}/"));
        assert (StringUtil.containsUpperCase("`¬¦!\"£$%^&*()_+=-€|\\,./<>?;'#:@O~[]{}/"));
        assert (!StringUtil.containsUpperCase("`¬¦!\"£$%^&*()_+=-€|\\,./<>?;'#:@~[6]{}/"));
    }

    @Test
    public void containsNumbersTest() {
        assert (!StringUtil.containsNumbers("abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"));
        assert (StringUtil.containsNumbers("726374986328795631948asdfasdfdfgdsfgsdfDF"));
        assert (!StringUtil.containsNumbers("`¬¦!\"£$%^&*()_+=-€|\\,./<>?;'#:@~[]{s}/"));
        assert (!StringUtil.containsNumbers("`¬¦!\"£$%^&*()_+=-€|\\,./<>?;'#:@O~[]{}/"));
        assert (StringUtil.containsNumbers("`¬¦!\"£$%^&*()_+=-€|\\,./<>?;'#:@~[6]{}/"));
    }

    @Test
    public void containsSpecialCharactersTest() {
        assert (!StringUtil.containsSpecialCharacters("abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"));
        assert (StringUtil.containsSpecialCharacters("726374986328795631948asdfasdfdfgdsfgsdfDF,"));
        assert (StringUtil.containsSpecialCharacters("`¬¦!\"£$%^&*()_+=-€|\\,./<>?;'#:@~[]{s}/"));
        assert (StringUtil.containsSpecialCharacters("`¬¦!\"£$%^&*()_+=-€|\\,./<>?;'#:@O~[]{}/"));
        assert (StringUtil.containsSpecialCharacters("`¬¦!\"£$%^&*()_+=-€|\\,./<>?;'#:@~[6]{}/"));
    }

    @Test
    public void containsLettersTest() {
        assert (StringUtil.containsLetters("abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"));
        assert (StringUtil.containsLetters("726374986328795631948asdfasdfdfgdsfgsdfDF"));
        assert (StringUtil.containsLetters("`¬¦!\"£$%^&*()_+=-€|\\,./<>?;'#:@~[]{s}/"));
        assert (StringUtil.containsLetters("`¬¦!\"£$%^&*()_+=-€|\\,./<>?;'#:@O~[]{}/"));
        assert (!StringUtil.containsLetters("`¬¦!\"£$%^&*()_+=-€|\\,./<>?;'#:@~[6]{}/"));
    }

    @Test
    public void isTest() {

        // char
        assert (StringUtil.is(StringUtil.Category.LOWER, 'a'));
        assert (StringUtil.is(StringUtil.Category.LOWER, 'b'));
        assert (!StringUtil.is(StringUtil.Category.LOWER, 'F'));
        assert (!StringUtil.is(StringUtil.Category.LOWER, '5'));
        assert (!StringUtil.is(StringUtil.Category.LOWER, '.'));

        assert (!StringUtil.is(StringUtil.Category.MIX_LOWER_NUMBER, 'a'));
        assert (!StringUtil.is(StringUtil.Category.MIX_LOWER_NUMBER, 'B'));
        assert (!StringUtil.is(StringUtil.Category.MIX_LOWER_NUMBER, '6'));
        assert (!StringUtil.is(StringUtil.Category.MIX_LOWER_NUMBER, '@'));

        assert (!StringUtil.is(StringUtil.Category.MIX_LOWER_NUMPER_SPECIAL, 'f'));
        assert (!StringUtil.is(StringUtil.Category.MIX_LOWER_NUMPER_SPECIAL, 'D'));
        assert (!StringUtil.is(StringUtil.Category.MIX_LOWER_NUMPER_SPECIAL, '5'));
        assert (!StringUtil.is(StringUtil.Category.MIX_LOWER_NUMPER_SPECIAL, '"'));

        assert (!StringUtil.is(StringUtil.Category.MIX_LOWER_SPECIAL, 'F'));
        assert (!StringUtil.is(StringUtil.Category.MIX_LOWER_SPECIAL, 'a'));
        assert (!StringUtil.is(StringUtil.Category.MIX_LOWER_SPECIAL, '"'));
        assert (!StringUtil.is(StringUtil.Category.MIX_LOWER_SPECIAL, '3'));

        assert (!StringUtil.is(StringUtil.Category.MIX_LOWER_UPPER, 'g'));
        assert (!StringUtil.is(StringUtil.Category.MIX_LOWER_UPPER, 'F'));
        assert (!StringUtil.is(StringUtil.Category.MIX_LOWER_UPPER, '5'));
        assert (!StringUtil.is(StringUtil.Category.MIX_LOWER_UPPER, '~'));

        assert (!StringUtil.is(StringUtil.Category.MIX_LOWER_UPPER_NUMBER, 't'));
        assert (!StringUtil.is(StringUtil.Category.MIX_LOWER_UPPER_NUMBER, 'J'));
        assert (!StringUtil.is(StringUtil.Category.MIX_LOWER_UPPER_NUMBER, '4'));
        assert (!StringUtil.is(StringUtil.Category.MIX_LOWER_UPPER_NUMBER, ';'));

        assert (!StringUtil.is(StringUtil.Category.MIX_LOWER_UPPER_NUMBER_SPECIAL, 'j'));
        assert (!StringUtil.is(StringUtil.Category.MIX_LOWER_UPPER_NUMBER_SPECIAL, 'G'));
        assert (!StringUtil.is(StringUtil.Category.MIX_LOWER_UPPER_NUMBER_SPECIAL, '0'));
//        assert (!StringUtil.is(StringUtil.Category.MIX_LOWER_UPPER_NUMBER_SPECIAL, '£'));

        assert (!StringUtil.is(StringUtil.Category.MIX_LOWER_UPPER_SPECIAL, 'p'));
        assert (!StringUtil.is(StringUtil.Category.MIX_LOWER_UPPER_SPECIAL, 'M'));
        assert (!StringUtil.is(StringUtil.Category.MIX_LOWER_UPPER_SPECIAL, '8'));
        assert (!StringUtil.is(StringUtil.Category.MIX_LOWER_UPPER_SPECIAL, '%'));

        assert (!StringUtil.is(StringUtil.Category.MIX_NUMBER_SPECIAL, 'q'));
        assert (!StringUtil.is(StringUtil.Category.MIX_NUMBER_SPECIAL, 'R'));
        assert (!StringUtil.is(StringUtil.Category.MIX_NUMBER_SPECIAL, '7'));
        assert (!StringUtil.is(StringUtil.Category.MIX_NUMBER_SPECIAL, '&'));

        assert (!StringUtil.is(StringUtil.Category.MIX_UPPER_NUMBER, 'm'));
        assert (!StringUtil.is(StringUtil.Category.MIX_UPPER_NUMBER, 'I'));
        assert (!StringUtil.is(StringUtil.Category.MIX_UPPER_NUMBER, '9'));
//        assert (!StringUtil.is(StringUtil.Category.MIX_UPPER_NUMBER, '€'));

        assert (!StringUtil.is(StringUtil.Category.MIX_UPPER_NUMBER_SPECIAL, 'n'));
        assert (!StringUtil.is(StringUtil.Category.MIX_UPPER_NUMBER_SPECIAL, 'W'));
        assert (!StringUtil.is(StringUtil.Category.MIX_UPPER_NUMBER_SPECIAL, '2'));
        assert (!StringUtil.is(StringUtil.Category.MIX_UPPER_NUMBER_SPECIAL, '%'));

        assert (StringUtil.is(StringUtil.Category.NUMBER, '4'));
        assert (StringUtil.is(StringUtil.Category.NUMBER, '5'));
        assert (!StringUtil.is(StringUtil.Category.NUMBER, 's'));
        assert (!StringUtil.is(StringUtil.Category.NUMBER, 'A'));
        assert (!StringUtil.is(StringUtil.Category.NUMBER, '+'));

        assert (StringUtil.is(StringUtil.Category.SPECIAL, '.'));
        assert (StringUtil.is(StringUtil.Category.SPECIAL, '%'));
        assert (!StringUtil.is(StringUtil.Category.SPECIAL, '3'));
        assert (!StringUtil.is(StringUtil.Category.SPECIAL, 'A'));
        assert (!StringUtil.is(StringUtil.Category.SPECIAL, 'y'));

        assert (StringUtil.is(StringUtil.Category.UPPER, 'T'));
        assert (StringUtil.is(StringUtil.Category.UPPER, 'C'));
        assert (!StringUtil.is(StringUtil.Category.UPPER, 'a'));
        assert (!StringUtil.is(StringUtil.Category.UPPER, '5'));
        assert (!StringUtil.is(StringUtil.Category.UPPER, '?'));

        // string
        assert (StringUtil.is(StringUtil.Category.LOWER, "asdfasdfsgryhfgghjgyuklghj"));
        assert (!StringUtil.is(StringUtil.Category.LOWER, "sdfasgfgjhjmbjyguouAdfgdfghgtujgfdhsfg"));
        assert (!StringUtil.is(StringUtil.Category.LOWER, "atrygnjkhuioh34sdfasdfdgdfsg"));
        assert (!StringUtil.is(StringUtil.Category.LOWER, "asdfsdgdfghg,iugfhfgd "));

        assert (StringUtil.is(StringUtil.Category.MIX_LOWER_NUMBER, "dfgsdfgdfsgert4564567654utrhfgughjg"));
        assert (!StringUtil.is(StringUtil.Category.MIX_LOWER_NUMBER, "asdfasdfasdffdghfd6576876hgjghjSdfgdfg456"));
        assert (!StringUtil.is(StringUtil.Category.MIX_LOWER_NUMBER, "sfasdfdsfasdfasdfasdfgdffr"));
        assert (!StringUtil.is(StringUtil.Category.MIX_LOWER_NUMBER, "45656745675665"));
        assert (!StringUtil.is(StringUtil.Category.MIX_LOWER_NUMBER, "shfghjghjgh67867;dfsdgf"));

        assert (StringUtil.is(StringUtil.Category.MIX_LOWER_NUMPER_SPECIAL, "sadfsadfsdfsd345345643564356^&*%$*hgjhgk"));
        assert (!StringUtil.is(StringUtil.Category.MIX_LOWER_NUMPER_SPECIAL, "^&%&$%&%*"));
        assert (!StringUtil.is(StringUtil.Category.MIX_LOWER_NUMPER_SPECIAL, "hjkggfjhnjklhjjughiu"));
        assert (!StringUtil.is(StringUtil.Category.MIX_LOWER_NUMPER_SPECIAL, "156321654651655063"));
        assert (!StringUtil.is(StringUtil.Category.MIX_LOWER_NUMPER_SPECIAL, "156321654651S65506A3"));
        assert (!StringUtil.is(StringUtil.Category.MIX_LOWER_NUMPER_SPECIAL, "sdfasdfsfSdfgdg"));
        assert (!StringUtil.is(StringUtil.Category.MIX_LOWER_NUMPER_SPECIAL, "^&*^%%*&(*^*&%*&^;'/'''][A}{}"));

        assert (StringUtil.is(StringUtil.Category.MIX_LOWER_SPECIAL, "gsdfgdfsgd^$£$&*)].;'.asdf"));
        assert (!StringUtil.is(StringUtil.Category.MIX_LOWER_SPECIAL, "^$£$&*)].;'."));
        assert (!StringUtil.is(StringUtil.Category.MIX_LOWER_SPECIAL, "gsdfgdfsgdasdf"));
        assert (!StringUtil.is(StringUtil.Category.MIX_LOWER_SPECIAL, "gsdfgdfsgd^$£$&*S)].;'.asdf"));
        assert (!StringUtil.is(StringUtil.Category.MIX_LOWER_SPECIAL, "gsdfgdfsgd^$£$&*4)].;'.asdf"));

        assert (StringUtil.is(StringUtil.Category.MIX_LOWER_UPPER, "sdfsSDFDFDSFdsfsdds"));
        assert (!StringUtil.is(StringUtil.Category.MIX_LOWER_UPPER, "DSDFSDFSDFSD"));
        assert (!StringUtil.is(StringUtil.Category.MIX_LOWER_UPPER, "sdfsdfasdfr"));
        assert (!StringUtil.is(StringUtil.Category.MIX_LOWER_UPPER, "dfasdf43534sdfdfdgd"));
        assert (!StringUtil.is(StringUtil.Category.MIX_LOWER_UPPER, "3434"));
        assert (!StringUtil.is(StringUtil.Category.MIX_LOWER_UPPER, "sdfasdfas/.';#dfasd"));

        assert (StringUtil.is(StringUtil.Category.MIX_LOWER_UPPER_NUMBER, "dfasdfsdff567657858BHJGJHGJKghjggy76576"));
        assert (!StringUtil.is(StringUtil.Category.MIX_LOWER_UPPER_NUMBER, "hjksdhfkjasdhl"));
        assert (!StringUtil.is(StringUtil.Category.MIX_LOWER_UPPER_NUMBER, "^*&%^*%&*"));
        assert (!StringUtil.is(StringUtil.Category.MIX_LOWER_UPPER_NUMBER, "686968"));
        assert (!StringUtil.is(StringUtil.Category.MIX_LOWER_UPPER_NUMBER, "BHJDLKFJHS"));
        assert (!StringUtil.is(StringUtil.Category.MIX_LOWER_UPPER_NUMBER, "£BHJDLKFJHS"));
        assert (!StringUtil.is(StringUtil.Category.MIX_LOWER_UPPER_NUMBER, "51651./dsf56"));

        assert (StringUtil.is(StringUtil.Category.MIX_LOWER_UPPER_NUMBER_SPECIAL, "sdfsadfsadfasdfHJKSDHFKJSDHFJHSDKJH76877868778^*&^*&^&*sdfasdf"));
        assert (!StringUtil.is(StringUtil.Category.MIX_LOWER_UPPER_NUMBER_SPECIAL, ""));
        assert (!StringUtil.is(StringUtil.Category.MIX_LOWER_UPPER_NUMBER_SPECIAL, "dfasdfsdff567657858BHJGJHGJKghjggy76576"));
        assert (!StringUtil.is(StringUtil.Category.MIX_LOWER_UPPER_NUMBER_SPECIAL, "hjksdhfkjasdhl"));
        assert (!StringUtil.is(StringUtil.Category.MIX_LOWER_UPPER_NUMBER_SPECIAL, "^*&%^*%&*"));
        assert (!StringUtil.is(StringUtil.Category.MIX_LOWER_UPPER_NUMBER_SPECIAL, "686968"));
        assert (!StringUtil.is(StringUtil.Category.MIX_LOWER_UPPER_NUMBER_SPECIAL, "BHJDLKFJHS"));
        assert (!StringUtil.is(StringUtil.Category.MIX_LOWER_UPPER_NUMBER_SPECIAL, "£BHJDLKFJHS"));
        assert (!StringUtil.is(StringUtil.Category.MIX_LOWER_UPPER_NUMBER_SPECIAL, "51651./dsf56"));

        assert (!StringUtil.is(StringUtil.Category.MIX_LOWER_UPPER_SPECIAL, "sdfasfsdfasdf"));
        assert (!StringUtil.is(StringUtil.Category.MIX_LOWER_UPPER_SPECIAL, "SDFSDFSDFSDF"));
        assert (!StringUtil.is(StringUtil.Category.MIX_LOWER_UPPER_SPECIAL, "$%&%&*(*^*';/."));
        assert (StringUtil.is(StringUtil.Category.MIX_LOWER_UPPER_SPECIAL, "JLSDJFLJFDSJLsldfjas;ldfj;"));
        assert (!StringUtil.is(StringUtil.Category.MIX_LOWER_UPPER_SPECIAL, "JLSDJFLJFDSJLsl34dfjas;ldfj;"));
        assert (!StringUtil.is(StringUtil.Category.MIX_LOWER_UPPER_SPECIAL, "sdfasfsdf344asdf"));
        assert (!StringUtil.is(StringUtil.Category.MIX_LOWER_UPPER_SPECIAL, "SDFSDFS45DFSDF"));
        assert (!StringUtil.is(StringUtil.Category.MIX_LOWER_UPPER_SPECIAL, "$%&%&*(*^*45';/."));

        assert (!StringUtil.is(StringUtil.Category.MIX_NUMBER_SPECIAL, "345634563456"));
        assert (!StringUtil.is(StringUtil.Category.MIX_NUMBER_SPECIAL, "^*%&%&**&^*&"));
        assert (StringUtil.is(StringUtil.Category.MIX_NUMBER_SPECIAL, "345234798570394&(*&*(&*(435"));
        assert (!StringUtil.is(StringUtil.Category.MIX_NUMBER_SPECIAL, "ASDFASDFASDF435345"));
        assert (!StringUtil.is(StringUtil.Category.MIX_NUMBER_SPECIAL, "345345*&()&(*&*(&(sd"));
        assert (!StringUtil.is(StringUtil.Category.MIX_NUMBER_SPECIAL, "asdfasdfasdfas"));

        assert (!StringUtil.is(StringUtil.Category.MIX_UPPER_NUMBER, "DFSDFGDSGSDFGDSFG"));
        assert (!StringUtil.is(StringUtil.Category.MIX_UPPER_NUMBER, "3456456456456"));
        assert (StringUtil.is(StringUtil.Category.MIX_UPPER_NUMBER, "ASDFASDFSD453345345"));
        assert (!StringUtil.is(StringUtil.Category.MIX_UPPER_NUMBER, "SDFSADFSADF43445345s"));
        assert (!StringUtil.is(StringUtil.Category.MIX_UPPER_NUMBER, "SDFSADFSADF4344&5345"));

        assert (StringUtil.is(StringUtil.Category.MIX_UPPER_NUMBER_SPECIAL, "SFSDFSDF454564563564&*&*(&%$$£%"));
        assert (!StringUtil.is(StringUtil.Category.MIX_UPPER_NUMBER_SPECIAL, "SFSDFSDF"));
        assert (!StringUtil.is(StringUtil.Category.MIX_UPPER_NUMBER_SPECIAL, "^*&^*&^&*^*"));
        assert (!StringUtil.is(StringUtil.Category.MIX_UPPER_NUMBER_SPECIAL, "4418415615"));
        assert (!StringUtil.is(StringUtil.Category.MIX_UPPER_NUMBER_SPECIAL, "4418415615sdf"));
        assert (!StringUtil.is(StringUtil.Category.MIX_UPPER_NUMBER_SPECIAL, "4418s415615"));
        assert (!StringUtil.is(StringUtil.Category.MIX_UPPER_NUMBER_SPECIAL, "SFSDFSDF45456456a3564&*&*(&%$$£%"));

        assert (StringUtil.is(StringUtil.Category.NUMBER, "456345635463456"));
        assert (!StringUtil.is(StringUtil.Category.NUMBER, "sdfasdf"));
        assert (!StringUtil.is(StringUtil.Category.NUMBER, "SDFSDF"));
        assert (!StringUtil.is(StringUtil.Category.NUMBER, "'#;'#/"));
        assert (StringUtil.is(StringUtil.Category.NUMBER, "335.345"));

        assert (StringUtil.is(StringUtil.Category.SPECIAL, "!£$%^&*()"));
        assert (!StringUtil.is(StringUtil.Category.SPECIAL, "sdfas"));
        assert (!StringUtil.is(StringUtil.Category.SPECIAL, "4355"));
        assert (!StringUtil.is(StringUtil.Category.SPECIAL, "SDFSD"));
        assert (!StringUtil.is(StringUtil.Category.SPECIAL, "&*%%&ds£"));

        assert (StringUtil.is(StringUtil.Category.UPPER, "HFDKHNJKSDHF"));
        assert (!StringUtil.is(StringUtil.Category.UPPER, "435345"));
        assert (!StringUtil.is(StringUtil.Category.UPPER, "asdfasdfsa"));
        assert (!StringUtil.is(StringUtil.Category.UPPER, "%&^%(^&*("));
        assert (!StringUtil.is(StringUtil.Category.UPPER, "HJDSFJHFKLSaD"));
    }

    @Test
    public void getCategoryTest() {
        assert (StringUtil.getCategory("sdfasdfasdf") == StringUtil.Category.LOWER);
        assert (StringUtil.getCategory("jflsdajflj34jljl") == StringUtil.Category.MIX_LOWER_NUMBER);
        assert (StringUtil.getCategory("sdfjldsfj48395893*(*((sdasd") == StringUtil.Category.MIX_LOWER_NUMPER_SPECIAL);
        assert (StringUtil.getCategory("asdfjljjreytoi*(sdfj") == StringUtil.Category.MIX_LOWER_SPECIAL);
        assert (StringUtil.getCategory("LKJSDflsjdfjsd") == StringUtil.Category.MIX_LOWER_UPPER);
        assert (StringUtil.getCategory("SsfjlsdfDF89") == StringUtil.Category.MIX_LOWER_UPPER_NUMBER);
        assert (StringUtil.getCategory("sdfdsfjlkj894u598345 DSLKJ dsfjla904") == StringUtil.Category.MIX_LOWER_UPPER_NUMBER_SPECIAL);
        assert (StringUtil.getCategory("sldfLKDJ*(skdfjl") == StringUtil.Category.MIX_LOWER_UPPER_SPECIAL);
        assert (StringUtil.getCategory("7698&^&*^86798789") == StringUtil.Category.MIX_NUMBER_SPECIAL);
        assert (StringUtil.getCategory("JSLKDJFLKSDJF89798") == StringUtil.Category.MIX_UPPER_NUMBER);
        assert (StringUtil.getCategory("SDFJDFSLK989898*(*(SFSDF") == StringUtil.Category.MIX_UPPER_NUMBER_SPECIAL);
        assert (StringUtil.getCategory("4156165156") == StringUtil.Category.NUMBER);
        assert (StringUtil.getCategory("^&*(^^*(&^(") == StringUtil.Category.SPECIAL);
        assert (StringUtil.getCategory("SJDFJLSD") == StringUtil.Category.UPPER);

        assert (StringUtil.getCategory('4') == StringUtil.Category.NUMBER);
        assert (StringUtil.getCategory(')') == StringUtil.Category.SPECIAL);
        assert (StringUtil.getCategory('D') == StringUtil.Category.UPPER);
        assert (StringUtil.getCategory('s') == StringUtil.Category.LOWER);
    }

    @Test
    public void containsTest() {
        char[] set = new char[]{'s', '&', '2', 'D', 'P', '.', '5'};

        assert (StringUtil.contains("s&2DP.5", set));
        assert (StringUtil.contains("s&25", set));
        assert (StringUtil.contains("s222222&22DPPPPPPPPD.....P.5", set));

        assert (!StringUtil.contains("s&62DP.5", set));
        assert (!StringUtil.contains("s&2T5", set));
        assert (!StringUtil.contains("s222222q&22DPPPPPPPPD.....P.5", set));

        assert (StringUtil.contains('&', set));
        assert (StringUtil.contains('2', set));
        assert (StringUtil.contains('D', set));

        assert (!StringUtil.contains('Y', set));
        assert (!StringUtil.contains('{', set));
        assert (!StringUtil.contains('o', set));
    }
    
    @Test
    public void extractNumbersTest() {
        String[] nums = StringUtil.extractNumbers("species-1-200-0001.g09");
        ArrayList<String> extractedNums = new ArrayList<String>();
        extractedNums.addAll(Arrays.asList(nums));
        assert(nums.length == 4);
        assert(extractedNums.contains("1"));
        assert(extractedNums.contains("200"));
        assert(extractedNums.contains("0001"));
        assert(extractedNums.contains("09"));
        
        nums = StringUtil.extractNumbers("species-000-sdfas-548.gau");
        extractedNums = new ArrayList<String>();
        extractedNums.addAll(Arrays.asList(nums));
        assert(nums.length == 2);
        assert(extractedNums.contains("000"));
        assert(extractedNums.contains("548"));
        
        nums = StringUtil.extractNumbers("species14564654asdfasdf");
        extractedNums = new ArrayList<String>();
        extractedNums.addAll(Arrays.asList(nums));
        assert(nums.length == 1);
        assert(extractedNums.contains("14564654"));
        
        nums = StringUtil.extractNumbers("4444444");
        extractedNums = new ArrayList<String>();
        extractedNums.addAll(Arrays.asList(nums));
        assert(nums.length == 1);
        assert(extractedNums.contains("4444444"));
    }
}
