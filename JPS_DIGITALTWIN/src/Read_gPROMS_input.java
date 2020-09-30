import java.io.File;  
import java.io.FileInputStream;  
import java.io.IOException; 
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import org.apache.poi.xssf.usermodel.XSSFSheet;  
import org.apache.poi.xssf.usermodel.XSSFWorkbook; 
import org.apache.poi.ss.usermodel.Cell;  
import org.apache.poi.ss.usermodel.FormulaEvaluator;  
import org.apache.poi.ss.usermodel.Row;


public class Read_gPROMS_input {
	
	public static void main(String args[]) throws IOException  
	{
	
		List sheetData = new ArrayList();
	//obtaining input bytes from a file  
	FileInputStream fis=new FileInputStream(new File("/Users/gourab/JParkSimulator-git/JPS_DIGITALTWIN/res/output/gPROMS_output.xlsx"));
	

	//creating workbook instance that refers to .xls file  
	XSSFWorkbook wb=new XSSFWorkbook(fis);   
	//creating a Sheet object to retrieve the object  
	XSSFSheet sheet=wb.getSheetAt(2472);
	
	
	//evaluating cell type   
	FormulaEvaluator formulaEvaluator=wb.getCreationHelper().createFormulaEvaluator();  //file initialization and excel header
	for(Row row: sheet)     //iteration over row using for each loop  
	{  
	for(Cell cell: row)    //iteration over cell using for each loop  
	{  
	switch(formulaEvaluator.evaluateInCell(cell).getCellType())  
	{  
	case Cell.CELL_TYPE_NUMERIC:   //field that represents numeric cell type  
	//getting the value of the cell as a number  
	System.out.print(cell.getNumericCellValue()+ "\t\t");   //append the value creating rows
	break;  
	case Cell.CELL_TYPE_STRING:    //field that represents string cell type  
	//getting the value of the cell as a string  
	System.out.print(cell.getStringCellValue()+ "\t\t");  //append the value creating rows
	break;  
	}  
	}  
	System.out.println();  
	}  
	}

}
