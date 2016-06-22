/*
 * Created on 27/01/2004
 *
 * To change the template for this generated file go to
 * Window&gt;Preferences&gt;Java&gt;Code Generation&gt;Code and Comments
 */
package matrix;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;

/**
 * @author Glaucio Melo
 *
 * To change the template for this generated type comment go to
 * Window&gt;Preferences&gt;Java&gt;Code Generation&gt;Code and Comments
 */
public class MatrixOutput {

	private int[][] matrix;
	
	/**
	 * Construtor para MatrixOutPut.
	 * @param matrix Matriz de entrada.
	 */
	public MatrixOutput(int[][] matrix) {
		this.matrix = matrix;
	}
	
	
	public String imprimirEstruturaOpcaoA() {
	 StringBuffer k1 = new StringBuffer();	 
	 k1.append("dim="+matrix.length+ "\n");
	 for(int i = 0; i < matrix.length; i++) {
	  k1.append("lin"+i+"=");
	  for(int j = 0; j < matrix[i].length-1; j++)
	   k1.append(matrix[i][j]+",");
	  k1.append(matrix[i][matrix.length-1]+"\n");
	 }
	return k1.toString();
	}
	
	public String imprimirEstruturaOpcaoB() {
	 StringBuffer k1 = new StringBuffer();	 
	 k1.append("dim="+matrix.length+ "\n");
	 int count = 0;
	 for(int i = 0; i < matrix.length; i++) {
	  for(int j = 0; j < matrix[i].length; j++)
	   if(matrix[i][j] != 0) {
	    k1.append("el"+count+"="+i+","+j+","+matrix[i][j]+"\n");
	    count++;
	   }
	 }
	 k1.append("elementos="+count+"\n");
    return k1.toString();	
	}
	
	public void imprimirArquivoTipoA(String outputFile) {
		try {
		 FileOutputStream out = new FileOutputStream(new File(outputFile));
		 String s = imprimirEstruturaOpcaoA();
		 out.write(s.getBytes());
		 out.close();
		} catch(IOException e) {
			e.printStackTrace();
		}	
	}
	
	public void imprimirArquivoTipoB(String outputFile) {
		try {
		 FileOutputStream out = new FileOutputStream(new File(outputFile));
		 String s = imprimirEstruturaOpcaoB();
		 out.write(s.getBytes());
		 out.close();
		} catch(IOException e) {
			e.printStackTrace();
		}	
	}

	
	public static void main(String[] args) {
		int[][] matrix = {{1,1,0},{2,2,2},{3,4,5}};
		MatrixOutput output = new MatrixOutput(matrix);
		System.out.println(output.imprimirEstruturaOpcaoB());
		output.imprimirArquivoTipoA("C://Documents and Settings//Glaucio Melo//Desktop//saida.ini");
	}
}