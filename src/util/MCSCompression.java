/*
 * Created on 28/06/2004
 *
 * To change the template for this generated file go to
 * Window&gt;Preferences&gt;Java&gt;Code Generation&gt;Code and Comments
 */
package util;

import java.io.File;

import biginteger.UnserialCompositionMethod;


/**
 * @author glaucio
 *
 * To change the template for this generated type comment go to
 * Window&gt;Preferences&gt;Java&gt;Code Generation&gt;Code and Comments
 */
public class MCSCompression {
	
	private File f;
	private int[][] matrix;
	private int minor;
	private int n = 0;
	private int k = 0;
	
	public MCSCompression(File f) {
		this.f = f;
		this.matrix = ReadingWritingInput.readZigZag(f);
	}
	public int getMinorElement() {
		int min = Integer.MAX_VALUE;
		for (int i = 0; i < matrix.length; i++) {
			for (int j = 0; j < matrix[i].length; j++) {
				if(min > matrix[i][j])
					min = matrix[i][j];
			}
		}
		return Math.abs(min-1);
	}
	
	public int getMaxElement() {
		int min = -Integer.MAX_VALUE;
		for (int i = 0; i < matrix.length; i++) {
			for (int j = 0; j < matrix[i].length; j++) {
				if(min < matrix[i][j])
					min = matrix[i][j];
			}
		}
		return Math.abs(min);
	}
	
	public void normalizeZigZag(int minor) {
		for (int i = 0; i < matrix.length; i++) {
			for (int j = 0; j < matrix[i].length; j++) {
				matrix[i][j] += minor;
			}
		}
	}
	
	public void invertMatrix(int index) {
		for (int i = 0; i < matrix.length; i++) {
			for (int j = 0; j < matrix[i].length; j++) {
				matrix[i][j] *= index;
			}
		}
	}
	
	public String toString() {
		String s = "";
		for (int i = 0; i < matrix.length; i++) {
			for (int j = 0; j < matrix[i].length; j++) {
				s += matrix[i][j] + " ";
			}
			s += "\n";
		}
		return s;
	}
	
	public String getVector(int[][] matrix) {
		String s = "";
		n = k = 0;
		for (int i = 0; i < matrix.length; i++) {
			for (int j = 0; j < matrix[i].length; j++) {
				s += matrix[i][j] + " ";
				n += matrix[i][j];
				k++;
			}
			s += "0 ";
			k++;
		}
		return s;
	}
	
	public int[] getCompositionFromVector(String s) {
		String parts[] = s.split(" ");
		int[] result = new int[parts.length];
		for(int i = 0; i < result.length; i++)
			result[i] = Integer.parseInt(parts[i]);
		return result;
	}
	
	public void routine() {
		int comp = getMinorElement();
		normalizeZigZag(comp);
	/*	String s = getVector(matrix);
		int[] composition = getCompositionFromVector(s);
		UnserialCompositionMethod method = new UnserialCompositionMethod(composition);
		for (int i = 0; i < composition.length; i++) {
			System.out.print(composition[i] + " ");
		}
		System.out.println();
		method.unserialCompositionAlgorithm(composition);
		System.out.println(n + " " + k + " " + method.getSerial());*/
		
		String[][] result = new String[matrix.length][3];
		
		for (int i = 0; i < matrix.length; i++) {
			UnserialCompositionMethod method = new UnserialCompositionMethod(matrix[3]);
			method.unserialCompositionAlgorithm(matrix[i]);
			result[i][0] = ""+method.getN();
			result[i][1] = ""+method.getK();
			result[i][2] = method.getSerial();
			System.out.println(result[i][0] + " " + result[i][1] + " " + result[i][2]);			
		}
	/*	int[][] resDescomp = new int[matrix.length][];
		for (int i = 0; i < resDescomp.length; i++) {
			SerialCompositionMethod test = new SerialCompositionMethod(Integer.parseInt(result[i][0]),Integer.parseInt(result[i][1]));
			test.serialComposition(result[i][2]);
			resDescomp[i] = test.getComposition();
			System.out.println(i);
		}
		matrix = resDescomp;
		normalizeZigZag((-1) * comp);
		resDescomp = matrix;
		for (int i = 0; i < resDescomp.length; i++) {
			for (int j = 0; j < resDescomp[i].length; j++) {
				System.out.print(resDescomp[i][j] + " ");
			}
			System.out.println();
		}
	/*	for (int i = 0; i < matrix.length; i++) {
			UnserialCompositionMethod method = new UnserialCompositionMethod(matrix[3]);
			method.unserialCompositionAlgorithm(matrix[i]);
			System.out.println(method);			
		}*/

	}
	
	public static void main(String[] args) {
		MCSCompression teste = new MCSCompression(new File("./outputmatrix.pgm"));
	/* int comp = teste.getMinorElement();
		System.out.println("minor = "+comp);
		teste.normalizeZigZag(comp);
		System.out.println(comp);
		System.out.println(teste);
		teste.normalizeZigZag(-comp);*/
		teste.routine();
//		System.out.println(teste);
	}
}
