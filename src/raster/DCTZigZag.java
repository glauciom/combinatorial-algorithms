/*
 * Created on 11/04/2004
 *
 * To change the template for this generated file go to
 * Window&gt;Preferences&gt;Java&gt;Code Generation&gt;Code and Comments
 */
package raster;

import java.awt.Point;
import java.util.Vector;

/**
 * @author Glaucio
 *
 * To change the template for this generated type comment go to
 * Window&gt;Preferences&gt;Java&gt;Code Generation&gt;Code and Comments
 */
public class DCTZigZag extends GenericZigZag implements MatrixHandler {

	private Point p;
	private int[] output;
	private int index;
	private int[][] finalOutput;

	/**
	 * Construtor para DCTZigZag
	 * @param matriz Quantizada
	 * @param width tamanho do bloco.
	 * @param height tamanho do bloco.
	 */
	public DCTZigZag(int[][] matrix, int width, int height) {
		super(matrix, false,width, height);
	}
	/**
	 * Metodo que copia a matrix de zigue-zague em output
	 * @param output Matrix de saída reconstruida
	 * @param input Matriz zigue-zague
	 * @param width tamanho do bloco
	 * @param height tamanho do bloco
	 */
	public void copyValues(int output[][], int[][] input, int width, int height) {
		int a = width;
		int b = height;
		int i = 0; int j = 0; int u = 0;
			do {
				while(b - height < output.length) {
				 p = super.algorithm(true,i,j,b,a,null);
				 Vector v = getPoints();
				 for(int k = 0; k < v.size(); k++) {
				  p = (Point) v.get(k);
				  if (k < input[u].length) { 
				   output[p.x][p.y] = input[u][k]; 				  	
				  } 
				 }
				 u++;
				 b = b + height;
				 j = j + height;
			    }
			    i = i + width;
			    a = a + width;
			    j = 0;
			    b = height;
			} while(u != input.length);
	}

	/**
	 * Algoritmo que Retorna a matrix de ZigZag 
	 * @param width tamanho do bloco 
	 * @param height Tamanho bloco
	 * @return Matrix ziguezague.
	 */
	public int[][] algorithm(int width, int height) {
		int[] d = null;
		int a = width;
		Vector v = new Vector();
		this.width = width;
		this.height = height;
		for (int i = 0; i < matrix.length; i = i + this.width) {
			for (int j = 0; j < matrix[i].length; j = j + this.height) {
				d = algorithm(i, j, j + height, a);
				v.addElement(d);
			}
			a += width;
		}
		return copyArrays(v);
	}

	public int[][] copyArrays(Vector v) {
		finalOutput = new int[v.size()][];
		for (int i = 0; i < v.size(); i++)
			finalOutput[i] = (int[]) v.get(i);
		return finalOutput;
	}

	public int[] algorithm(int x, int y, int width, int height) {
		p = super.algorithm(false, x, y, width, height, this);
		int outputSize = this.width * this.height - getCountNumbers();
		if (outputSize == 0) {
			output = new int[1];
			output[0] = 0;
		} else {
			output = new int[outputSize];
			p = super.algorithm(true, x, y, width, height, this);
		}
		return output;
	}

	/* (non-Javadoc)
	 * @see raster.MatrixHandler#processAscendentHandle(double[][], int, int)
	 */
	public Point processAscendentHandle(int[][] m, int i, int j) {
		Point p1 = new Point(i, j);
		output[index++] = m[i][j];
		if (p.equals(p1)) {
			index = 0;
			return new Point(i, j);
		}
		return null;
	}

	/* (non-Javadoc)
	 * @see raster.MatrixHandler#processDescendentHandle(double[][], int, int)
	 */
	public Point processDescendentHandle(int[][] m, int i, int j) {
		if (m[i][j] != 0)
			return new Point(i, j);
		return null;
	}

	/**
	 * @return
	 */
	public int[] getOutput() {
		return output;
	}

	/**
	 * @param ds
	 */
	public void setOutput(int[] ds) {
		output = ds;
	}

	public String toString() {
		StringBuffer k = new StringBuffer();
		for (int i = 0; i < finalOutput.length; i++) {
			for (int j = 0; j < finalOutput[i].length; j++)
				k.append(finalOutput[i][j] + " ");
			k.append("\n");
		}
		return k.toString();
	}

}