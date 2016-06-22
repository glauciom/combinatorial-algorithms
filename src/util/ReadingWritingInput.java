/*
 * Created on 11/04/2004
 *
 * To change the template for this generated file go to
 * Window&gt;Preferences&gt;Java&gt;Code Generation&gt;Code and Comments
 */
package util;

import java.io.*;
import java.util.StringTokenizer;
import java.util.Vector;

/**
 * @author Glaucio
 *
 * To change the template for this generated type comment go to
 * Window&gt;Preferences&gt;Java&gt;Code Generation&gt;Code and Comments
 */
public class ReadingWritingInput {

	private static int[][] finalOutPut;
	public static int width, height;
	public static int levels;
	
	public static int[][] readZigZag(File f) {
		StringBuffer buffer = new StringBuffer();
		int[][] output = null;
		try {
			BufferedReader reader = new BufferedReader(new FileReader(f));
			String s;
			for(int i = 0; i < 2; i++) {
			 s = reader.readLine();
			 buffer.append(s + "\n");
			}
			s = buffer.toString();
			StringTokenizer token = new StringTokenizer(s, " \n\r");
			int x = Integer.parseInt(token.nextToken());
			width = Integer.parseInt(token.nextToken());
			height = Integer.parseInt(token.nextToken());
			output = new int[x][];
		    Vector v;			
			for(int i = 0; i < x; i++) {
				token = new StringTokenizer(reader.readLine());
				v = new Vector();
			    while(token.hasMoreTokens())
			     v.addElement(token.nextToken());
			    output[i] = new int[v.size()];
			    for(int k = 0; k < v.size(); k++)
			     output[i][k] = Integer.parseInt((String) v.get(k));
			}
			
			int linhas, colunas;
			
			int raiz = (int) Math.floor(Math.sqrt(x)); 
			linhas =  raiz * width;
			colunas = raiz * height;
			finalOutPut = new int[linhas][colunas];

		} catch (Exception e) {
			e.printStackTrace();
		}
		return output;
	}
	
	public static int[][] readMatrix(File f) {
		StringBuffer buffer = new StringBuffer();
		int[][] output = null;
		try {
			BufferedReader reader = new BufferedReader(new FileReader(f));
			String s;
			while ((s = reader.readLine()) != null)
				buffer.append(s + "\n");
			s = buffer.toString();
			StringTokenizer token = new StringTokenizer(s, " \n\r");
			token.nextToken();
			int x = Integer.parseInt(token.nextToken());
			int y = Integer.parseInt(token.nextToken());
			levels = Integer.parseInt(token.nextToken());
			output = new int[x][y];
			for (int i = 0; i < x; i++)
				for (int j = 0; j < y; j++)
					output[i][j] = Integer.parseInt(token.nextToken());

		} catch (Exception e) {
			e.printStackTrace();
		}
		return output;
	}
	
	public static void writeZigZag(int input[][], int width, int height, File f) {
		try {
			BufferedWriter writer = new BufferedWriter(new FileWriter(f));
			writer.write("" + input.length + "\r\n");
			writer.write("" + width + " ");
			writer.write("" + height + "\r\n");
			String s = "";
			for (int i = 0; i < input.length; i++) {
				for (int j = 0; j < input[i].length - 1; j++)
					s = s + input[i][j] + " ";
				writer.write(s + input[i][input[i].length - 1] + "\r\n");
				s = "";
			}
			writer.close();
		} catch (IOException e) {
			e.printStackTrace();
		}
	}

	public static void writeMatrix(int[][] input, File f) {
		try {
			BufferedWriter writer = new BufferedWriter(new FileWriter(f));
			writer.write("P2\r\n");
			writer.write("" + input.length + " ");
			writer.write("" + input[input.length-1].length + "\r\n");
			writer.write("255\r\n");
			String s = "";
			for (int i = 0; i < input.length; i++) {
				for (int j = 0; j < input[i].length - 1; j++)
					s = s + input[i][j] + " ";
				writer.write(s + input[i][input[i].length - 1] + "\r\n");
				s = "";
			}
			writer.close();
		} catch (IOException e) {
			e.printStackTrace();
		}
	}
	public static void main(String[] args) {
		int[][] input = ReadingWritingInput.readMatrix(new File("./matrix.pgm"));
		ReadingWritingInput.writeMatrix(input,new File("./matriz2.pgm"));
		
		int[][] teste = ReadingWritingInput.readZigZag(new File("./saida.pgm"));
		
		ReadingWritingInput.writeZigZag(teste,4,4,new File("./saida2.pgm"));
		
	}
	/**
	 * @return
	 */
	public static int[][] getFinalOutPut() {
		return finalOutPut;
	}

	/**
	 * @return
	 */
	public static int getHeight() {
		return height;
	}

	/**
	 * @return
	 */
	public static int getWidth() {
		return width;
	}

}