/*
 * Created on 12/09/2004
 *
 * TODO To change the template for this generated file go to
 * Window - Preferences - Java - Code Style - Code Templates
 */
package sa;

import java.io.File;

/**
 * Classe que encapsula as caracter�sticas do dicion�rio.
 * 
 * @author GLAUCIO
 */
public class CodeBook {
	/**
	 * Representa os dados do Dicion�rio.
	 */
	private int data[][];

	/**
	 * Numero de vetores do dicion�rio.
	 */
	private int numberOfVectors;

	/**
	 * Dimensao do vetor.
	 */
	private int dimension;

	/**
	 * Construtor para CodeBook (padr�o Java Beans);
	 */
	public CodeBook() {
		this.data = null;
	}

	/**
	 * Construtor para codebook.
	 * 
	 * @param f
	 *            Nome do Arquivo do dicion�rio.
	 * @param numberOfVectors
	 *            N�mero de vetores do dicion�rio.
	 * @param dimension
	 *            Dimensao do vetor
	 */
	public CodeBook(File f, int numberOfVectors, int dimension) {
		this.init(f, numberOfVectors, dimension);
	}

	/**
	 * Inicializa��o do objeto;
	 * 
	 * @param f
	 *            Nome do Arquivo do dicion�rio.
	 * @param numberOfVectors
	 *            N�mero de vetores do dicion�rio.
	 * @param dimension
	 *            Dimensao do Vetor.
	 */
	public void init(File f, int numberOfVectors, int dimension) {
		this.numberOfVectors = numberOfVectors;
		this.dimension = dimension;
		this.data = Utils.initCodeBook(f, numberOfVectors, dimension);
	}

	/**
	 * Metodo sobreposto para a sa�da padr�o do objeto CodeBook.
	 */
	public String toString() {
		StringBuffer k = new StringBuffer();
		for (int i = 0; i < data.length; i++) {
			for (int j = 0; j < data[i].length; j++) {
				k.append(data[i][j] + " ");
			}
			k.append("\n");
		}
		return k.toString();
	}

	/**
	 * Captura os dados do Dicion�rio.
	 * 
	 * @return Matriz de inteiro que representa o dicion�rio.
	 */
	public int[][] getData() {
		return data;
	}

	/**
	 * Altera as informa��es do dicion�rio.
	 * 
	 * @param data
	 *            Nova matriz que representa o dicionario.
	 */
	public void setData(int[][] data) {
		this.data = data;
	}

	/**
	 * @return Returns the dimension.
	 */
	public int getDimension() {
		return dimension;
	}

	/**
	 * @param dimension
	 *            The dimension to set.
	 */
	public void setDimension(int dimension) {
		this.dimension = dimension;
	}

	/**
	 * @return Returns the numberOfVectors.
	 */
	public int getNumberOfVectors() {
		return numberOfVectors;
	}

	/**
	 * @param numberOfVectors
	 *            The numberOfVectors to set.
	 */
	public void setNumberOfVectors(int numberOfVectors) {
		this.numberOfVectors = numberOfVectors;
	}

	/**
	 * Metodo main (para testes).
	 * 
	 * @param args
	 *            String do prompt de comando.
	 */
	public static void main(String[] args) {
		CodeBook test = new CodeBook(new File(
				"src\\codebooks\\airplane_boat_gull_goldhill_128.dic"), 128, 16);
		System.out.println(test);
	}
}