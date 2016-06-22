/*
 * Created on 12/09/2004
 *
 * TODO To change the template for this generated file go to
 * Window - Preferences - Java - Code Style - Code Templates
 */
package sa;

import java.io.File;

/**
 * Classe que encapsula as características do dicionário.
 * 
 * @author GLAUCIO
 */
public class CodeBook {
	/**
	 * Representa os dados do Dicionário.
	 */
	private int data[][];

	/**
	 * Numero de vetores do dicionário.
	 */
	private int numberOfVectors;

	/**
	 * Dimensao do vetor.
	 */
	private int dimension;

	/**
	 * Construtor para CodeBook (padrão Java Beans);
	 */
	public CodeBook() {
		this.data = null;
	}

	/**
	 * Construtor para codebook.
	 * 
	 * @param f
	 *            Nome do Arquivo do dicionário.
	 * @param numberOfVectors
	 *            Número de vetores do dicionário.
	 * @param dimension
	 *            Dimensao do vetor
	 */
	public CodeBook(File f, int numberOfVectors, int dimension) {
		this.init(f, numberOfVectors, dimension);
	}

	/**
	 * Inicialização do objeto;
	 * 
	 * @param f
	 *            Nome do Arquivo do dicionário.
	 * @param numberOfVectors
	 *            Número de vetores do dicionário.
	 * @param dimension
	 *            Dimensao do Vetor.
	 */
	public void init(File f, int numberOfVectors, int dimension) {
		this.numberOfVectors = numberOfVectors;
		this.dimension = dimension;
		this.data = Utils.initCodeBook(f, numberOfVectors, dimension);
	}

	/**
	 * Metodo sobreposto para a saída padrão do objeto CodeBook.
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
	 * Captura os dados do Dicionário.
	 * 
	 * @return Matriz de inteiro que representa o dicionário.
	 */
	public int[][] getData() {
		return data;
	}

	/**
	 * Altera as informações do dicionário.
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