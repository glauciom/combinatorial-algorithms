package matrix;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.util.Properties;
import java.util.StringTokenizer;
import java.util.Vector;
/*
 * Created on 15/01/2004
 *
 * To change the template for this generated file go to
 * Window&gt;Preferences&gt;Java&gt;Code Generation&gt;Code and Comments
 */

/**
 * @author Jota
 *
 * To change the template for this generated type comment go to
 * Window&gt;Preferences&gt;Java&gt;Code Generation&gt;Code and Comments
 */
public class Esparse {
	private Vector[] rows;
	private int nrows, ncolumns;

	/**
	 * Constroi uma Matriz Esparsa vazia. 
	 *
	 */
	public Esparse() {
		this.rows = null;
		this.ncolumns = -1;
		this.nrows = -1;
	}

	/**
	 * Constroi uma Matriz Esparsa apartir de um arquivo.
	 * @param file Caminho do arquivo.
	 */
	public Esparse(FileInputStream file) {
		this();
		loadEsparse(file);
	}

	/**
	 * Constroi uma Matriz Esparsa com numero de linhas e colunas especificados.
	 * @param newnrows Numero de linhas da nova matriz. 
	 * @param newncolumns Numero de colunas da nova matriz.
	 */
	public Esparse(int newnrows, int newncolumns) {
		this.nrows = newnrows;
		this.ncolumns = newncolumns;
		this.rows = new Vector[nrows];
	}

	/**
	 * Constroi uma Matriz Esparsa quadrada.
	 * @param nrowcolumn Numero de linhas e colunas da nova matriz.
	 */
	public Esparse(int nrowcolumn) {
		this(nrowcolumn, nrowcolumn);
	}

	/**
	 * Retorna a estrutura que guarda as linhas da Matriz Esparsa.
	 * @return
	 */
	private Vector[] getStruct() {
		return rows;
	}

	/**
	 * Retorna o numero de linhas da Matriz Esparsa.
	 * @return
	 */
	public int getNrows() {
		return nrows;
	}

	/**
	 * Retorna o numero de colunas da Matriz Esparsa
	 * @return
	 */
	public int getNcolumns() {
		return ncolumns;
	}

	/**
	 * Retorna o valor de um determinado elemento da Matriz Esparsa na forma de String.
	 * @param row Linha do Elemento. 
	 * @param column Coluna do Elemento.
	 * @return
	 */
	private String getElementValueStr(int row, int column) {
		Element temp = getElement(row, column);
		if (temp != null)
			return "" + temp.getValue();
		return "0";
	}

	/**
	 * Retorna o valor de um determinado elemento da Matriz Esparsa.
	 * @param row Linha do Elemento
	 * @param column Coluna do Elemento
	 * @return
	 */
	private double getElementValue(int row, int column) {
		Element temp = getElement(row, column);
		if (temp != null)
			return +temp.getValue();
		return 1 - 1;
	}

	/**
	 * Modifica o numero de colunas da Matriz Esparsa.
	 * @param newNcolumns Novo numero de colunas da matriz.
	 */
	public void setNcolumns(int newNcolumns) {
		this.ncolumns = newNcolumns;
	}

	/**
	 * Modifica o numero de linhas da Matriz Esparsa.
	 * @param newNrows Novo numero de linhas da matriz.
	 */
	public void setNrows(int newNrows) {
		nrows = newNrows;
	}

	/**
	 * Adiciona um Elemento na Matriz Esparsa.
	 * @param row Linha do Elemento a ser inserido.
	 * @param column Coluna do Elemento a ser inserido.
	 * @param value Valor do Elemento a ser inserido.
	 */
	public void addElement(int row, int column, double value) {
		if (value != 0) {
			Element temp = new Element(row, column, value);
			if (rows[row] == null && row < nrows)
				//Se o vector correspondente a linha estiver vazio
				rows[row] = new Vector();
			if (column < ncolumns && rows[row].size() < ncolumns) {
				rows[row].addElement(temp);
				if (rows[row].size() > 1) {
					Element temp1 =
						(Element) rows[row].get(rows[row].size() - 2);
					if (temp1 != null
						&& temp.getColumn() < temp1.getColumn()) {
						rows[row].removeElementAt(rows[row].size() - 2);
						rows[row].addElement(temp1);
					}
				}
			}
		}
	}

	/**
	 * Adiciona um Elemento do tipo Element na Matriz Esparsa.
	 * @param nelement Elemento a ser inserido,.
	 */
	public void addElement(Element nelement) {
		if (nelement.value != 0) {
			int row = nelement.getLine();
			int column = nelement.getColumn();
			if (rows[row] == null && row < nrows)
				//Se o vector correspondente a linha estiver vazio
				rows[row] = new Vector();
			if (column < ncolumns && rows[row].size() < ncolumns) {
				rows[row].addElement(nelement);
				if (rows[row].size() > 1) {
					Element temp1 =
						(Element) rows[row].get(rows[row].size() - 2);
					if (temp1 != null
						&& nelement.getColumn() < temp1.getColumn()) {
						rows[row].removeElementAt(rows[row].size() - 2);
						rows[row].addElement(temp1);
					}
				}
			}
		}
	}

	/**
	 * Retorna o Elemento com a posicao especificada. Se não encontrar o Elemento retorna null.
	 * @param row Linha do Elemento.
	 * @param column Coluna do Elemento.
	 * @return
	 */
	public Element getElement(int row, int column) {
		for (int i = 0; i < rows[row].size(); i++) {
			Element temp1 = (Element) rows[row].get(i);
			if (temp1.getLine() == row && temp1.getColumn() == column) {
				return temp1;
			}
		}
		return null;
	}

	/**
	 * Retorna a posicao na lista do Elemento com a posicao especificada. Se não encontrar o
	 * Elemento retorna -1.
	 * @param row Linha do Elemento.
	 * @param column Coluna do Elemento.
	 * @return
	 */
	public int getElementPosition(int row, int column) {
		for (int i = 0; i < rows[row].size(); i++) {
			Element temp1 = (Element) rows[row].get(i);
			if (temp1.getLine() == row && temp1.getColumn() == column) {
				return i;
			}
		}
		return -1;
	}

	/**
	 * Modifica o Elemento com a posicao especificada.
	 * @param row Linha do Elemento a ser modificado.
	 * @param column Coluna do Elemento a ser modificado.
	 * @param newrow Nova linha do Elemento.
	 * @param newcolumn Nova coluna do Elemento.
	 * @param newvalue Novo Valor do Elemento.
	 */
	public void setElement(
		int row,
		int column,
		int newrow,
		int newcolumn,
		int newvalue) {
		int i = getElementPosition(row, column);
		Element temp = new Element(newrow, newcolumn, newvalue);
		if (i != -1)
			rows[row].set(i, temp);
	}

	/**
	 * Modifica o Elemento com a posicao especificada.
	 * @param row Linha do Elemento a ser modificado.
	 * @param column Coluna do Elemento a ser modificado.
	 * @param newelement Novo Elemento.
	 */
	public void setElement(int row, int column, Element newelement) {
		int i = getElementPosition(row, column);
		if (i != -1)
			rows[row].set(i, newelement);
	}

	/**
	 * Remove o Elemento com a posicao especificada.
	 * @param row Linha do Elemento a ser modificado.
	 * @param column Coluna do Elemento a ser modificado.
	 */
	public void removeElement(int row, int column) {
		int temp = getElementPosition(row, column);
		if (temp != -1)
			rows[row].removeElementAt(temp);
	}

	/**
	 * Retorna uma representacao da Matriz Esparsa em forma de String.
	 */
	public String toString() {
		String temp = "|\t";
		for (int i = 0; i < nrows; i++) {
			for (int j = 0; j < ncolumns; j++) {
				if (rows[i] != null)
					temp += getElementValue(i, j) + "\t";
			}
			temp += "\t|\n|\t";
		}
		return temp.substring(0, temp.length() - 2);
	}

	/**
	 * Carregar uma Matriz Esparsa apartir de um arquivo.
	 * @param file Caminho do arquivo.
	 */
	public void loadEsparse(FileInputStream file) {
		try {
			Properties initfile = new Properties();
			initfile.load(file);
			String proprety = initfile.getProperty("dim");
			int dimension = Integer.parseInt(proprety);
			proprety = initfile.getProperty("elementos");
			if (proprety == null) {
				//arquivo do tipo b				
				this.setNcolumns(dimension);
				this.setNrows(dimension);
				this.rows = new Vector[dimension];
				for (int row = 0; row < dimension; row++) {
					StringTokenizer line =
						new StringTokenizer(
							initfile.getProperty("lin" + row),
							",");
					if (line == null) {
						//linha toda de zero
						rows[row] = null;
					} else {
						int col = 0;
						while (line.hasMoreTokens()) {
							double elemento =
								Double.parseDouble(line.nextToken());
							this.addElement(row, col, elemento);
							col++;
						}
					}
				}
			} else {
				//arquvio tipo a
				proprety = initfile.getProperty("dim");
				this.setNcolumns(dimension);
				this.setNrows(dimension);
				this.rows = new Vector[dimension];
				int nelements =
					Integer.parseInt(initfile.getProperty("elementos"));
				for (int i = 0; i < nelements; i++) {
					StringTokenizer el =
						new StringTokenizer(
							initfile.getProperty("el" + i),
							",");
					while (el.hasMoreTokens()) {
						int row = Integer.parseInt(el.nextToken());
						int col = Integer.parseInt(el.nextToken());
						double elemento = Double.parseDouble(el.nextToken());
						this.addElement(row, col, elemento);
						col++;
					}
				}
			}
		} catch (IOException e) {
			System.err.println("Erro de arquivo! " + e.getMessage());
		}
	}

	/**
	 * Retorna um Elemento resultado da soma de dois Elementos.
	 * @param t1 Primeiro Elemento a ser somado.
	 * @param t2 Segundo Elemento a ser somado.
	 * @return
	 */
	private Element sumElement(Element t1, Element t2) {
		if (!t1.equals(t2))
			return null;
		double aux = t1.getValue() + t2.getValue();
		return new Element(t1.getLine(), t1.getColumn(), aux);
	}

	/**
	 * Retorna um Elemento resultado da subtracao de dois Elementos.
	 * @param t1 Primeiro Elemento a ser subtraido.
	 * @param t2 Segundo Elemento a ser subtraido.
	 * @return
	 */
	private Element subElement(Element t1, Element t2) {
		if (!t1.equals(t2))
			return null;
		double aux = t1.getValue() - t2.getValue();
		return new Element(t1.getLine(), t1.getColumn(), aux);
	}

	/**
	 * Retorna a Matriz Esparsa resultado da soma de duas Matrizes Esparsas.
	 * @param matriz1 Primeira matriz a ser somada.
	 * @param matriz2 Segunda matriz a ser somada.
	 * @return
	 */
	public Esparse sumMatrix(Esparse matriz1, Esparse matriz2) {
		Esparse aux = new Esparse(matriz1.getNrows(), matriz1.getNcolumns());
		for (int i = 0; i < aux.getNrows(); i++) {
			int column = getHigherElement(matriz1, matriz2, i);
			for (int j = 0; j < column; j++) {
				Element t1 = (Element) matriz1.getStruct()[i].get(j);
				Element t2 = (Element) matriz2.getStruct()[i].get(j);
				Element aux2 = sumElement(t1, t2);
				if (aux2 != null)
					aux.addElement(aux2);
			}
		}
		return aux;
	}

	/**
	 * Retorna a Matriz Esparsa resultado da subtracao de duas Matrizes Esparsas.
	 * @param matriz1 Primeira Matriz a ser subtraida.
	 * @param matriz2 Segunsa Matriz a ser subtraida.
	 * @return
	 */
	public Esparse subMatrix(Esparse matriz1, Esparse matriz2) {
		Esparse aux = new Esparse(matriz1.getNrows(), matriz1.getNcolumns());
		for (int i = 0; i < aux.getNrows(); i++) {
			int column = getHigherElement(matriz1, matriz2, i);
			for (int j = 0; j < column; j++) {
				Element t1 = (Element) matriz1.getStruct()[i].get(j);
				Element t2 = (Element) matriz2.getStruct()[i].get(j);
				Element aux2 = subElement(t1, t2);
				if (aux2 != null)
					aux.addElement(aux2);
			}
		}
		return aux;
	}

	/**
	 * Retorna O maior numero de Elementos em uma determinada linha entre duas Matrizes 
	 * Esaprsa.
	 * @param t1
	 * @param t2
	 * @param index
	 * @return
	 */
	private int getHigherElement(Esparse t1, Esparse t2, int index) {
		return (
			t1.getStruct()[index].size() > t1.getStruct()[index].size()
				? t1.getStruct()[index].size()
				: t2.getStruct()[index].size());
	}

	/**
	 * Retorna um Elemento resultado da multiplicacao de um Elemento por um numero constante.
	 * @param t1 Elemnto da matriz.
	 * @param number Constante.
	 * @return
	 */
	private Element multElement(Element t1, int number) {
		int num = number;
		double aux = t1.getValue() * num;
		return new Element(t1.getLine(), t1.getColumn(), aux);
	}

	/**
	 * Retorna a Matriz Esparsa resultado da multiplicacao de uma Matriz Esparsa por um numero
	 * constante.
	 * @param matrix Matriz a ser multiplicada. 
	 * @param number Constante.
	 * @return
	 */
	public Esparse multMatrix(Esparse matrix, int number) {
		Esparse aux = new Esparse(matrix.getNrows(), matrix.getNcolumns());
		for (int i = 0; i < aux.getNrows(); i++) {
			for (int j = 0; j < matrix.getStruct()[i].size(); j++) {
				Element t1 = (Element) matrix.getStruct()[i].get(j);
				Element aux2 = multElement(t1, number);
				if (aux2 != null)
					aux.addElement(aux2);
			}
		}
		return aux;
	}

	/**
	 * Retorna a Matriz Esparsa Estruturada para ser salva em um arquivo do tipo B.
	 * @return
	 */
	private String structorB() {
		StringBuffer k1 = new StringBuffer();
		k1.append("dim=" + nrows + "\n");
		for (int i = 0; i < nrows; i++) {
			k1.append("lin" + i + "=");
			for (int j = 0; j < ncolumns - 1; j++)
				k1.append(getElementValueStr(i, j) + ",");
			k1.append(getElementValueStr(i, ncolumns - 1) + "\n");
		}
		return k1.toString();
	}

	/**
	 * Retorna a Matriz Esparsa Estruturada para ser salva em um arquivo do tipo A.
	 * @return
	 */
	private String structorA() {
		StringBuffer k1 = new StringBuffer();
		k1.append("dim=" + nrows + "\n");
		int count = 0;
		for (int i = 0; i < nrows; i++) {
			for (int j = 0; j < ncolumns; j++)
				if (!"0".equals(getElementValueStr(i, j))) {
					k1.append(
						"el"
							+ count
							+ "="
							+ i
							+ ","
							+ j
							+ ","
							+ getElementValueStr(i, j)
							+ "\n");
					count++;
				}
		}
		k1.append("elementos=" + count + "\n");
		return k1.toString();
	}

	/**
	 * Salva a Matriz Esparsa estruturada no tipo B em um arquivo.
	 * @param outputFile Caminho do arquivo a ser salvo.
	 */
	public void saveToFileB(String outputFile) {
		try {
			FileOutputStream out = new FileOutputStream(new File(outputFile));
			String s = structorB();
			out.write(s.getBytes());
			out.close();
		} catch (IOException e) {
			e.printStackTrace();
		}
	}
	/**
	 * Salva a Matriz Esparsa estruturada no tipo A em um arquivo.
	 * @param outputFile Caminho do arquivo a ser salvo.
	 */
	public void saveToFileA(String outputFile) {
		try {
			FileOutputStream out = new FileOutputStream(new File(outputFile));
			String s = structorA();
			out.write(s.getBytes());
			out.close();
		} catch (IOException e) {
			e.printStackTrace();
		}
	}

	public static void main(String Args[]) {
		try {
			FileInputStream file = new FileInputStream("c://matrixa.ini");
			Esparse Ep1 = new Esparse(file);
			FileInputStream file2 = new FileInputStream("c://matrixb.ini");
			Esparse Ep2 = new Esparse(file2);
			Esparse res = new Esparse(Ep1.ncolumns);
			res = res.multMatrix(Ep1, 2);
			System.out.println("Resultado: ");
			System.out.println(res);
			System.out.println("Arquivo gerado: ");
			System.out.println(res.structorB());
			res.saveToFileB("C://matrixres.ini");
		} catch (IOException err) {
			System.out.println(
				"Erro de leitura de arquivo! " + err.getMessage());
			System.exit(-1);
		}
	}
}