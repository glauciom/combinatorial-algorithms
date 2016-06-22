/*
 * Created on 11/09/2004
 *
 * To change the template for this generated file go to
 * Window&gt;Preferences&gt;Java&gt;Code Generation&gt;Code and Comments
 */
package sa;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.math.BigInteger;

/**
 * Classe que provê o encapsulamento de métodos úteis para a execução dos
 * algoritmos.
 * 
 * @author GLAUCIO
 */
public class Utils {

	/**
	 * Calcula o fatorial de um número, sem restrição de número de dígitos.
	 * 
	 * @param r
	 *            número a ser calculado o fatorial.
	 * @param op
	 *            limite inferior da multiplicação. Util para calculo de
	 *            coeficientes binomiais. Para o calculo apenas do fatorial,
	 *            usa-se o número 1 para este parametro.
	 * @return String que representa um inteiro longo, de ordem arbitrária.
	 */
	public static String factorial(int r, int op) {
		BigInteger aux = BigInteger.ONE;
		for (int t = r; t > op; t--) {
			aux = aux.multiply(BigInteger.valueOf((long) t));
		}
		return aux.toString();
	}

	/**
	 * Lê um Dicionário em arquivo e joga seu conteudo para uma String.
	 * 
	 * @param f
	 *            Objeto que representa o arquivo físico do dicionário.
	 * @return String em memória, representando o arquivo.
	 */
	public static String readCodeBookFormat(File f) {
		StringBuffer buffer = new StringBuffer();
		try {
			BufferedReader reader = new BufferedReader(new FileReader(f));
			String s = "";
			while ((s = reader.readLine()) != null) {
				buffer.append(s + "\n");
			}
		} catch (Exception e) {
			e.printStackTrace();
		}
		return buffer.toString();
	}

	/**
	 * Preenche a estrutura de dados com as informações do arquivos dic.
	 * 
	 * @param input
	 *            Dados do arquivo sob forma de string.
	 * @param numberOfVectors
	 *            Numero de vetores do dicionario
	 * @param dimension
	 *            Dimensão do vetor.
	 * @return Matriz que representará o dicionário.
	 */
	private static int[][] fillData(String input, int numberOfVectors,
			int dimension) {
		int[][] out = new int[numberOfVectors][dimension];
		String[] parts = input.split("\n");
		int i = -1;
		for (int j = 0; j < parts.length; j++) {
			if (j % dimension == 0)
				i++;
			out[i][j % dimension] = (int) Math.round(Double
					.parseDouble(parts[j]));
		}
		return out;
	}

	/**
	 * Inicializa o Dicionario.
	 * 
	 * @param f
	 *            Objeto que representa o caminho do dicionario.
	 * @param numberOfVectors
	 *            Numero de vetores do dicionario.
	 * @param dimension
	 *            Dimensão do vetor.
	 * @return Matriz que preenche os dados com a invocação de fill data.
	 */
	public static int[][] initCodeBook(File f, int numberOfVectors,
			int dimension) {
		return fillData(readCodeBookFormat(f), numberOfVectors, dimension);
	}
	
	public static String divideBy2(String divider) {
		return new BigInteger(divider).divide(new BigInteger("2")).toString();
	}
	
	public static String divideAndOffset(String divisor, String offset) {
		return divideBy2(new BigInteger(divisor).add(new BigInteger(offset)).toString());
	}
	
	public static String divideAndOffsetAlternative(String divisor, String offset) {
		return new BigInteger(offset).add(new BigInteger(divideBy2(divisor))).toString();
	}
	
	public static String divideAndOffsetSub(String divisor, String offset) {
		return new BigInteger(offset).subtract(new BigInteger(divideBy2(divisor))).toString();
	}

	/**
	 * Método main (Para testes).
	 * 
	 * @param args
	 *            String de parametros para o prompt de comando.
	 */
	public static void main(String[] args) {
		System.out.println(Utils.readCodeBookFormat(new File(
				"src\\codebooks\\airplane_boat_gull_goldhill_128.dic")));
		
		System.out.println(Utils.divideAndOffset("40","60"));
	}
}