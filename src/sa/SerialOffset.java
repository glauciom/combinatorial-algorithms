package sa;

import java.math.BigInteger;

/**
 * @author Glaucio Melo
 * Classe reponsavel pelo processamento do algoritmo SerialOffset.
 */
public class SerialOffset {

	// Campos da Classe SerialOffset.

	private BigInteger[] fatorial;

	private int[] offset;

	private int i, n;

	/**
	 * Construtor da Classe SerialOffset.
	 * @param fatorial vetor contendo os fatoriais dos numeros.
	 * @param n tamanho do vetor de deslocamento.
	 */
	public SerialOffset(BigInteger[] fatorial, int n) {
		this.fatorial = fatorial;
		this.n = n;
		this.offset = new int[n];
	}

	/**
	 * Metodo que processa o vetor de deslocamento.
	 * @param s Serial da permutacao.
	 */
	public void blindOffsetAlgorithm(BigInteger s) {
		offset = blindAlgorithm(s);
	}

	/**
	 * Metodo que captura o vetor de deslocamento.
	 * @return int[] Vetor de deslocamento.
	 */
	public int[] getOffset() {
		return offset;
	}

	// Metodo nucleo do algoritmo.

	private boolean isOdd(BigInteger s) {
		return s.divide(fatorial[i]).mod(new BigInteger("2")).toString()
				.equals("1");
	}

	private int atributeValue(BigInteger s) {
		return Integer.parseInt(s.mod(fatorial[i]).divide(fatorial[i - 1])
				.toString());

	}

	private int[] blindAlgorithm(BigInteger s) {
		s = s.subtract(BigInteger.ONE);
		for (i = 1; i <= n; i++) {
			int k = atributeValue(s);
			offset[i - 1] = isOdd(s) ? i - k : k;
		}
		return offset;
	}

	/**
	 * Metodo que retorna o vetor de deslocamento em forma de String.
	 * @return String String do vetor de deslocamento.
	 */
	public String getOutPut() {
		StringBuffer k1 = new StringBuffer();
		for (i = 0; i < n; i++)
			k1.append(offset[i] + " ");
		return k1.toString();
	}
}

