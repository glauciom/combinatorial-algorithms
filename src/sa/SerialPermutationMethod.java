package sa;

import java.math.BigInteger;
import java.util.Date;

/**
 * @author Glaucio Melo Classe responsavel por todo o processamento do pacote
 *         permutation
 */
public class SerialPermutationMethod {

	// Campos da classe SerialPermutationMethod

	private SerialPermutation permutation;

	private SerialOffset offset;

	private int n;

	private BigInteger[] factorial;

	/**
	 * Construtor de SerialPermutationMethod.
	 * 
	 * @param n
	 *            Tamanho do vetor de Permutacao.
	 */
	public SerialPermutationMethod(int n) {
		this.n = n;
		init();
	}

	// Inicializando as Variaveis.
	private void init() {
		this.factorial = new BigInteger[n];
		for (int j = 0; j < n; j++)
			factorial[j] = new BigInteger(Utils.factorial(j + 1, 1));
	}

	/**
	 * Metodo principal de invocacao dos sub-algoritmos.
	 * 
	 * @param serial
	 *            Numero da permutacao.
	 */
	public void algorithm(BigInteger serial) {
		offset = new SerialOffset(factorial, n - 1);
		offset.blindOffsetAlgorithm(serial);
		permutation = new SerialPermutation(offset.getOffset(), n);
		permutation.blindPermutationAlgorithm();
	}

	/**
	 * Invocacao do Algoritmo
	 * 
	 * @param serial
	 *            String que representa o inteiro grande.
	 */
	public void algorithm(String serial) {
		this.algorithm(new BigInteger(serial));
	}

	/**
	 * Captura o vetor de deslocamento, sob forma de String.
	 * 
	 * @return String String representando o vetor de deslocamento.
	 */
	public String getOffset() {
		return offset.getOutPut();
	}

	/**
	 * Captura o vetor de permutacao, sob forma de string.
	 * 
	 * @return String String representando o vetor de permutacao.
	 */
	public String getPermutation() {
		return permutation.getOutPut();
	}

	/**
	 * Captura o objeto SerialPermutation.
	 * 
	 * @return SerialPermutation Objeto SerialPermutation.
	 */
	public SerialPermutation getBlindPermutation() {
		return permutation;
	}

	/**
	 * Captura os indices permutados
	 * 
	 * @return vetor de indices permutados.
	 */
	public int[] getPermutationArray() {
		return permutation.getPermutation();
	}

	/**
	 * Captura o objeto SerialOffset.
	 * 
	 * @return SerialOffset Objeto SerialOffset.
	 */
	public SerialOffset getBlindOffset() {
		return offset;
	}

	/**
	 * Metodo main (para testes).
	 * 
	 * @param args
	 *            String[] args.
	 */
	public static void main(String args[]) {
		int n = 4;
		long t1, t0;
		Date r;
		r = new Date();
		t0 = r.getTime();
		SerialPermutationMethod test = new SerialPermutationMethod(n);
		/*test.algorithm("263130836933693530167218012160000000");
		System.out.println(test.getPermutation());
		System.out.println(Utils.factorial(32, 1));
		test.algorithm("-263130836933693530167218012159999999");
		System.out.println(test.getPermutation());*/
		for (int i = 0; i < 24; i++) {
			test.algorithm(""+(i+1));
			System.out.println((i+1) + " - " +test.getPermutation() + " " + test.getOffset());
		}
		r = new Date();
		t1 = r.getTime() - t0;
		System.out.println("tempo: " + t1);
	}
}