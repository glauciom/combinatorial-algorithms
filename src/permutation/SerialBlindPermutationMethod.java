package permutation;

import java.util.Date; 

/**
 * @author Glaucio Melo
 * Classe responsavel por todo o processamento do pacote permutation
 */
public class SerialBlindPermutationMethod {
	
	// Campos da classe SerialPermutationMethod
	
	private BlindPermutation permutation;
	private BlindOffset offset;
	private int n;
	private int[] factorial;
	
	/**
	 * Construtor de SerialPermutationMethod.
	 * @param n Tamanho do vetor de Permutacao.
	 */
	public SerialBlindPermutationMethod(int n) {
		this.n = n;
		init();
	}
	
		
	/**
	 * Calcula o fatorial de um numero.
	 * @param n Tamanho do vetor.
	 * @return int Fatorial do tamanho do vetor.
	 */
	public int f(int n) {
	 int val = 1;
	 for(int i = 2; i <= n; i++)
	  val *= i;
	return val;
	}
	
	// Inicializando as Variaveis.
	private void init() {
	 this.factorial = new int[n];
	 for(int j = 0; j < n; j++)
	  factorial[j] = f(j+1);
	}	
	
	/**
	 * Metodo principal de invocacao dos sub-algoritmos.
	 * @param serial Numero da permutacao.
	 */
	public void algorithm(int serial) {
		offset = new BlindOffset(factorial,n-1);
		offset.blindOffsetAlgorithm(serial);
		permutation = new BlindPermutation(offset.getOffset(),n);
		permutation.blindPermutationAlgorithm();
	}
	
	/**
	 * Captura o vetor de deslocamento, sob forma de String.
	 * @return String String representando o vetor de deslocamento.
	 */
	public String getOffset() {
		return offset.getOutPut();
	}
	
	/**
	 * Captura o vetor de permutacao, sob forma de string.
	 * @return String String representando o vetor de permutacao.
	 */
	public String getPermutation() {
		return permutation.getOutPut();
	}
	
	/**
	 * Captura o objeto SerialPermutation.
	 * @return SerialPermutation Objeto SerialPermutation.
	 */
	public BlindPermutation getBlindPermutation() {
		return permutation;
	}
	
	/**
	 * Captura o objeto SerialOffset.
	 * @return SerialOffset Objeto SerialOffset.
	 */
	public BlindOffset getBlindOffset() {
		return offset;
	}
	
	/**
	 * Metodo main (para testes).
	 * @param args String[] args.
	 */
	public static void main(String args[]) {
		int n = 7;
		long t1,t0;
		Date r;
		r = new Date();
		t0 = r.getTime();
		SerialBlindPermutationMethod test = new SerialBlindPermutationMethod(n);
		for(int i = 1; i <= test.f(n); i++) {
		 test.algorithm(i);
		} 
		r = new Date();
		t1 = r.getTime() - t0; 
		System.out.println("tempo: "+ t1);
	}
}
