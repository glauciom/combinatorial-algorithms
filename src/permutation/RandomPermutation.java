package permutation;

/**
 * @author Glaucio Melo.
 * Classe responsavel pelo processamento do algoritmo RandomPermutation, a fim
 * de gerar uma permutacao aleatoria.
 */
public class RandomPermutation {

	// Campos da classe RandomPermutation.
	private int p[];
	private int m,n,v;
	
	
	/**
	 * Construtor de RandomPermutation
	 * @param n Tamanho do vetor de permutacao.
	 */
	public RandomPermutation(int n) {
		this.n = n;
		init();
		
	}
	
	// Inicializa o vetor de permutacao.
	private void init() {
	 p = new int[n];
	 for(int i = 0; i < n; i++)
	  p[i] = i+1;	 	
	}
	
	// Trocando os elementos randomicamente.
	private void swap(int[] p, int v, int m) {
		int aux;
		v = ((m + 1) + (int) 
		      Math.floor(Math.random() * (n+1-(m+1)))) -1;
		aux = p[v];
		p[v] = p[m];
		p[m] = aux; 
	}
	
	//Metodo nucleo da classe RandomPermutation.
	private int[] randomPermutation() {
	 for(m = 0; m < n; m++) 
	  swap(p,v,m);
	return p;
	}
	
	/**
	 * Metodo que invoca o algoritmo nucleo da classe RandomPermutation;
	 */
	public void randomPermutationAlgorithm() {
		p = randomPermutation();
	}
	
	/**
	 * Captura o vetor de permutacao, em forma de String.
	 * @return String Vetor de permutacao, transformado em String.
	 */
	public String getOutPut() {
	 StringBuffer k1 = new StringBuffer();
	 for(int g = 0; g < n; g++)
	  k1.append(p[g] + " ");
      return k1.toString();
	 }
	 
	 /**
	  * Captura o vetor de permutacao.
	  * @return int[] Vetor de permutacao.
	  */
	 public int[] getRandomPermutation() {
	 	return p;
	 }
	
	/**
	 * Metodo main (para testes).
	 * @param args String de entrada do prompt de comando.
	 */
	public static void main(String[] args) {
	 RandomPermutation test = new RandomPermutation(5);
	 System.out.println(test.getOutPut());
	 for(int y = 0; y < 50; y++) {
	  test.randomPermutationAlgorithm();
	  System.out.println(test.getOutPut());
	 }
	}
}
