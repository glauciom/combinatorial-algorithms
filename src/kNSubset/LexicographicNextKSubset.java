package kNSubset;

/**
 * @author Glaucio Melo
 * Classe responsavel pelo processamento do algoritmo NEXKSB.
 */
public class LexicographicNextKSubset {

	// Campos da classe LexicographicNextKSubset.
	
	private int m,h,k,n,j;
	private int[] subset;
	private boolean isLastElement;
	
	/**
	 * Construtor para a classe LexicographicNextKSubset.
	 * @param n Tamanho do conjunto total.
	 * @param k Tamanho do subconjunto.
	 */
	public LexicographicNextKSubset(int n, int k) {
		this.n = n;
		this.k = k;
		this.isLastElement = false;
		init();
	}
	
	// Inicializa o algoritmo.
	private void init() {
	 this.subset = new int[k];
	 this.m = 0;
	 this.h = k;
	 for(j = 0; j < k; j++)
	  subset[j] = j+1;
	}
	// Metodo nucleo do algoritmo.
	private int[] nextKSBAlgorithm() {
	 if (isLastElement)
	  return null;
	 if (m < n -h)
	  h = 0;
	 h++;
	 m = subset[k-h];
	 for(j = 0; j < h; j++) {
	  subset[k+j-h] = m + j+1;
	  if (subset[0] == n - k + 1)
	   isLastElement = true;
	 }
	 return subset;
	}
	
	/**
	 * Invoca o algoritmo LexicographicNextKSubset.
	 */
	public void lexicographicNextKSubsetAlgorithm() {
		subset = nextKSBAlgorithm();
	}
	/**
	 * Captura o vetor de conjunto, em forma de String.
	 * @return String Vetor transformado em String.
	 */
	public String getOutPut() {
	 StringBuffer k1 = new StringBuffer();
	 for(int h = 0; h < k; h++)
	  k1.append(subset[h] + " ");
     return k1.toString();
	}
	
	/**
	 * Retorna true caso o subconjunto seja o ultimo da lista.
	 * @return Valor logico de ultimo da lista (true ou false).
	 */
	public boolean isLastElement() {
		return isLastElement;
	}
	
	/**
	 * Atualiza o valor do subconjunto, dado um serial especifico.
	 * @param serial Numero inteiro correpondente a numeracao do subconjunto da lista. 
	 */
	public void serialLexicographicNextKSubset(int serial) {
		init();
		for(int g = 1; g < serial;g++)
		 lexicographicNextKSubsetAlgorithm();
	}
	
	/**
	 * Captura o vetor que representa o subconjunto.
	 * @return Vetor do subconjunto.
	 */
	public int[] getSubset() {
		return subset;
	}
	
	/**
	 * Metodo main (para testes).
	 * @param args Argumento de entrada.
	 */
	public static void main(String[] args) {
	 LexicographicNextKSubset test  = new LexicographicNextKSubset(7,4);
	 //System.out.println(test.getOutPut());
	 //test.serialLexicographicNextKSubset(1);
	// System.out.println(test.getOutPut());  
	 long r = System.currentTimeMillis();
	 while (!test.isLastElement()) {
	  test.lexicographicNextKSubsetAlgorithm();
//	  System.out.println(test.getOutPut()); 
	 }
	 long r1 = System.currentTimeMillis();
	 System.out.println("Tempo = "+(r1 - r));
	}
}
