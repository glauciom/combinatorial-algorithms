package kNSubset;

/**
 * @author Glaucio Melo
 * Classe responsavel pelo processamento do algoritmo SimpleRandomKSubset.
 */
public class SimpleRandomKSubset {
	
	// Campos da classe SimpleRandomKSubset.
	private int k,n,k0,i;
	private double c1,c2;
	private int a[];
	
	/**
	 * Construtor para a classe SimpleRandomKSubset.
	 * @param n Numero do conjunto.
	 * @param k Tamanho do vetor que representa o subconjunto.
	 */
	
	public SimpleRandomKSubset(int n, int k) {
		this.n = n;
		this.k = k;
		init();
	}
	
	//Inicializa o metodo.
	private void init() {
		this.k0 = -1;
		this.i = -1;
		a = new int[k];
		c1 = k;
		c2 = n;
	}
	
	//Metodo nucleo da classe.
	private int[] rks() {
	 init();
	 while (c1 > 0) {
	  i++;
	  if (Math.random() <= c1 / c2) {
	  	c1--;
	 	k0++;
	 	a[k0] = i+1;
	  }
	  c2--;
	 }
	 return a;
	}
	
	/**
	 * Invoca o metodo principal dessa classe.
	 */
	public void simpleRandomKSubsetAlgorithm() {
		a = rks();
	}
	
	/**
	 * Captura o vetor que representa o subconjunto.
	 * @return Vetor que representa o subconjunto. 
	 */
	public int[] getSubset() {
		return a;
	}
	
	/**
	 * Captura o vetor de subconjunto, em forma de String.
	 * @return String Vetor transformado em String.
	 */
	public String getOutPut() {
	 StringBuffer k1 = new StringBuffer();
	 for(int h = 0; h < k; h++)
	  k1.append(a[h] + " ");
	 return k1.toString();
	}
	
	/**
	 * Metodo main (Para testes).
	 * @param args String de entrada no prompt de comando.
	 */
	public static void main(String[] args) {
		SimpleRandomKSubset test = new SimpleRandomKSubset(12,4);
		for(int g = 0; g < 100; g++) {
			test.simpleRandomKSubsetAlgorithm();
			System.out.println(test.getOutPut());
		} 
	}
}