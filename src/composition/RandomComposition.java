package composition;

import kNSubset.SimpleRandomKSubset;

/**
 * Classe Respons�vel pelo processamento do algoritmo RandomComposition
 * @author Glaucio
 */
public class RandomComposition {
	
	//Campos principais.
	private int n,k;
	
	//Vetor de composicao.
	private int[] composition;
	
	//Metodo auxiliar para gerar o subconjunto aleatorio.
	private SimpleRandomKSubset subset;
	
	
	/**
	 * Construtor para RandomComposition
	 * @param n Numero que ira ser composto.
	 * @param k Partes do numero a ser composto.
	 */
	public RandomComposition(int n, int k) {
		this.n = n;
		this.k = k;
		this.composition = new int[k];
		
	}
	
	//M�todo n�cleo da classe.
	private int[] randomCompostionAlgorithm() {
		subset = new SimpleRandomKSubset(n+k-1,k-1);
		subset.simpleRandomKSubsetAlgorithm();
		int[] random = subset.getSubset();
		
		composition[0] = random[0] - 1;
		for(int j = 1; j < k-1; j++)
		 composition[j] = random[j] - random[j-1] -1;
		composition[k-1] = n + k - 1 - random[k-2];
		
	return composition;
	}
	
	
	/**
	 * M�todo de invoca��o do algoritmo principal.
	 */
	public void randomComposition() {
		composition = randomCompostionAlgorithm();
	}
	
	/**
	 * M�todo sobreposto que retorna como sa�da o vetor de composi��o, sob forma
	 * de String.
	 * @see java.lang.Object#toString()
	 */
	public String toString() {
		StringBuffer k1 = new StringBuffer();
		for (int i = 0; i < k; i++)
			k1.append(composition[i] + " ");
		return k1.toString();
	}

	/**
	 * M�todo main (para testes).
	 * @param args Par�metros de entrada para o m�todo.
	 */
	public static void main(String[] args) {
		RandomComposition test = new RandomComposition(6,3);
		for(int i = 0; i < 100; i++) {
			test.randomComposition();
			System.out.println(test);
		}
	}
}
