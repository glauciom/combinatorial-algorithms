package subset;

/**
 * @author Glaucio Melo
 * Classe responsavel pelo processamento do algoritmo RandomSubset.
 */
public class RandomSubset {
	
	// Campos da classe RandomSubset.
	private int[] set;
	private int i,n;

	public RandomSubset(int n) {
		this.n = n;
		this.set = new int[n];
	}

	// Metodo nucleo da classe RandomSubset.
	private int[] randomSubset() {
	 for(i = 0; i < n; i++)
	  set[i] = (int) (2 * Math.random());
	return set;
	}
	
	/**
	 * Metodo que invoca o algoritmo RandomSubset.
	 */
	public void randomSubsetAlgorithm() {
		set = randomSubset();
	}
	/**
	 * Captura o subconjunto gerado pelo RandomSubset.
	 * @return Vetor representando o subconjunto. 
	 */
	public int[] getSubset() {
		return set;
	}
	
	/**
	 * Captura o vetor do subconjunto, em forma de String.
	 * @return String Vetor transformado em String.
	 */
	public String getOutPut() {
	 StringBuffer k1 = new StringBuffer();
	 for(int h = 0; h < set.length; h++)
	  k1.append(set[h] + " ");
	 return k1.toString();
	}
	
	/**
	 * Metodo main (para testes).
	 * @param args String de entrada do prompt de comando.
	 */
	public static void main(String[] args) {
		RandomSubset test = new RandomSubset(3);
		for(int i = 0; i < 10; i++) {
			test.randomSubsetAlgorithm();
			System.out.println(test.getOutPut());
		}
	}
}
