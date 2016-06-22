package subset;

/**
 * @author Glaucio Melo.
 * Classe responsavel pelo processamento do algoritmo LexicographicSubset.
 */
public class LexicographicSubset {
	
	// Campos da Classe LexicographicSubset.
	 
	private int k,j,s,n;
	private int[] a;
	private int[] subset;
	private int cardinality;

	/**
	 * Construtor de LexicographicSubset.
	 * @param n tamanho do conjunto.
	 * @param cardinality Cardinalidadde do conjunto.
	 */
	public LexicographicSubset(int n, int cardinality) {
		this.n = n;
		this.cardinality = cardinality;
		init();
	}
	
	// Inicializa a classe.
	private void init() {
		this.k = -1;
		this.a = new int[n];
		
	}
	
	// Aloca o resultado do subconjunto em um vetor.
	private int[] transform(int f) {
	 subset = new int[f+1];
	 for(int h = 0; h <= f; h++)
	  subset[h] = a[h];
	 return subset;
	}

	// Metodo acessorio ao algoritmo.
	private int[] up() {
	 if (k == cardinality - 1)
	  return null;
	 s = 0;
	 k++;
	 a[k] = s+1;
	return a;
	}
	
	// Metodo acessorio ao algoritmo.
	private int[] down() {
	 if (a[k] == n)  {
	  k--;
	  if( k == -1)
	   return null;
	  s = a[k];
	  a[k] = s+1;
	 } else {
		s = a[k];
		if (k != cardinality -1)
		 k++;
		a[k] = s+1;
	   }
	 return a;
	}
	
	/**
	 * Algoritmo para retornar o proximo subconjunto, em ordem lexicografica.
	 */
	public void lexicographicSubsetAlgorithm() {
	 a = (k == -1 ? up() : down());
	 subset = transform(k);
	}
	
	/**
	 * Captura o subconjunto na ordem lexicografica da lista.
	 * @return int[] Vetor represntando o subconjunto corrente.
	 */
	public int[] getSubset() {
		return subset;
	}
	
	/**
	 * Captura o vetor do subconjunto, em forma de String.
	 * @return String Vetor transformado em String.
	 */
	public String getOutPut() {
	 StringBuffer k1 = new StringBuffer();
	 for(int h = 0; h < subset.length; h++)
	  k1.append(subset[h] + " ");
	 return k1.toString();
	}
	
	/**
	 * Captura o subconjunto especifico, dado o serial.
	 * @param serial Serial do subconjunto.
	 * @return int[] vetor representando o subconjunto.
	 */
	public int[] serialLexicographicSubset(int serial) {
	 init();
	 for (int h = 1; h <= serial; h++)
	  lexicographicSubsetAlgorithm();
	 return subset;
	}
	
	/**
	 * Metodo main (Para Testes).
	 * @param args String de Entrada.
	 */
	public static void main(String args[]) {
		LexicographicSubset test = new LexicographicSubset(6,3);
		test.serialLexicographicSubset(40);
		System.out.println(test.getOutPut());
		for(int f = 0; f < 41; f++) {
			test.lexicographicSubsetAlgorithm();
			System.out.println(test.getOutPut());
		}	 
	}
}