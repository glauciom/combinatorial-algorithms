package br.com.gm2.routines.java.next;

/**
 * Java implementation - Documentation: Combinatorial Algorithms, WILF /
 * NIJENHUIS, page 13
 * 
 * @author Glaucio Melo.
 */
public class LexicographicSubset {

	private int k, s, n;
	private int[] a;
	private int[] subset;
	private int cardinality;

	/**
	 * LexicographicSubset Constructor
	 * 
	 * @param n
	 *            Size of the subset
	 * @param cardinality
	 *            Set Cardinality
	 */
	public LexicographicSubset(int n, int cardinality) {
		this.n = n;
		this.cardinality = cardinality;
		init();
	}

	// Initialize the object.
	private void init() {
		this.k = -1;
		this.a = new int[n];

	}

	// Allocates the subset result into an array.
	private int[] transform(int f) {
		subset = new int[f + 1];
		for (int h = 0; h <= f; h++) {
			subset[h] = a[h];
		}
		return subset;
	}

	private int[] up() {
		if (k == cardinality - 1) {
			return null;
		}
		s = 0;
		k++;
		a[k] = s + 1;
		return a;
	}

	private int[] down() {
		if (a[k] == n) {
			k--;
			if (k == -1) {
				return null;
			}
			s = a[k];
			a[k] = s + 1;
		} else {
			s = a[k];
			if (k != cardinality - 1)
				k++;
			a[k] = s + 1;
		}
		return a;
	}

	/**
	 * Returns the next subset, in lexicographic order.
	 */
	public void lexicographicSubsetAlgorithm() {
		a = (k == -1 ? up() : down());
		subset = transform(k);
	}

	public int[] getSubset() {
		return subset;
	}

	public String getOutPut() {
		StringBuffer k1 = new StringBuffer();
		for (int h = 0; h < subset.length; h++) {
			k1.append(subset[h] + " ");
		}
		return k1.toString();
	}

	/**
	 * Captures an specific subset, given a serial number (position of the list
	 * of subsets)
	 */
	public int[] serialLexicographicSubset(int serial) {
		init();
		for (int h = 1; h <= serial; h++) {
			lexicographicSubsetAlgorithm();
		}
		return subset;
	}

	/**
	 * Metodo main (Para Testes).
	 * 
	 * @param args
	 *            String de Entrada.
	 */
	public static void main(String args[]) {
		LexicographicSubset test = new LexicographicSubset(6, 3);
		for (int f = 0; f < 41; f++) {
			test.lexicographicSubsetAlgorithm();
			System.out.println(test.getOutPut());
		}
	}
}