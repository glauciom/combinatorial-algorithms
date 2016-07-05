package br.com.gm2.routines.java.random;

/**
 * Java implementation - Documentation: Combinatorial Algorithms, WILF /
 * NIJENHUIS, page 52.
 * 
 * @author Glaucio Melo.
 */
public class RandomComposition {

	private int n, k;

	private int[] composition;

	private RandomKSubset subset;

	public RandomComposition(int n, int k) {
		this.n = n;
		this.k = k;
		this.composition = new int[k];
	}

	private int[] randomCompostionAlgorithm() {
		subset = new RandomKSubset(n + k - 1, k - 1);
		subset.simpleRandomKSubsetAlgorithm();
		int[] random = subset.getSubset();

		composition[0] = random[0] - 1;
		for (int j = 1; j < k - 1; j++) {
			composition[j] = random[j] - random[j - 1] - 1;
		}
		composition[k - 1] = n + k - 1 - random[k - 2];

		return composition;
	}

	public void randomComposition() {
		composition = randomCompostionAlgorithm();
	}

	public String toString() {
		StringBuffer k1 = new StringBuffer();
		for (int i = 0; i < k; i++) {
			k1.append(composition[i] + " ");
		}
		return k1.toString();
	}

	public static void main(String[] args) {
		RandomComposition test = new RandomComposition(6, 3);
		for (int i = 0; i < 100; i++) {
			test.randomComposition();
			System.out.println(test);
		}
	}
}
