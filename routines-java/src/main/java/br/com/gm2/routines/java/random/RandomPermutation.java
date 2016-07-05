package br.com.gm2.routines.java.random;

/**
 * Java implementation - Documentation: Combinatorial Algorithms, WILF /
 * NIJENHUIS, page 62.
 * 
 * @author Glaucio Melo.
 */
public class RandomPermutation {

	private int p[];
	private int m, n, v;

	public RandomPermutation(int n) {
		this.n = n;
		init();

	}

	private void init() {
		p = new int[n];
		for (int i = 0; i < n; i++) {
			p[i] = i + 1;
		}
	}

	private void swap(int[] p, int v, int m) {
		int aux;
		v = ((m + 1) + (int) Math.floor(Math.random() * (n + 1 - (m + 1)))) - 1;
		aux = p[v];
		p[v] = p[m];
		p[m] = aux;
	}

	private int[] randomPermutation() {
		for (m = 0; m < n; m++) {
			swap(p, v, m);
		}
		return p;
	}

	public void randomPermutationAlgorithm() {
		p = randomPermutation();
	}

	public String getOutPut() {
		StringBuffer k1 = new StringBuffer();
		for (int g = 0; g < n; g++) {
			k1.append(p[g] + " ");
		}
		return k1.toString();
	}

	public int[] getRandomPermutation() {
		return p;
	}
}
