package br.com.gm2.routines.java.random;

/**
 * Java implementation - Documentation: Combinatorial Algorithms, WILF /
 * NIJENHUIS, page 23.
 * 
 * @author Glaucio Melo.
 */
public class RandomSubset {

	private int[] set;
	private int i, n;

	public RandomSubset(int n) {
		this.n = n;
		this.set = new int[n];
	}

	private int[] randomSubset() {
		for (i = 0; i < n; i++) {
			set[i] = (int) (2 * Math.random());
		}
		return set;
	}

	public void randomSubsetAlgorithm() {
		set = randomSubset();
	}

	public int[] getSubset() {
		return set;
	}

	public String getOutPut() {
		StringBuffer k1 = new StringBuffer();
		for (int h = 0; h < set.length; h++)
			k1.append(set[h] + " ");
		return k1.toString();
	}

	public static void main(String[] args) {
		RandomSubset test = new RandomSubset(3);
		for (int i = 0; i < 10; i++) {
			test.randomSubsetAlgorithm();
			System.out.println(test.getOutPut());
		}
	}
}
