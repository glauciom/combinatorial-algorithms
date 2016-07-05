package br.com.gm2.routines.java.random;

/**
 * Java implementation - Documentation: Combinatorial Algorithms, WILF /
 * NIJENHUIS, page 39.
 * 
 * @author Glaucio Melo.
 */
public class RandomKSubset {

	private int k, n, k0, i;
	private double c1, c2;
	private int a[];

	public RandomKSubset(int n, int k) {
		this.n = n;
		this.k = k;
		init();
	}

	private void init() {
		this.k0 = -1;
		this.i = -1;
		a = new int[k];
		c1 = k;
		c2 = n;
	}

	private int[] rks() {
		init();
		while (c1 > 0) {
			i++;
			if (Math.random() <= c1 / c2) {
				c1--;
				k0++;
				a[k0] = i + 1;
			}
			c2--;
		}
		return a;
	}

	public void simpleRandomKSubsetAlgorithm() {
		a = rks();
	}

	public int[] getSubset() {
		return a;
	}

	public String getOutPut() {
		StringBuffer k1 = new StringBuffer();
		for (int h = 0; h < k; h++) {
			k1.append(a[h] + " ");
		}
		return k1.toString();
	}

	public static void main(String[] args) {
		RandomKSubset test = new RandomKSubset(12, 4);
		for (int g = 0; g < 100; g++) {
			test.simpleRandomKSubsetAlgorithm();
			System.out.println(test.getOutPut());
		}
	}
}