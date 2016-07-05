package br.com.gm2.routines.java.random;

/**
 * Java implementation - Documentation: Combinatorial Algorithms, WILF /
 * NIJENHUIS, page 72.
 * 
 * @author Glaucio Melo.
 */
public class RandomPartition {

	private int m, n, d, j, k;
	private int p[];
	private int partition[];

	public RandomPartition(int n) {
		this.n = n;
	}

	private int[] transformPartition(int f) {
		partition = new int[f + 1];
		for (int i = 0; i <= f; i++) {
			partition[i] = p[i];
		}
		return partition;
	}

	public String getOutPut() {
		StringBuffer k1 = new StringBuffer();
		for (int i = 0; i < partition.length; i++) {
			k1.append(partition[i] + " ");
		}
		return k1.toString();
	}

	private int[] randomPartition() {
		p = new int[n];
		m = n;
		k = 0;
		while (m != 0) {
			do {
				d = (int) (Math.random() * n) + 1;
				j = (int) (Math.random() * n) + 1;
			} while ((m - j * d) < 0);
			for (int i = k; i < j + k; i++) {
				p[i] = d;
			}
			m = m - j * d;
			k = k + j;
		}
		return transformPartition(k - 1);
	}

	public void randomPartitionAlgorithm() {
		partition = randomPartition();
	}

	public int[] getPartition() {
		return partition;
	}

	public static void main(String[] args) {
		RandomPartition test = new RandomPartition(6);
		for (int t = 0; t < 100; t++) {
			test.randomPartitionAlgorithm();
			System.out.println(test.getOutPut());
		}
	}
}