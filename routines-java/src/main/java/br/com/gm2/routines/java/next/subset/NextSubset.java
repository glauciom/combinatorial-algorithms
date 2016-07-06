package br.com.gm2.routines.java.next.subset;

import br.com.gm2.routines.java.exception.OutOfRangeException;

/**
 * Java implementation - Documentation: Combinatorial Algorithms, WILF /
 * NIJENHUIS, page 13
 * 
 * @author Glaucio Melo.
 */
public class NextSubset {

	private int k, t, j, n;
	private int subset[];
	private boolean flag = false;

	public NextSubset(int n) {
		this.n = n;
		this.subset = new int[n];
	}

	private void calc() {
		subset[j] = 1 - subset[j];
		k = k + 2 * subset[j] - 1;
	}

	private int[] calcEven() {
		if (flag) {
			return null;
		}
		calc();
		if (k == subset[n - 1]) {
			flag = true;
		}
		return subset;

	}

	private int[] calcOdd() {
		if (flag) {
			return null;
		}
		do {
			j++;
		} while (subset[j - 1] != 1);
		calc();
		if (k == subset[n - 1]) {
			flag = true;
		}
		return subset;
	}

	private int[] algorithm() {
		t = k % 2;
		j = 0;
		return subset = (t == 0 ? calcEven() : calcOdd());
	}

	private void nextSubset() throws OutOfRangeException {
		subset = algorithm();
		if (subset == null) {
			throw new OutOfRangeException();
		}
	}

	public void nextSubsetAlgorithm() {
		try {
			nextSubset();
		} catch (OutOfRangeException error) {
			error.printStackTrace();
		}
	}

	public int[] getSubset() {
		return subset;
	}

	public int[] getSerialSubset(int serial) {
		this.subset = new int[n];
		for (int g = 1; g < serial; g++) {
			nextSubsetAlgorithm();
		}
		return subset;
	}

	public String getOutPut() {
		StringBuffer k1 = new StringBuffer();
		for (int h = 0; h < n; h++) {
			k1.append(subset[h] + " ");
		}
		return k1.toString();
	}

	public boolean isLastSubset() {
		return flag;
	}

	public static void main(String args[]) {
		NextSubset test = new NextSubset(5);
		System.out.println("1\t->\t" + test.getOutPut());
		int count = 2;
		while (!test.isLastSubset()) {
			test.nextSubsetAlgorithm();
			System.out.println(count + "\t->\t" + test.getOutPut());
			count++;
		}
	}
}
