package br.com.gm2.routines.java.next.permutation;

/**
 * Java implementation - Documentation: Combinatorial Algorithms, WILF /
 * NIJENHUIS, page 54
 * 
 * @author Glaucio Melo.
 */
public class NextPermutation {

	private int[] a;
	private int[] d;
	private boolean flag;
	private boolean end = false;

	private int n;

	public NextPermutation(int n) {
		this.n = n;
		initialize(n);
	}

	public int[] actual() {
		return a;
	}

	public int[] offset() {
		return d;
	}

	public void allPermutations() {
		// Algorithm knows how to stop to generate permutations.
		// Generating all possible permutations at once
		int r = 0;
		do {
			System.out.println(outPut(actual(), r));
			nextper();
			r++;
		} while (!isLastPermutation());
	}

	public void allPermutationsNoOutput() {
		// Algorithm knows how to stop to generate permutations.
		// Generating all possible permutations at once
		do {
			nextper();
		} while (!isLastPermutation());
	}

	private void initialize(int n) {
		if (n < 2) {
			throw new IndexOutOfBoundsException();
		} else {
			a = new int[n];
			d = new int[n - 1];
			flag = false;
			// Generating Identity Permutation
			for (int j = 0; j < n; j++) {
				a[j] = j;
			}
		}
	}

	public boolean isLastPermutation() {
		return end;
	}

	public int[] nextper() {
		return nextper(1);
	}

	private int[] nextper(int i) {
		int value = sum(i);
		if (!flag) {
			sumNeuter();
		} else if (value == -1) { // reached the last permutation
			end = true;
		} else if ((!odd(value) || (d[i] > i)) && (odd(value) || (d[i] <= 0))) {
			return nextper(i + 1);
		} else if (odd(value)) {
			sumOdd(i);
		} else {
			sumEven(i);
		}
		return a;
	}

	/**
	 * Check if a given number is odd.
	 */
	private boolean odd(int j) {
		return j % 2 == 1;
	}

	public String outPut(int[] a, int r) {
		if (isLastPermutation()) {
			return "";
		}
		return outPut(a, r, "(", ",", ")");
	}

	public String outPut(int[] a, int r, String open, String space, String close) {
		StringBuffer str = new StringBuffer();
		str.append((r + 1) + " ->\t" + open);
		for (int j = 0; j < n - 1; j++) {
			str.append((a[j] + 1) + space);
		}
		str.append((a[n - 1] + 1) + close);
		str.append("\t" + open + d[0] + space);
		for (int j = 1; j < n - 2; j++) {
			str.append(d[j] + space);
		}
		str.append(d[n - 2] + close);
		return str.toString();
	}

	private int searchForEven(int i, int x) {
		int w = 0;
		for (int j = 0; j <= i; j++) {
			if ((a[j] > a[i + 1]) && (a[j] < x)) {
				x = a[j];
				w = j;
			}
		}
		return w;
	}

	private int searchForOdd(int i, int x) {
		int w = 0;
		for (int j = 0; j <= i; j++) {
			if ((a[j] < a[i + 1]) && (a[j] > x)) {
				x = a[j];
				w = j;
			}
		}
		return w;
	}

	/**
	 * Sum all elements from the offset vector
	 */
	private int sum(int i) {
		int s = 0;
		for (int j = 0; j <= i; j++) {
			// This mark the end of possible permutations, when offset reaches
			// its limit.
			if (j == d.length) {
				s = -1;
			} else {
				s += d[j];
			}
		}
		return s;
	}

	private void sumEven(int i) {
		swap(a, i + 1, searchForEven(i, n), false);
		d[i] = Math.abs(d[i] - 1);
	}

	private void sumNeuter() {
		swap(a, 0, 1, true);
		d[0] = Math.abs(d[0] - 1);
	}

	private void sumOdd(int i) {
		swap(a, i + 1, searchForOdd(i, -1), false);
		d[i]++;
	}

	private void swap(int a[], int k, int f, boolean t) {
		int j = a[k];
		a[k] = a[f];
		flag = t;
		a[f] = j;
	}

}