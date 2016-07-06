package br.com.gm2.routines.java.next;

/**
 * Java implementation - Documentation: Combinatorial Algorithms, WILF /
 * NIJENHUIS, page 13
 * 
 * @author Glaucio Melo.
 */
public class FullGrayCode {

	private int i, j, g[];
	private int n;

	public FullGrayCode(int n) {
		this.n = n;
		g = new int[p(n)];
		gray(this.n);
	}

	private void gray(int n) {
		g[0] = 0;
		g[1] = 1;
		for (j = 2; j <= n; j++) {
			for (i = p(j - 1); i <= p(j) - 1; i++) {
				if (i - p(j - 1) <= p(j - 2) - 1) {
					g[i] = g[i - p(j - 1)] + p(j - 1) + p(j - 2);
				} else {
					g[i] = g[i - p(j - 1)] + p(j - 2);
				}
			}
		}
	}

	private int p(int b) {
		return (int) Math.pow(2, (int) b);
	}

	public String getIntegerOutPut() {
		StringBuffer k1 = new StringBuffer();
		for (j = 0; j < p(n); j++) {
			k1.append(g[j] + " ");
		}
		return k1.toString();
	}

	public String getBinaryOutPut() {
		StringBuffer k1 = new StringBuffer();
		for (i = 0; i < p(n); i++) {
			k1.append((i + 1) + "\t->\t");
			for (j = p(n - 1); j > 0; j = j / 2) {
				k1.append(((g[i] & j) == 0 ? "0 " : "1 "));
			}
			k1.append("\n");
		}
		return k1.toString();
	}

	public static void main(String[] args) {
		FullGrayCode g = new FullGrayCode(10);
		System.out.println(g.getIntegerOutPut());
		System.out.println(g.getBinaryOutPut());
	}
}
