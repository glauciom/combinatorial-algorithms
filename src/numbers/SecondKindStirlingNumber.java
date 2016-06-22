/*
 * Created on 25/06/2004
 *
 * To change the template for this generated file go to
 * Window&gt;Preferences&gt;Java&gt;Code Generation&gt;Code and Comments
 */
package numbers;

/**
 * @author glaucio
 *
 * To change the template for this generated type comment go to
 * Window&gt;Preferences&gt;Java&gt;Code Generation&gt;Code and Comments
 */
public class SecondKindStirlingNumber {

	private int n, k;

	public SecondKindStirlingNumber(int n, int k) {
		this.n = n;
		this.k = k;
	}

	public static long getStirlingSecondKind(int n, int k) {
		long result = 0;
		for (int i = 0; i <= k - 1; i++)
			result += ((long) Math.pow((double) - 1, (double) i))
				* BinomialCoefficient.getBinomialElements(k, i)
				* ((long) Math.pow((double) k - i, (double) n));
		return ((long) result / BinomialCoefficient.factorial(k, 1));
	}

	public String toString() {
		return "" + getStirlingSecondKind(n, k);
	}

	public static long B(int n) {
		long res = 0;
		for (int i = 1; i <= n; i++) {
			res += SecondKindStirlingNumber.getStirlingSecondKind(n, i);
			System.out.print(SecondKindStirlingNumber.getStirlingSecondKind(n, i)+ " ");
		}
		return res;
	}
	public static void main(String[] args) {
		int n = 6;
		System.out.println(SecondKindStirlingNumber.B(n));
	}
}
