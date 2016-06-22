/*
 * Created on 18/03/2004
 *
 * To change the template for this generated file go to
 * Window&gt;Preferences&gt;Java&gt;Code Generation&gt;Code and Comments
 */
package numbers;

/**
 * @author Glaucio
 *
 * To change the template for this generated type comment go to
 * Window&gt;Preferences&gt;Java&gt;Code Generation&gt;Code and Comments
 */
public class BellNumbers {

	private int n;
	
	public BellNumbers(int n) {
		this.n = n;
	}
	
	public BellNumbers() {
	}
	
	public long B(int n) {
		this.n = n;
		long res = 0;
		for (int i = 1; i <= n; i++) { 
			res += SecondKindStirlingNumber.getStirlingSecondKind(n, i);
	//		System.out.print(SecondKindStirlingNumber.getStirlingSecondKind(n, i) + " ");
		}
			
		return res;
	}
	
	public String toString() {
		return ""+B(n);
	}
	
	public static void main(String[] args) {
		BellNumbers B = new BellNumbers();
		for (int i = 1; i <= 10; i++) {
			System.out.println(B.B(i));	
	//		System.out.println(B);
		}
	}
}
