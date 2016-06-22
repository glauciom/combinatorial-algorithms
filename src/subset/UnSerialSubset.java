/*
 * Created on 02/11/2004
 *
 * TODO To change the template for this generated file go to
 * Window - Preferences - Java - Code Style - Code Templates
 */
package subset;

/**
 * @author GLAUCIO
 *
 * TODO To change the template for this generated type comment go to
 * Window - Preferences - Java - Code Style - Code Templates
 */
public class UnSerialSubset {

	private int[] subset;
	private int n;
	
	public UnSerialSubset() {
	}
	
	// Potencia em base 2.
	private  int p(int b) {
	 return (int) Math.pow(2,(int) b);
	}
	
	public int unserialAlgorithm(int[] subset) {
		int serial = 1;
		this.subset = subset;
		this.n = subset.length;
		boolean p = true;
		for(int i = n-1; i >= 0; i--) {
			if (p ^ subset[i] == 1) {
				 p = true;
			} else {
				p = false;
				serial += p(i);
			}
		}
		return serial;
	}
	
	public static void main(String[] args) {
		int n = 4;
		UnSerialSubset subset = new UnSerialSubset();
		SerialSubset sub = new SerialSubset(n);
		for (int i = 1; i <= subset.p(n); i++) {
			System.out.println(subset.unserialAlgorithm(sub.algorithm(i)));
		}		
	}
}
