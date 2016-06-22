/*
 * Created on 01/11/2004
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
public class SerialSubset {

	private int n;
	private int[] subset;
	public SerialSubset(int n) {
		this.n = n;
	}
	
	// Potencia em base 2.
	private  int p(int b) {
	 return (int) Math.pow(2,(int) b);
	}
	
	public int[] algorithm(int serial) {
		this.subset = new int[n];
		int index = 0;
		for (int i = 0; i < n; i++) {			
			int res = serial - 1 + p(i);
			res = res % p(i+2);
			res = (int) (res / p(i+1));
			subset[i] = res;
		}
		
		return subset;
	}
	
	public String toString() {
		StringBuffer k1 = new StringBuffer();
		for (int i = 0; i < subset.length; i++) {
			k1.append(subset[i] + " ");
		}
		return k1.toString();
	}
	
	public static void main(String[] args) {
		int n = 5;
		SerialSubset subset = new SerialSubset(n);
		long r = System.currentTimeMillis();
		for (int i = 1; i <= subset.p(n); i++) {
			int result[] = subset.algorithm(i);
			System.out.println(i + "-"+subset);
		}
		int[] result = subset.algorithm(subset.p(n));
		//System.out.println("tempo = "+(System.currentTimeMillis() - r));
	//	System.out.println(subset);
		NextSubset test = new NextSubset(n);
		r = System.currentTimeMillis();
		while (!test.isLastSubset()) {
			 test.nextSubsetAlgorithm();
		   }
	//	System.out.println("tempo = "+(System.currentTimeMillis() - r));
	//	System.out.println(test.getOutPut());
	}
}
