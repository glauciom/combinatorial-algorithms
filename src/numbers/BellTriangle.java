/*
 * Created on 22/09/2004
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
public class BellTriangle {


private int n;
/**
 * Construtor para BellTriangle
 *
 */
public BellTriangle(int n) {
	this.n = n;
}

public int G(int n, int f) {
	BellNumbers b = new BellNumbers();
	int result = 1;
	for (int i = 0; i <= n-f; i++) {
		long r = b.B(i);
		long cc11 = BinomialCoefficient.getBinomialElements((int) n,(int) i);
		result += (int) (r * cc11);
		//System.out.println(result);
	}
	return result;
}

public static void main(String[] args) {
	BellTriangle bt = new BellTriangle(7);
	System.out.println(bt.G(3,3));
}
}
